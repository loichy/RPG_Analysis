#===============================================================================
# Description: Master RCode that compiles all RPG shapefiles and aggregate parcel
# data at the department or communal level
# author: Loic Henry and El Ghali Debbagh
# Contact loic.henry@dauphine.psl.eu
#===============================================================================

# Important note: to run this code, open first the R project "RPGAnalysis.Rproj"

##### Clean memory
rm(list=ls())
gc()

##### Only required package to initiate
# Clean and load packages (other packages are install and loaded from "00_PrepareEnvironment.R")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)


#####  Precised relative access to useful Rcodes
dir <- list() # First create an empty list, that we then fill with subobjects
dir$root <- getwd() # First: the place of the R project
dir$rcode <- paste0(dir$root, "/RCode")

##### Prepare working environment
source(here(dir$rcode, "00_PrepareEnvironment.R"))

##### Extract all RPG data files
source(here(dir$rcode, "01_RPGFileNames.R")) 
# cREATE THE ALL_RPG_LINKS OBJECT WHICH CONTAINS ALL URL LINKS TO DOWNLOAD DATA, AND THE CORRESPONDING REGION NAME AND YEAR

##### Load, prepare and append all rpg shapefiles

# Open french communes shapefile (that is used to aggregate parcels at the commune level)
contours <- st_read(here(dir$communes,"communes-20220101.shp")) %>% 
  mutate(geo_unit = insee)

# Load culture labels
libelle <- read.csv(file = here(dir$rpg_documentation,"REF_CULTURES_GROUPES_CULTURES_2020.csv"), sep = ";") # Load libelle data
label_data <- libelle %>% 
  mutate(CODE_GROUP = as.character(CODE_GROUPE_CULTURE),
         LABEL_CODE_GROUP = LIBELLE_GROUPE_CULTURE) %>% 
  dplyr::select(CODE_GROUP, LABEL_CODE_GROUP) %>% # Keep relevant variables
  group_by(CODE_GROUP) %>% # Keep single observation by group
  slice(1) %>% 
  ungroup()

# Set download timeout
options(timeout=300)

# 1. ouvrir dans l'objet destfile les données avec le nom "parcelles grpahiques", sinon avec "ilot parcellaire"
# 2. Dans le cas de ilot parcellaire, regarder  si le code "load RPG" fonctionne (noms des variables similaires entre parcelles graphiques et ilot parecellaires?")
# Ajhouter dans le tableau issu du code "RPG Load data",  "corse_culture_bydep" (qu'il faudra renommer culture_bydep") ajouter le nom de la région
# Même chose avec l'année
# Rendre le code 02_Load_RPGFiles nettoyer des références à la corse
# Objectif: pouvoir le faire pour 3 éléments diofférents des liens de tlééchargement (1, 90 et 182)

# La boucle :
system.time(
results <- lapply(X = seq_along(all_rpg_links$url)[c(1,82,182)], FUN = function(i){
  # for(i in c(1,82,182)){ #c(1:length(all_rpg_links$url))
  
  print(paste("Iteration ", i, " for region ",all_rpg_links$region_name[i], " and for year ", all_rpg_links$year[i]))
  
  i <- 1
  # i <- 82
  # i <- 182
  
  # Create objects useful for iteration i
  region_i <- all_rpg_links$region_name[i]
  year_i <- all_rpg_links$year[i]
  url_i <- all_rpg_links$url[i]   # Specify the URL of the file to be downloaded
  
  # Prepare folders for downloading
  tf <- tempfile()
  # Specify the destination file path
  destfile_zip <- here(dir$rpg_data, "temp_zip")
  dir.create(destfile_zip)
  destfile <- here(dir$rpg_data, "tempfile")
  dir.create(destfile)
  
  # Download the file
  download.file(url_i, here(destfile_zip,"temp.001"), mode = "wb")
  
  # Print a message indicating the download is complete
  print(paste("File downloaded to", destfile))
  
  # Unzip file
  # archive(here(destfile_zip, "temp.001"))
  archive_extract(archive = here(destfile_zip,"temp.001"), dir = here(destfile))
  
  # Function to find the .shp file
  find_pg_shp_file <- function(directory) {
    shp_files <- list.files(destfile, pattern = "PARCELLES_GRAPHIQUES\\.shp$", recursive = TRUE, full.names = TRUE)
    if (length(shp_files) > 0) {
      return(shp_files[1])  # Return the first matching .shp file
    } else {
      return(NULL)
    }
  }
  
  find_ia_shp_file <- function(directory) {
    shp_files <- list.files(destfile, pattern = "ILOTS_ANONYMES\\.shp$", recursive = TRUE, full.names = TRUE)
    if (length(shp_files) > 0) {
      return(shp_files[1])  # Return the first matching .shp file
    } else {
      return(NULL)
    }
  }
  
  # Find the PARCELLES_GRAPHIQUES.shp file
  pg_shp_file <- find_pg_shp_file(destfile)
  # Find the PARCELLES_GRAPHIQUES.shp file
  ia_shp_file <- find_ia_shp_file(destfile)
  
  
  if (!is.null(pg_shp_file)) {
    print(paste("Shapefile 'PARCELLES_GRAPHIQUES' exists:", pg_shp_file))
    
    # Set the shapefile object name to be parcelles graphiques
    rpg_data <- pg_shp_file
    print(paste("Shapefile can be loaded:", rpg_data))
    
    # Additional processing can go here
  } else if (!is.null(ia_shp_file)){
    print(paste("Shapefile 'ILOTS_ANONYMES' is the only one:", ia_shp_file))
    
    # Set the shapefile object name to be ilots anonymes
    rpg_data <- ia_shp_file
    print(paste("Shapefile can be loaded:", rpg_data))
  } else {
    print("No shapefile found")
  }
  
  
  if (!is.null(rpg_data)) { # Apply to each RPG file (each url link, when it exists) the data preparation process
    
    if(!is.null(pg_shp_file)){
      system.time(source(here(dir$rcode, "02_Load_RPG_PG.R")))
    } else if (!is.null(ia_shp_file)){
      system.time(source(here(dir$rcode, "02_Load_RPG_IA.R")))
    }
    
    names(result_i) <- paste(region_i, year_i, sep="_")
    result_i
    
    # bind_raw
    # rbind
  } else {
    print(paste("The region", region_i, " does not have shapefile for year ", year_i))
  }
  
  # Delete file
  if (dir.exists(here(destfile_zip))) {
    #Delete file if it exists
    unlink(here(destfile_zip), recursive = T)
  }
  if (dir.exists(here(destfile))) {
    #Delete file if it exists
    unlink(here(destfile), recursive=TRUE)
  }
}
)
)
str(results)
