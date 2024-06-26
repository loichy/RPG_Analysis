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

##### Apply to each RPG data (each url link) the data preparation process

# Avant la boucle (à l'extérieur): créer une data frame vide ou une liste qui est vide
tableau_final <- list()

# 1. ouvrir dans l'objet destfile les données avec le nom "parcelles grpahiques", sinon avec "ilot parcellaire"
# 2. Dans le cas de ilot parcellaire, regarder  si le code "load RPG" fonctionne (noms des variables similaires entre parcelles graphiques et ilot parecellaires?")
# Ajhouter dans le tableau issu du code "RPG Load data",  "corse_culture_bydep" (qu'il faudra renommer culture_bydep") ajouter le nom de la région
# Même chose avec l'année
# Rendre le code 02_Load_RPGFiles nettoyer des références à la corse
# Objectif: pouvoir le faire pour 3 éléments diofférents des liens de tlééchargement (1, 90 et 182)
tf <- tempfile()
# Specify the URL of the file to be downloaded
url <- all_rpg_links$url[82]
region <- all_rpg_links$region_name[82]

# Specify the destination file path
destfile_zip <- here(dir$rpg_data, "temp_zip")
dir.create(destfile_zip)
destfile <- here(dir$rpg_data, "tempfile")
dir.create(destfile)

# Download the file
download.file(url, here(destfile_zip,"temp.001"), mode = "wb")

# Print a message indicating the download is complete
print(paste("File downloaded to", destfile))

# Unzip file
archive(here(destfile_zip, "temp.001"))
archive_extract(archive = here(destfile_zip,"temp.001"), dir = here(destfile))

# Open file

#test
# Avant de supprimer:
# alimenter l'objet /rajouter à l'objet, le tableau final, à l'intérieur

# Delete file
if (dir.exists(here(destfile_zip))) {
  #Delete file if it exists
  unlink(here(destfile_zip), recursive = T)
}
if (dir.exists(here(destfile))) {
  #Delete file if it exists
  unlink(here(destfile), recursive=TRUE)
}

