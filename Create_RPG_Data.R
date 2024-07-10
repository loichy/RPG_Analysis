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
# Load culture labels
libelle <- read.csv(file = here(dir$rpg_documentation,"REF_CULTURES_GROUPES_CULTURES_2020.csv"), sep = ";") # Load libelle data
label_data <- libelle %>% 
  mutate(CODE_GROUP = as.character(CODE_GROUPE_CULTURE),
         LABEL_CODE_GROUP = LIBELLE_GROUPE_CULTURE) %>% 
  dplyr::select(CODE_GROUP, LABEL_CODE_GROUP) %>% # Keep relevant variables
  group_by(CODE_GROUP) %>% # Keep single observation by group
  slice(1) %>% 
  ungroup() %>% 
  bind_rows(data.frame(CODE_GROUP = c("10", "12", "13", "27"),
                       LABEL_CODE_GROUP = c("Semences",
                                            "Gel industriel",
                                            "Autre gel",
                                            "Arboriculture")))
save(label_data, file = here(dir$prep_data, "label_rpg.Rdata"))

##### Load function that load and prepare RPG data
source(here(dir$rcode, "02_Load_RPG.R"))

##### Apply function that download, load in R and prepare RPG data to all RPG shapefiles 

# Test with sapply
# system.time(
# results <- sapply(X = seq_along(all_rpg_links$url)[c(13,26)], FUN = PrepareRPGData, simplify = FALSE)
# )
# utilisateur     système      écoulé 
# 253.83        8.68      719.91 

# Set up parallel computing
no_cores <- availableCores() - 1 # Number of core/clusters
plan(multisession, # Parameters of the parallel computing session
     workers = 7 # Number of cores
     )
options(timeout=600) # Set downloading timeout limit to ten minutes
load(here(dir$prep_data, "PrepareData_RFunction.Rdata"))  # Load function to downlaod and prepare RPG data on each core
list_object <- as.list(which(all_rpg_links$region_code %in% c("R11","R94")))# seq_along(all_rpg_links$url)) #[c(13,26,39,52,65)],39,52,65,78,91,104,130,182

# Parallel computing session
tic() # Count total time
results <- future_map(list_object, PrepareRPGData) # Apply PrepareRPGData function to all list_object elements in a parallel computing session
toc()

results_df <- do.call(rbind, results) # Unlist all results in a dataframe

save(results_df, file = here(dir$prep_data, "RPG_combined_R11_R94.Rdata")) # Save results

