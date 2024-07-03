#===============================================================================
# Description: Prepare working environment
# author: Loic Henry and El Ghali Debbagh
# Contact loic.henry@dauphine.psl.eu
#===============================================================================

# Install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, readr, sp, raster, sf, here, tmap, rvest, archive)

# Then: names of other folders in the R project folder
dir$raw_data <- paste0(dir$root, "/Raw_Data") 
dir$rpg_data <- paste0(dir$raw_data, "/RPG_Data")
dir$departments <- paste0(dir$raw_data, "/Departments_Shp")
dir$communes <- paste0(dir$raw_data, "/Communes_Shp")
dir$prep_data <- paste0(dir$root, "/Prepared_Data")
dir$rcode <- paste0(dir$root, "/RCode")
dir$figures <- paste0(dir$root, "/Figures")

# Create folders in working directory, only if not existing
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F)) 
