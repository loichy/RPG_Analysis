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

##### Only required package to initiate is pacman
# Install and load packages (other packages are install and loaded from "00_PrepareEnvironment.R")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)


#####  Precised relative access to useful Rcodes
dir <- list() # First create an empty list, that we then fill with subobjects
dir$root <- getwd() # First: the place of the R project
dir$rcode <- paste0(dir$root, "/RCode")

##### Prepare working environment
# Run R scrit "00"
source(here(dir$rcode, "00_PrepareEnvironment.R"))

##### Extract all RPG data files for url page of IGN, and load RPG data documentation on labels
# Run Rscript 01
source(here(dir$rcode, "01_RPGFileNames.R")) 
# cREATE THE ALL_RPG_LINKS OBJECT WHICH CONTAINS ALL URL LINKS TO DOWNLOAD DATA, AND THE CORRESPONDING REGION NAME AND YEAR

##### Load function that load and prepare all RPG data files

# First funcction: download data
# Takes approximately five hours (DO NOT RUN!)
# for(i in seq(all_rpg_links$url)){
#   # i <- 13
#   print(paste(all_rpg_links$region_code[i], " for year ", all_rpg_links$year[i]))
#   # Create file where to download
#   destfile_zip <- here(dir$rpg_data, paste0("tempfile_zip_", all_rpg_links$region_code[i], "_", all_rpg_links$year[i]))
#   dir.create(destfile_zip)
#   # Download the file
#   options(timeout=1000)
#   download.file(all_rpg_links$url[i], here(destfile_zip,"temp.001"), mode = "wb", quiet = T )
#   # Print a message indicating the download is complete
#   print(paste("File downloaded to", destfile_zip))
#   Sys.sleep(1)
#   
#   # Specify the destination file path
#   
#   destfile <- here(dir$rpg_data, paste0("tempfile_", all_rpg_links$region_code[i], "_", all_rpg_links$year[i]))
#   dir.create(destfile)
#   
#   # Unzip file
#   # archive(here(destfile_zip, "temp.001"))
#   archive_extract(archive = here(destfile_zip,"temp.001"), dir = here(destfile))
#   
#   # Function to find the .shp file
#   find_pg_shp_file <- function(directory) {
#     shp_files <- list.files(destfile, pattern = "PARCELLES_GRAPHIQUES\\.shp$", recursive = TRUE, full.names = TRUE)
#     if (length(shp_files) > 0) {
#       return(shp_files[1])  # Return the first matching .shp file
#     } else {
#       return(NULL)
#     }
#   }
#   
#   find_ia_shp_file <- function(directory) {
#     shp_files <- list.files(destfile, pattern = "ILOTS_ANONYMES\\.shp$", recursive = TRUE, full.names = TRUE)
#     if (length(shp_files) > 0) {
#       return(shp_files[1])  # Return the first matching .shp file
#     } else {
#       return(NULL)
#     }
#   }
#   
#   # Find the PARCELLES_GRAPHIQUES.shp file
#   pg_shp_file <- find_pg_shp_file(destfile)
#   # Find the PARCELLES_GRAPHIQUES.shp file
#   ia_shp_file <- find_ia_shp_file(destfile)
#   
#   
#   if (!is.null(pg_shp_file)) {
#     print(paste("Shapefile 'PARCELLES_GRAPHIQUES' exists:", pg_shp_file))
#     
#     # Additional processing can go here
#   } else if (!is.null(ia_shp_file)){
#     print(paste("Shapefile 'ILOTS_ANONYMES' is the only one:", ia_shp_file))
#   
#   } else {
#     print("No shapefile found")
#   }
#   
#   if (dir.exists(here(destfile_zip))) {
#     #Delete file if it exists
#     unlink(here(destfile_zip), recursive = T)
#   }
#   
#   Sys.sleep(1)
# }

# Run Rscript "02" that creates function which load and  data prepares all RPG files individually
source(here(dir$rcode, "02_Load_RPG.R"))
# Apply function to all files
# with sapply
system.time(
results <- sapply(X = seq_along(all_rpg_links$url), FUN = PrepareRPGData, simplify = FALSE)
)

# Test with parallel computing (not working yet) using future package
# no_cores <- availableCores() - 1 # Number of core/clusters
# plan(multisession, # Parameters of the parallel computing session
#      workers = 14 # Number of cores
#      )
# options(future.globals.maxSize = 8000 * 1024^2)
# load(here(dir$prep_data, "PrepareData_RFunction.Rdata"))  # Load function to downlaod and prepare RPG data on each core
# list_object <- as.list(seq_along(all_rpg_links$url))# which(all_rpg_links$region_code %in% c("R94")) # seq_along(all_rpg_links$url)) #[c(13,26,39,52,65)],39,52,65,78,91,104,130,182
# tic() # Count total time
# results <- future_map(list_object, PrepareRPGData) # Apply PrepareRPGData function to all list_object elements in a parallel computing session
# toc()

##### Compile all individual prepared RPG file aggregated at the commune level to create a single global dataframe
# Get all rpg file names
RPG_files <- list.files(dir$prep_rpg_data, full.names = T)
# Create an empty list, where we will save them
df_list <- list()
# For loop over all rpg files
for (file in RPG_files) {
  load(file) # Load the file
  df_list[[file]] <- get("result_i") # Put the file into the list
}

# Combine all RPG files into a single data frame
combined_df <- do.call(rbind, df_list)

# Correct data preparation: communes that appear twice, as their field are at the border of two regions -> add those fields to the original commune data
temp <- combined_df %>% 
  group_by(year, geo_unit, CODE_GROUP) %>% 
  mutate(surf_cult_m2 = sum(surf_cult_m2),
         parcel_cult_n = sum(parcel_cult_n)) %>% 
  ungroup() %>% 
  group_by(year, geo_unit) %>% 
  mutate(max_agri = max(surf_agri_geo_unit_m2)) %>% 
  filter(surf_agri_geo_unit_m2 == max_agri) %>% 
  mutate(surf_agri_geo_unit_m2 = sum(surf_cult_m2),
         N_Parcels = sum(parcel_cult_n),
         surf_cult_perc = surf_cult_m2 / surf_agri_geo_unit_m2,
         parcel_cult_perc = parcel_cult_n / N_Parcels) %>% 
  ungroup()

RPG_20072010 <- temp

# save object
save(RPG_20072010, file = here(dir$prep_data, "RPG_20072010.Rdata")) 

# Add geometry column for all communes
load(here(dir$prep_data, "RPG_20072010.Rdata"))  # Load created file
# load(here(dir$prep_data, "RPG_R94_sf.Rdata"))
contours <- st_read(here(dir$communes,"communes-20220101.shp")) %>% 
  mutate(geo_unit = insee)
# Add geometry variable
results_df_contours <- inner_join(RPG_20072010 %>% as.data.frame(), 
                                  contours %>% as.data.frame(), 
                                  by = "geo_unit")

# Convert df aas a sf object
RPG_20072010_sf <- results_df_contours  %>%  
  st_as_sf() %>% 
  dplyr::select(geo_unit, name, region, year,data_type, surf_tot_geo_unit_m2, surf_agri_geo_unit_m2, N_Parcels,
                CODE_GROUP, cult_label, surf_cult_m2, surf_cult_perc, parcel_cult_n, parcel_cult_perc)
  
# # Test to plot
# test <- rpg_r94_sf %>%
#   group_by(name) %>%
#   slice(1)
# fig_temp3 <- ggplot() +
#   geom_sf(data=test)
# ggsave(plot = fig_temp3, filename = here(dir$figures, "fig_temp3.pdf"), width = 16, height = 9)

# Save object
save(RPG_20072010_sf, file = here(dir$prep_data, "RPG_20072010.Rdata"))
