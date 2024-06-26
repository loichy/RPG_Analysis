#===============================================================================
# Description: Load RPG shp data 
# author: Loic Henry and El Ghali Debbagh
# Contact loic.henry@dauphine.psl.eu
#===============================================================================

#===============================================================================
# 1). Preliminary ------
#===============================================================================
# Clean and load packages
rm(list=ls())
gc()

# Clean and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, readr, sp, raster, sf, here, tmap)

# Objects that specify the file route
dir <- list() # First create an empty list, that we then fill with subobjects
dir$root <- getwd() # First: the place of the R project
# Then: names of other folders in the R project folder
dir$rpgdata_corse2022 <- paste0(dir$root, "/RPG_Corse_2022") 
dir$rpgdata_idf2022 <- paste0(dir$root, "/RPG_IDF_2022")
dir$departements <- paste0(dir$root, "/departements-20230101-shp")
dir$prepdata <- paste0(dir$root, "/Prepared_data")
dir$figures <- paste0(dir$root, "/Figures")
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F)) # This code create the folder if not already existing, if it exists already, it does nothing

#===============================================================================
# 2). Load agricultural field data ------
#===============================================================================
# RPG shp files have always the same name (whatever the region and name):
shp_rpg_name <- "/PARCELLES_GRAPHIQUES.shp"

# Corsica 2022
shp_rpg_corsica_name <- paste0(dir$rpgdata_corse2022, shp_rpg_name) # Absolute path to the shp file
rpg_corsica_map <- st_read(shp_rpg_corsica_name) # package sf to open shp data
rpg_corsica_map <- rpg_corsica_map %>% 
  mutate(
    AREA_PARC = st_area(geometry) #To have it in m²
  )
sum(rpg_corsica_map$Area_parcels)
sum(rpg_corsica_map$SURF_PARC)

rpg_corsica_map_small <- rpg_corsica_map %>% # Take only 10% of all parcels randomly to work on a random smaller subsample
  slice_sample(n = round(0.1*length(rpg_corsica_map$ID_PARCEL)))

# IDF 2022
shp_rpg_idf_name <- paste0(dir$rpgdata_idf2022, shp_rpg_name) # Absolute path to shp file for Corsica
rpg_idf_map <- st_read(shp_rpg_idf_name) # 
rpg_idf_map_small <- rpg_idf_map %>% # Take only 10% of all parcels randomly to work on a random smaller subsample
  slice_sample(n = round(0.1*length(rpg_idf_map$ID_PARCEL)))


#===============================================================================
# 3). Plot parcel geometry ------
#===============================================================================
# I only plot the small subset as it is too slow for my computer
# (this is not needed in your code to clean data: it is just to check the different maps and vizualise it)
corsica_rpg_plot <- ggplot(data = rpg_corsica_map_small) +
  geom_sf(mapping = aes(fill=CODE_GROUP), lwd = 0)
ggsave(filename = here(dir$figures, "rpg_corsica_small_2022.pdf"), plot = corsica_rpg_plot, width = 9, height = 16)
idf_rpg_plot <- ggplot(data = rpg_idf_map_small) +
  geom_sf(mapping = aes(fill=CODE_GROUP), lwd = 0)
ggsave(filename = here(dir$figures, "rpg_idf_small_2022.pdf"), plot = idf_rpg_plot, width = 16, height = 9)

#===============================================================================
# 4) Load department data ------
#===============================================================================
# Import shp file:
departments <- st_read(here(dir$departements, "departements-20230101.shp"))
# Transpose CRS to have the same as RPG files CRS:
departments_rgf93 <- st_transform(departments, crs = st_crs(rpg_corsica_map))
# Create smaller sf objects of departments by regions:
departments_idf <- departments_rgf93 %>% 
  filter(code_insee %in% c("75", "77", "78", "91", "92", "93", "94", "95"))
departments_corsica <- departments_rgf93 %>% 
  filter(code_insee %in% c("2A", "2B"))

#===============================================================================
# 5) Superimpose parcels and department boundaries to check same CRS systems ------
#===============================================================================
# Thos plots are not needed in the cleaning code, but they can be useful in a .RMD document that
# aims at showing our procedure to aggregate data at a larger level
corsica_rpg_plot2 <- ggplot() +
  geom_sf(data = departments_corsica) +
  geom_sf(data = rpg_corsica_map_small, 
          mapping = aes(fill=CODE_GROUP), lwd = 0) 
ggsave(filename = here(dir$figures, "rpg_corsica_small_2022_2.pdf"), plot = corsica_rpg_plot, width = 9, height = 16)
idf_rpg_plot2 <- ggplot() +
  geom_sf(data = departments_idf) +
  geom_sf(data = rpg_idf_map_small,
          mapping = aes(fill=CODE_GROUP), lwd = 0)
ggsave(filename = here(dir$figures, "rpg_idf_small_2022_2.pdf"), plot = idf_rpg_plot2, width = 16, height = 9)

# An alternative to plot using the package tmap (perfectly equivalent as geom_sf), I don't know which one you prefer
idf_rpg_plot3 <- tm_shape(rpg_idf_map_small) +
  tm_polygons(col = "CODE_GROUP", lwd = 0) +
  tm_shape(departments_idf) +
  tm_polygons(alpha = 0) +
  tm_layout(frame = FALSE, legend.outside = TRUE)
tmap_save(tm = idf_rpg_plot3, filename = here(dir$figures, "rpg_idf_small_2022_3.pdf"), width = 16, height = 9)
#===============================================================================
# 6) Get parcels in each department ------
#===============================================================================
# Check commands here: https://tmieno2.github.io/R-as-GIS-for-Economists/spatial-join.html
corse_joined <- st_join(departments_corsica, rpg_corsica_map_small) # First: target layer (department layer) and second: source layer (agricultural parcels)

temp_corsica_joined <- corse_joined %>% 
  slice_sample(n = 100)

# Plot geometry
# With tmap
temp_corsica_joined_plot <-  tm_shape(temp_corsica_joined) +
  tm_polygons() +
  tm_layout(frame = FALSE)
tmap_save(filename = here(dir$figures, "corsica_joined_map.pdf"),tm = temp_corsica_joined_plot, width = 9, height = 16)
# With geom_sf (perfectly equivalent as tmap (maps ar slightly different though))
corsica_joined_plot2 <- ggplot() +
  geom_sf(data = temp_corsica_joined) 

#===============================================================================
# 7) Summary statistics of types of crops by department ------
#===============================================================================

corse_joined_dep <- corse_joined %>%
  group_by(code_insee) %>% #First: construct a variable that for each parcel gives the surface of the agricultural area of the department in which it is located
  mutate(Surf_Agri_Dep = sum(SURF_PARC), # Surface of all agricultural parcels in the department
         N_Parcels      = n()
         ) %>% # Number of parcels in the department
  ungroup()
table(corse_joined_dep$Surf_Agri_Dep) # Just to check the results
table(corse_joined_dep$N_Parcels)

corse_culture_bydep <- corse_joined_dep %>% # Object that contain descriptive statistics
  group_by(code_insee, CODE_GROUP) %>% # Aggregate by culturer
  summarise(SURF_km2 = sum(SURF_PARC, na.rm = T),# Surface of parcels dedicated to each culture in each department
            SURF_perc = sum(SURF_PARC, na.rm = T) / Surf_Agri_Dep[1], # Percentage in the total agricultural area of the department 
            n_parcels  = n(),
            perc_parcels = n_parcels / N_Parcels[1])

# Test plot:
corsica_cultbydep_plot <- tm_shape(corse_culture_bydep) +
  tm_polygons()  
tmap_save(filename = here(dir$figures, "corsica_cultbydep_map.pdf"),tm = corsica_cultbydep_plot, width = 9, height = 16)

#===============================================================================
# 8) Save intermediary data ------
#===============================================================================

# Save intermediary data
# Rdata
save(dir, corse_culture_bydep, file = here(dir$prepdata, "corse_joined.Rdata"))

# Format shp
st_write(obj = corse_culture_bydep,
         dsn = here(dir$prepdata, "corse_joined.shp"), 
         delete_dsn = TRUE)


tempor <- st_read(here(dir$prepdata, "corse_joined.shp"))

