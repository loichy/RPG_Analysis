rpg_map <- st_read(rpg_data)
result_i <- rpg_map # culture_bydep
# The first part for the ILOTS ones 


rpg_map_area <- rpg_map %>% 
  mutate(
    AREA_PARC = st_area(geometry),#To have it in m²
    CODE_GROUP = CODE_CULTU
  ) %>% 
  dplyr::select(-CODE_CULTU) 
# sum(rpg_map_area$Area_parcels)
# sum(rpg_map_area$SURF_PARC)

rpg_map_small <- rpg_map_area %>% # Take only 10% of all parcels randomly to work on a random smaller subsample
  slice_sample(n = round(0.1*length(rpg_map_area$NUM_ILOT)))

## deparments 
# departments <- st_read(here(dir$departments, "map_fr_dept_remaked.shx.shp"))
# Transpose CRS to have the same as RPG files CRS:
contours_rgf93 <- st_transform(contours, crs = st_crs(rpg_map_area))

rpg_joined <- st_join(contours_rgf93, rpg_map_small) # To test, use rpg_map small
rpg_joined_filtered <- rpg_joined %>% 
  filter(!is.na(NUM_ILOT))
# temp_disappear_afterjoin <- rpg_map_small %>%
#   filter(! NUM_ILOT %in% c(rpg_joined_filtered$NUM_ILOT))
# twice_afterjoin <- rpg_joined_filtered %>% 
#   group_by(NUM_ILOT) %>% 
#   mutate(count_ilot = n()) %>% 
#   filter(count_ilot == 2)
# temp_twice_afterjoin <- rpg_map_small %>% 
#   filter(NUM_ILOT %in% c(twice_afterjoin$NUM_ILOT))
# departments_ara <- departments_rgf93 %>% 
#   filter(insee_dep %in% c("74","73","69","63","43","42","38","26","15","7","3","1"))
# ara_rpg_plot <- ggplot() +
#   geom_sf(data = departments_ara) +
#   geom_sf(data = rpg_map_small, 
#           mapping = aes(fill=CODE_CULTU), 
#           lwd = 0) +
#   geom_sf(data = temp_twice_afterjoin, 
#           fill="red",
#           lwd = 0)
# ggsave(filename = here(dir$figures, "rpg_ara_2007_3.pdf"), plot = ara_rpg_plot, width = 9, height = 16)


# table(temp_twice_afterjoin$count_ilot)
#   filter(is.na(NUM_ILOT))
# length(unique(rpg_map_small$NUM_ILOT))
# length(unique(rpg_joined$NUM_ILOT))
# joindre sans déterminer les départements de la région.


rpg_joined_dep <- rpg_joined_filtered %>%
  group_by(geo_unit) %>% #First: construct a variable that for each parcel gives the surface of the agricultural area of the department in which it is located
  mutate(Surf_Agri_Dep = sum(AREA_PARC), # Surface of all agricultural parcels in the department
         N_Parcels      = n()
  ) %>% # Number of parcels in the department
  ungroup()
# table(corse_joined_dep$Surf_Agri_Dep) # Just to check the results
# table(corse_joined_dep$N_Parcels)

result_i <- rpg_joined_dep %>% # Object that contain descriptive statistics
  group_by(geo_unit, CODE_GROUP) %>% # Aggregate by culturer
  summarise(SURF_km2 = sum(AREA_PARC, na.rm = T),# Surface of parcels dedicated to each culture in each department
            SURF_perc = sum(AREA_PARC, na.rm = T) / Surf_Agri_Dep[1], # Percentage in the total agricultural area of the department 
            n_parcels  = n(),
            perc_parcels = n_parcels / N_Parcels[1])

# la fonction source ? 

