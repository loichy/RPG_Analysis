rpg_map <- st_read(rpg_data)

rpg_map <- rpg_map %>% 
  mutate(
    AREA_PARC = st_area(geometry) #To have it in m²
  ) # Ajouter le libellé! Nom variable: NOM_CULTU
# sum(rpg_map$Area_parcels)
# sum(rpg_map$SURF_PARC)

rpg_map_small <- rpg_map %>% # Take only 10% of all parcels randomly to work on a random smaller subsample
  slice_sample(n = round(0.1*length(rpg_map$ID_PARCEL)))

contours_rgf93 <- st_transform(contours, crs = st_crs(rpg_map))

rpg_joined <- st_join(contours_rgf93, rpg_map_small) # To test, use rpg_map_small to be quicker
rpg_joined_filtered <- rpg_joined %>% 
  filter(!is.na(ID_PARCEL))

rpg_joined_dep <- rpg_joined_filtered %>%
  group_by(geo_unit) %>% #First: construct a variable that for each parcel gives the surface of the agricultural area of the department in which it is located
  mutate(Surf_Agri_Dep = sum(AREA_PARC), # Surface of all agricultural parcels in the department
         N_Parcels      = n()
  ) %>% # Number of parcels in the department
  ungroup()
# table(rpg_joined_dep$Surf_Agri_Dep) # Just to check the results
# table(rpg_joined_dep$N_Parcels)

result_i <- rpg_joined_dep %>% # Object that contain descriptive statistics
  group_by(geo_unit, CODE_GROUP) %>% # Aggregate by culturer
  summarise(SURF_km2 = sum(AREA_PARC, na.rm = T),# Surface of parcels dedicated to each culture in each department
            SURF_perc = sum(AREA_PARC, na.rm = T) / Surf_Agri_Dep[1], # Percentage in the total agricultural area of the department 
            n_parcels  = n(),
            perc_parcels = n_parcels / N_Parcels[1])


