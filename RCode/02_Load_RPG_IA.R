rpg_map <- st_read(rpg_data)

# The first part for the ILOTS ones 
rpg_map_area <- rpg_map %>% 
  mutate(
    AREA_PARC = st_area(geometry),#To have it in m²
    CODE_GROUP = CODE_CULTU
  ) %>% 
  dplyr::select(-CODE_CULTU) 

# Adapt CRS of contours to rpg data
contours_rgf93 <- st_transform(contours, crs = st_crs(rpg_map_area))

# Tell in which geo_unit falls each parcel
rpg_joined <- st_join(contours_rgf93, rpg_map_area) # To test, use rpg_map small
rpg_joined_filtered <- rpg_joined %>% 
  filter(!is.na(NUM_ILOT)) # Only keep geo_unit with at least one parcel

# Compute useful variable at the geo-unit level
rpg_joined_dep <- rpg_joined_filtered %>%
  group_by(geo_unit) %>% #First: construct a variable that for each parcel gives the surface of the agricultural area of the department in which it is located
  mutate(Surf_Agri_Tot = sum(AREA_PARC), # Surface of all agricultural parcels in the geo_unit
         N_Parcels      = n()
  ) %>% # Number of parcels in the geo_unit
  ungroup()

# Finally, create object that aggregates by culture and by geo_unit
result_i <- rpg_joined_dep %>% # Object 
  group_by(geo_unit, CODE_GROUP) %>% # Aggregate by culture and by geo_unit
  summarise(name = first(nom),
            surf_geo_unit = first(surf_ha),
            surf_cult_km2 = sum(AREA_PARC, na.rm = T),# Surface of parcels dedicated to each culture in each department
            surf_cult_perc = sum(AREA_PARC, na.rm = T) / Surf_Agri_Tot[1], # Percentage in the total agricultural area of the department 
            parcel_cult_n  = n(),
            parcel_cult_perc = n_parcels / N_Parcels[1]) %>% 
  left_join(label_data, by = "CODE_GROUP")


