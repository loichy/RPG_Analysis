# Load RPG data in R
rpg_map <- st_read(rpg_data)

# Compute parcel area and add culture label 
rpg_map_area <- rpg_map %>% 
  mutate(
    AREA_PARC = st_area(geometry), #To have it in m²
    year = year_i,
    region = region_i
  ) %>% 
  left_join(label_data, by = "CODE_GROUP")# Ajouter le libellé! Nom variable: NOM_CULTU

# Adapt CRS of geo_unit to match CRS of rpg data
contours_rgf93 <- st_transform(contours, crs = st_crs(rpg_map_area))

# Tell in which geo_unit falls each parcel
rpg_joined <- st_join(contours_rgf93, rpg_map_area) # To test, use rpg_map_small to be quicker

# Reatain geo_unit with at least one parcel
rpg_joined_filtered <- rpg_joined %>% 
  filter(!is.na(ID_PARCEL))

# Compute useful variable at the geo_unit level
rpg_joined_gu <- rpg_joined_filtered %>%
  group_by(geo_unit) %>% #First: construct a variable that for each parcel gives the surface of the agricultural area of the geo_unit in which it is located
  mutate(Surf_Agri_Tot = sum(AREA_PARC), # Surface of all agricultural parcels in the geo_unit
         N_Parcels      = n()
  ) %>% # Number of parcels in the geo_unit
  ungroup()

result_i <- rpg_joined_gu %>% # Object that contain descriptive statistics
  group_by(geo_unit, CODE_GROUP) %>% # Aggregate by culture
  summarise(
    cult_label = first(LABEL_CODE_GROUP),
    region = first(region),
    name = first(nom),
    year = first(year),
    surf_tot_geo_unit_m2 = first(surf_ha)*10000,
    surf_agri_geo_unit_m2 = first(Surf_Agri_Tot),
    surf_cult_m2 = sum(AREA_PARC, na.rm = T),# Surface of parcels dedicated to each culture in each department
    surf_cult_perc = sum(AREA_PARC, na.rm = T) / Surf_Agri_Tot[1], # Percentage in the total agricultural area of the commune
    parcel_cult_n  = n(),
    parcel_cult_perc = parcel_cult_n / N_Parcels[1]) 
