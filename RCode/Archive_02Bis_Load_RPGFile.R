rpg_map <- st_read(rpg_data)

# The first part for the ILOTS ones 
rpg_map <- rpg_map %>% 
  mutate(
    AREA_PARC = st_area(geometry) #To have it in m²
  )
sum(rpg_map$Area_parcels)
sum(rpg__map$SURF_PARC)

rpg_map_small <- rpg_map %>% # Take only 10% of all parcels randomly to work on a random smaller subsample
  slice_sample(n = round(0.1*length(rpg_map$ID_PARCEL)))
departments_rgf93 <- st_transform(departments, crs = st_crs(rpg_map))
rpg_joined <- st_join(departments_rgf93, rpg_map_small)

rpg_joined_dep <- rpg_joined %>%
  group_by(insee_dep) %>% #First: construct a variable that for each parcel gives the surface of the agricultural area of the department in which it is located
  mutate(Surf_Agri_Dep = sum(SURF_PARC), # Surface of all agricultural parcels in the department
         N_Parcels      = n()
  ) %>% # Number of parcels in the department
  ungroup()
table(rpg_joined_dep$Surf_Agri_Dep) # Just to check the results
table(rpg_joined_dep$N_Parcels)

result_i  <- rpg_joined_dep %>% # Object that contain descriptive statistics
  group_by(insee_dep, CODE_GROUP) %>% # Aggregate by culturer
  summarise(SURF_km2 = sum(SURF_PARC, na.rm = T),# Surface of parcels dedicated to each culture in each department
            SURF_perc = sum(SURF_PARC, na.rm = T) / Surf_Agri_Dep[1], # Percentage in the total agricultural area of the department 
            n_parcels  = n(),
            perc_parcels = n_parcels / N_Parcels[1])



