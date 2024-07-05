rpg_data <- find_ia_shp_file(destfile)
rpg_map <- st_read(rpg_data)

# Compute parcel area and add culture label 
rpg_map_area <- rpg_map %>% 
  mutate(
    AREA_PARC = st_area(geometry),#To have it in m²
    CODE_GROUP = as.character(CODE_CULTU), # To homegenize names
    year = year_i,
    region = region_i
  ) %>% 
  dplyr::select(-CODE_CULTU) %>% 
  left_join(label_data, by = "CODE_GROUP")# Ajouter le libellé! Nom variable: NOM_CULTU

# Adapt CRS of contours to rpg data
contours_rgf93 <- st_transform(contours, crs = st_crs(rpg_map_area))

# Tell in which geo_unit falls each parcel
rpg_joined <- st_join(rpg_map_area, contours_rgf93, largest = T) # To test, use rpg_map small

  # plottest <- ggplot() +
#   geom_sf(data=rpg_joined, 
#           mapping = aes(fill=nom), 
#           lwd = 0) +
#   geom_sf(data = contours_rgf93, 
#           aes(geometry = geometry), 
#           fill=NA) +
#   coord_sf(crs=st_crs('+proj=lcc +lat_0=46.5 +lon_0=3 +lat_1=49 +lat_2=44 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'),
#            xlim = c(573567.302074, 755250.729560), 
#            ylim = c(6773621.222320, 6905868.489852), 
#            expand = FALSE) +
#   theme(legend.position = "none") 
# ggsave(filename = here(dir$figures,"plottest.pdf"), width = 16, height =12) 

# Retain geo_unit with at least one parcel
rpg_joined_filtered <- rpg_joined %>% 
  filter(!is.na(NUM_ILOT)) # Only keep geo_unit with at least one parcel

# Compute useful variable at the geo-unit level
rpg_joined_gu <- rpg_joined_filtered %>%
  group_by(geo_unit) %>% #First: construct a variable that for each parcel gives the surface of the agricultural area of the department in which it is located
  mutate(Surf_Agri_Tot = sum(AREA_PARC), # Surface of all agricultural parcels in the geo_unit
         N_Parcels      = n()
  ) %>% # Number of parcels in the geo_unit
  ungroup()

# Finally, create object that aggregates by culture and by geo_unit
result_i <- rpg_joined_gu %>% # Object 
  group_by(geo_unit, CODE_GROUP) %>% # Aggregate by culture and by geo_unit
  summarise(cult_label = first(LABEL_CODE_GROUP),
            data_type = "ia",
            region = first(region),
            name = first(nom),
            year = first(year),
            surf_tot_geo_unit_m2 = first(surf_ha)*10000,
            surf_agri_geo_unit_m2 = first(Surf_Agri_Tot),
            surf_cult_m2 = sum(AREA_PARC, na.rm = T),# Surface of parcels dedicated to each culture in each department
            surf_cult_perc = sum(AREA_PARC, na.rm = T) / Surf_Agri_Tot[1], # Percentage in the total agricultural area of the commune
            parcel_cult_n  = n(),
            parcel_cult_perc = parcel_cult_n / N_Parcels[1]
            )


