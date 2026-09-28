#Insert BUI Values
treated_area <- st_union(ring_10_20)
clip_area <- st_union(treated_area, st_union(ring_20_50))
clip_vect <- vect(clip_area)
bui2000 <- crop(rast("FILE_NAME.tif"), clip_vect)
bui2020 <- crop(rast("FILE_NAME.tif"), clip_vect)
bui2000_rs <- rast("FILE_NAME.tif")  # reference grid
bui2020_rs<- resample(bui2020, bui2000_rs , method="bilinear")
bui2000_c     <- mask(crop(bui2000_rs , clip_vect), clip_vect)
bui2020_c <- mask(crop(bui2020_rs, clip_vect), clip_vect)
library(terra)
library(dplyr)
library(sf)

# Extract BUI raster values using terra
bui_vals2000 <- terra::extract(bui2000_c, pts_sf)
bui_vals2020 <- terra::extract(bui2020_c, pts_sf)

#DF File Creation
psm_df_bui <- pts_sf %>%
  mutate(
    bui2000 = ifelse(is.na(bui_vals2000[,2]), 0, bui_vals2000[,2]),
    bui2020 = ifelse(is.na(bui_vals2020[,2]), 0, bui_vals2020[,2]),
    bui_change = bui2020 - bui2000,
    elev = ifelse(is.na(elev), 0, elev),
    slope = ifelse(is.na(slope), 0, slope),
    province = ifelse(is.na(province), "Unknown", province),
    lulc = ifelse(is.na(lulc), "Unknown", lulc),
    elev_class = ifelse(is.na(elev_class), "Unknown", elev_class),
    roadd  = ifelse(is.na(roadd), 0, roadd),
    ttcity = ifelse(is.na(ttcity), 0, ttcity)
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(mc, province, PA, elev, slope, roadd, ttcity, lulc, elev_class,
                bui2000, bui2020, bui_change)

bui_per_pa <- psm_df_bui %>% 
  filter(!is.na(mc)) %>%      
  group_by(mc) %>%            
  summarise(
    bui2000_mean = mean(bui2000, na.rm = TRUE),
    bui2020_mean = mean(bui2020, na.rm = TRUE),
    bui_change_mean = mean(bui2020 - bui2000, na.rm = TRUE), # compute change directly
    n_pixels = n()            
  ) %>% ungroup()
