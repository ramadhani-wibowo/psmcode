#Vector Areas
pa <- st_read("FILE_NAME.shp") |> st_make_valid()
ring_20_50 <- st_read(" FILE_NAME.shp") |> st_make_valid()
treated_area <- st_union(pa)
clip_area <- st_union(treated_area, st_union(ring_20_50))
clip_vect <- vect(clip_area)

#Raster Files
bui2000 <- crop(rast("FILE_NAME.tif"), clip_vect)
bui2020 <- crop(rast("FILE_NAME.tif"), clip_vect)
elev    <- crop(rast("FILE_NAME.tif"), clip_vect)
slope   <- crop(rast("FILE_NAME.tif"), clip_vect)
lulc    <- crop(rast("FILE_NAME.tif"), clip_vect)
ttcity  <- crop(rast("FILE_NAME.tif"), clip_vect)
roadd   <- crop(rast("FILE_NAME.tif"), clip_vect)

bui2000_rs <- rast("FILE_NAME.tif")  # reference grid
bui2020_rs<- resample(bui2020, bui2000_rs , method="bilinear")
elev_rs   <- resample(elev, bui2000_rs , method="bilinear")
slope_rs  <- resample(slope, bui2000_rs , method="bilinear")
lulc_rs   <- resample(lulc, bui2000_rs , method="near")     # categorical
ttcity_rs <- resample(ttcity, bui2000_rs , method="bilinear")
roadd_rs  <- resample(roadd, bui2000_rs , method="bilinear")

#Crop and Mask Raster
bui2000_c     <- mask(crop(bui2000_rs , clip_vect), clip_vect)
bui2020_c <- mask(crop(bui2020_rs, clip_vect), clip_vect)
elev_c    <- mask(crop(elev_rs, clip_vect), clip_vect)
slope_c   <- mask(crop(slope_rs, clip_vect), clip_vect)
lulc_c    <- mask(crop(lulc_rs, clip_vect), clip_vect)
ttcity_c  <- mask(crop(ttcity_rs, clip_vect), clip_vect)
roadd_c   <- mask(crop(roadd_rs, clip_vect), clip_vect)

# Stack everything together
stack_all <- c(lulc_c, bui2000_c, bui2020_c, elev_c, slope_c, ttcity_c, roadd_c)
names(stack_all) <- c("lulc","bui2000","bui2020","elev","slope","ttcity","roadd")

#Points Stacking
pts <- as.points(stack_all[[1]], na.rm = FALSE)
# --- Convert to sf object ---
pts_sf <- st_as_sf(pts)
pts_sf <- pts_sf |> 
dplyr::filter(lulc %in% 1:9)
# Read provinces shapefile (make sure CRS matches pts_sf)
prov <- st_read("FILE_NAME.shp") |> 
st_transform(st_crs(pts_sf))
# Spatial join: assign province to each pixel
pts_sf <- st_join(pts_sf, prov["name"])
# Rename the province column if needed
colnames(pts_sf)[which(names(pts_sf) == "name")] <- "province"

#Defining Treated and Control For PSM
#For Nature Reserves
pts_sf <- st_join(pts_sf, pa["mc"], left = TRUE)
pts_sf$PA <- ifelse(is.na(pts_sf$mc), 0, 1)

#For Buffer Zones
pts_sf <- pts_sf %>%
mutate(treat_buffer_0_10 = ifelse(
st_intersects(geometry, ring_0_10, sparse = FALSE)[,1],
1, 0
))

#Stratification
writeRaster(elev_c, "FILE_NAME.tif", overwrite = TRUE)
writeRaster(slope_c, "FILE_NAME.tif", overwrite = TRUE)
writeRaster(ttcity_c, "FILE_NAME.tif", overwrite = TRUE)
writeRaster(roadd_c, "FILE_NAME.tif", overwrite = TRUE)
elev_rast <- rast("FILE_NAME.tif")
slope_rast <- rast("FILE_NAME.tif")
ttcity_rast <- rast("FILE_NAME.tif")
roadd_rast <- rast("FILE_NAME.tif")

# Convert sf to SpatVector for terra
 pts_vect <- vect(pts_sf)
# Extract elevation values for each point
 elev_vals <- terra::extract(elev_rast, pts_vect)
 slope_vals <- terra::extract(slope_rast, pts_vect)
 roadd_vals <- terra::extract(roadd_rast, pts_vect)
 ttcity_vals <- terra::extract(ttcity_rast, pts_vect)

# Add elevation back into pts_sf
# extract() returns a data.frame with ID column -> drop it
pts_sf$elev <- elev_vals[,2]
pts_sf$slope <- slope_vals[,2]
pts_sf$ttcity <- ttcity_vals[,2]
pts_sf$roadd <- roadd_vals[,2]
pts_sf <- pts_sf %>%
mutate(
elev_class = cut(
elev,
breaks = c(-Inf, 500, 1000, 1500),         # your bands
labels = c("<500", "500–1000", "1000–1500"),
right = FALSE,                              # left-closed, right-open: [500,1000)
include.lowest = TRUE
),
elev_class = factor(elev_class,
levels = c("<500", "500–1000", "1000–1500"),
ordered = TRUE)
)

psm_df_pa <- pts_sf %>%
sf::st_drop_geometry() %>%
mutate(
roadd   = ifelse(is.na(roadd), 0, roadd),
ttcity  = ifelse(is.na(ttcity), 0, ttcity)
) %>%
filter(
!is.na(elev),
!is.na(slope),
!is.na(province),
!is.na(lulc),
!is.na(elev_class)
) %>%
select(mc, province, PA, elev, slope, roadd, ttcity, lulc, elev_class)
