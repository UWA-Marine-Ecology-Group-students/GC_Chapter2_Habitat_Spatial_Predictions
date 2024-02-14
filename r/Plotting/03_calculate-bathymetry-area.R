library(terra)
library(tidyverse)
library(sf)
library(landscapemetrics)

eez <- st_read("data/spatial/shapefiles/eez.shp")

bathy <- rast("data/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  mask(eez) %>%
  clamp(lower = -200, values = F) 
plot(bathy)

bathycl <- classify(bathy, c(-200, -100, -40, 0), include.lowest = TRUE, brackets = TRUE) %>%
  project("epsg:9473")
plot(bathycl)

check_landscape(bathycl)

area <- lsm_p_area(bathycl, directions = 8)

sum <- area %>%  
  group_by(class) %>%  
  dplyr::summarise(total_area = sum(value)) %>%
  dplyr::mutate(total_area_km = total_area * 0.01,
                depth_class = case_when(class == 0 ~ "0-40",
                                        class == 1 ~ "40-100",
                                        class == 2 ~ "100-200")) %>%
  glimpse()

write.csv(sum, file = "data/tidy/depth-class-area.csv",
          row.names = F)
