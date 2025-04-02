# Code used to calculate the amount of regenerated vegetation in the Atlantic Forest
# Vinicius Tonetti - vrtonetti@ufscar.br

# Loading packages -------------------------------------------------------------

library(terra)
library(sf)
library(exactextractr)
library(raster)
library(tidyverse)
library(writexl)
library(readxl)
library(future.apply)
library(classInt)


# cleaning directory -----------------------------------------------------------
rm(list = ls())


# Defining number of nucleous available in R
numb_cores <- parallel::detectCores() - 1
plan(multisession, workers =  numb_cores)

#### Reprojecting --------------------------------------------------------------
################################################################################

# Loading layers --------------------------------------------------------------- 

# Municipalities

# Shapefile downloaded from https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html on 15/02/2025
mun <- terra::vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/BR_Municipios_2023.shp")

# Regeneration 11_21
reg_11_21 <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21.tif")


# Reprojecting -----------------------------------------------------------------

# Projecting municipalities and raster data CRS to SAD69 Brazil Polyconic, EPSG:29101

mun <- terra::project(mun, "EPSG:29101")

reg_11_21  <- terra::project(reg_11_21, "EPSG:29101", method = "mode") # using the method "mode" to interpolate

# Saving raster SAD69
#terra::writeRaster(reg_11_21, "D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Polyconic.tif")

# Saving municipalities polygons in SAD69
#terra::writeVector(mun, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/BR_Municipios_2023_SAD69_Polyconic.shp")


# Calculating areas in SAD69 Brazil Polyconic ----------------------------------
################################################################################

# Loading raster and municipalities in SAD69 Polyconic

mun_SAD69_Poly <- terra::vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/BR_Municipios_2023_SAD69_Polyconic.shp")

reg_11_21_SAD69_Poly <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Polyconic.tif")
plot(reg_11_21_SAD69_Poly)


# Computing the area of each pixel of the secondary forest patches
pixel_area <- cellSize(reg_11_21_SAD69_Poly, unit = "m")

# Considering values only for forest pixels
pixel_area_1_only_Poly <- reg_11_21_SAD69_Poly * pixel_area
plot(pixel_area_1_only_Poly)


# Saving raster with area of each pixel
#terra::writeRaster(pixel_area_1_only_Poly, "D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Poly_Area.tif", overwrite = T)

# Loading raster with pixel area, patches only 
reg_11_21_SAD69_Poly_Area <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Poly_Area.tif")

# Loading binary raster 0 - 1
binary_raster_SAD69 <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Polyconic.tif")

# Masking raster with pixel area to exclude 0 values
raster_ones <- mask(reg_11_21_SAD69_Poly_Area, binary_raster_SAD69, maskvalue = 0)
plot(raster_ones)

#terra::writeRaster(raster_ones, "D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Area_patch_only_Poly.tif", overwrite = T)


# Loading binary raster with pixel area only for forest patches
reg_11_21_SAD69_area_forest_only_Poly <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Area_patch_only_Poly.tif")
plot(reg_11_21_SAD69_area_forest_only_Poly)

# Loading Municipality boundaries SAD69 Polyconic
mun_SAD69_Poly <- terra::vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/BR_Municipios_2023_SAD69_Polyconic.shp")
plot(mun_SAD69_Poly)


# Calculating the area of forest for each municipality -------------------------
# Extracting values using the "exactextractr" package as it did not run in Terra with function extract()

# Converting the Terra raster to a "raster::" object
reg_11_21_SAD69_area_forest_only_raster <- raster::raster(reg_11_21_SAD69_area_forest_only_Poly)

# Converting municipality shp to sf object
mun_SAD69_sf <- sf::st_as_sf(mun_SAD69_Poly)

extract_area <- exactextractr::exact_extract(reg_11_21_SAD69_area_forest_only_raster, mun_SAD69_sf, "sum")
plot(reg_11_21_SAD69_area_forest_only_raster)
#raster::writeRaster(reg_11_21_SAD69_area_forest_only_raster,
#                    "D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Area_patch_only_raster.tif")

# Checking the number of columns
ncol(mun_SAD69_sf) #14

# Creating a new column to add area of regenerating forest
mun_SAD69_sf[,15] <- extract_area
colnames(mun_SAD69_sf)[15] <- "sec_for"

length(extract_area)
nrow(mun_SAD69_sf)

# Save to shapefile
#st_write(mun_SAD69_sf, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_area_Poly.shp", delete_dsn = T)


# Saving areas to Excel --------------------------------------------------------

# Loading shapefile with the area of regenerated forests
mun_with_area <- terra::vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_area_Poly.shp")

# Extracting and saving values for the ammount of forest in each municipality and saving as Data Frame
names(mun_with_area)
df_area <- as.data.frame(mun_with_area[,c("NM_MUN","NM_UF","sec_for")])

df_area <- df_area %>% 
  arrange(desc(sec_for))

sum(df_area[,"sec_for"])

#writexl::write_xlsx(df_area, "D:/__PESSOAL/Vinicius_T/raster_pacto/reg_by_municipalities_Poly.xlsx")


# Analysis by State ------------------------------------------------------------

# Loading state polygon
state_area <- terra::vect("D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/BR_UF_2023_area.shp")
plot(state_area)


# Converting the CRS of the states to the same CRS of the raster
state_area_SAD69_Poly <- project(state_area, "EPSG:29101")

# Converting the state polygon to sf object
state_area_SAD69_sf <- sf::st_as_sf(state_area_SAD69_Poly)

extract_area_state <- exactextractr::exact_extract(reg_11_21_SAD69_area_forest_only_raster, state_area_SAD69_sf, "sum")

# Checking the number of columns
ncol(state_area_SAD69_sf) #8

# Creating a new column to add area of regenerating forest
state_area_SAD69_sf[,7] <- extract_area_state
colnames(state_area_SAD69_sf)[7] <- "sec_for_Poly"

length(extract_area_state)
nrow(state_area_SAD69_sf)

# Save to shapefile
#st_write(state_area_SAD69_sf, "D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/BR_UF_2023_area_Poly.shp", delete_dsn = T)


# Saving areas of secondary forest by State to Excel ---------------------------


# Loading shapefile with the area of regenerated forests in each state
state_with_area_Poly <- terra::vect("D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/BR_UF_2023_area_Poly.shp")

# Extracting and saving values for the ammount of forest in each municipality and saving as Data Frame
names(state_with_area_Poly)
df_area_by_state <- as.data.frame(state_with_area_Poly[,c("NM_UF","sc_fr_P")])

df_area_by_state <- df_area_by_state  %>% 
  arrange(desc(sc_fr_P))

sum(df_area_by_state[,"sc_fr_P"])

#writexl::write_xlsx(df_area_by_state, "D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/reg_by_states_Poly.xlsx")



# Calculating the proportion of secondary forest by municipality and state #####
################################################################################

# Cropping MapBiomas Col09 to Brazil -------------------------------------------

# Loading layers

# Atlantic Forest (AF) limit
AF <- terra::vect("D:/__PESSOAL/Vinicius_T/Limite Mata Atlantica/bioma_MA_IBGE_250mil/bioma_MA_IBGE_250mil.shp")
plot(AF)

# MapBiomas Col 9 2010
MB_09_2010 <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2010.tif")

# Cropping raster
MB_09_AF_2010 <- mask(crop(MB_09_2010, AF), AF)

# Saving MB raster in WGS84 cropped for the AF considering all land cover types
#terra::writeRaster(MB_09_AF_2010, "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/MB_09_AF_2010_WGS_84.tif")


# Converting MB raster cropped by the AF linit to SAD69 Poly
MB_09_AF_2010_SAD69_Poly  <- terra::project(MB_09_AF_2010, "EPSG:29101", method = "mode")

# Saving MB raster in SAD69 Brazil Polyconic cropped for the AF considering all land cover types
#terra::writeRaster(MB_09_AF_2010_SAD69_Poly, "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/MB_09_AF_2010_SAD69_Poly.tif")


# Converting raster values to set all forest pixels as 1 and the remaining 0 ------

MB_09_AF_2010_SAD69_Poly <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/MB_09_AF_2010_WGS_84.tif")
MB_09_AF_2010_SAD69_Poly_forest_only <- terra::ifel(MB_09_AF_2010_SAD69_Poly == 3, 1, 0)
plot(MB_09_AF_2010_SAD69_Poly_forest_only)

# Saving raster MB 2010 forest only
#terra::writeRaster(MB_09_AF_2010_SAD69_Poly_forest_only,
#                    "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/MB_09_AF_2010_WGS84_forest_only.tif")


# Calculating area of forest only ----------------------------------------------

MB_09_AF_2010_WGS84_forest_only <- rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/MB_09_AF_2010_WGS84_forest_only.tif")
MB_09_AF_2010_SAD69_Poly_forest_only <- terra::project(MB_09_AF_2010_WGS84_forest_only,"EPSG:29101", method = "mode")

plot(MB_09_AF_2010_SAD69_Poly_forest_only)

#terra::writeRaster(MB_09_AF_2010_SAD69_Poly_forest_only,
#                    "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/MB_09_AF_2010_SAD69_Poly_forest_only.tif")



# Loading raster 2010 forest only
MB_09_AF_2010_SAD69_Poly_forest_only <- rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/MB_09_AF_2010_SAD69_Poly_forest_only")

# Computing the area of each pixel of the secondary forest patches
pixel_area_forest_only <- cellSize(MB_09_AF_2010_SAD69_Poly_forest_only, unit = "m")

# Considering values only for forest pixels
pixel_area_forest_only_Poly <- MB_09_AF_2010_SAD69_Poly_forest_only * pixel_area_forest_only
plot(pixel_area_forest_only_Poly)

#terra::writeRaster(pixel_area_forest_only_Poly,
#                    "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/pixel_area_forest_only_Poly.tif")


# Extracting values of pixel area in the forest only raster to polygons --------
# ------------------------------------------------------------------------------

pixel_area_forest_only <- raster("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/pixel_area_forest_only_Poly.tif")

# Loading municipalities shp
mun <- sf::st_read("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_area_Poly.shp")

# Loading states shp
estados <- sf::st_read("D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/BR_UF_2023_area_Poly.shp")

# Extracting area to Municipalities
extract_area_forest_mun <- exactextractr::exact_extract(pixel_area_forest_only, mun, "sum")

# Extracting area to States
extract_area_forest_states <- exactextractr::exact_extract(pixel_area_forest_only, estados, "sum")

# Creating a new column to add area of forest in 2010
ncol(mun)
mun[,16] <- extract_area_forest_mun
colnames(mun)[16] <- "for_area_mun"
plot(mun)

ncol(estados)
estados[,9] <- extract_area_forest_states
colnames(estados)[9] <- "for_area_states"

# Saving polygons

#st_write(mun, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_mun_all_areas.shp", delete_dsn = T)
#st_write(estados, "D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/_estados_all_areas.shp", delete_dsn = T)


# Calculating the proportion of secondary forest in each polygon in relation
# to the amount of forest in 2010 

# Loading municipalities shp
mun <- sf::st_read("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_mun_all_areas.shp")

# Loading states shp
estados <- sf::st_read("D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/_estados_all_areas.shp")


# Calculating proportion of secondary forest in relation to the total amount of forest in 2010
# --------------------------------------------------------------------------------------------

colnames(mun)
ncol(mun)

mun[,17] <- data.frame(mun[,"sec_for"])[,1]/data.frame(mun[,"fr_r_mn"])[,1]
colnames(mun)[17] <- "prop_sec_fr"

#st_write(mun, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_mun_all_areas_prop.shp", delete_dsn = T)


colnames(estados)
ncol(estados)

estados[,10] <- data.frame(estados[,"sc_fr_P"])[,1]/data.frame(estados[,"fr_r_st"])[,1]
colnames(estados)[10] <- "prop_sec_fr"

#st_write(estados, "D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/_estados_all_areas_prop.shp", delete_dsn = T)



################################################################################
## Extracting data for Quilombola, Assentamento, TI, and UC



# Quilombola -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# cleaning directory -----------------------------------------------------------
rm(list = ls())

# Loading raster of Secondary Forest
reg <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Area_patch_only_raster.tif")

# Loading raster 2010
forest_2010 <- raster::raster("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/pixel_area_forest_only_Poly.tif")



# reprojecting Polygons and converting to sf object

quilombola <- vect("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_area_quilombola_incra2024.shp")
quilombola_Poly <- terra::project(quilombola, "EPSG:29101")
quilombola_Poly <- sf::st_as_sf(quilombola_Poly)

# Extracting forest area values

quilombola_Poly_Area_forest_2010 <- exactextractr::exact_extract(forest_2010, quilombola_Poly, "sum")
quilombola_Poly_Area_reg <- exactextractr::exact_extract(reg, quilombola_Poly, "sum")

# Creating new columns for area values

# Amount of forest in 2010

quilombola_Poly[,ncol(quilombola_Poly)+1] <- quilombola_Poly_Area_forest_2010
colnames(quilombola_Poly)[ncol(quilombola_Poly)] <- "forest_area"


# Regenerated forest 2011 - 2020

quilombola_Poly[,ncol(quilombola_Poly)+1] <- quilombola_Poly_Area_reg
colnames(quilombola_Poly)[ncol(quilombola_Poly)] <- "secondary_forest"

# Proportion of regenerated forest

prop <- quilombola_Poly_Area_reg/quilombola_Poly_Area_forest_2010
quilombola_Poly[,ncol(quilombola_Poly)+1] <- prop
colnames(quilombola_Poly)[ncol(quilombola_Poly)] <- "prop_reg"

#sf::st_write(quilombola_Poly, "D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_area_quilombola_incra2024_Area.shp")


# Assentamento -----------------------------------------------------------------
# ------------------------------------------------------------------------------

# cleaning directory -----------------------------------------------------------
rm(list = ls())

# Loading raster of Secondary Forest
reg <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Area_patch_only_raster.tif")

# Loading raster 2010
forest_2010 <- raster::raster("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/pixel_area_forest_only_Poly.tif")


# reprojecting Polygons and converting to sf object

assentamento <- vect("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_assentamento_incra2024.shp")
assentamento_Poly <- terra::project(assentamento, "EPSG:29101")
assentamento_Poly <- sf::st_as_sf(assentamento_Poly)

# Extracting forest area values

assentamento_Poly_Area_forest_2010 <- exactextractr::exact_extract(forest_2010, assentamento_Poly, "sum")
assentamento_Poly_Area_reg <- exactextractr::exact_extract(reg, assentamento_Poly, "sum")

# Creating new columns for area values

# Amount of forest in 2010

assentamento_Poly[,ncol(assentamento_Poly)+1] <- assentamento_Poly_Area_forest_2010
colnames(assentamento_Poly)[ncol(assentamento_Poly)] <- "forest_area"


# Regenerated forest 2011 - 2020

assentamento_Poly[,ncol(assentamento_Poly)+1] <- assentamento_Poly_Area_reg
colnames(assentamento_Poly)[ncol(assentamento_Poly)] <- "secondary_forest"

# Proportion of regenerated forest

prop <- assentamento_Poly_Area_reg/assentamento_Poly_Area_forest_2010
assentamento_Poly[,ncol(assentamento_Poly)+1] <- prop
colnames(assentamento_Poly)[ncol(assentamento_Poly)] <- "prop_reg"

#sf::st_write(assentamento_Poly, "D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_assentamento_incra2024_Area.shp")


# Terra Indígena ---------------------------------------------------------------
# ------------------------------------------------------------------------------

# cleaning directory -----------------------------------------------------------
rm(list = ls())

# Loading raster of Secondary Forest
reg <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Area_patch_only_raster.tif")

# Loading raster 2010
forest_2010 <- raster::raster("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/pixel_area_forest_only_Poly.tif")


# reprojecting Polygons and converting to sf object

terra_indigena <- vect("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_tis_funai2024.shp")
terra_indigena_Poly <- terra::project(terra_indigena, "EPSG:29101")
terra_indigena_Poly <- sf::st_as_sf(terra_indigena_Poly)


# Extracting forest area values

terra_indigena_Poly_Area_forest_2010 <- exactextractr::exact_extract(forest_2010, terra_indigena_Poly, "sum")
terra_indigena_Poly_Area_reg <- exactextractr::exact_extract(reg, terra_indigena_Poly, "sum")

# Creating new columns for area values

# Amount of forest in 2010

terra_indigena_Poly[,ncol(terra_indigena_Poly)+1] <- terra_indigena_Poly_Area_forest_2010
colnames(terra_indigena_Poly)[ncol(terra_indigena_Poly)] <- "forest_area"


# Regenerated forest 2011 - 2020

terra_indigena_Poly[,ncol(terra_indigena_Poly)+1] <- terra_indigena_Poly_Area_reg
colnames(terra_indigena_Poly)[ncol(terra_indigena_Poly)] <- "secondary_forest"

# Proportion of regenerated forest

prop <- terra_indigena_Poly_Area_reg/terra_indigena_Poly_Area_forest_2010
terra_indigena_Poly[,ncol(terra_indigena_Poly)+1] <- prop
colnames(terra_indigena_Poly)[ncol(terra_indigena_Poly)] <- "prop_reg"

#sf::st_write(terra_indigena_Poly, "D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_tis_funai2024_Area.shp")


# UC ---------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# cleaning directory -----------------------------------------------------------
rm(list = ls())

# Loading raster of Secondary Forest
reg <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Area_patch_only_raster.tif")

# Loading raster 2010
forest_2010 <- raster::raster("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/pixel_area_forest_only_Poly.tif")


# reprojecting Polygons and converting to sf object

UC <- vect("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_UC_mma2024.shp")
UC_Poly <- terra::project(UC, "EPSG:29101")
UC_Poly <- sf::st_as_sf(UC_Poly)


# Extracting forest area values

UC_Poly_Area_forest_2010 <- exactextractr::exact_extract(forest_2010, UC_Poly, "sum")
UC_Poly_Area_reg <- exactextractr::exact_extract(reg, UC_Poly, "sum")

# Creating new columns for area values

# Amount of forest in 2010

UC_Poly[,ncol(UC_Poly)+1] <- UC_Poly_Area_forest_2010
colnames(UC_Poly)[ncol(UC_Poly)] <- "forest_area"


# Regenerated forest 2011 - 2020

UC_Poly[,ncol(UC_Poly)+1] <- UC_Poly_Area_reg
colnames(UC_Poly)[ncol(UC_Poly)] <- "secondary_forest"

# Proportion of regenerated forest

prop <- UC_Poly_Area_reg/UC_Poly_Area_forest_2010
UC_Poly[,ncol(UC_Poly)+1] <- prop
colnames(UC_Poly)[ncol(UC_Poly)] <- "prop_reg"

#sf::st_write(UC_Poly, "D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_UC_mma2024_Area.shp")


## Previous land cover type ----------------------------------------------------
################################################################################

# cleaning directory -----------------------------------------------------------
rm(list = ls())

# Atlantic Forest (AF) limit
AF <- terra::vect("D:/__PESSOAL/Vinicius_T/Limite Mata Atlantica/bioma_MA_IBGE_250mil/bioma_MA_IBGE_250mil.shp")

# MB 2008 in WGS84
MB_2008 <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2008.tif")
#plot(MB_2008)

# MB 2010 in WGS84
MB_2010 <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2010.tif")
#plot(MB_2008)

# Cropping to the AF limit

MB_2008_AF <- mask(crop(MB_2008, AF), AF)
#plot(MB_2008_AF)

#terra::writeRaster(MB_2008_AF, "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2008_AF.tif",
#                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)

MB_2010_AF <- mask(crop(MB_2010, AF), AF)
#plot(MB_2010_AF)

#terra::writeRaster(MB_2010_AF, "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2010_AF.tif",
#                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


# Reg map in WGS84
reg <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21.tif")
#plot(reg)

reg_11_21_AF <- mask(crop(reg, AF), AF)
#terra::writeRaster(reg_11_21_AF, "D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21_AF.tif",
#                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)



MB_2008_AF <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2008_AF.tif")
#plot(MB_2008_AF)
MB_2010_AF <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2010_AF.tif")
#plot(MB_2010_AF)
reg_11_21_AF <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21_AF.tif")
#plot(reg_11_21_AF)


# Resample to match extent
reg_resampled_AF <- resample(reg_11_21_AF, MB_2008_AF, method = "near")
#plot(reg_resampled_AF)
#terra::writeRaster(reg_resampled_AF, "D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21_AF_resampled.tif")


# Masking ----------------------------------------------------------------------

reg_resampled_AF <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21_AF_resampled.tif")
#plot(reg_resampled)

MB_2008_AF <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2008_AF.tif")
MB_2010_AF <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2010_AF.tif")


masked_MB_2008 <- terra::mask(MB_2008_AF, reg_resampled_AF, maskvalue = 0)
#plot(masked_MB)
#terra::writeRaster(masked_MB_2008, "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/previous_land_cover_type_MB_2008.tif")

masked_MB_2010 <- terra::mask(MB_2010_AF, reg_resampled_AF, maskvalue = 0)
#plot(masked_MB)
#terra::writeRaster(masked_MB_2010, "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/previous_land_cover_type_MB_2010.tif")

MB_08_previous_land_use <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/previous_land_cover_type_MB_2008.tif")
MB_10_previous_land_use <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/previous_land_cover_type_MB_2010.tif")

# Frequency of each previous land cover type -----------------------------------

# 2008

MB_08_previous_land_use <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/previous_land_cover_type_MB_2008.tif")

previous_land_use_freq_08 <- terra::freq(MB_08_previous_land_use)

previous_land_use_freq_08 <- data.frame(previous_land_use_freq_08)

previous_land_use_freq_08 <- previous_land_use_freq_08 %>% 
  mutate(value = as.character(value)) %>% 
  mutate(case_when(
    value == "0"  ~ "NA",
    value == "3"  ~ "Forest",
    value == "4"  ~ "Savanna",
    value == "5"  ~ "Mangrove",
    value == "9"  ~ "Forest plantation",
    value == "11" ~ "Wetland",
    value == "12" ~ "Grassland",
    value == "15" ~ "Pasture",
    value == "20" ~ "Sugar cane",
    value == "21" ~ "Mosaic of Uses",
    value == "23" ~ "Beach, Dune and Sand Spot",
    value == "24" ~ "Urban area",
    value == "25" ~ "Other non vegetated areas",
    value == "29" ~ "Rocky Outcrop",
    value == "30" ~ "Mining",
    value == "31" ~ "Acquaculture",
    value == "32" ~ "Hypersaline Tidal Flat",
    value == "33" ~ "River, Lake and Ocean",
    value == "39" ~ "Soybean",
    value == "40" ~ "Rice",
    value == "41" ~ "Other temporary crops",
    value == "46" ~ "Coffee",
    value == "47" ~ "Citrus",
    value == "48" ~ "Other Perennial Crops",
    value == "49" ~ "Wooded Sandbank Vegetation",
    value == "50" ~ "Herbaceous Sandbank Vegetation")) %>%
  select(-layer) %>% 
  rename(pixel_values = value,
         frequency_of_pixels = count,
         land_cover_type = "case_when(...)") %>% 
  mutate(total_pixels = sum(frequency_of_pixels)) %>% 
  mutate(percentage = round((frequency_of_pixels/total_pixels)*100, 2)) %>% 
  select(land_cover_type, percentage, pixel_values) %>% 
  arrange(desc(percentage))

#writexl::write_xlsx(previous_land_use_freq_08, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/freq_previous_land_use_2008.xlsx")


# 2010 -------------------------------------------------------------------------

MB_10_previous_land_use <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/previous_land_cover_type_MB_2010.tif")

previous_land_use_freq_10 <- terra::freq(MB_10_previous_land_use)

previous_land_use_freq_10 <- data.frame(previous_land_use_freq_10)

previous_land_use_freq_10 <- previous_land_use_freq_10 %>% 
  mutate(value = as.character(value)) %>% 
  mutate(case_when(
    value == "0"  ~ "NA",
    value == "3"  ~ "Forest",
    value == "4"  ~ "Savanna",
    value == "5"  ~ "Mangrove",
    value == "9"  ~ "Forest plantation",
    value == "11" ~ "Wetland",
    value == "12" ~ "Grassland",
    value == "15" ~ "Pasture",
    value == "20" ~ "Sugar cane",
    value == "21" ~ "Mosaic of Uses",
    value == "23" ~ "Beach, Dune and Sand Spot",
    value == "24" ~ "Urban area",
    value == "25" ~ "Other non vegetated areas",
    value == "29" ~ "Rocky Outcrop",
    value == "30" ~ "Mining",
    value == "31" ~ "Acquaculture",
    value == "32" ~ "Hypersaline Tidal Flat",
    value == "33" ~ "River, Lake and Ocean",
    value == "39" ~ "Soybean",
    value == "40" ~ "Rice",
    value == "41" ~ "Other temporary crops",
    value == "46" ~ "Coffee",
    value == "47" ~ "Citrus",
    value == "48" ~ "Other Perennial Crops",
    value == "49" ~ "Wooded Sandbank Vegetation",
    value == "50" ~ "Herbaceous Sandbank Vegetation")) %>%
  select(-layer) %>% 
  rename(pixel_values = value,
         frequency_of_pixels = count,
         land_cover_type = "case_when(...)") %>% 
  mutate(total_pixels = sum(frequency_of_pixels)) %>% 
  mutate(percentage = round((frequency_of_pixels/total_pixels)*100, 2)) %>% 
  select(land_cover_type, percentage, pixel_values) %>% 
  arrange(desc(percentage))

#writexl::write_xlsx(previous_land_use_freq_10, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/freq_previous_land_use_2010.xlsx")

## Hotspots of regeneration and deforestation of secondary forests -------------
################################################################################

# cleaning directory -----------------------------------------------------------

rm(list = ls())


# Loading AF limits to crop merged tiles

AF <- terra::vect("D:/__PESSOAL/Vinicius_T/Limite Mata Atlantica/bioma_MA_IBGE_250mil/bioma_MA_IBGE_250mil.shp")


dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto"

# File names
tiles_reg <- list.files(dir)


# Loop to merge, crop and save raster merged

setwd(dir)

for(i in 1:length(unique(substr(tiles_reg, 1, 11)))){
unique(substr(tiles_reg, 1, 11))[i]
names <- tiles_reg[grepl(unique(substr(tiles_reg, 1, 11))[i], tiles_reg)]
tiles <- lapply(names, terra::rast)
merged_tile <- do.call(merge, tiles)
cropped <- mask(crop(merged_tile, AF), AF)
directory <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto"
writeRaster(cropped,
            paste(directory, paste(unique(substr(tiles_reg, 1, 11))[i], ".tif", sep = ""), sep = "/"),
            overwrite = T, gdal=c("COMPRESS=DEFLATE", "TFW=YES"))

}


# Loading cropped raster to sum 

rast_stack <- rast(c("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen11_1ha_SAD69.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen12_1ha_SAD69.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen13_1ha_SAD69.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen14_1ha_SAD69.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen15_1ha_SAD69.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen16_1ha_SAD69.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen17_1ha_SAD69.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen18_1ha_SAD69.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen19_1ha_SAD69.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen20_1ha_SAD69.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen21_1ha_SAD69.tif"))

summed_stack <- app(rast_stack, fun = sum, na.rm = T)
plot(summed_stack)

writeRaster(summed_stack,
            "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/summed_reg.tif",
            gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)

# Convert values = 2 and 3, to 1

summed_stack <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/summed_reg.tif")

# Trying using raster package, as it is not working with terra

summed_stack_r <- raster::raster(summed_stack)

reclass_matrix <- matrix(c(0, 0,
                           1, 1,
                           2, 1,
                           3, 1),
                        ncol = 2, byrow = T)

summed_stack_1_only <- raster::reclassify(summed_stack_r, reclass_matrix)

raster::writeRaster(summed_stack_1_only,
                    "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/summed_reg_1_only.tif",
                    options = c("COMPRESS=LZW", "ZLEVEL=9"))


# Raster of the final secondary forest map

# cleaning directory -----------------------------------------------------------
rm(list = ls())

summed_stack_1_only <-  terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/summed_reg_1_only.tif")
reg_11_21 <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21.tif")

# Putting into the same CRS
summed_stack_1_only <- terra::project(summed_stack_1_only, "EPSG:29101", method = "mode")
reg_11_21 <- terra::project(reg_11_21, "EPSG:29101", method = "mode")

reg_11_21_resampled <- resample(reg_11_21, summed_stack_1_only, method = "near")

plot(summed_stack_1_only)
plot(reg_11_21_resampled)


secondary_forest_loss <- summed_stack_1_only - reg_11_21_resampled
plot(secondary_forest_loss)


terra::writeRaster(secondary_forest_loss,
                   "D:/__PESSOAL/Vinicius_T/raster_pacto/secondary_forest_loss.tif",
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)

reclass_matrix <- matrix(c(0, 0,
                           1, 1,
                          -1, 0),
                         ncol = 2, byrow = T)

secondary_forest_loss <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/secondary_forest_loss.tif")
secondary_forest_loss <- raster::reclassify(secondary_forest_loss, reclass_matrix)

raster::writeRaster(secondary_forest_loss,
                    "D:/__PESSOAL/Vinicius_T/raster_pacto/secondary_forest_loss_0_1.tif",
                    options = c("COMPRESS=LZW", "ZLEVEL=9"))


# Calculating areas total regeneration and deforestation of regenerating forests
secondary_forest_loss <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/secondary_forest_loss_0_1.tif")
all_regeneration <- terra::rast("")

# Computing the area of each pixel of the secondary forest patches
pixel_area <- cellSize(secondary_forest_loss, unit = "m")

# Considering values only for forest pixels
secondary_forest_loss_Area <- secondary_forest_loss * pixel_area
plot(secondary_forest_loss_Area)

terra::writeRaster(secondary_forest_loss_Area,
                   "D:/__PESSOAL/Vinicius_T/raster_pacto/secondary_forest_loss_Area.tif",
                    gdal=c("COMPRESS=DEFLATE", "TFW=YES"))


all_secondary_forest <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/summed_reg_1_only.tif")


all_secondary_forest <- terra::project(all_secondary_forest , "EPSG:29101", method = "mode") # using the method "mode" to interpolate

# Computing the area of each pixel of the secondary forest patches
pixel_area <- cellSize(all_secondary_forest, unit = "m")

# Considering values only for forest pixels
all_secondary_forest_Area <- all_secondary_forest * pixel_area
plot(all_secondary_forest_Area)

terra::writeRaster(all_secondary_forest_Area,
                   "D:/__PESSOAL/Vinicius_T/raster_pacto/All_secondary_forest_loss_Area.tif",
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"))



# Extracting areas of all forest regeneration and secondary forest loss to polygons
# ----------------------------------------------------------------------------------

# cleaning directory -----------------------------------------------------------
rm(list = ls())


all_secondary_forest <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/summed_reg_1_only.tif")
secondary_forest_loss <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/secondary_forest_loss_0_1.tif")

all_secondary_forest_area <- cellSize(all_secondary_forest, unit = "m")
all_secondary_forest_area <- all_secondary_forest_area * all_secondary_forest

secondary_forest_loss_area <- cellSize(secondary_forest_loss, unit = "m")
secondary_forest_loss_area <- secondary_forest_loss_area * secondary_forest_loss

terra::writeRaster(all_secondary_forest_area,
                   "D:/__PESSOAL/Vinicius_T/raster_pacto/All_secondary_forest_GAIN_Area.tif",
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"))

terra::writeRaster(secondary_forest_loss_area,
                   "D:/__PESSOAL/Vinicius_T/raster_pacto/All_secondary_forest_LOSS_Area.tif",
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"))


mun <- sf::st_read("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_mun_all_areas_prop_total_reg_defo.shp")

all_reg <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/All_secondary_forest_GAIN_Area.tif")
all_defo <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/All_secondary_forest_LOSS_Area.tif")

for_reg_mun <- extract_area <- exactextractr::exact_extract(all_reg, mun, "sum")
defo_mun <- extract_area <- exactextractr::exact_extract(all_defo, mun, "sum")

# Converting areas from square meters to hectares
for_reg_mun_ha <- for_reg_mun/10000
defo_mun_ha <- defo_mun/10000

sum(for_reg_mun_ha)

# Adding reg and defo to the shapefile

mun[,"for_reg"] <- for_reg_mun_ha
mun[,"defo_mun"] <- defo_mun_ha

ncol(mun)
colnames(mun)


# Proportion of deforestation in relation to the total amount of forest that regenerated 
mun[,"prop_defo"] <- data.frame(mun[,"defo_mun"])[,1]/
                     data.frame(mun[,"for_reg"])[,1]

#sf::st_write(mun, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_mun_all_areas_prop_total_reg_defo.shp")


# Extracting data for municipalities after including reg 2021

mun_all_areas <- sf::st_read("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_mun_all_areas_prop_total_reg_defo.shp")
mun_all_areas[,"for_reg"] <- for_reg_mun
mun_all_areas[,"defo_mun"] <- defo_mun
mun_all_areas[,19] <- data.frame(mun_all_areas[,18])[,1]/
                                 data.frame(mun_all_areas[,17])[,1]


################################################################################
## Saving area values in dataframes --------------------------------------------


# Municipalities ---------------------------------------------------------------
# ------------------------------------------------------------------------------


# cleaning directory -----------------------------------------------------------
rm(list = ls())


mun <- sf::st_read("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_mun_all_areas_prop_total_reg_defo.shp")
colnames(mun)

mun_data <- data.frame(mun[,c("NM_MUN", "NM_UF", "sec_for", "fr_r_mn", "prp_sc_", "for_reg", "defo_mun", "prop_defo")])[,c(1:8)]

colnames(mun_data) <- c("Município", "Estado", "reg_2011_2021_ha", "total_area_2010_ha",
                        "prop_reg_ha", "total_reg_ha", "total_defo_ha", "prop_reg_defo_ha")


# Reg 2011_2021 ----------------------------------------------------------------

reg_2011_2020_mun <- mun_data[,c("Município", "Estado", "reg_2011_2021_ha",
                                 "prop_reg_ha", "total_area_2010_ha")]

reg_2011_2020_mun <- reg_2011_2020_mun %>% 
  arrange(desc(reg_2011_2020_ha)) %>% 
  mutate(across(c("reg_2011_2020_ha", "prop_reg_ha", "total_area_2010_ha"),
                 ~ .x /10000))

#writexl::write_xlsx(reg_2011_2020_mun, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/reg_2011_2021_mun.xlsx")


# Total reg --------------------------------------------------------------------

total_reg_mun <- mun_data[,c("Município", "Estado", "total_reg_ha")]

total_reg_mun <- total_reg_mun %>% 
  arrange(desc(total_reg_ha))


#writexl::write_xlsx(total_reg_mun, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/total_reg_2011_2021_mun.xlsx")


# Total and propotional deforestation ------------------------------------------

total_prop_defo_mun <- mun_data[,c("Município", "Estado", "total_defo_ha", "prop_reg_defo_ha")]

total_prop_defo_mun <- total_prop_defo_mun %>% 
  arrange(desc(prop_reg_defo_ha))

#writexl::write_xlsx(total_prop_defo_mun, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/total_prop_defo_mun.xlsx")



# States -----------------------------------------------------------------------
# ------------------------------------------------------------------------------


# cleaning directory -----------------------------------------------------------
rm(list = ls())


estados <- sf::st_read("D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/_estados_all_areas_prop_total_reg_defo.shp")
colnames(estados)

estados_data <- data.frame(estados[,c("NM_UF", "sc_fr_P", "fr_r_st",
                                      "prp_sc_", "for_reg", "defo_est",
                                      "prop_defo")])[,c(1:8)]

# Column names

# sc_fr_P = area of secondary forest that regenerated between 2011 - 2020
# fr_r_st = area of forest in 2010
# prp_sc_ = Proportion of secondary forest that regenerated between 2011 - 2020 in relation to the total amount in 2010
# for_reg = Total amount of forest that regenerated between 2011 and 2020
# defo_est = Total amount of deforestation of the total amount of forest that regenerated between 2011 and 2020
# prop_defo = Secondary forest deforestation in relation to the total amount of forest that regenerated

colnames(estados_data) <- c("Estado", "reg_2011_2020_ha", "total_area_2010_ha",
                            "prop_reg_ha", "total_reg_ha", "total_defo_ha",
                            "prop_reg_defo_ha")


# Reg 2011_2020 ----------------------------------------------------------------

reg_2011_2020_estados <- estados_data[,c("Estado", "reg_2011_2020_ha",
                                         "prop_reg_ha", "total_area_2010_ha")]

reg_2011_2020_estados <- reg_2011_2020_estados %>% 
  arrange(desc(reg_2011_2020_ha)) %>% 
  mutate(across(c("reg_2011_2020_ha", "prop_reg_ha", "total_area_2010_ha"),
                ~ .x /10000))

#writexl::write_xlsx(reg_2011_2020_estados, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/reg_2011_2020_estados.xlsx")


# Total reg --------------------------------------------------------------------

total_reg_estados <- estados_data[,c("Estado", "total_reg_ha")]

total_reg_estados <- total_reg_estados %>% 
  arrange(desc(total_reg_ha)) %>% 
  mutate(across(c("total_reg_ha"),
                ~ .x /10000))

#writexl::write_xlsx(total_reg_estados, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/total_reg_2011_2020_estados.xlsx")


# Total and propotional deforestation ------------------------------------------

total_prop_defo_estados <- estados_data[,c("Estado", "total_defo_ha", "prop_reg_defo_ha")]

total_prop_defo_estados <- total_prop_defo_estados %>% 
  arrange(desc(total_defo_ha)) %>% 
  mutate(across(c("total_defo_ha", "prop_reg_defo_ha"),
                ~ .x /10000))

#writexl::write_xlsx(total_prop_defo_estados, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/total_prop_defo_estados.xlsx")



# Quilombola, Assentamento, Terra Indígena, UC ---------------------------------
# ------------------------------------------------------------------------------


# cleaning directory -----------------------------------------------------------
rm(list = ls())


# Quilombola -------------------------------------------------------------------

reg_2011_2020_quilombola <- sf::st_read("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_area_quilombola_incra2024_Area.shp")

reg_2011_2020_quilombola <- data.frame(reg_2011_2020_quilombola[,c("nm_cmnd", "nm_mncp", "cd_uf",
                                                                   "forst_r", "scndry_", "prop_rg")])[,1:6]

# Column names, meaning

# forst_r = Amount of forest in 2010
# scndry_ = Amount of secondary forest 2011-2020
# prop_rg = Proportion of regenerated forest in relation to the amount in 2010

colnames(reg_2011_2020_quilombola) <- c("nome_quilombo", "municipio_quilombo", "estado_quilombo",
                                        "total_area_2010_ha", "reg_2011_2020_ha", "prop_reg_ha")


reg_2011_2020_quilombola <- reg_2011_2020_quilombola %>% 
  arrange(desc(reg_2011_2020_ha)) %>% 
  mutate(across(c("total_area_2010_ha", "reg_2011_2020_ha", "prop_reg_ha"),
                ~ .x /10000))

#writexl::write_xlsx(reg_2011_2020_quilombola, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/reg_2011_2020_quilombo.xlsx")



# Assentamento -----------------------------------------------------------------

# cleaning directory -----------------------------------------------------------
rm(list = ls())

reg_2011_2020_assentamento <- sf::st_read("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_assentamento_incra2024_Area.shp")

reg_2011_2020_assentamento <- data.frame(reg_2011_2020_assentamento[,c("nom_prj", "municip", "uf",
                                                                   "forst_r", "scndry_", "prop_rg")])[,1:6]

# Column names, meaning

# forst_r = Amount of forest in 2010
# scndry_ = Amount of secondary forest 2011-2020
# prop_rg = Proportion of regenerated forest in relation to the amount in 2010

colnames(reg_2011_2020_assentamento) <- c("nome_assentamento", "municipio_assentamento", "estado_assentamento",
                                          "total_area_2010_ha", "reg_2011_2020_ha", "prop_reg_ha")


reg_2011_2020_assentamento <- reg_2011_2020_assentamento %>% 
  arrange(desc(reg_2011_2020_ha)) %>% 
  mutate(across(c("total_area_2010_ha", "reg_2011_2020_ha", "prop_reg_ha"),
                ~ .x /10000))

#writexl::write_xlsx(reg_2011_2020_assentamento, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/reg_2011_2020_assentamento.xlsx")


# Terra indígena ---------------------------------------------------------------

# cleaning directory -----------------------------------------------------------
rm(list = ls())

reg_2011_2020_TI <- sf::st_read("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_tis_funai2024_Area.shp")

reg_2011_2020_TI <- data.frame(reg_2011_2020_TI[,c("terr_nm", "municp_", "uf_sigl",
                                                   "forst_r", "scndry_", "prop_rg")])[,1:6]

# Column names, meaning

# forst_r = Amount of forest in 2010
# scndry_ = Amount of secondary forest 2011-2020
# prop_rg = Proportion of regenerated forest in relation to the amount in 2010

colnames(reg_2011_2020_TI) <- c("nome_TI", "municipio_TI", "estado_TI",
                                "total_area_2010_ha", "reg_2011_2020_ha", "prop_reg_ha")


reg_2011_2020_TI <- reg_2011_2020_TI %>% 
  arrange(desc(reg_2011_2020_ha)) %>% 
  mutate(across(c("total_area_2010_ha", "reg_2011_2020_ha", "prop_reg_ha"),
                ~ .x /10000)) %>% 
  mutate(across(where(is.numeric), round))

#writexl::write_xlsx(reg_2011_2020_TI, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/reg_2011_2020_TI.xlsx")


# UC ---------------------------------------------------------------------------

# cleaning directory -----------------------------------------------------------
rm(list = ls())

reg_2011_2020_UC <- sf::st_read("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_UC_mma2024_Area.shp")

reg_2011_2020_UC <- data.frame(reg_2011_2020_UC[,c("nome_uc", "grupo", "categor", 
                                                   "municip", "uf", "scndry_")])[,1:6]

# Column names, meaning

# forst_r = Amount of forest in 2010
# scndry_ = Amount of secondary forest 2011-2020
# prop_rg = Proportion of regenerated forest in relation to the amount in 2010

colnames(reg_2011_2020_UC) <- c("nome_UC", "Grupo", "Categoria",
                                "municipio_UC", "estado_UC",
                                "reg_2011_2020_ha")


reg_2011_2020_UC <- reg_2011_2020_UC %>% 
  arrange(desc(reg_2011_2020_ha)) %>% 
  mutate(across(c("reg_2011_2020_ha"),
                ~ .x /10000))

#writexl::write_xlsx(reg_2011_2020_UC, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/reg_2011_2020_UC.xlsx")


################################################################################
# Area of regenerated forest per year 2011 - 2021

# cleaning directory -----------------------------------------------------------
rm(list = ls())


# Changing CRS from WGS84 to SAD69 Brazil Polyconic ----------------------------

dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto"
setwd(dir)

reg_year <- list.files(dir, pattern = "_1ha.tif")

stack_reg_year <- terra::rast(reg_year)

for(i in 1:length(names(stack_reg_year))){
obj_names <- gsub(".tif", "_SAD69",reg_year[i])
projected_raster <- terra::project(stack_reg_year[[i]], "EPSG:29101", method = "mode")
output_path <- file.path(dir, paste(obj_names, ".tif", sep = ""))
terra::writeRaster(x = projected_raster, filename = output_path,
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
}

# ------------------------------------------------------------------------------

# cleaning directory
rm(list = ls())

dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto"
setwd(dir)

reg_year_sad69 <- list.files(dir, pattern = "_1ha_SAD69.tif")

stack_reg_year <- terra::rast(reg_year_sad69)


for(i in 1:length(names(stack_reg_year))){
  obj_names <- gsub(".tif", "", reg_year_sad69[i])
  pixel_area <- cellSize(stack_reg_year[[i]], unit = "m")
  pixel_area_only_1 <- pixel_area * stack_reg_year[[i]]
  output_path <- file.path(dir, paste(obj_names, "_Area.tif", sep = ""))
  terra::writeRaster(x = pixel_area_only_1, filename = output_path,
                     gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
}


# Calculating area of forest regeneration for each year ------------------------

# cleaning directory
rm(list = ls())

dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto"
setwd(dir)

reg_year_sad69_Area <- list.files(dir, pattern = "_1ha_SAD69_Area.tif")

stack_reg_year <- raster::stack(reg_year_sad69_Area)


# Loop to save area values for each individual pixel and saving in a matrix

names <- gsub(".tif", "", reg_year_sad69_Area)

mtx <- matrix(names, ncol = 2, nrow = 11)

for (i in 1:length(reg_year_sad69_Area)) {
  mtx[i,2] <- cellStats(stack_reg_year[[i]], stat = 'sum')
}

areas_reg_per_year <- data.frame(mtx)
areas_reg_per_year[,2] <- as.numeric(areas_reg_per_year[,2])

areas_reg_per_year[,2] <- areas_reg_per_year[,2]/10000

colnames(areas_reg_per_year) <- c("reg_year", "area_ha")
areas_reg_per_year[,2] <- round(areas_reg_per_year[,2])

write_xlsx(areas_reg_per_year, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/reg_per_year.xlsx")


################################################################################
# Creating the Bivariate map ---------------------------------------------------

# cleaning directory
rm(list = ls())

# Loading Municipalities

mun <- sf::st_read("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_mun_all_areas_prop_total_reg_defo.shp")


# Ensure the variables are numeric
mun <- mun %>%
  mutate(for_reg = as.numeric(for_reg),
         defo_mun = as.numeric(defo_mun))


# Remove municipalities where both variables are zero
mun <- mun %>%
  filter(!(for_reg == 0 & defo_mun == 0)) %>%
  filter(!is.na(for_reg) & !is.na(defo_mun))


# Remove NA
mun <- mun %>%
  filter(!is.na(for_reg) & !is.na(defo_mun))


# Define number of classes (3 per variable)
num_classes <- 3

# Function to create unique breaks
get_unique_breaks <- function(var, num_classes) {
  breaks <- classIntervals(var, num_classes, style = "quantile")$brks
  unique_breaks <- unique(breaks)  # Remove duplicates
  if (length(unique_breaks) < num_classes + 1) {
    unique_breaks <- seq(min(var), max(var), length.out = num_classes + 1)  # Create equal breaks
  }
  return(unique_breaks)
}


# Classify "for_reg" using unique breaks
breaks_for_reg <- get_unique_breaks(mun$for_reg, num_classes)
mun <- mun %>%
  mutate(class_for_reg = cut(for_reg, breaks = breaks_for_reg, include.lowest = TRUE, labels = c(1, 2, 3)))


# Classify "defo_mun" using unique breaks
breaks_defo_mun <- get_unique_breaks(mun$defo_mun, num_classes)
mun <- mun %>%
  mutate(class_defo_mun = cut(defo_mun, breaks = breaks_defo_mun, include.lowest = TRUE, labels = c(1, 2, 3)))


# Combine classifications into a bivariate category
mun <- mun %>%
  mutate(bivar_class = paste0(class_for_reg, "-", class_defo_mun))


# Define bivariate color scheme
bivar_colors <- data.frame(
  bivar_class = c("3-1", "3-2", "3-3",
                  "2-1", "2-2", "2-3",
                  "1-1", "1-2", "1-3"),
  bivar_color = c("#1A9850", "#E6F598", "#FEE08B",  # Green - High Regen, Low Deforest
                  "#3288BD", "#66C2A5", "#FDAE61",  # Blue - Medium Regen, Medium Deforest
                  "#3F007D", "#D73027", "#A50026")  # Purple - Low Regen, High Deforest
)


# Merge color data into the municipality dataset
mun <- mun %>%
  left_join(bivar_colors, by = "bivar_class")

# Save the modified polygon for use in QGIS
st_write(mun, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_bivariate_map_muni.shp", delete_dsn = TRUE)


################################################################################
# Bar chart reg per year -------------------------------------------------------

# cleaning directory
rm(list = ls())

# Loading excel spreadsheet
reg_per_year <- readxl::read_excel("D:/__PESSOAL/Vinicius_T/data_frames_result_areas/reg_per_year.xlsx")

(bar_chart <- ggplot(reg_per_year, aes(x = factor(reg_year), y = area_ha))+
             geom_bar(stat = "identity", fill = "gray50") +
             labs(x = "", y = "Area of regenerated forest (thousand hectares)", title = "") +
             scale_y_continuous(breaks = c(50000, 100000, 150000, 200000, 224000),
                                labels = c("50", "100", "150", "200", "224"),
                                expand = c(0.01, 0))+
             theme_classic() + 
             theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                   axis.text.y = element_text(size = 12),       
                   axis.title.x = element_text(size = 14),      
                   axis.title.y = element_text(size = 14)))

#ggsave("D:/__PESSOAL/Vinicius_T/bar_chart/bar_chart.png", plot = bar_chart, width = 20, height = 15, units = "cm")


################################################################################
# Bar chart reg per state ------------------------------------------------------

# cleaning directory
rm(list = ls())

# Loading excel spreadsheet
reg_per_state <- readxl::read_excel("D:/_Vinicius/artigos/2024.12.d04 - Pacto, secondary forests, natural regeneration/dataframes/dataframes/reg_2011_2020_estados.xlsx")


reg_per_state <- reg_per_state %>% 
  filter(reg_2011_2020_ha != 0) %>% 
  select(-prop_reg_ha) %>% 
  rename(state = Estado, area_ha = reg_2011_2020_ha) %>% 
  mutate(rank = rank(-area_ha), 
         group = factor(ifelse(rank <= 8, "Top 8", "Bottom 7"), levels = c("Top 8", "Bottom 7"))) %>% 
  mutate(case_when(
    state == "Minas Gerais" ~ "MG",
    state == "Paraná" ~ "PR",
    state == "Bahia" ~ "BA",
    state == "São Paulo" ~ "SP",
    state == "Santa Catarina" ~ "SC",
    state == "Rio Grande do Sul" ~ "RS",
    state == "Espírito Santo" ~ "ES",
    state == "Rio de Janeiro" ~ "RJ",
    state == "Pernambuco" ~ "PE",
    state == "Alagoas" ~ "AL",
    state == "Sergipe" ~ "SE",
    state == "Mato Grosso do Sul" ~ "MS",
    state == "Paraíba" ~ "PB",
    state == "Goiás" ~ "GO",
    state == "Rio Grande do Norte" ~ "RN",
  )) %>%
  select(c("case_when(...)", "area_ha", "rank", "group")) %>% 
  rename(state = `case_when(...)`)




(bar_chart_state <- ggplot(reg_per_state, aes(x = reorder(state, -area_ha), y = area_ha)) +
    geom_bar(stat = "identity", fill = "#7B9FCF", width = 0.8) +
    labs(x = "", y = "", title = "") +
    scale_y_continuous(
      breaks = c(0, 100000, 200000, 300000, 423887),
      labels = c("0","100", "200", "300", "423"),
      expand = expansion(mult = c(0.02, 0.05)) 
    ) +
    theme_classic() +
    theme(
      strip.text = element_blank(),
      text = element_text(size = 28),
      axis.text.y = element_text(margin = margin(r = 4)),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ))

ggsave("D:/_Vinicius/artigos/2024.12.d04 - Pacto, secondary forests, natural regeneration/Figuras/Bar Chart/bar_chart_REG_state.png", plot = bar_chart_state, width = 60, height = 20, units = "cm")


## Defo per state

defo_per_state <- readxl::read_excel("D:/_Vinicius/artigos/2024.12.d04 - Pacto, secondary forests, natural regeneration/dataframes/dataframes/total_defo_states.xlsx")


state_order <- rev(c("MG", "PR", "BA", "SP",  
  "SC", "RS", "ES", "RJ", "PE", "AL", "SE", 
  "MS", "PB", "GO", "RN"
))


defo_per_state <- defo_per_state %>% 
  filter(defo_est != 0) %>% 
  mutate(case_when(
    NM_UF == "Minas Gerais" ~ "MG",
    NM_UF == "Paraná" ~ "PR",
    NM_UF == "Bahia" ~ "BA",
    NM_UF == "São Paulo" ~ "SP",
    NM_UF == "Santa Catarina" ~ "SC",
    NM_UF == "Rio Grande do Sul" ~ "RS",
    NM_UF == "Espírito Santo" ~ "ES",
    NM_UF == "Rio de Janeiro" ~ "RJ",
    NM_UF == "Pernambuco" ~ "PE",
    NM_UF == "Alagoas" ~ "AL",
    NM_UF == "Sergipe" ~ "SE",
    NM_UF == "Mato Grosso do Sul" ~ "MS",
    NM_UF == "Paraíba" ~ "PB",
    NM_UF == "Goiás" ~ "GO",
    NM_UF == "Rio Grande do Norte" ~ "RN",
  )) %>%
  select(c("case_when(...)", defo_est)) %>% 
  rename(state = `case_when(...)`) %>% 
  mutate(state = factor(state, levels = state_order))


(bar_chart_DEFO_state <- ggplot(defo_per_state, aes(x = state, y = defo_est/10000)) +
    geom_bar(stat = "identity", fill = "#ee6b6e", width = 0.8) +
    labs(x = "", y = "", title = "") +
    scale_y_reverse(  # Reverse the y-axis
      breaks = c(0, 20000, 40000, 60000, 87755),
      labels = c("0", "20", "40", "60", "87"),
      expand = expansion(mult = c(0.19, 0.05))  # Reduce space below the bars
    ) +
    theme_classic() +
    theme(
      axis.text.y = element_text(margin = margin(r = 10)),
      axis.ticks.y = element_line(),  
      axis.line.y = element_line(),  
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text.x = element_text(angle = 45, hjust = 1.5, vjust = 3), 
      text = element_text(size = 25)
    ))


ggsave("D:/_Vinicius/artigos/2024.12.d04 - Pacto, secondary forests, natural regeneration/Figuras/Bar Chart/bar_chart_DEFO_state.png", plot = bar_chart_DEFO_state, width = 60, height = 20, units = "cm")


################################################################################
## APP FBDS --------------------------------------------------------------------

APP <- terra::vect("D:/__PESSOAL/Vinicius_T/app_FBDS/app fbds/app.gpkg")
plot(APP)


################################################################################
## Anual deforestation 

# cleaning directory
rm(list = ls())

annual_stack <- terra::rast(c("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2011.tif",
                             "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2012.tif",
                             "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2013.tif",
                             "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2014.tif",
                             "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2015.tif",
                             "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2016.tif",
                             "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2017.tif",
                             "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2018.tif",
                             "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2019.tif",
                             "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2020.tif",
                             "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2021.tif"))

AF <- terra::vect("D:/__PESSOAL/Vinicius_T/Limite Mata Atlantica/bioma_MA_IBGE_250mil/bioma_MA_IBGE_250mil.shp")
  
dir <- "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/"

for (i in 1:length(names(annual_stack))) {
  obj_name <- names(annual_stack[[i]])
  raster <- mask(crop(annual_stack[[i]], AF), AF)
  terra::writeRaster(raster, paste(dir, obj_name, "_AF.tif", sep = ""),
    gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)
}

# Cropping 2023 after asking Marcos --------------------------------------------


MB_2023 <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2023.tif")
MB_2023_AF <- mask(crop(MB_2023, AF), AF)
terra::writeRaster(MB_2023_AF, paste(dir, "brasil_coverage_2023", "_AF.tif", sep = ""),
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


################################################################################
## Proportion of deforestation in relation to 2010

# cleaning directory 
rm(list = ls())

mun <- terra::vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_mun_all_areas_prop_total_reg_defo.shp")
names(mun)

# fr_r_mn - total amount of forest in 2010
# defo_mun - deforestation of all forest that regenerated (individual years summed up) 

# Checking if area values are in hectare

max(data.frame(mun[,"defo_mun"])) # hectares
max(data.frame(mun[,"fr_r_mn"])) # square meters

defo_mun <- data.frame(mun[,"defo_mun"])
forest_area_2010 <- data.frame(mun[,"fr_r_mn"])/10000

prop_defo_2010 <- defo_mun/forest_area_2010

# Adding prportional deforestation in relation to 2010 in the mun polygon
mun[,ncol(mun)+1] <- prop_defo_2010
ncol(mun) #20
data.frame(mun[,ncol(mun)])

# pdefo_10 - proportion of total deforestation in relation to 2010
names(mun)[ncol(mun)] <- "pdefo_10"
names(mun)

#terra::writeVector(mun, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_mun_all_areas_prop_total_reg_defo.shp", overwrite = T)

# Calculating quantiles to mask Mun with low forest cover

forest_2010_higher_zero <- forest_area_2010[forest_area_2010 > 0]

quantile(forest_2010_higher_zero, probs = 0.5, na.rm = T)
max(forest_2010_higher_zero)



################################################################################
# Loss of annual regeneration in relation to 2023

# cleaning directory 
rm(list = ls())


# Converting MB 2023 to forest only --------------------------------------------
MB_2023_AF <- raster::raster("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2023_AF.tif")


# Converting pixel values MB 2023

reclass_matrix <- matrix(c(0, 0,
                           1, 0,
                           3, 1,
                           4, 0,
                           5, 1,
                           6, 0,
                           49, 1,
                           10, 0,
                           11, 0,
                           12, 0,
                           32, 0,
                           29, 0,
                           50, 0,
                           14, 0,
                           15, 0,
                           18, 0,
                           19, 0,
                           39, 0,
                           20, 0,
                           40, 0,
                           62, 0,
                           41, 0,
                           36, 0,
                           46, 0,
                           47, 0,
                           35, 0,
                           48, 0,
                           9,  0,
                           21, 0,
                           22, 0,
                           23, 0,
                           24, 0,
                           30, 0,
                           25, 0,
                           26, 0,
                           33, 0,
                           31, 0,
                           27, 0),
                           ncol = 2, byrow = T)

MB_2023_AF_forest_only <- raster::reclassify(MB_2023_AF, reclass_matrix)

#raster::writeRaster(MB_2023_AF_forest_only,
#                    "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2023_AF_forest_only.tif",
#                    options = c("COMPRESS=LZW", "ZLEVEL=9"))


# Converting MB Brasil AF pixel values

MB_2023_AF_forest_only <- raster::raster("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2023_AF_forest_only.tif")

reclass_matrix <- matrix(c(0, 0,
                           1, 2),
                         ncol = 2, byrow = T)

MB_2023_AF_forest_only_pixel_2 <- raster::reclassify(MB_2023_AF_forest_only, reclass_matrix)

#raster::writeRaster(MB_2023_AF_forest_only_pixel_2,
#                    "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2023_AF_forest_only_pixel_2.tif",
#                    options = c("COMPRESS=LZW", "ZLEVEL=9"))

# Aligning MB and annual regeneration to sum rasters ---------------------------


MB_2023_AF_forest_only <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2023_AF_forest_only.tif")

dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto"
reg_year <- list.files(dir, pattern = "_1ha.tif")

setwd(dir)
stack_reg_year <- terra::rast(reg_year)

reg_resampled_AF_stack <- terra::resample(stack_reg_year, MB_2023_AF_forest_only, method = "near")

#raster::writeRaster(reg_resampled_AF_stack,
#                    "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 #Pacto/annual_reg_stack_resampled_MB_AF.tif")



# ---------------------------------------------------------------------------------------
## Testing if there are pixels of annual regeneration that are not classified as forest in 2023

# cleaning directory
rm(list = ls())


reg_annual_year_stack <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_stack_resampled_MB_AF.tif")

forest_only_AF_2023 <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2023_AF_forest_only.tif")

mtx <- matrix(ncol = 11, nrow = 3, byrow = F)
colnames(mtx) <- 2011:2021

for (i in 1:11) {
  reg_year_not_forest_2023 <- reg_annual_year_stack[[i]] - forest_only_AF_2023
  mtx[,i] <- terra::unique(reg_year_not_forest_2023)
}

# ------------------------------------------------------------------------------

# Pixels of annual reg that are not forest in the layer Reg 2011-2021


# cleaning directory
rm(list = ls())


reg_annual_year_stack <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_stack_resampled_MB_AF.tif")

reg_2011_2021 <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21.tif")



