# Code used to calculate the amount of regenerated vegetation in the Atlantic Forest
# Vinicius Tonetti - vrtonetti@ufscar.br

# Loading packages -------------------------------------------------------------

library(terra)
library(sf)
library(exactextractr)
library(raster)
library(tidyverse)
library(writexl)
library(future.apply)


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

# Converting the raster to a "raster::" object
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

# MB 2010 in WGS84
MB_2010 <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/MB_09_AF_2010_WGS_84.tif")
#plot(MB_2010)

# Reg map in WGS84
reg <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21.tif")
#plot(reg)

# Resample to match extent
reg_resampled <- resample(reg, MB_2010, method = "near")
#terra::writeRaster(reg_resampled, "D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21_resampled.tif")

# Masking
masked_MB <- terra::mask(MB_2010, reg_resampled, maskvalue = 0)
#plot(masked_MB)
#terra::writeRaster(masked_MB , "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/previous_land_cover_type.tif")

# Frequency of each previous land cover type
previous_land_use_count <- freq(masked_MB)


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

rast_stack <- rast(c("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen11_1ha.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen12_1ha.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen13_1ha.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen14_1ha.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen15_1ha.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen16_1ha.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen17_1ha.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen18_1ha.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen19_1ha.tif",
                     "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen20_1ha.tif"))

summed_stack <- app(rast_stack, fun = sum, na.rm = T)

#writeRaster(summed_stack,
#            "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/summed_reg.tif",
#            gdal=c("COMPRESS=DEFLATE", "TFW=YES"))

# Convert values = 2, to 1

summed_stack <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/summed_reg.tif")

# Trying using raster package, as it is not working with terra

summed_stack_r <- raster::raster(summed_stack)

reclass_matrix <- matrix(c(0, 0,
                           1, 1,
                           2, 1),
                        ncol = 2, byrow = T)

summed_stack_1_only <- raster::reclassify(summed_stack_r, reclass_matrix)

#raster::writeRaster(summed_stack_1_only,
#                    "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/summed_reg_1_only.tif",
#                    options = c("COMPRESS=LZW", "ZLEVEL=9"))


# Raster of the final secondary forest map
reg_11_21 <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21.tif")

secondary_forest_loss <- summed_stack_1_only - reg_11_21
plot(secondary_forest_loss)

reclass_matrix <- matrix(c(0, 0,
                           1, 1,
                           -1, 0),
                         ncol = 2, byrow = T)

secondary_forest_loss <- raster::reclassify(secondary_forest_loss, reclass_matrix)


raster::writeRaster(secondary_forest_loss,
                    "D:/__PESSOAL/Vinicius_T/raster_pacto/secondary_forest_loss.tif",
                    options = c("COMPRESS=LZW", "ZLEVEL=9"))

# Converting to a Terra object to project to EPSG 29101 and calculate area
secondary_forest_loss <- terra::rast(secondary_forest_loss)

secondary_forest_loss <- terra::project(secondary_forest_loss , "EPSG:29101", method = "mode") # using the method "mode" to interpolate

# Computing the area of each pixel of the secondary forest patches
pixel_area <- cellSize(secondary_forest_loss, unit = "m")

# Considering values only for forest pixels
secondary_forest_loss_Area <- secondary_forest_loss * pixel_area
plot(secondary_forest_loss_Area)

#terra::writeRaster(secondary_forest_loss_Area,
#                   "D:/__PESSOAL/Vinicius_T/raster_pacto/secondary_forest_loss_Area.tif",
#                    gdal=c("COMPRESS=DEFLATE", "TFW=YES"))


all_secondary_forest <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/summed_reg_1_only.tif")


all_secondary_forest <- terra::project(all_secondary_forest , "EPSG:29101", method = "mode") # using the method "mode" to interpolate

# Computing the area of each pixel of the secondary forest patches
pixel_area <- cellSize(all_secondary_forest, unit = "m")

# Considering values only for forest pixels
all_secondary_forest_Area <- all_secondary_forest * pixel_area
plot(all_secondary_forest_Area)

#terra::writeRaster(all_secondary_forest_Area,
#                   "D:/__PESSOAL/Vinicius_T/raster_pacto/All_secondary_forest_loss_Area.tif",
#                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"))



# Extracting areas of all forest regeneration and secondary forest loss to polygons
# ----------------------------------------------------------------------------------

# cleaning directory -----------------------------------------------------------
rm(list = ls())


all_secondary_forest <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/All_secondary_forest_loss_Area.tif")
secondary_forest_loss <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/secondary_forest_loss_Area.tif")

mun <- sf::st_read("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_mun_all_areas_prop.shp")
estados <- sf::st_read("D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/_estados_all_areas_prop.shp")

for_reg_mun <- extract_area <- exactextractr::exact_extract(all_secondary_forest, mun, "sum")
for_reg_state <- extract_area <- exactextractr::exact_extract(all_secondary_forest, estados, "sum")

defo_mun <- extract_area <- exactextractr::exact_extract(secondary_forest_loss, mun, "sum")
defo_state <- extract_area <- exactextractr::exact_extract(secondary_forest_loss, estados, "sum")


# Creating a new column to add area of regenerating forest

ncol(mun)

mun[,18] <- for_reg_mun 
colnames(mun)[18] <- "for_reg"

mun[,19] <- defo_mun
colnames(mun)[19] <- "defo_mun"

# Proportion of deforestation in relation to the total amount of forest that regenerated 
mun[,20] <- data.frame(mun[,19])[,1]/data.frame(mun[,18])[,1]
colnames(mun)[20] <- "prop_defo"

#sf::st_write(mun, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_mun_all_areas_prop_total_reg_defo.shp")



ncol(estados)

estados[,11] <- for_reg_state 
colnames(estados)[11] <- "for_reg"

estados[,12] <- defo_state
colnames(estados)[12] <- "defo_est"

# Proportion of deforestation in relation to the total amount of forest that regenerated 
estados[,13] <- data.frame(estados[,12])[,1]/data.frame(estados[,11])[,1]
colnames(estados)[13] <- "prop_defo"

#sf::st_write(estados, "D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/_estados_all_areas_prop_total_reg_defo.shp")


################################################################################
## Saving area values in dataframes --------------------------------------------


# Municipalities ---------------------------------------------------------------
# ------------------------------------------------------------------------------


# cleaning directory -----------------------------------------------------------
rm(list = ls())


mun <- sf::st_read("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/_mun_all_areas_prop_total_reg_defo.shp")
colnames(mun)

mun_data <- data.frame(mun[,c("NM_MUN", "NM_UF", "sec_for", "fr_r_mn", "prp_sc_", "for_reg", "defo_mun", "prop_defo")])[,c(1:8)]

colnames(mun_data) <- c("Município", "Estado", "reg_2011_2020_ha", "total_area_2010_ha",
                        "prop_reg_ha", "total_reg_ha", "total_defo_ha", "prop_reg_defo_ha")


# Reg 2011_2020 ----------------------------------------------------------------

reg_2011_2020_mun <- mun_data[,c("Município", "Estado", "reg_2011_2020_ha",
                                 "prop_reg_ha", "total_area_2010_ha")]

reg_2011_2020_mun <- reg_2011_2020_mun %>% 
  arrange(desc(reg_2011_2020_ha)) %>% 
  mutate(across(c("reg_2011_2020_ha", "prop_reg_ha", "total_area_2010_ha"),
                 ~ .x /10000))

#writexl::write_xlsx(reg_2011_2020_mun, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/reg_2011_2020_mun.xlsx")


# Total reg --------------------------------------------------------------------

total_reg_mun <- mun_data[,c("Município", "Estado", "total_reg_ha")]

total_reg_mun <- total_reg_mun %>% 
  arrange(desc(total_reg_ha)) %>% 
  mutate(across(c("total_reg_ha"),
                ~ .x /10000))

#writexl::write_xlsx(total_reg_mun, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/total_reg_2011_2020_mun.xlsx")


# Total and propotional deforestation ------------------------------------------

total_prop_defo_mun <- mun_data[,c("Município", "Estado", "total_defo_ha", "prop_reg_defo_ha")]

total_prop_defo_mun <- total_prop_defo_mun %>% 
  arrange(desc(total_defo_ha)) %>% 
  mutate(across(c("total_defo_ha", "prop_reg_defo_ha"),
                ~ .x /10000))

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
                                                   "municip", "uf",
                                                   "forst_r", "scndry_", "prop_rg")])[,1:8]

# Column names, meaning

# forst_r = Amount of forest in 2010
# scndry_ = Amount of secondary forest 2011-2020
# prop_rg = Proportion of regenerated forest in relation to the amount in 2010

colnames(reg_2011_2020_UC) <- c("nome_UC", "Grupo", "Categoria",
                                "municipio_UC", "estado_UC",
                                "total_area_2010_ha", "reg_2011_2020_ha", "prop_reg_ha")


reg_2011_2020_UC <- reg_2011_2020_UC %>% 
  arrange(desc(reg_2011_2020_ha)) %>% 
  mutate(across(c("total_area_2010_ha", "reg_2011_2020_ha", "prop_reg_ha"),
                ~ .x /10000)) %>% 
  mutate(across(where(is.numeric), round))

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



