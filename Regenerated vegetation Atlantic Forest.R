# Code used to calculate the amount of regenerated vegetation in the Atlantic Forest
# Vinicius Tonetti - vrtonetti@ufscar.br

# Loading packages

library(terra)
library(sf)
library(exactextractr)
library(raster)
library(tidyverse)
library(writexl)


# cleaning directory
rm(list = ls())

# Municipalities

# Shapefile downloaded from https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html on 15/02/2025
#mun <- terra::vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/BR_Municipios_2023.shp")

# Regeneration 11_21
#reg_11_21 <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21.tif")

# Projecting municipalities and raster data CRS to SAD69 Brazil Polyconic
#mun <- project(mun, "EPSG:29101")
#reg_11_21  <- project(reg_11_21, "EPSG:29101", method = "mode") # using the method "mode" to interpolate

# Saving raster SAD69
#terra::writeRaster(reg_11_21, "D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Polyconic.tif")

# Saving municipalities polygons in SAD69
#terra::writeVector(mun, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/BR_Municipios_2023_SAD.shp")


# Loading raster and municipalities in SAD69

mun_SAD69 <- terra::vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/BR_Municipios_2023_SAD.shp")
#reg_11_21_SAD69 <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_mode.tif")

# Computing the area of each pixel of the secondary forest patches
#pixel_area <- cellSize(reg_11_21_SAD69, unit = "m")

#pixel_area_1_only <- reg_11_21_SAD69 * pixel_area
#plot(pixel_area_1_only)


# Saving raster with area of each pixel
#terra::writeRaster(pixel_area_1_only, "D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Area.tif", overwrite = T)

# Loading raster with pixel area, patches only 
#reg_11_21_SAD69 <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Area.tif")

# Loading binary raster 0 - 1
#binary_raster_SAD69 <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_mode.tif")


#raster_ones <- mask(reg_11_21_SAD69, binary_raster_SAD69, maskvalue = 0)
#plot(raster_ones)

#terra::writeRaster(raster_ones, "D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Area_patch_only.tif", overwrite = T)

# Loading binary raster with pixel area only for forest patches
reg_11_21_SAD69_area_forest_only <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Area_patch_only.tif")
plot(reg_11_21_SAD69_area_forest_only)


# Calculating the area of forest for each municipality
#area_per_mun <- terra::extract(reg_11_21_SAD69_area_forest_only, mun_SAD69, fun = sum, na.rm = T) # DOES NOT WORK

# Extracting values using the "exactextractr" package as it did not run in Terra

# Converting the raster to a "raster::" object
reg_11_21_SAD69_area_forest_only_raster <- raster::raster(reg_11_21_SAD69_area_forest_only)

# Converting municipality shp to sf object
mun_SAD69_sf <- sf::st_as_sf(mun_SAD69)

extract_area <- exactextractr::exact_extract(reg_11_21_SAD69_area_forest_only_raster, mun_SAD69_sf, "sum")
plot(reg_11_21_SAD69_area_forest_only_raster)
#raster::writeRaster(reg_11_21_SAD69_area_forest_only_raster,
#                    "D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Area_patch_only_raster.tif")

# Checking the number of columns = 14
#ncol(mun_SAD69_sf)

# Creating a new column to add area of regenerating forest
#mun_SAD69_sf[,15] <- extract_area
#colnames(mun_SAD69_sf)[15] <- "sec_for"

#length(extract_area)
#nrow(mun_SAD69_sf)

# Save to shapefile
#st_write(mun_SAD69_sf, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_area.shp", delete_dsn = T)


# Loading shapefile with the area of regenerated forests
mun_with_area <- terra::vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_area.shp")

# Extracting and saving values for the ammount of forest in each municipality and saving as Data Frame
names(mun_with_area)
df_area <- as.data.frame(mun_with_area[,c("NM_MUN","NM_UF","sec_for")])

df_area <- df_area %>% 
  arrange(desc(sec_for))

sum(df_area[,"sec_for"])

#writexl::write_xlsx(df_area, "D:/__PESSOAL/Vinicius_T/raster_pacto/reg_by_municipalities.xlsx")


##### Analysis by State --------------------------------------------------------

# Loading state polygon
#state_area <- terra::vect("D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/BR_UF_2023_area.shp")
#plot(state_area)


# Converting the CRS of the states to the same CRS of the raster
state_area_SAD69 <- project(state_area, "EPSG:4291")

# Converting the state polygon to sf object
state_area_SAD69_sf <- sf::st_as_sf(state_area_SAD69)

extract_area_state <- exactextractr::exact_extract(reg_11_21_SAD69_area_forest_only_raster, state_SAD69_sf, "sum")

# Checking the number of columns = 14
ncol(state_SAD69_sf)

# Creating a new column to add area of regenerating forest
state_SAD69_sf[,8] <- extract_area_state
colnames(state_SAD69_sf)[8] <- "sec_for"

#length(extract_area_state)
#nrow(state_SAD69_sf)

# Save to shapefile
#st_write(state_SAD69_sf, "D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/BR_UF_2023_area.shp", delete_dsn = T)



# Loading shapefile with the area of regenerated forests in each state
state_with_area <- terra::vect("D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/BR_UF_2023_area.shp")

# Extracting and saving values for the ammount of forest in each municipality and saving as Data Frame
names(state_with_area)
df_area_by_state <- as.data.frame(state_with_area[,c("NM_UF","sec_for")])

df_area_by_state <- df_area_by_state  %>% 
  arrange(desc(sec_for))

sum(df_area_by_state[,"sec_for"])

#writexl::write_xlsx(df_area_by_state, "D:/__PESSOAL/Vinicius_T/estados_Brasil/BR_UF_2023/reg_by_states.xlsx")

