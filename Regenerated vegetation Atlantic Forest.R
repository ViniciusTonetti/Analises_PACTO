# Code used to calculate the amount of regenerated vegetation in the Atlantic Forest
# Vinicius Tonetti - vrtonetti@ufscar.br

# Loading packages

library(terra)
library(sf)

# cleaning directory
rm(list = ls())

# Municipalities
# Shapefile downloaded from https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html on 15/02/2025
#mun <- terra::vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/BR_Municipios_2023.shp")


# Regeneration 11_21
#reg_11_21 <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21.tif")


# Projecting municipalities and raster data CRS to SAD69
#mun <- project(mun, "EPSG:4291")
#reg_11_21  <- project(reg_11_21, "EPSG:4291", method = "mode")

# Saving raster SAD69
#terra::writeRaster(reg_11_21, "D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_mode.tif")

# Saving municipalities polygons in SAD69
#terra::writeVector(mun, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/BR_Municipios_2023_SAD.shp")


# Loading raster and municipalities in SAD69

mun_SAD69 <- terra::vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/BR_Municipios_2023_SAD.shp")
reg_11_21_SAD69 <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69.tif")

# Computing the area of each pixel
pixel_area <- cellSize(reg_11_21_SAD69, unit = "m")

pixel_area_1_only <- reg_11_21_SAD69 * pixel_area
plot(pixel_area_1_only)


# Saving raster with area of each pixel
terra::writeRaster(pixel_area_1_only, "D:/__PESSOAL/Vinicius_T/raster_pacto/reg_11_21_SAD69_Area.tif", overwrite = T)



