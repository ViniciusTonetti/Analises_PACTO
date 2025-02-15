# Code used to calculate the amount of regenerated vegetation in the Atlantic Forest
# Vinicius Tonetti - vrtonetti@ufscar.br

# Loading packages

library(terra)
library(sf)

# cleaning directory
rm(list = ls())

# Municipalities
# Shapefile downloaded from https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html on 15/02/2025
mun <- terra::vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/BR_Municipios_2023.shp")


# Regeneration 11_21
reg_11_21 <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21.tif")


# Projecting municipalities and raster data CRS to SAD69
mun <- project(mun, "EPSG:4291")
reg_11_21  <- project(reg_11_21, "EPSG:4291")

