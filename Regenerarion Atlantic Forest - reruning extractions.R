# Code used to rerun the extraction 
# Vinicius Tonetti - vrtonetti@ufscar.br

# packages

library(terra)
library(sf)

# cleaning directory 
rm(list = ls())

# Considering only Atlantic Forest municipalities

AF <- st_read("D:/__PESSOAL/Vinicius_T/Limite Mata Atlantica/bioma_MA_IBGE_250mil/bioma_MA_IBGE_250mil.shp")
mun <- st_read("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/BR_Municipios_2023.shp")
mun_WGS <- st_transform(mun, st_crs(AF))

# Municipalities that intersect AF 

mun_AF_WGS <- mun_WGS[st_intersects(mun_WGS, AF, sparse = F), ]

# Writing vector

st_write(mun_AF_WGS, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_AF_WGS.shp")

