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


## Cropping Annual reg raster --------------------------------------------------

AF <- vect("D:/__PESSOAL/Vinicius_T/Limite Mata Atlantica/bioma_MA_IBGE_250mil/bioma_MA_IBGE_250mil.shp")

annual_rast <- rast(c("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen11_1ha.tif",
                      "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen12_1ha.tif",
                      "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen13_1ha.tif",
                      "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen14_1ha.tif",
                      "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen15_1ha.tif",
                      "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen16_1ha.tif",
                      "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen17_1ha.tif",
                      "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen18_1ha.tif",
                      "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen19_1ha.tif",
                      "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen20_1ha.tif",
                      "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/regen21_1ha.tif"))

dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/"

obj_name <- paste0("annual_reg_", 11:21)

for (i in 1:length(names(annual_rast))) {
  raster <- mask(crop(annual_rast[[i]], AF), AF)
  terra::writeRaster(raster, paste(dir, obj_name[i], "AF.tif", sep = ""),
                     gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)
}







