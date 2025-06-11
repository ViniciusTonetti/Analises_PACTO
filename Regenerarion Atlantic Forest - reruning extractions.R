# Forest recovery in the Atlantic Forest
# Code used to rerun the extraction 
# Vinicius Tonetti - vrtonetti@ufscar.br

# packages

library(terra)
library(raster)
library(tidyverse)
library(sf)
library(exactextractr)
library(writexl)

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


## Calculating total regeneration (including what did not persist)

dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF"

setwd(dir)
annual_reg_stack <- rast(list.files(dir, pattern = "AF.tif"))
annual_reg_stack_sum <- sum(annual_reg_stack, na.rm = T)


#terra::writeRaster(annual_reg_stack_sum, paste(dir, "annual_reg_stack_sum.tif", sep = "/"),
#                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


# Converting values higher than 1 to 1

annual_reg_stack_sum_0_1 <- terra::ifel(annual_reg_stack_sum %in% c(2,3), 1, annual_reg_stack_sum)
plot(annual_reg_stack_sum_0_1)

#terra::writeRaster(annual_reg_stack_sum_0_1, paste(dir, "all_reg.tif", sep = "/"),
#                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)

## Calculating total deforestation

reg_11_21 <- rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21_AF.tif")

total_defo <- annual_reg_stack_sum_0_1 - reg_11_21

#terra::writeRaster(total_defo, paste(dir, "total_defo.tif", sep = "/"),
#                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)




# Estimating annual reg that did not persist until 2023 ------------------------
################################################################################

# cleaning directory 
rm(list = ls())

dir <- ("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/")
setwd(dir)

#stack with annual reg raster
annual_rast_AF <- terra::rast(list.files(dir, pattern = "AF.tif"))

obj_name <- paste0("annual_reg_", 11:21)

reg_11_21 <- rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21_AF.tif")

reg11_21_pixel02 <- terra::ifel(reg_11_21 == 1, 2, reg_11_21) # Reg 2011 - 2021 were converted to pixel values = 2

#terra::writeRaster(reg11_21_pixel02, paste(dir, "reg11_21_pixel02.tif", sep = ""),
#                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)

# Rasters generated by the lines below were checked in QGis, as almost all other rasters generated through this code

for (i in 1:length(names(annual_rast_AF))){
  annual_reg_summed <- annual_rast_AF[[i]] + reg11_21_pixel02
  annual_reg_did_not_persist <- terra::ifel(annual_reg_summed %in% c(2,3), 0, annual_reg_summed) # pixel values = 2 and 3 converted to zero.
  terra::writeRaster(annual_reg_did_not_persist, paste(dir, obj_name[i], "_did_not_persist.tif", sep = ""),
                     gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)
}



################################################################################
# Converting CRS to calculate area ---------------------------------------------
################################################################################

# cleaning directory 
rm(list = ls())

# TRYING TO CONVERT TO ALBERS SIRGAS 2000, DID NOT WORK ------------------------

#albers_sirgas2000 <- "+proj=aea +lat_1=-5 +lat_2=-30 +lat_0=-18 +lon_0=-54 +x_0=0 +y_0=0 +datum=SAD69 +units=m +no_defs"
# +proj=aea  - Projeção Albers Equal Area
# +lat_1=-5  - primeiro paralelo padrão (próximo ao norte da Mata Atântica)
# +lat_2=-30 - segundo paralelo padrão, próximo ao sul da Mata Atlantica
# +lat_0=-18 - Latitude de origem, próximo ao centro da Mata Atlantica
# +lon_0=-54 - Meridiano central do Brasil
# +datum=SIRGAS2000 - Datum Geodésico oficial do Brasil

dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/"


# Converting to Albers SAD 69 EPSG 102033


# Reg 2011 - 2021 that persisted until 2023

reg_11_21 <- rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21_AF.tif")


# Testing converted methods "near" and "mode" and comparing to the original data in WGS84

# Based on visual inspection, the method "near" seems more accurate, 
# especially for small and linear forest patches


reg_11_21_AlbersSAD69 <- terra::project(reg_11_21, "ESRI:102033", method = "near")

terra::writeRaster(reg_11_21_AlbersSAD69, paste(dir, "reg_11_21_AlbersSAD69_near.tif", sep = ""),
                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)


#reg_11_21_AlbersSAD69 <- terra::project(reg_11_21, "ESRI:102033", method = "mode")

#terra::writeRaster(reg_11_21_AlbersSAD69, paste(dir, "reg_11_21_AlbersSAD69_mode.tif", sep = ""),
#                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)



# Loop to convert CRS of all raster --------------------------------------------

# cleaning directory 
rm(list = ls())

dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/"

setwd(dir)
all_rast_files <- terra::rast(list.files(dir, pattern = ".tif"))
obj_names <- paste(gsub(".tif", "", list.files(dir, pattern = ".tif")), "_ALBERS", sep = "")

dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/"

# Loop to convert and save files

for(i in 1:length(names(all_rast_files))){
assign(obj_names[i], terra::project(all_rast_files[[i]], "ESRI:102033", method = "near"))
terra::writeRaster(
  get(obj_names[i]),
  paste0(dir, obj_names[i], ".tif", sep = ""),
  gdal=c("COMPRESS=DEFLATE", "TFW=YES"),
  overwrite = T)
}


# Converting MB 2010 to Albers -------------------------------------------------

mb_2010_only_forest <- rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2010_AF_forest_only.tif")

mb_2010_only_forest_ALBERS <- terra::project(mb_2010_only_forest, "ESRI:102033", method = "near")

#terra::writeRaster(mb_2010_only_forest_ALBERS, "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/mb_2010_only_forest_ALBERS.tif",
#                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)



################################################################################
# Calculating areas for raster in Albers format
################################################################################

library(terra)

# cleaning directory 
rm(list = ls())

dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/"

setwd(dir)
all_rast_files <- terra::rast(list.files(dir, pattern = "ALBERS.tif"))
obj_names <- paste(gsub(".tif", "", list.files(dir, pattern = ".tif")), "_AREA", sep = "")


# Creating raster with all pixel values = 1. With this raster calculate the area of each pixel,
# and then multiply by binary rasters (0 - 1)

#raster_one <- terra::ifel(all_rast_files[[1]] == 0, 1, all_rast_files[[1]])
#raster_one_area <- mask(cellSize(raster_one, unit = "m"), raster_one)

#terra::writeRaster(raster_one_area, paste(dir, "raster_one_area.tif", sep = ""),
#                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)



# Loop to calculate areas

raster_one_area <- rast("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/raster_albers_SAD69_AREA/raster_one_area.tif")


dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/raster_albers_SAD69_AREA/"

for(i in 1:length(names(all_rast_files))){
  assign(obj_names[i], raster_one_area * all_rast_files[[i]])
  terra::writeRaster(
    get(obj_names[i]),
    paste0(dir, obj_names[i], ".tif", sep = ""),
    gdal=c("COMPRESS=DEFLATE", "TFW=YES"),
    overwrite = T)
}

# MB 2010 forest only area -----------------------------------------------------

mb_2010_only_forest_ALBERS_AREA <- cellSize(mb_2010_only_forest_ALBERS, unit = "m")

mb_2010_only_forest_ALBERS_AREA <- mb_2010_only_forest_ALBERS * mb_2010_only_forest_ALBERS_AREA


#terra::writeRaster(mb_2010_only_forest_ALBERS_AREA, "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/raster_albers_SAD69_AREA/mb_2010_only_forest_ALBERS_AREA.tif",
#                   gdal=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite = T)



################################################################################
######## Extracting areas for Municipalities polygons
################################################################################

# Converting Municipalities shape to Albers

library(terra)
library(raster)
library(sf)
library(exactextractr)
library(tidyverse)

# cleaning directory 
rm(list = ls())

#mun_af <- vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_AF_WGS.shp")
#mun_af_ALBERS <- project(mun_af, "ESRI:102033")
#writeVector(mun_af_ALBERS, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_AF_ALBERS.shp")


mun_af <- sf::st_read("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_AF_ALBERS.shp")

# Extracting reg 11-21 to Municipalities ---------------------------------------
# ------------------------------------------------------------------------------

reg_11_21 <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/raster_albers_SAD69_AREA/reg_11_21_ALBERS_AREA.tif")

# I compared the results of extractions using exact_extract() with rasters in QGis and it makes sense
area_reg_11_21_mun <- exactextractr::exact_extract(reg_11_21, mun_af, "sum")

ncol(mun_af[,]) #14
mun_af[,15] <- round((area_reg_11_21_mun/10000), 2)
colnames(mun_af)[15] <- "r11_21"


# Extracting total regeneration (also considering what was subsequently lost)---
# ------------------------------------------------------------------------------

total_reg <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/raster_albers_SAD69_AREA/all_reg_ALBERS_AREA.tif")

total_reg <- exactextractr::exact_extract(total_reg, mun_af, "sum")

ncol(mun_af[,]) #15
mun_af[,16] <- round((total_reg/10000), 2)
colnames(mun_af)[16] <- "tt_reg"


# Extracting total deforestation of secondary forest ---------------------------
# ------------------------------------------------------------------------------

total_defo <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/raster_albers_SAD69_AREA/total_defo_ALBERS_AREA.tif")

total_defo <- exactextractr::exact_extract(total_defo, mun_af, "sum")

ncol(mun_af[,]) #16
mun_af[,17] <- round((total_defo/10000), 2)
colnames(mun_af)[17] <- "tt_defo"


# Extracting forest area 2010 --------------------------------------------------
# ------------------------------------------------------------------------------

forest_2010 <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/raster_albers_SAD69_AREA/mb_2010_only_forest_ALBERS_AREA.tif")


forest_2010 <- exactextractr::exact_extract(forest_2010, mun_af, "sum")

ncol(mun_af[,]) #17
mun_af[,18] <- round((forest_2010/10000), 2)
colnames(mun_af)[18] <- "f_2010"


# Writing vector with areas
#sf::st_write(mun_af, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_AF_ALBERS_AREA.shp", delete_dsn = T)

sum(data.frame(mun_af[,"r11_21"])[,"r11_21"]) # 1.677.412 ha of regenerated forests

# ------------------------------------------------------------------------------

# Proportion of deforestation in relation to the total amount of forest that regenerated 

prop_loss <- round(data.frame(mun_af[,"tt_defo"])[,"tt_defo"] / 
                   data.frame(mun_af[,"tt_reg"])[,"tt_reg"], 2)


ncol(mun_af) #18
mun_af[,19] <- prop_loss
colnames(mun_af)[19] <- "p_loss"

# Proportion of deforestation in relation to forest area in 2010

pl_2010 <- round(data.frame(mun_af[,"tt_defo"])[,"tt_defo"] / 
                 data.frame(mun_af[,"f_2010"])[,"f_2010"], 2)


ncol(mun_af) #19
mun_af[,20] <- pl_2010
colnames(mun_af)[20] <- "pl_2010"

max(pl_2010, na.rm = T)

# Writing vector with areas
#sf::st_write(mun_af, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_AF_ALBERS_AREA.shp", delete_dsn = T)

#data.frame(mun_af %>% 
#  filter(pl_2010 > 0.5))


################################################################################
# Extracting values reg and did not persist per year ---------------------------
################################################################################


# Calculating area of forest regeneration for each year ------------------------

# cleaning directory
rm(list = ls())

dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/raster_albers_SAD69_AREA/"
setwd(dir)

annual_reg <- list.files(dir, pattern = "annual_reg_.*AF.*ALBERS_AREA\\.tif$")

stack_annual_reg <- raster::stack(annual_reg )


# Loop to save area values for each individual pixel and saving in a matrix

names <- paste0("annual_reg", 11:21)

mtx <- matrix(names, ncol = 2, nrow = 11)

for (i in 1:length(names)) {
  mtx[i,2] <- cellStats(stack_annual_reg[[i]], stat = 'sum')
}


areas_reg_per_year <- data.frame(mtx)
areas_reg_per_year[,2] <- round(as.numeric(areas_reg_per_year[,2]), 2)

areas_reg_per_year[,2] <- areas_reg_per_year[,2]/10000
areas_reg_per_year[,2] <- round(areas_reg_per_year[,2])


colnames(areas_reg_per_year) <- c("reg_year", "area_ha")

write_xlsx(areas_reg_per_year, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/reg_per_year.xlsx")



# Calculating area of forest that did not persist for each year ----------------

# cleaning directory
rm(list = ls())

dir <- "D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/raster_albers_SAD69_AREA/"
setwd(dir)

did_not_persist <- list.files(dir, pattern = "did_not_persist.*ALBERS_AREA\\.tif$")

stack_annual_did_not <- raster::stack(did_not_persist)


# Loop to save area values for each individual pixel and saving in a matrix

names <- paste0("did_not_persist", 11:21)

mtx <- matrix(names, ncol = 2, nrow = 11)

for (i in 1:length(names)) {
  mtx[i,2] <- cellStats(stack_annual_did_not[[i]], stat = 'sum')
}


annual_did_not <- data.frame(mtx)
annual_did_not[,2] <- round(as.numeric(annual_did_not[,2]), 2)

annual_did_not[,2] <- annual_did_not[,2]/10000
annual_did_not[,2] <- round(annual_did_not[,2])


colnames(annual_did_not) <- c("did_not_persist", "area_ha")

write_xlsx(annual_did_not, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/did_not_persist_year.xlsx")


# Extracting reg per state -----------------------------------------------------

# cleaning directory
rm(list = ls())

mun_AF_AREA <- vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_AF_ALBERS_AREA.shp")
mun_AF_AREA <- data.frame(mun_AF_AREA)

reg_defo_states <- 
mun_AF_AREA %>% 
  select(NM_UF, tt_reg, tt_defo) %>% 
  group_by(NM_UF) %>% 
  summarise(tt_reg_states = sum(tt_reg, na.rm = T), 
            tt_defo_states = sum(tt_defo, na.rm = T)) %>% 
  arrange(desc(tt_reg_states))

reg_defo_states <- data.frame(reg_defo_states)

reg_defo_states[,"tt_reg_states"] <- round(reg_defo_states[,"tt_reg_states"])
reg_defo_states[,"tt_defo_states"] <- round(reg_defo_states[,"tt_defo_states"])

reg_defo_states

write_xlsx(reg_defo_states, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/reg_defo_states.xlsx")




################################################################################
# Extracting values reg TI, UC, Reforma Agrária and Quilombolas
################################################################################


# Converting these shapefiles to Albers

# Quilombola -------------------------------------------------------------------

quilombola <- vect("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_area_quilombola_incra2024.shp")
quilombola <- project(quilombola, "ESRI:102033")
writeVector(quilombola, "D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_area_quilombola_incra2024_ALBERS.shp")


# Indigenous Land --------------------------------------------------------------

TI <- vect("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_tis_funai2024.shp")
TI <- project(TI, "ESRI:102033")
writeVector(TI, "D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_tis_funai2024_ALBERS.shp")


# Protected Areas --------------------------------------------------------------

PA <- vect("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_UC_mma2024.shp")
PA <- project(PA, "ESRI:102033")
writeVector(PA, "D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_UC_mma2024_ALBERS.shp")


# Agrarian Settlements ---------------------------------------------------------


AS <- vect("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_assentamento_incra2024.shp")
AS <- project(AS, "ESRI:102033")
writeVector(AS, "D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_assentamento_incra2024_ALBERS.shp")



################################################################################
# Extracting Area values -------------------------------------------------------

# cleaning directory
rm(list = ls())

reg_11_21 <- raster::raster("D:/__PESSOAL/Vinicius_T/raster_pacto/Tiles Reg 11 - 20 Pacto-20250308T211602Z-001/Tiles Reg 11 - 20 Pacto/annual_reg_AF/raster_albers_SAD69/raster_albers_SAD69_AREA/reg_11_21_ALBERS_AREA.tif")



# Quilombola -------------------------------------------------------------------

quilombola <- st_read("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_area_quilombola_incra2024_ALBERS.shp")
quilombola_area <- exactextractr::exact_extract(reg_11_21, quilombola, "sum")

ncol(quilombola) #24
quilombola[,"25"] <- round(quilombola_area/10000)
colnames(quilombola)[25] <- "reg_ha"
st_write(quilombola, "D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_area_quilombola_incra2024_ALBERS_AREA.shp")




# Indigenous Land --------------------------------------------------------------


TI <- st_read("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_tis_funai2024_ALBERS.shp")
TI_area <- exactextractr::exact_extract(reg_11_21, TI, "sum")

ncol(TI) #19
TI[,"20"] <- round(TI_area/10000)
colnames(TI)[20] <- "reg_ha"
st_write(TI, "D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_tis_funai2024_ALBERS_AREA.shp")


# Protected Areas --------------------------------------------------------------

UC <- st_read("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_UC_mma2024_ALBERS.shp")
UC_area <- exactextractr::exact_extract(reg_11_21, UC, "sum")

ncol(UC) #33
UC[,"34"] <- round(UC_area/10000)
colnames(UC)[34] <- "reg_ha"
st_write(UC, "D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_UC_mma2024_ALBERS_AREA.shp")

UC_data_frame <- data.frame(UC)


UC_data_frame %>% 
  select(grupo, reg_ha) %>% 
  group_by(grupo) %>% 
  summarise(tt_reg = sum(reg_ha))





# Agrarian Settlements ---------------------------------------------------------


AS <- st_read("D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_assentamento_incra2024_ALBERS.shp")
AS_area <- exactextractr::exact_extract(reg_11_21, AS, "sum")

ncol(AS) #15
AS[,"16"] <- round(AS_area/10000)
colnames(AS)[16] <- "reg_ha"
st_write(UC, "D:/__PESSOAL/Vinicius_T/dados Pacto/CAMADAS/MA_assentamento_incra2024_ALBERS_AREA.shp")


# ------------------------------------------------------------------------------

# Saving data frames with areas


mtx <- matrix(ncol = 2, nrow = 5)
mtx[1,2] <- sum(round(quilombola_area/10000))
mtx[2,2] <- sum(round(TI_area/10000))
mtx[3,2] <- sum(round(AS_area/10000))
mtx[4,2] <- 23927
mtx[5,2] <- 141338

mtx[1,1] <- "quilombola area"
mtx[2,1] <- "TI area"
mtx[3,1] <- "Agrarian Settlement"
mtx[4,1] <- "UC proteção integral"
mtx[5,1] <- "UC uso sustentável"


mtx <- data.frame(mtx)
colnames(mtx) <- c("tipo_area" ,"area_reg_ha")

write_xlsx(mtx, "D:/__PESSOAL/Vinicius_T/data_frames_result_areas/UC_TI_PA_AS_Areas.xlsx")



################################################################################
# Prop loss of recovered forests in relation to forest area in 2010
################################################################################

# cleaning directory
rm(list = ls())

# Municipalities with areas

mun <- vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_AF_ALBERS_AREA.shp")

mun_data_frame <- data.frame(mun)
colnames(mun_data_frame)


# Calculating quantiles to mask Mun with low forest cover

forest_2010 <- mun_data_frame[,"f_2010"]

quantile(forest_2010, probs = 0.5, na.rm = T) # 4075.3
max(forest_2010)

mun_areas_above_threshold <- mun[mun$f_2010 > 4075.3, ]

plot(mun_areas_above_threshold)

writeVector(mun_areas_above_threshold, "D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_AF_ALBERS_Above_50th.shp")


# ------------------------------------------------------------------------------

################################################################################
# Bar chart reg per year -------------------------------------------------------

# cleaning directory
rm(list = ls())

# Loading excel spreadsheet
reg_per_year <- readxl::read_excel("D:/_Vinicius/artigos/2024.12.d04 - Pacto, secondary forests, natural regeneration/dataframes/dataframes/reg_per_year.xlsx")

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


# Loading annual loss dataframe ------------------------------------------------


################################################################################
#### Plotting Bar charts
################################################################################

# cleaning directory
rm(list = ls())

# Annual regeneration / deforestation ------------------------------------------



# Loading excel spreadsheet
reg_per_year_long <- readxl::read_excel("D:/_Vinicius/artigos/2024.12.d04 - Pacto, secondary forests, natural regeneration/dados/data_frames/reg_per_year.xlsx")

annual_loss_year <- readxl::read_excel("D:/_Vinicius/artigos/2024.12.d04 - Pacto, secondary forests, natural regeneration/dados/data_frames/did_not_persist_year.xlsx")

annual_loss_long <- annual_loss_year %>%
  mutate(year = str_extract(did_not_persist, "\\d+")) %>% 
  mutate(year = as.integer(paste0("20", year))) %>%       
  select(year, area_ha) %>% 
  mutate(type = "annual_defo_ha")


annual_reg_long <- reg_per_year_long %>%
  mutate(year = str_extract(reg_year, "\\d+")) %>% 
  mutate(year = as.integer(paste0("20", year))) %>%       
  select(year, area_ha) %>% 
  mutate(type = "annual_reg_ha")

annual_loss_reg_long <- bind_rows(annual_reg_long, annual_loss_long) %>%
  mutate(year = factor(year)) 


ggplot(annual_loss_reg_long, aes(x = factor(year), y = area_ha, fill = type)) +
  geom_bar(stat = "identity", width = 1.1, position = position_dodge(width = 0)) +
  scale_fill_manual(values = c("annual_defo_ha" = "#ee6b6e", "annual_reg_ha" = "#7B9FCF")) +
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000, 250000, 276000),
                     labels = c("50", "100", "150", "200", "250","276"),
                     expand = c(0.01, 0)) +
  labs(x = "", y = "Area (thousands ha)", title = "") +
  theme_classic(base_size = 4) +
  theme(
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12.5),
    axis.title.y = element_text(size = 12.5, margin = margin(r = 7))
  ) +
  guides(fill = "none")


ggsave("D:/_Vinicius/artigos/2024.12.d04 - Pacto, secondary forests, natural regeneration/Figuras/Bar Chart/annual_reg_defo.png", width = 17, height = 10, units = "cm", dpi = 300)


# Reg per state ----------------------------------------------------------------

# cleaning directory
rm(list = ls())

# Loading excel spreadsheet
reg_per_state <- readxl::read_excel("D:/_Vinicius/artigos/2024.12.d04 - Pacto, secondary forests, natural regeneration/dados/data_frames/reg_defo_states.xlsx")

reg_per_state <- reg_per_state %>% 
  rename(state = NM_UF) %>% 
  mutate(rank = rank(-tt_reg_states)) %>% 
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
  ))


# Convert to long format if not already
reg_per_state_long <- reg_per_state %>%
  rename(state_abbr = `case_when(...)`) %>%
  pivot_longer(
    cols = c(tt_reg_states, tt_defo_states),
    names_to = "type",
    values_to = "area_ha"
  )

state_order <- reg_per_state_long %>%
  filter(type == "tt_reg_states") %>%
  arrange(desc(area_ha)) %>%
  pull(state_abbr)

reg_per_state_long <- reg_per_state_long %>%
  mutate(state_abbr = factor(state_abbr, levels = state_order))


(bar_chart_state <- 
ggplot(reg_per_state_long, aes(x = state_abbr, y = area_ha, fill = type)) +
  geom_bar(stat = "identity", width = 1.3, position = position_dodge(width = 0)) +
  scale_fill_manual(values = c("tt_reg_states" = "#7B9FCF", "tt_defo_states" = "#ee6b6e")) +
  labs(x = "", y = "", title = "") +
  scale_y_continuous(breaks = c(0, 200000, 400000, 595000),
                     labels = c("0", "200", "400", "595"),
                     expand = expansion(mult = c(0.02, 0.05))) +
  labs(x = "", y = "Area (thousands ha)", title = "") +
  theme_classic() +
  theme(strip.text = element_blank(),
        text = element_text(size = 28),
        axis.text.y = element_text(margin = margin(r = 4)),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none"))

ggsave("D:/_Vinicius/artigos/2024.12.d04 - Pacto, secondary forests, natural regeneration/Figuras/Bar Chart/states_reg_defo.png", plot = bar_chart_state, width = 40, height = 20, units = "cm")


## Previous Land cover type ----------------------------------------------------
################################################################################

# cleaning directory
rm(list = ls())


MB_2010_AF <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/brasil_coverage_2010_AF.tif")
#plot(MB_2010_AF)
reg_11_21_AF <- terra::rast("D:/__PESSOAL/Vinicius_T/raster_pacto/_reg_11_21_AF_resampled.tif")
#plot(reg_11_21_AF)


masked_MB_2010 <- terra::mask(MB_2010_AF, reg_11_21_AF, maskvalue = 0)
#plot(masked_MB)
#terra::writeRaster(masked_MB_2010, "D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/previous_land_cover_type_MB_2010.tif")

MB_10_previous_land_use <- terra::rast("D:/__PESSOAL/Vinicius_T/MapBiomas_Col_09/previous_land_cover_type_MB_2010.tif")

# Frequency of each previous land cover type -----------------------------------

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




################################################################################
## Plotting bivariate analysis
################################################################################

# Leandro's code

#script obtido de https://stackoverflow.com/questions/45177590/map-of-bivariate-spatial-correlation-in-r-bivariate-lisa


library(boot)
library(dplyr)
library(ggplot2)
library(sf)
library(spdep)
library(rgdal)
library(stringr)
library(UScensus2000tract)
install.packages("stringr")
install.packages("UScensus2000tract")
install.packages("spatialreg")
install.packages("rgdal")
install.packages("terra")
install.packages("maptools")
require(spatialreg)
require(raster)
require(maptools)
require(terra)




ma<-vect("D:/_Vinicius/artigos/2024.12.d04 - Pacto, secondary forests, natural regeneration/dados/mun_AF_ALBERS_AREA.shp")

head(ma)
tail(ma)
summary(ma)
dim(ma)
ma$CD_MUN<-as.numeric(ma$CD_MUN)
str(ma$CD_MUN)

# r11_21 - área de floresta (ha) que regenerou entre 2011 e 2021 e que persistiu até 2023
# tt_reg - área total de floresta (ha) que regenerou entre 2011 e 2021, incluindo o que não persistiu
# tt_defo - área total de floresta (ha) que regenerou e que foi perdida
# f_2010  - área de floresta em 2010 (ha), MapBiomas coleção 09
# p_loss - proporção de floresta perdida em relação ao que regenerou
# pl_2010 - proporção de floresta que regenerou e foi perdida em relação à área de floresta em 2010





#======================================================
# load data

#LE
sec_for_area<-ma$r11_21 # Área de flores

for_area<-ma$f_2010 # (square meters) = Forest area in each polygon in 2010 (MapBiomas, col 09).

sec_for_prop<- (ma$r11_21 / ma$f_2010) # = Proportion of regenerated forest between 2011 and 2021 that persisted until 2023 (sec_for) in relation to the amount of forest in 2010 (fr_r_mn).

all_reg<-ma$tt_reg # (hectares) = All regeneration that occurred between 2011 and 2021, including forests that were lost up to 2023. Pixels were counted only once, even if they regenerated multiple times.

sec_def_area<-ma$tt_defo # (hectares) = All secondary forest that regenerated between 2011 and 2021 but did not persist until 2023. Pixels were counted only once, even if they were deforested multiple times.

sec_def_prop<-ma$pl_2010 #= Proportion of all secondary forest that regenerated during 2011-2021 and did not persist until 2023, in relation to the total amount of forest in 2010 (MapBiomas col09). A deforested pixel was counted only once, even if it was regenerated and deforested multiple times.



x<-ma$f_2010
#y<-ma$sec_for
y<-sec_def_area



x<-ma$prp_sc_
y<-ma$pdefo_10
# ----------------------------------------------------- #
# Program a function
## Permutations for Lee's L statistic
## Modification of the lee.mc() function within the {spdep} package
## Saves 'localL' output instead of 'L' output
simula_lee <- function(x, y, listw, nsim = nsim, zero.policy = NULL, na.action = na.fail) {
  
  if (deparse(substitute(na.action)) == "na.pass") 
    stop ("na.pass not permitted")
  na.act <- attr(na.action(cbind(x, y)), "na.action")
  x[na.act] <- NA
  y[na.act] <- NA
  x <- na.action(x)
  y <- na.action(y)
  if (!is.null(na.act)) {
    subset <- !(1:length(listw$neighbours) %in% na.act)
    listw <- subset(listw, subset, zero.policy = zero.policy)
  }
  n <- length(listw$neighbours)
  if ((n != length(x)) | (n != length(y))) 
    stop ("objects of different length")
  gamres <- suppressWarnings(nsim > gamma(n + 1))
  if (gamres) 
    stop ("nsim too large for this number of observations")
  if (nsim < 1) 
    stop ("nsim too small")
  xy <- data.frame(x, y)
  S2 <- sum((unlist(lapply(listw$weights, sum)))^2)
  
  lee_boot <- function(var, i, ...) {
    return(lee(x = var[i, 1], y = var[i, 2], ...)$localL)
  }
  
  res <- boot(xy, statistic = lee_boot, R = nsim, sim = "permutation", 
              listw = listw, n = n, S2 = S2, zero.policy = zero.policy)
}

# ----------------------------------------------------- #
# Adjacency Matrix

#LE
ma2 <- sf::st_as_sf(ma)

nb <- poly2nb(ma2)
lw <- nb2listw(nb, style = "B", zero.policy = T)
W  <- as(lw, "symmetricMatrix")
W  <- as.matrix(W / rowSums(W))
W[which(is.na(W))] <- 0


# ----------------------------------------------------- #
# Calculate the index and its simulated distribution
# for global and local values

# Global Lee's L
lee.test(x = x, y = y, listw = lw, zero.policy = TRUE,
         alternative = "two.sided", na.action = na.omit)

# Local Lee's L values
m <- lee(x = x, y = y, listw = lw, n = length(x), 
         zero.policy = TRUE, NAOK = TRUE)

# Local Lee's L simulations
local_sims <- simula_lee(x = x, y = y, listw = lw, nsim = 10000,
                         zero.policy = TRUE, na.action = na.omit)

m_i <- m[[2]]  # local values

# Identify the significant values 
alpha <- 0.05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t(apply(t(local_sims[[2]]), 1, function(x) quantile(x, probs = probs)))
sig <- (m_i < intervals[ , 1] ) | ( m_i > intervals[ , 2])

#======================================================
# Preparing for plotting

#LE
ma.tract<-st_as_sf(ma)
ma.tract$sig<-sig

# Identifying the Lee's L patterns
xp <- scale(x)
yp <- scale(y)

#LE
patterns <- as.character(interaction(xp > 0, W%*%yp > 0)) 
patterns <- patterns %>% 
  str_replace_all("TRUE","High") %>% 
  str_replace_all("FALSE","Low")
patterns[ma.tract$sig == 0] <- "Not significant"
ma.tract$patterns <- patterns

# Plotting
#LE
x11()
ggplot() +
  geom_sf(data = ma.tract, aes(fill = patterns), color = "NA") +
  scale_fill_manual(values = c("red", "pink", "light blue", "dark blue", "grey95")) + 
  guides(fill = guide_legend(title = "Lee's L clusters")) +
  theme_minimal()

head(ma.tract)
names(ma.tract)
dim(ma.tract)
dim(ma)


#aqui vai juntar o resultado da primeira rodada (Forest area x Secondary forest) com o shape original
ma_for_sec_for<-merge(as.data.frame(ma),ma.tract[,c(1, 22,23)], by="CD_MUN")
head(ma_for_sec_for)
names(ma_for_sec_for)


#antes de rodar aqui precisa substituir lá no começo (em load data) o objeto z por y e rodar novamente apenas para facilitar
#aí roda todo o script acima novamente, exceto a parte do "vip2". Com isso vai rodar UC x Absolute E
#e ai vai juntar o resultado com o shape original
ma_for_secdefarea<-merge(as.data.frame(ma),ma.tract[,c(1, 22,23)], by="CD_MUN")


#antes de rodar aqui precisa substituir lá no começo (em load data) o objeto w por y e rodar novamente apenas para facilitar
#aí roda todo o script acima novamente, exceto a parte do "vip2" e do "vip3". Com isso vai rodar UC x Relative E
#e ai vai juntar o resultado com o shape original
ma_secarea_secdefarea<-merge(as.data.frame(ma),ma.tract[,c(1, 22,23)], by="CD_MUN")


#x<-ma$prp_sc_
#y<-ma$pdefo_10
ma_secprop_secdefprop<-merge(as.data.frame(ma),ma.tract[,c(1, 22,23)], by="CD_MUN")

colnames(ma_for_sec_for)[21:22]<-c("sig_fsc", "pat_fsc")
colnames(ma_for_secdefarea)[21:22]<-c("sig_fsd", "pat_fsd")
colnames(ma_secarea_secdefarea)[21:22]<-c("sig_ssd", "pat_ssd")
colnames(ma_secprop_secdefprop)[21:22]<-c("sig_spdp", "pat_spdp")

ma3<-merge(ma_for_sec_for, ma_for_secdefarea[,c(1,21,22)], by="CD_MUN")
ma3<-merge(ma3,ma_secarea_secdefarea[,c(1,21,22)], by="CD_MUN")
ma3<-merge(ma3,ma_secprop_secdefprop[,c(1,21,22)], by="CD_MUN")

ma_fim<-merge(ma, ma3[,c(1,21:29)], by="CD_MUN")
#salva os resultados
writeVector(ma_fim, "C:\\Users\\Leandro\\Google Drive\\Artigos\\PACTO\\ma_pattern.shp", overwrite=T)



# Calculating total reg 2011 - 2021

mun <- terra::vect("D:/__PESSOAL/Vinicius_T/municipios_Brasil/BR_Municipios_2023/mun_AF_ALBERS_AREA.shp")

head(mun)
sum(mun$tt_defo)



