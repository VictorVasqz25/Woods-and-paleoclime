#Sacar ocurrencias en serie e información climática
library(readxl)
library(stringr)
library(BIEN)
library(raster)

#Aquí tenemos las capas de clima y elevación
setwd("C:/Users/PERSONAL/Downloads/Tesis/Talleres/Taller 1")

#cargamos un excel solo con especies
sps_list <- read_excel("C:/Users/PERSONAL/Downloads/Tesis/Matrices/Matriz9/sp_list.xlsx")

#Creamos un vector con todas las especies
spec <- sps_list[,"species"]

#volvemos "species" una lista
species <- unlist(spec)

#creamos un patrón para buscar en nuestra lista de especies descargada
#Nota: esto lo ahcemos porque no todos la individuos tienen epitato específico
#Y la forma ade busqueda se hae por género en lugar de por especie
patron <- '\\s(sp|SP|Sp|sP|SPP|spp|aff|sect|cf|subgrp)'

#cargamos el la capa de elevación
elevation <- raster('./Elevation/World/elevation.tif') 
projection(elevation) <-
  '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

#cargamos la capa de (MAP) precipitación
map <- raster('./WORLDCLIM/1970-2000/World/wc2.0_bio_10m_12.tif')

#cargamos la capa de (MAT) temperatura promedio anual
TPA <- raster('./WORLDCLIM/1970-2000/World/wc2.0_bio_10m_01.tif')

#cargamos capa de (TS) temperatura estacional
TS <- raster('./WORLDCLIM/1970-2000/World/wc2.0_bio_10m_04.tif')

#cargamos capa de (PDQ) Pricipitación cuarto más seco
PDQ <- raster('./WORLDCLIM/1970-2000/World/wc2.0_bio_10m_17.tif')

#Creamos lsitas para almacenar el valor de cada variable y cada especie	
list_meanMAP <- list ()
list_meanMAT <- list()
list_meanTS <- list()
list_meanPDQ <- list()
list_total_occ <- list()


#Creamos el path final donde queda mi archivo
file_path <- paste("C:/Users/PERSONAL/Downloads/Tesis/Matrices/Matriz8/", "final_clim.xlsx", sep= "")

#loop para extraer ocurrencias de lista de especies
#Este loop busca en la lista que le entregué, especie por especie, extrae las ocurrencias
#les da proyección geográfica para que las coordenadas se ubiquen en el espacio y leugo
#extraigo el valor de las variables climáticas para cada punto, promedio el total de esos datos
# y obtengo un valor promedio para cada variable climática
for (sp in species) {
  detector <- str_detect(sp,patron)
 
  if  (detector) { 
    sp <- unlist(strsplit(sp," "))[1]
    ocurrence <- BIEN_occurrence_genus(sp, only.new.world = TRUE) }
  else {
    ocurrence <- BIEN_occurrence_species(sp, only.new.world = TRUE)}
  coords<-cbind(ocurrence$longitude, ocurrence$latitude)
  coords<-na.omit(coords)
  points <- SpatialPoints(coords, proj4string =CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
  total_occ <- dim(coords)[0]
  valuesSpMAP <- na.omit(extract(map,points))
  meanMap <- mean(valuesSpMAP)
  valuesSpMAT <- na.omit(extract(TPA,points))
  meanMAT <- mean(valuesSpMAT)
  valuesSpTS <- na.omit(extract(TS,points))
  meanTS <- mean(valuesSpTS)
  valuesSpPDQ <- na.omit(extract(PDQ,points))
  meanPDQ <- mean(valuesSpPDQ)
  
  list_meanMAP <- append(list_meanMAP, meanMap)
  list_meanMAT <- append(list_meanMAT, meanMAT)
  list_meanTS <- append(list_meanTS, meanTS)
  list_meanPDQ <- append(list_meanPDQ, meanPDQ)
  list_total_occ <- append(list_total_occ, total_occ)
}

write.xlsx(cbind(species, list_meanMAP, list_meanMAT,
                 list_meanTS, list_meanPDQ, list_total_occ), file_path, 
                 password = NULL)

#-------------------------------------------------------------------------------------------
#Para analizar neustra cantidad de familas, géneros y analizar la distribución de estados
#caracter

#cargamos un excel solo con especies
sps_list <- read.csv2("filo_m6.csv")
traits9<-read.csv2("Matriz_9_31-08-2022.csv")

#contar numero de genero
num_gen <- length(unique(sps_list$genus))
num_gen
#Contar el número de repetidos
repetidos_gen <- table(sps_list$genus)
repetidos_gen <- as.data.frame(repetidos)
repetidos_gen

#ordenar en orden decreciente
repetidos_gen <- repetidos_gen[order(repetidos_gen$Freq, decreasing = TRUE), ]
repetidos_gen

plot(repetidos_gen)

#contar numero de familia
num_fam <- length(unique(sps_list$family))
num_fam

#Contar el número de repetidos
repetidos_fam <- table(sps_list$family)
repetidos_fam <- as.data.frame(repetidos_fam)
repetidos_fam

#ordenar en orden decreciente
repetidos_fam <- rehpetidos_fam[order(repetidos_fam$Freq, decreasing = TRUE), ]
repetidos_fam

plot(repetidos_fam)

#Indices de caracter
ind_GR <- table(traits9$Growth_Rings)
ind_VP <- table(traits9$Vessel_porosity)
ind_VA <- table(traits9$Vessel_arangement)
ind_PP <- table(traits9$Perforation_plates)
ind_IPA <- table(traits9$Intervessel_pits_arregement)
ind_MTDVL <- table(traits9$Mean_tangential_diameter_of_vessel_lumina)
ind_SF <- table(traits9$Septa_in_fibers)
ind_FWT <- table(traits9$Fibre_wall_thickness)
ind_GTF <- table(traits9$Ground_tissue_fibres)
ind_ParenPA <- table(traits9$Parenchyma_Paratracheal)
ind_TPB <- table(traits9$Type_of_parenchyma_banding)
ind_RW <- table(traits9$Ray_width)
ind_RCC <- table(traits9$Rays_cellular_composition)

ind_gen <- table(traits9$gen)
ind_gen
#plots indices de caracter
plot(ind_GR)
plot(ind_VP)
plot(ind_VA)
plot(ind_PP)
plot(ind_IPA)
plot(ind_MTDVL)
plot(ind_SF)
plot(ind_FWT)
plot(ind_GTF)
plot(ind_ParenPA)
plot(ind_TPB)
plot(ind_RW)
plot(ind_RCC)
plot(ind_gen)

#contar número de búsquedas por género

#Creamos un vector con todas las especies
spec <- traits9[,"sp"]

#volvemos el a "species" una lista
species <- unlist(spec)
species <- gsub('_',' ',species)

#creamos un patrón para buscar en nuestra lista de especies descargada
patron <- '\\s(sp|SP|Sp|sP|SPP|spp|aff|sect|cf|subgrp)'

#contador de generos
count <- 0
for (sp in species) {
  detector <- str_detect(sp,patron)
  if  (detector) {
    count <- count + 1 }
}    
print(count) 