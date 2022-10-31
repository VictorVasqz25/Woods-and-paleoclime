
#limpair workspace
rm(list = ls())
install.packages("randomForest")
library(ape)
library(nlme)
library(geiger)
library(maps)
library(phytools)
library(AICcmodavg)
library(randomForest)#para resolver na
setwd("C:/Users/PERSONAL/Downloads/Tesis/Matrices/Matriz8")

#Cargo mis datos de maderas con información climática
traits_gls<-read.csv2("Matriz_8_trait_clim_29-06-2022.csv")

#Cargo mi filogenia
tree_gls <- read.tree('Figure.1c.phy')

#Ordeno mi matriz con el orden de tip labels de filogenia
traits_gls <-traits_gls[match(tree_gls$tip.label, traits_gls$sp),]
row.names(traits_gls)<-traits_gls[,1]
traits_gls<-traits_gls[,-1]

#eliminación columnas
traits_gls$gen <- NULL
traits_gls$fam <- NULL

# leo el caracter Ground_tissue_fibres como int para luego leerlo como factor. 
#Por alguna razón,si se pasa directamente a factor, queda con errores
traits_gls$Ground_tissue_fibres = as.integer(traits_gls$Ground_tissue_fibres)

#leo mis caracteres como factores
traits_gls$Growth_Rings = as.factor(traits_gls$Growth_Rings)
traits_gls$Vessel_porosity = as.factor(traits_gls$Vessel_porosity)
traits_gls$Vessel_arangement = as.factor(traits_gls$Vessel_arangement)
traits_gls$Perforation_plates = as.factor(traits_gls$Perforation_plates)
traits_gls$Intervessel_pits_arregement = as.factor(traits_gls$Intervessel_pits_arregement)
traits_gls$Mean_tangential_diameter_of_vessel_lumina = as.factor(traits_gls$Mean_tangential_diameter_of_vessel_lumina)
traits_gls$Ground_tissue_fibres = as.factor(traits_gls$Ground_tissue_fibres)
traits_gls$Septa_in_fibers = as.factor(traits_gls$Septa_in_fibers)
traits_gls$Fibre_wall_thickness = as.factor(traits_gls$Fibre_wall_thickness)
traits_gls$Parenchyma_Paratracheal = as.factor(traits_gls$Parenchyma_Paratracheal)
traits_gls$Type_of_parenchyma_banding = as.factor(traits_gls$Type_of_parenchyma_banding)
traits_gls$Ray_width = as.factor(traits_gls$Ray_width)
traits_gls$Rays_cellular_composition = as.factor(traits_gls$Rays_cellular_composition)

#leo mis variables climáticas como valores numéricos
traits_gls$list_meanMAP <- as.numeric(traits_gls$list_meanMAP)
traits_gls$list_meanMAT <- as.numeric(traits_gls$list_meanMAT)
traits_gls$list_meanPDQ <- as.numeric(traits_gls$list_meanPDQ)


#Creo vectores con cada caracter por aparte
Growth_Rings <- traits_gls[,1]
Vessel_porosity <- traits_gls[,2]
Vessel_arangament <- traits_gls[,3]
Perforation_plates <- traits_gls[,4]
Intervessel_pits_arregement <- traits_gls[,5]
Mean_tangential_diameter_of_vessel_lumina <- traits_gls[,6]
Ground_tissue_fibres <- traits_gls[,7]
Septa_in_fibers <- traits_gls[,8]
Fibre_wall_thickness <- traits_gls[,9]
Parenchyma_Paratracheal <- traits_gls[,10]
Type_of_parenchyma_banding <- traits_gls[,11]
Ray_width <- traits_gls[,12] 
Rays_cellular_composition <- traits_gls[,13]

#Creo vectores con cada variable por aparte
mean_MAP_gls <- traits_gls[,14]
mean_MAT_gls <- traits_gls[,15]
mean_PDQ_gls <- traits_gls[,17]

#correlaciones

#creo mis variables como numericas
Growth_Rings_n <- as.numeric(Growth_Rings)
Vessel_porosity_n <- as.numeric(Vessel_porosity)
Vessel_arangament_n <- as.numeric(Vessel_arangament)
Perforation_plates_n <- as.numeric(Perforation_plates)
Intervessel_pits_arregement_n <- as.numeric(Intervessel_pits_arregement)
Mean_tangential_diameter_of_vessel_lumina_n <- as.numeric(Mean_tangential_diameter_of_vessel_lumina)
Ground_tissue_fibres_n <- as.numeric(Ground_tissue_fibres)
Septa_in_fibers_n <- as.numeric(Septa_in_fibers)
Fibre_wall_thickness_n <- as.numeric(Fibre_wall_thickness)
Parenchyma_Paratracheal_n <- as.numeric(Parenchyma_Paratracheal)
Type_of_parenchyma_banding_n <- as.numeric(Type_of_parenchyma_banding)
Ray_width_n <- as.numeric(Ray_width) 
Rays_cellular_composition_n <- as.numeric(Rays_cellular_composition)

#corrrelaciones de variables con varaible climática
                                         
#correlaciones entre caracteres
chi_GR_VP <- chisq.test(Growth_Rings,Vessel_porosity)
chi_GR_VA <- chisq.test(Growth_Rings,Vessel_arangament)
chi_GR_PP <- chisq.test(Growth_Rings,Perforation_plates)
chi_GR_IPA <- chisq.test(Growth_Rings,Intervessel_pits_arregement)
chi_GR_MTDVL <- chisq.test(Growth_Rings,Mean_tangential_diameter_of_vessel_lumina)
chi_GR_GTF <- chisq.test(Growth_Rings,Ground_tissue_fibres)
chi_GR_SF <- chisq.test(Growth_Rings,Septa_in_fibers)
chi_GR_FWT <- chisq.test(Growth_Rings,Fibre_wall_thickness)
chi_GR_Parepa <- chisq.test(Growth_Rings,Parenchyma_Paratracheal)
chi_GR_TPB <- chisq.test(Growth_Rings,Type_of_parenchyma_banding)
chi_GR_RW <- chisq.test(Growth_Rings,Ray_width)
chi_GR_RCC <- chisq.test(Growth_Rings,Rays_cellular_composition)

COR_GR_names <- c("chi_GR_VP","chi_GR_VA", "chi_GR_PP", "chi_GR_IPA", "chi_GR_MTDVL", "chi_GR_GTF",
                  "chi_GR_SF", "chi_GR_FWT", "chi_GR_Parepa", "chi_GR_TPB", "chi_GR_RW",
                  "chi_GR_RCC")

chi_GR <- c(chi_GR_VP$statistic, chi_GR_VA$statistic, chi_GR_PP$statistic, chi_GR_IPA$statistic,
            chi_GR_MTDVL$statistic, chi_GR_GTF$statistic, chi_GR_SF$statistic, chi_GR_FWT$statistic,
            chi_GR_Parepa$statistic, chi_GR_TPB$statistic, chi_GR_RW$statistic, chi_GR_RCC$statistic )

df_GR<- c(chi_GR_VP$parameter, chi_GR_VA$parameter, chi_GR_PP$parameter, chi_GR_IPA$parameter,
        chi_GR_MTDVL$parameter, chi_GR_GTF$parameter, chi_GR_SF$parameter, chi_GR_FWT$parameter,
        chi_GR_Parepa$parameter, chi_GR_TPB$parameter, chi_GR_RW$parameter, chi_GR_RCC$parameter)

p_value_GR <- c(chi_GR_VP$p.value, chi_GR_VA$p.value, chi_GR_PP$p.value, chi_GR_IPA$p.value,
                chi_GR_MTDVL$p.value, chi_GR_GTF$p.value, chi_GR_SF$p.value, chi_GR_FWT$p.value,
                chi_GR_Parepa$p.value, chi_GR_TPB$p.value, chi_GR_RW$p.value, chi_GR_RCC$p.value)

table_chi_GR <- data.frame(COR_GR_names, chi_GR, df_GR, p_value_GR )
table_chi_GR

chi_VP_VA <- chisq.test(Vessel_porosity,Vessel_arangament)
chi_VP_PP <- chisq.test(Vessel_porosity,Perforation_plates)
chi_VP_IPA <- chisq.test(Vessel_porosity,Intervessel_pits_arregement)
chi_VP_MTDVL <- chisq.test(Vessel_porosity,Mean_tangential_diameter_of_vessel_lumina)
chi_VP_GTF <- chisq.test(Vessel_porosity,Ground_tissue_fibres)
chi_VP_SF <- chisq.test(Vessel_porosity,Septa_in_fibers)
chi_VP_FWT <- chisq.test(Vessel_porosity,Fibre_wall_thickness)
chi_VP_Parepa <- chisq.test(Vessel_porosity,Parenchyma_Paratracheal)
chi_VP_TPB <- chisq.test(Vessel_porosity,Type_of_parenchyma_banding)
chi_VP_RW <- chisq.test(Vessel_porosity,Ray_width)
chi_VP_RCC <- chisq.test(Vessel_porosity,Rays_cellular_composition)

COR_VP_names <- c("chi_VP_VA", "chi_VP_PP", "chi_VP_IPA", "chi_VP_MTDVL", "chi_VP_GTF",
                  "chi_VP_SF", "chi_VP_FWT", "chi_VP_Parepa", "chi_VP_TPB", "chi_VP_RW",
                  "chi_VP_RCC")
chi_VP <- c(chi_VP_VA$statistic, chi_VP_PP$statistic, chi_VP_IPA$statistic,
            chi_VP_MTDVL$statistic, chi_VP_GTF$statistic, chi_VP_SF$statistic, chi_VP_FWT$statistic,
            chi_VP_Parepa$statistic, chi_VP_TPB$statistic, chi_VP_RW$statistic, chi_VP_RCC$statistic )

df_VP<- c(chi_VP_VA$parameter, chi_VP_PP$parameter, chi_VP_IPA$parameter,
          chi_VP_MTDVL$parameter, chi_VP_GTF$parameter, chi_VP_SF$parameter, chi_VP_FWT$parameter,
          chi_VP_Parepa$parameter, chi_VP_TPB$parameter, chi_VP_RW$parameter, chi_VP_RCC$parameter)

p_value_VP <- c(chi_VP_VA$p.value, chi_VP_PP$p.value, chi_VP_IPA$p.value,
                chi_VP_MTDVL$p.value, chi_VP_GTF$p.value, chi_VP_SF$p.value, chi_VP_FWT$p.value,
                chi_VP_Parepa$p.value, chi_VP_TPB$p.value, chi_VP_RW$p.value, chi_VP_RCC$p.value)

table_chi_VP <- data.frame(COR_VP_names, chi_VP, df_VP, p_value_VP )
table_chi_VP

chi_VA_PP <- chisq.test(Vessel_arangament,Perforation_plates)
chi_VA_IPA <- chisq.test(Vessel_arangament,Intervessel_pits_arregement)
chi_VA_MTDVL <- chisq.test(Vessel_arangament,Mean_tangential_diameter_of_vessel_lumina)
chi_VA_GTF <- chisq.test(Vessel_arangament,Ground_tissue_fibres)
chi_VA_SF <- chisq.test(Vessel_arangament,Septa_in_fibers)
chi_VA_FWT <- chisq.test(Vessel_arangament,Fibre_wall_thickness)
chi_VA_Parepa <- chisq.test(Vessel_arangament,Parenchyma_Paratracheal)
chi_VA_TPB <- chisq.test(Vessel_arangament,Type_of_parenchyma_banding)
chi_VA_RW <- chisq.test(Vessel_arangament,Ray_width)
chi_VA_RCC <- chisq.test(Vessel_arangament,Rays_cellular_composition)

COR_VA_names <- c("chi_VA_PP", "chi_VA_IPA", "chi_VA_MTDVL", "chi_VA_GTF",
                  "chi_VA_SF", "chi_VA_FWT", "chi_VA_Parepa", "chi_VA_TPB", "chi_VA_RW",
                  "chi_VA_RCC")
chi_VA <- c(chi_VA_PP$statistic, chi_VA_IPA$statistic,
            chi_VA_MTDVL$statistic, chi_VA_GTF$statistic, chi_VA_SF$statistic, chi_VA_FWT$statistic,
            chi_VA_Parepa$statistic, chi_VA_TPB$statistic, chi_VA_RW$statistic, chi_VA_RCC$statistic )

df_VA<- c(chi_VA_PP$parameter, chi_VA_IPA$parameter,
          chi_VA_MTDVL$parameter, chi_VA_GTF$parameter, chi_VA_SF$parameter, chi_VA_FWT$parameter,
          chi_VA_Parepa$parameter, chi_VA_TPB$parameter, chi_VA_RW$parameter, chi_VA_RCC$parameter)

p_value_VA <- c(chi_VA_PP$p.value, chi_VA_IPA$p.value,
                chi_VA_MTDVL$p.value, chi_VA_GTF$p.value, chi_VA_SF$p.value, chi_VA_FWT$p.value,
                chi_VA_Parepa$p.value, chi_VA_TPB$p.value, chi_VA_RW$p.value, chi_VA_RCC$p.value)

table_chi_VA <- data.frame(COR_VA_names, chi_VA, df_VA, p_value_VA )
table_chi_VA



#-----------------------------------------------------------------------------------------
#Modelos PGLS

#Creamos una matriz con solo mis variables y con los nombres reducido a las iniciales

traits_MAP <- traits_gls
traits_MAP$list_meanMAT <- NULL
traits_MAP$list_meanPDQ <- NULL
traits_MAP$list_meanTS <- NULL
names(traits_MAP)
names(traits_MAP) <- c("GR","VP","VA","PP","IPA","MTDVL","FP","SF","FWT","ParenPa",
                       "TPB", "RW","RCC","MAP")
GR <- traits_MAP[,1]
VP <- traits_MAP[,2]
VA <- traits_MAP[,3]
PP <- traits_MAP[,4]
IPA <- traits_MAP[,5]
MTDVL <- traits_MAP[,6]
FP <- traits_MAP[,7]
SF <- traits_MAP[,8]
FWT <- traits_MAP[,9]
ParenPa <- traits_MAP[,10]
TPB <- traits_MAP[,11]
RW <- traits_MAP[,12] 
RCC <- traits_MAP[,13]
MAP <- traits_MAP[,14]

names(GR) <- sp
  names(VP) <- sp
  names(VA) <- sp
  names(PP) <- sp
  names(IPA) <- sp
  names(MTDVL) <- sp
  names(FP) <- sp
  names(SF) <- sp
  names(FWT) <- sp
  names(ParenPa) <-sp
  names(TPB) <-sp
  names(RW) <-sp
  names(RCC) <-sp
  
#modelos pgls simples para MAP


gls_MAP_GR <- gls(mean_MAP_gls ~ GR, correlation = corBrownian(phy = tree_gls),
                     data = traits_MAP, method = "ML")

gls_MAP_VP <- gls(mean_MAP_gls ~ Vessel_porosity, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML")

gls_MAP_VA <- gls(mean_MAP_gls ~ Vessel_arangament, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_MAP_PP <- gls(mean_MAP_gls ~ Perforation_plates, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_MAP_IPA <- gls(mean_MAP_gls ~ Intervessel_pits_arregement, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_MAP_MTDVL <- gls(mean_MAP_gls ~ Mean_tangential_diameter_of_vessel_lumina, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML")

gls_MAP_GTF <- gls(mean_MAP_gls ~ Ground_tissue_fibres, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_SF <- gls(mean_MAP_gls ~ Septa_in_fibers, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML")

gls_MAP_FWT <- gls(mean_MAP_gls ~ Fibre_wall_thickness, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_MAP_ParenPa <- gls(mean_MAP_gls ~ Parenchyma_Paratracheal, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML")

gls_MAP_TPB <- gls(mean_MAP_gls ~ Type_of_parenchyma_banding, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML")

gls_MAP_RW <- gls(mean_MAP_gls ~ Ray_width, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML")

gls_MAP_RCC <- gls(mean_MAP_gls ~ Rays_cellular_composition, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAP_IND <- list(gls_MAP_GR, gls_MAP_VP, gls_MAP_VA, gls_MAP_PP,
                       gls_MAP_IPA, gls_MAP_MTDVL, gls_MAP_GTF, gls_MAP_SF,
                       gls_MAP_FWT,gls_MAP_ParenPa,gls_MAP_TPB,gls_MAP_RW,
                       gls_MAP_RCC)

#specify model names
models_MAP_IND_names <- c('GR','VP', 'VA', 'PP','IPA','MTDVL','GTF','SF','FWT','ParenPa','TPB','RW',
                          'RCC')

#calculate AIC of each model
AIC_MAP_IND <- aictab(cand.set = models_MAP_IND, modnames = models_MAP_IND_names)
AIC_MAP_IND
#--------------------------------------------------------------------------------------
#stepwise manual para MAP
gls_MAP_noGR <- gls(mean_MAP_gls ~  VP+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_MAP_noVP <- gls(mean_MAP_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noVA <- gls(mean_MAP_gls ~ GR+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noPP <- gls(mean_MAP_gls ~ GR+VA+VP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noIPA <- gls(mean_MAP_gls ~ GR+VA+VP+PP+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noMTDVL <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noGTF <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noSF <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noFWT <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noParenPa <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                         data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noTPB <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noRW <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noRCC <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAP_no <- list(gls_MAP_noGR, gls_MAP_noVP, gls_MAP_noVA, gls_MAP_noPP,
                      gls_MAP_noIPA, gls_MAP_noMTDVL, gls_MAP_noGTF, gls_MAP_noSF,
                      gls_MAP_noFWT,gls_MAP_noParenPa,gls_MAP_noTPB,gls_MAP_noRW,
                      gls_MAP_noRCC)

#specify model names
models_MAP_no_names <- c('GR','VP', 'VA', 'PP','IPA','MTDVL','GTF','SF','FWT','ParenPa','TPB','RW',
                         'RCC')

#calculate AIC of each model
AIC_MAP_no <- aictab(cand.set = models_MAP_no, modnames = models_MAP_no_names)
AIC_MAP_no #sacamos a RW
#------------------------------------------------------------------------------------------
#no RW
gls_MAP_noGR_RW <- gls(mean_MAP_gls ~ VP+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_MAP_noVP_RW <- gls(mean_MAP_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noVA_RW <- gls(mean_MAP_gls ~ GR+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noPP_RW <- gls(mean_MAP_gls ~ GR+VA+VP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noIPA_RW <- gls(mean_MAP_gls ~ GR+VA+VP+PP+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noMTDVL_RW <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+FP+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noGTF_RW <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noSF_RW <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noFWT_RW <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noParenPa_RW <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                         data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noTPB_RW <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noRCC_RW <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAP_no_RW <- list(gls_MAP_noGR_RW, gls_MAP_noVP_RW, gls_MAP_noVA_RW, gls_MAP_noPP_RW,
                      gls_MAP_noIPA_RW, gls_MAP_noMTDVL_RW, gls_MAP_noGTF_RW, gls_MAP_noSF_RW,
                      gls_MAP_noFWT_RW,gls_MAP_noParenPa_RW,gls_MAP_noTPB_RW,
                      gls_MAP_noRCC_RW)

#specify model names
models_MAP_no_RW_names <- c('GR','VP', 'VA', 'PP',
                            'IPA','MTDVL','GTF','SF',
                            'FWT','ParenPa','TPB',
                         'RCC')

#calculate AIC of each model
AIC_MAP_no_RW <- aictab(cand.set = models_MAP_no_RW, modnames = models_MAP_no_RW_names)
AIC_MAP_no_RW #Sacamos FWT
#-------------------------------------------------------------------------------------
#no RW FWT
gls_MAP_noGR_RW_FWT <- gls(mean_MAP_gls ~ VP+VA+PP+IPA+MTDVL+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_MAP_noVP_RW_FWT <- gls(mean_MAP_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noVA_RW_FWT <- gls(mean_MAP_gls ~ GR+VP+PP+IPA+MTDVL+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noPP_RW_FWT <- gls(mean_MAP_gls ~ GR+VA+VP+IPA+MTDVL+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noIPA_RW_FWT <- gls(mean_MAP_gls ~ GR+VA+VP+PP+MTDVL+FP+SF+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noMTDVL_RW_FWT <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                          data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noGTF_RW_FWT <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noSF_RW_FWT <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noParenPa_RW_FWT <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noTPB_RW_FWT <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noRCC_RW_FWT <- gls(mean_MAP_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+ParenPa+TPB, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAP_no_RW_FWT <- list(gls_MAP_noGR_RW_FWT, gls_MAP_noVP_RW_FWT, gls_MAP_noVA_RW_FWT, gls_MAP_noPP_RW_FWT,
                         gls_MAP_noIPA_RW_FWT, gls_MAP_noMTDVL_RW_FWT, gls_MAP_noGTF_RW_FWT, gls_MAP_noSF_RW_FWT,
                         gls_MAP_noParenPa_RW_FWT,gls_MAP_noTPB_RW_FWT,
                         gls_MAP_noRCC_RW_FWT)

#specify model names
models_MAP_no_RW_FWT_names <- c('GR','VP', 'VA', 'PP',
                            'IPA','MTDVL','GTF','SF',
                            'ParenPa','TPB',
                            'RCC')

#calculate AIC of each model
AIC_MAP_no_RW_FWT <- aictab(cand.set = models_MAP_no_RW_FWT, modnames = models_MAP_no_RW_FWT_names)
AIC_MAP_no_RW_FWT #Sacamos VP
#------------------------------------------------------------------------------------
#no RW FWT
gls_MAP_noGR_RW_FWT_VP <- gls(mean_MAP_gls ~ VA+PP+IPA+MTDVL+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                           data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_MAP_noVA_RW_FWT_VP <- gls(mean_MAP_gls ~ GR+PP+IPA+MTDVL+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                           data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noPP_RW_FWT_VP <- gls(mean_MAP_gls ~ GR+VA+IPA+MTDVL+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                           data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noIPA_RW_FWT_VP <- gls(mean_MAP_gls ~ GR+VA+PP+MTDVL+FP+SF+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noMTDVL_RW_FWT_VP <- gls(mean_MAP_gls ~ GR+VA+PP+IPA+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                              data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noGTF_RW_FWT_VP <- gls(mean_MAP_gls ~ GR+VA+PP+IPA+MTDVL+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noSF_RW_FWT_VP <- gls(mean_MAP_gls ~ GR+VA+PP+IPA+MTDVL+FP+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                           data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noParenPa_RW_FWT_VP <- gls(mean_MAP_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noTPB_RW_FWT_VP <- gls(mean_MAP_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noRCC_RW_FWT_VP <- gls(mean_MAP_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+ParenPa+TPB, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAP_no_RW_FWT_VP <- list(gls_MAP_noGR_RW_FWT_VP, gls_MAP_noVA_RW_FWT_VP, gls_MAP_noPP_RW_FWT_VP,
                             gls_MAP_noIPA_RW_FWT_VP, gls_MAP_noMTDVL_RW_FWT_VP, gls_MAP_noGTF_RW_FWT_VP, gls_MAP_noSF_RW_FWT_VP,
                             gls_MAP_noParenPa_RW_FWT_VP,gls_MAP_noTPB_RW_FWT_VP,
                             gls_MAP_noRCC_RW_FWT_VP)

#specify model names
models_MAP_no_RW_FWT_VP_names <- c('GR', 'VA', 'PP',
                                'IPA','MTDVL','GTF','SF',
                                'ParenPa','TPB',
                                'RCC')

#calculate AIC of each model
AIC_MAP_no_RW_FWT_VP <- aictab(cand.set = models_MAP_no_RW_FWT_VP, modnames = models_MAP_no_RW_FWT_VP_names)
AIC_MAP_no_RW_FWT_VP #Sacamos GR
summary()
#-------------------------------------------------------------------------------------
#no RW FWT GR
gls_MAP_noVA_RW_FWT_VP_GR <- gls(mean_MAP_gls ~ PP+IPA+MTDVL+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                              data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noPP_RW_FWT_VP_GR <- gls(mean_MAP_gls ~ VA+IPA+MTDVL+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                              data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noIPA_RW_FWT_VP_GR <- gls(mean_MAP_gls ~ VA+PP+MTDVL+FP+SF+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                               data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noMTDVL_RW_FWT_VP_GR <- gls(mean_MAP_gls ~ VA+PP+IPA+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                 data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noGTF_RW_FWT_VP_GR <- gls(mean_MAP_gls ~ VA+PP+IPA+MTDVL+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                               data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noSF_RW_FWT_VP_GR <- gls(mean_MAP_gls ~ VA+PP+IPA+MTDVL+FP+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                              data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noParenPa_RW_FWT_VP_GR <- gls(mean_MAP_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                   data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noTPB_RW_FWT_VP_GR <- gls(mean_MAP_gls ~ VA+PP+IPA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                               data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noRCC_RW_FWT_VP_GR <- gls(mean_MAP_gls ~ VA+PP+IPA+MTDVL+FP+SF+ParenPa+TPB, correlation = corBrownian(phy = tree_gls),
                               data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAP_no_RW_FWT_VP_GR <- list(gls_MAP_noVA_RW_FWT_VP_GR, gls_MAP_noPP_RW_FWT_VP_GR,
                                gls_MAP_noIPA_RW_FWT_VP_GR, gls_MAP_noMTDVL_RW_FWT_VP_GR, gls_MAP_noGTF_RW_FWT_VP_GR, gls_MAP_noSF_RW_FWT_VP_GR,
                                gls_MAP_noParenPa_RW_FWT_VP_GR,gls_MAP_noTPB_RW_FWT_VP_GR,
                                gls_MAP_noRCC_RW_FWT_VP_GR)

#specify model names
models_MAP_no_RW_FWT_VP_GR_names <- c('VA', 'PP',
                                   'IPA','MTDVL','GTF','SF',
                                   'ParenPa','TPB',
                                   'RCC')

#calculate AIC of each model
AIC_MAP_no_RW_FWT_VP_GR <- aictab(cand.set = models_MAP_no_RW_FWT_VP_GR, modnames = models_MAP_no_RW_FWT_VP_GR_names)
AIC_MAP_no_RW_FWT_VP_GR #Sacamos SF
summary()
#----------------------------------------------------------------------------------------
#no RW FWT GR SF
gls_MAP_noVA_RW_FWT_VP_GR_SF <- gls(mean_MAP_gls ~ PP+IPA+MTDVL+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                 data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noPP_RW_FWT_VP_GR_SF <- gls(mean_MAP_gls ~ VA+IPA+MTDVL+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                 data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noIPA_RW_FWT_VP_GR_SF <- gls(mean_MAP_gls ~ VA+PP+MTDVL+FP+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                                  data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noMTDVL_RW_FWT_VP_GR_SF <- gls(mean_MAP_gls ~ VA+PP+IPA+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noGTF_RW_FWT_VP_GR_SF <- gls(mean_MAP_gls ~ VA+PP+IPA+MTDVL+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                  data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noParenPa_RW_FWT_VP_GR_SF <- gls(mean_MAP_gls ~ VA+PP+IPA+MTDVL+FP+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                      data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noTPB_RW_FWT_VP_GR_SF <- gls(mean_MAP_gls ~ VA+PP+IPA+MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                  data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noRCC_RW_FWT_VP_GR_SF <- gls(mean_MAP_gls ~ VA+PP+IPA+MTDVL+FP+ParenPa+TPB, correlation = corBrownian(phy = tree_gls),
                                  data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAP_no_RW_FWT_VP_GR_SF <- list(gls_MAP_noVA_RW_FWT_VP_GR_SF, gls_MAP_noPP_RW_FWT_VP_GR_SF,
                                   gls_MAP_noIPA_RW_FWT_VP_GR_SF, gls_MAP_noMTDVL_RW_FWT_VP_GR_SF, gls_MAP_noGTF_RW_FWT_VP_GR_SF,
                                   gls_MAP_noParenPa_RW_FWT_VP_GR_SF,gls_MAP_noTPB_RW_FWT_VP_GR_SF,
                                   gls_MAP_noRCC_RW_FWT_VP_GR_SF)

#specify model names
models_MAP_no_RW_FWT_VP_GR_SF_names <- c('VA', 'PP',
                                      'IPA','MTDVL','GTF',
                                      'ParenPa','TPB',
                                      'RCC')

#calculate AIC of each model
AIC_MAP_no_RW_FWT_VP_GR_SF <- aictab(cand.set = models_MAP_no_RW_FWT_VP_GR_SF, modnames = models_MAP_no_RW_FWT_VP_GR_SF_names)
AIC_MAP_no_RW_FWT_VP_GR_SF #Sacamos PP
summary()
#----------------------------------------------------------------------------------
#no RW FWT GR SF PP
gls_MAP_noVA_RW_FWT_VP_GR_SF_PP <- gls(mean_MAP_gls ~ IPA+MTDVL+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noIPA_RW_FWT_VP_GR_SF_PP <- gls(mean_MAP_gls ~ VA+MTDVL+FP+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noMTDVL_RW_FWT_VP_GR_SF_PP <- gls(mean_MAP_gls ~ VA+IPA+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noGTF_RW_FWT_VP_GR_SF_PP <- gls(mean_MAP_gls ~ VA+IPA+MTDVL+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noParenPa_RW_FWT_VP_GR_SF_PP <- gls(mean_MAP_gls ~ VA+IPA+MTDVL+FP+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                         data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noTPB_RW_FWT_VP_GR_SF_PP <- gls(mean_MAP_gls ~ VA+IPA+MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noRCC_RW_FWT_VP_GR_SF_PP <- gls(mean_MAP_gls ~ VA+IPA+MTDVL+FP+ParenPa+TPB, correlation = corBrownian(phy = tree_gls),
                                     data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAP_no_RW_FWT_VP_GR_SF_PP <- list(gls_MAP_noVA_RW_FWT_VP_GR_SF_PP, 
                                         gls_MAP_noIPA_RW_FWT_VP_GR_SF_PP, gls_MAP_noMTDVL_RW_FWT_VP_GR_SF_PP, gls_MAP_noGTF_RW_FWT_VP_GR_SF_PP,
                                         gls_MAP_noParenPa_RW_FWT_VP_GR_SF_PP,gls_MAP_noTPB_RW_FWT_VP_GR_SF_PP,
                                         gls_MAP_noRCC_RW_FWT_VP_GR_SF_PP)

#specify model names
models_MAP_no_RW_FWT_VP_GR_SF_PP_names <- c('VA',
                                         'IPA','MTDVL','GTF',
                                         'ParenPa','TPB',
                                         'RCC')

#calculate AIC of each model
AIC_MAP_no_RW_FWT_VP_GR_SF_PP <- aictab(cand.set = models_MAP_no_RW_FWT_VP_GR_SF_PP, modnames = models_MAP_no_RW_FWT_VP_GR_SF_PP_names)
AIC_MAP_no_RW_FWT_VP_GR_SF_PP #Sacamos TPB
summary
#-------------------------------------------------------------------------------------
#no RW FWT GR SF PP TPB
gls_MAP_noVA_RW_FWT_VP_GR_SF_PP_TPB <- gls(mean_MAP_gls ~ IPA+MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noIPA_RW_FWT_VP_GR_SF_PP_TPB  <- gls(mean_MAP_gls ~ VA+MTDVL+FP+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noMTDVL_RW_FWT_VP_GR_SF_PP_TPB  <- gls(mean_MAP_gls ~ VA+IPA+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                          data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noGTF_RW_FWT_VP_GR_SF_PP_TPB  <- gls(mean_MAP_gls ~ VA+IPA+MTDVL+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noParenPa_RW_FWT_VP_GR_SF_PP_TPB  <- gls(mean_MAP_gls ~ VA+IPA+MTDVL+FP+RCC, correlation = corBrownian(phy = tree_gls),
                                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noRCC_RW_FWT_VP_GR_SF_PP_TPB  <- gls(mean_MAP_gls ~ VA+IPA+MTDVL+FP+ParenPa, correlation = corBrownian(phy = tree_gls),
                                        data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAP_no_RW_FWT_VP_GR_SF_PP_TPB  <- list(gls_MAP_noVA_RW_FWT_VP_GR_SF_PP_TPB, 
                                              gls_MAP_noIPA_RW_FWT_VP_GR_SF_PP_TPB, gls_MAP_noMTDVL_RW_FWT_VP_GR_SF_PP_TPB, gls_MAP_noGTF_RW_FWT_VP_GR_SF_PP_TPB,
                                              gls_MAP_noParenPa_RW_FWT_VP_GR_SF_PP_TPB,
                                              gls_MAP_noRCC_RW_FWT_VP_GR_SF_PP_TPB)

#specify model names
models_MAP_no_RW_FWT_VP_GR_SF_PP_TPB_names <- c('VA',
                                            'IPA','MTDVL','GTF',
                                            'ParenPa',
                                            'RCC')

#calculate AIC of each model
AIC_MAP_no_RW_FWT_VP_GR_SF_PP_TPB  <- aictab(cand.set = models_MAP_no_RW_FWT_VP_GR_SF_PP_TPB , modnames = models_MAP_no_RW_FWT_VP_GR_SF_PP_TPB_names)
AIC_MAP_no_RW_FWT_VP_GR_SF_PP_TPB  #Sacamos VA
summary()
#-------------------------------------------------------------------------------------
#no RW FWT GR SF PP TPB IPA
gls_MAP_noVA_RW_FWT_VP_GR_SF_PP_TPB_IPA <- gls(mean_MAP_gls ~ MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                           data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noMTDVL_RW_FWT_VP_GR_SF_PP_TPB_IPA  <- gls(mean_MAP_gls ~ VA+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                               data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noGTF_RW_FWT_VP_GR_SF_PP_TPB_IPA  <- gls(mean_MAP_gls ~ VA+MTDVL+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                             data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noParenPa_RW_FWT_VP_GR_SF_PP_TPB_IPA  <- gls(mean_MAP_gls ~ VA+MTDVL+FP+RCC, correlation = corBrownian(phy = tree_gls),
                                                 data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAP_noRCC_RW_FWT_VP_GR_SF_PP_TPB_IPA  <- gls(mean_MAP_gls ~ VA+MTDVL+FP+ParenPa, correlation = corBrownian(phy = tree_gls),
                                             data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAP_no_RW_FWT_VP_GR_SF_PP_TPB_IPA  <- list(gls_MAP_noVA_RW_FWT_VP_GR_SF_PP_TPB_IPA, 
                                               gls_MAP_noMTDVL_RW_FWT_VP_GR_SF_PP_TPB_IPA, gls_MAP_noGTF_RW_FWT_VP_GR_SF_PP_TPB_IPA,
                                              gls_MAP_noParenPa_RW_FWT_VP_GR_SF_PP_TPB_IPA,
                                              gls_MAP_noRCC_RW_FWT_VP_GR_SF_PP_TPB_IPA)

#specify model names
models_MAP_no_RW_FWT_VP_GR_SF_PP_TPB_IPA_names <- c('VA',
                                                'MTDVL','GTF',
                                                'ParenPa',
                                                'RCC')

#calculate AIC of each model
AIC_MAP_no_RW_FWT_VP_GR_SF_PP_TPB_IPA  <- aictab(cand.set = models_MAP_no_RW_FWT_VP_GR_SF_PP_TPB_IPA , modnames = models_MAP_no_RW_FWT_VP_GR_SF_PP_TPB_IPA_names)
AIC_MAP_no_RW_FWT_VP_GR_SF_PP_TPB_IPA  #Sacamos VA
summary(gls_MAP_noMTDVL_RW_FWT_VP_GR_SF_PP_TPB_IPA)
#---------------------------------------------------------------------------------------
#stepwise manual para MAT
gls_MAT_noGR <- gls(mean_MAT_gls ~  VP + VA + PP + IPA + MTDVL + FP + SF + FWT + ParenPa + TPB + RW + RCC, correlation = corBrownian(phy = tree_gls),
                  data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_MAT_noVP <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVA <- gls(mean_MAT_gls ~ GR+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noPP <- gls(mean_MAT_gls ~ GR+VA+VP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noIPA <- gls(mean_MAT_gls ~ GR+VA+VP+PP+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noMTDVL <- gls(mean_MAT_gls ~ GR+VA+VP+PP+IPA+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noGTF <- gls(mean_MAT_gls ~ GR+VA+VP+PP+IPA+MTDVL+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noSF <- gls(mean_MAT_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noFWT <- gls(mean_MAT_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noParenPa <- gls(mean_MAT_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noTPB <- gls(mean_MAT_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noRW <- gls(mean_MAT_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noRCC <- gls(mean_MAT_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAT_no <- list(gls_MAT_noGR, gls_MAT_noVP, gls_MAT_noVA, gls_MAT_noPP,
                       gls_MAT_noIPA, gls_MAT_noMTDVL, gls_MAT_noGTF, gls_MAT_noSF,
                       gls_MAT_noFWT,gls_MAT_noParenPa,gls_MAT_noTPB,gls_MAT_noRW,
                       gls_MAT_noRCC)

#specify model names
models_MAT_no_names <- c('GR','VP', 'VA', 'PP','IPA','MTDVL','GTF','SF','FWT','ParenPa','TPB','RW',
                          'RCC')

#calculate AIC of each model
AIC_MAT_no <- aictab(cand.set = models_MAT_no, modnames = models_MAT_no_names)
AIC_MAT_no #sacamos a VP
#--------------------------------------------------------------------------------------
#stepwise manual no VP y otros
gls_MAT_noVP_GR <- gls(mean_MAT_gls ~   VA + PP + IPA + MTDVL + FP + SF + FWT + ParenPa + TPB + RW + RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_VA <- gls(mean_MAT_gls ~ GR+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_PP <- gls(mean_MAT_gls ~ GR+VA+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_IPA <- gls(mean_MAT_gls ~ GR+VA+PP+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_MTDVL <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_GTF <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_SF <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_FWT <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_ParenPa <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+FWT+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                         data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_TPB <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RCC <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAT_noVP <- list(gls_MAT_noVP_GR, gls_MAT_noVP_VA, gls_MAT_noVP_PP,
                      gls_MAT_noVP_IPA, gls_MAT_noVP_MTDVL,gls_MAT_noVP_GTF, gls_MAT_noVP_SF,
                      gls_MAT_noVP_FWT,gls_MAT_noVP_ParenPa,gls_MAT_noVP_TPB,gls_MAT_noVP_RW,
                      gls_MAT_noVP_RCC)

#specify model names
models_MAT_noVP_names <- c('GR', 'VA', 'PP',
                            'IPA','MTDVL','GTF','SF',
                            'FWT','ParenPa','TPB','RW',
                            'RCC')

#calculate AIC of each model
AIC_MAT_noVP <- aictab(cand.set = models_MAT_noVP, modnames = models_MAT_noVP_names)
AIC_MAT_noVP #sacamos a VP y RW
#---------------------------------------------------------------------------------------
#stepwise manual no VP, RW y otros
gls_MAT_noVP_RW_GR <- gls(mean_MAT_gls ~   VA + PP + IPA + MTDVL + FP + SF + FWT + ParenPa + TPB + RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_VA <- gls(mean_MAT_gls ~ GR+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_PP <- gls(mean_MAT_gls ~ GR+VA+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_IPA <- gls(mean_MAT_gls ~ GR+VA+PP+MTDVL+FP+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_MTDVL <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+FP+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                          data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_GTF <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_FWT <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_ParenPa <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+FWT+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_TPB <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_RCC <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAT_noVP_RW <- list(gls_MAT_noVP_RW _GR, gls_MAT_noVP_RW_VA, gls_MAT_noVP_RW_PP,
                        gls_MAT_noVP_RW_IPA, gls_MAT_noVP_RW_MTDVL,gls_MAT_noVP_RW_GTF, gls_MAT_noVP_RW_SF,
                        gls_MAT_noVP_RW_FWT,gls_MAT_noVP_RW_ParenPa,gls_MAT_noVP_RW_TPB,
                        gls_MAT_noVP_RW_RCC)

#specify model names
models_MAT_noVP_RW_names <- c('GR', 'VA', 'PP',
                           'IPA','MTDVL','GTF','SF',
                           'FWT','ParenPa','TPB',
                           'RCC')

#calculate AIC of each model
AIC_MAT_noVP_RW <- aictab(cand.set = models_MAT_noVP_RW, modnames = models_MAT_noVP_RW_names)
AIC_MAT_noVP_RW  #sacamos a VP RW y SF
#---------------------------------------------------------------------------------------
#stepwise manual no VP, RW,SF y otros
gls_MAT_noVP_RW_SF_GR <- gls(mean_MAT_gls ~   VA + PP + IPA + MTDVL + FP+ FWT + ParenPa + TPB + RCC, correlation = corBrownian(phy = tree_gls),
                          data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_VA <- gls(mean_MAT_gls ~ GR+PP+IPA+MTDVL+FP+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                          data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP <- gls(mean_MAT_gls ~ GR+VA+IPA+MTDVL+FP+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                          data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_IPA <- gls(mean_MAT_gls ~ GR+VA+PP+MTDVL+FP+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                           data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_MTDVL <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+FP+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                             data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_GTF <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                           data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_FWT <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                           data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_ParenPa <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+FWT+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                               data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_TPB <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+FWT+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                           data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_RCC <- gls(mean_MAT_gls ~ GR+VA+PP+IPA+MTDVL+FP+FWT+ParenPa+TPB, correlation = corBrownian(phy = tree_gls),
                           data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAT_noVP_SF_RW <- list(gls_MAT_noVP_GR, gls_MAT_noVP_VA, gls_MAT_noVP_PP,
                           gls_MAT_noVP_IPA, gls_MAT_noVP_MTDVL,gls_MAT_noVP_GTF, 
                           gls_MAT_noVP_FWT,gls_MAT_noVP_ParenPa,gls_MAT_noVP_TPB,
                           gls_MAT_noVP_RCC)

#specify model names
models_MAT_noVP_RW_SF_names <- c('GR', 'VA', 'PP',
                              'IPA','MTDVL','GTF',
                              'FWT','ParenPa','TPB',
                              'RCC')

#calculate AIC of each model
AIC_MAT_noVP_RW_SF <- aictab(cand.set = models_MAT_noVP_SF_RW, modnames = models_MAT_noVP_RW_SF_names)
AIC_MAT_noVP_RW_SF  #sacamos a VP RW SF y PP
#--------------------------------------------------------------------------------------
#stepwise manual no VP, RW,SF PP y otros
gls_MAT_noVP_RW_SF_PP_GR <- gls(mean_MAT_gls ~   VA  + IPA + MTDVL + FP+ FWT + ParenPa + TPB + RCC, correlation = corBrownian(phy = tree_gls),
                             data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_VA <- gls(mean_MAT_gls ~ GR+IPA+MTDVL+FP+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                             data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_IPA <- gls(mean_MAT_gls ~ GR+VA+MTDVL+FP+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                              data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_MTDVL <- gls(mean_MAT_gls ~ GR+VA+IPA+FP+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_GTF <- gls(mean_MAT_gls ~ GR+VA+IPA+MTDVL+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                              data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT <- gls(mean_MAT_gls ~ GR+VA+IPA+MTDVL+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                              data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_ParenPa <- gls(mean_MAT_gls ~ GR+VA+IPA+MTDVL+FP+FWT+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                  data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_TPB <- gls(mean_MAT_gls ~ GR+VA+IPA+MTDVL+FP+FWT+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                              data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_RCC <- gls(mean_MAT_gls ~ GR+VA+IPA+MTDVL+FP+FWT+ParenPa+TPB, correlation = corBrownian(phy = tree_gls),
                              data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAT_noVP_SF_PP_RW <- list(gls_MAT_noVP_RW_SF_PP_GR, gls_MAT_noVP_RW_SF_PP_VA,
                              gls_MAT_noVP_RW_SF_PP_IPA, gls_MAT_noVP_RW_SF_PP_MTDVL,gls_MAT_noVP_RW_SF_PP_GTF, 
                              gls_MAT_noVP_RW_SF_PP_FWT,gls_MAT_noVP_RW_SF_PP_ParenPa,gls_MAT_noVP_RW_SF_PP_TPB,
                              gls_MAT_noVP_RW_SF_PP_RCC)

#specify model names
models_MAT_noVP_RW_SF_PP_names <- c('GR', 'VA', 
                                 'IPA','MTDVL','GTF',
                                 'FWT','ParenPa','TPB',
                                 'RCC')

#calculate AIC of each model
AIC_MAT_noVP_RW_SF_PP <- aictab(cand.set = models_MAT_noVP_SF_PP_RW, modnames = models_MAT_noVP_RW_SF_PP_names)
AIC_MAT_noVP_RW_SF_PP  #sacamos a VP RW SF PP Y FWT
#--------------------------------------------------------------------------------------
#stepwise manual no VP, RW,SF PP FWT y otros
gls_MAT_noVP_RW_SF_PP_FWT_GR <- gls(mean_MAT_gls ~   VA  + IPA + MTDVL + FP + ParenPa + TPB + RCC, correlation = corBrownian(phy = tree_gls),
                                data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_VA <- gls(mean_MAT_gls ~ GR+IPA+MTDVL+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_IPA <- gls(mean_MAT_gls ~ GR+VA+MTDVL+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                 data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_MTDVL <- gls(mean_MAT_gls ~ GR+VA+IPA+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                   data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GTF <- gls(mean_MAT_gls ~ GR+VA+IPA+MTDVL+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                 data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_ParenPa <- gls(mean_MAT_gls ~ GR+VA+IPA+MTDVL+FP+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                         data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_TPB <- gls(mean_MAT_gls ~ GR+VA+IPA+MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                 data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_RCC <- gls(mean_MAT_gls ~ GR+VA+IPA+MTDVL+FP+ParenPa+TPB, correlation = corBrownian(phy = tree_gls),
                                 data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAT_noVP_SF_PP_FWT_RW <- list(gls_MAT_noVP_RW_SF_PP_FWT_GR, gls_MAT_noVP_RW_SF_PP_FWT_VA,
                                 gls_MAT_noVP_RW_SF_PP_FWT_IPA, gls_MAT_noVP_RW_SF_PP_FWT_MTDVL,gls_MAT_noVP_RW_SF_PP_FWT_GTF, 
                                gls_MAT_noVP_RW_SF_PP_FWT_ParenPa,gls_MAT_noVP_RW_SF_PP_FWT_TPB,
                                 gls_MAT_noVP_RW_SF_PP_FWT_RCC)

#specify model names
models_MAT_noVP_RW_SF_PP_FWT_names <- c('GR', 'VA', 
                                    'IPA','MTDVL','GTF',
                                   'ParenPa','TPB',
                                    'RCC')

#calculate AIC of each model
AIC_MAT_noVP_RW_SF_PP_FWT <- aictab(cand.set = models_MAT_noVP_SF_PP_FWT_RW, modnames = models_MAT_noVP_RW_SF_PP_FWT_names)
AIC_MAT_noVP_RW_SF_PP_FWT  #sacamos a VP RW SF PP FWT Y GR
#--------------------------------------------------------------------------------------
#stepwise manual no VP, RW,SF PP FWT GR y otros


gls_MAT_noVP_RW_SF_PP_FWT_GR_VA <- gls(mean_MAT_gls ~ IPA+MTDVL+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA <- gls(mean_MAT_gls ~ VA+MTDVL+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_MTDVL <- gls(mean_MAT_gls ~ VA+IPA+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_GTF <- gls(mean_MAT_gls ~ VA+IPA+MTDVL+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_ParenPa <- gls(mean_MAT_gls ~ VA+IPA+MTDVL+FP+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                         data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_TPB <- gls(mean_MAT_gls ~ IPA+MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_RCC <- gls(mean_MAT_gls ~ VA+IPA+MTDVL+FP+ParenPa+TPB, correlation = corBrownian(phy = tree_gls),
                                     data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAT_noVP_SF_PP_FWT_GR_RW <- list(gls_MAT_noVP_RW_SF_PP_FWT_GR_VA,
                                     gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA, gls_MAT_noVP_RW_SF_PP_FWT_GR_MTDVL,gls_MAT_noVP_RW_SF_PP_FWT_GR_GTF, 
                                     gls_MAT_noVP_RW_SF_PP_FWT_GR_ParenPa,gls_MAT_noVP_RW_SF_PP_FWT_GR_TPB,
                                     gls_MAT_noVP_RW_SF_PP_FWT_GR_RCC)

#specify model names
models_MAT_noVP_RW_SF_PP_FWT_GR_names <- c('VA', 
                                        'IPA','MTDVL','GTF',
                                        'ParenPa','TPB',
                                        'RCC')

#calculate AIC of each model
AIC_MAT_noVP_RW_SF_PP_FWT_GR <- aictab(cand.set = models_MAT_noVP_SF_PP_FWT_GR_RW, modnames = models_MAT_noVP_RW_SF_PP_FWT_GR_names)
AIC_MAT_noVP_RW_SF_PP_FWT_GR  #sacamos a VP RW SF PP FWT GR
#-------------------------------------------------------------------------------------
#stepwise manual no VP, RW,SF PP FWT GR IPA y otros


gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_VA <- gls(mean_MAT_gls ~ MTDVL+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_MTDVL <- gls(mean_MAT_gls ~ VA+FP+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                          data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_GTF <- gls(mean_MAT_gls ~ VA+MTDVL+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa <- gls(mean_MAT_gls ~ VA+MTDVL+FP+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_TPB <- gls(mean_MAT_gls ~ MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_RCC <- gls(mean_MAT_gls ~ VA+MTDVL+FP+ParenPa+TPB, correlation = corBrownian(phy = tree_gls),
                                        data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAT_noVP_SF_PP_FWT_GR_IPA_RW <- list(gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_VA,
                                        gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_MTDVL,gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_GTF, 
                                        gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa,gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_TPB,
                                        gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_RCC)


#specify model names
models_MAT_noVP_RW_SF_PP_FWT_GR_IPA_names <- c('VA', 
                                           'MTDVL','GTF',
                                           'ParenPa','TPB',
                                           'RCC')

#calculate AIC of each model
AIC_MAT_noVP_RW_SF_PP_FWT_GR_IPA <- aictab(cand.set = models_MAT_noVP_SF_PP_FWT_GR_IPA_RW, modnames = models_MAT_noVP_RW_SF_PP_FWT_GR_IPA_names)
AIC_MAT_noVP_RW_SF_PP_FWT_GR_IPA #sacamos a VP RW SF PP FWT GR IPA ParenPa
mod <- list(gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_GTF,gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_GTF_0)
mod_n <-c("pgls", "gls")
aictab(cand.set = mod, modnames = mod_n)
#----------------------------------------------------------------------------------------
#stepwise manual no VP, RW,SF PP FWT GR IPA ParenPa y otros


gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA <- gls(mean_MAT_gls ~ MTDVL+FP+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                           data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_MTDVL <- gls(mean_MAT_gls ~ VA+FP+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                              data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_GTF <- gls(mean_MAT_gls ~ VA+MTDVL+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_TPB <- gls(mean_MAT_gls ~ MTDVL+FP+RCC, correlation = corBrownian(phy = tree_gls),
                                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_RCC <- gls(mean_MAT_gls ~ VA+MTDVL+FP+TPB, correlation = corBrownian(phy = tree_gls),
                                            data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAT_noVP_SF_PP_FWT_GR_IPA_ParenPa_RW <- list(gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA,
                                            gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_MTDVL,gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_GTF, 
                                            gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_TPB,
                                            gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_RCC)


#specify model names
models_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_names <- c('VA', 
                                               'MTDVL','GTF',
                                               'TPB',
                                               'RCC')

#calculate AIC of each model
AIC_MAT_noVP_RW_SF_PP_FWT_GR_ParenPa_IPA <- aictab(cand.set = models_MAT_noVP_SF_PP_FWT_GR_IPA_ParenPa_RW, modnames = models_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_names)
AIC_MAT_noVP_RW_SF_PP_FWT_GR_ParenPa_IPA #sacamos a VP RW SF PP FWT GR IPA ParenPa VA
#----------------------------------------------------------------------------------------
#stepwise manual no VP, RW,SF PP FWT GR IPA ParenPa VAy otros




gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA_MTDVL <- gls(mean_MAT_gls ~ FP+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                                      data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA_GTF <- gls(mean_MAT_gls ~ MTDVL+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                                                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA_TPB <- gls(mean_MAT_gls ~ MTDVL+FP+RCC, correlation = corBrownian(phy = tree_gls),
                                                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA_RCC <- gls(mean_MAT_gls ~ MTDVL+FP+TPB, correlation = corBrownian(phy = tree_gls),
                                                    data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAT_noVP_SF_PP_FWT_GR_IPA_ParenPa_VA_RW <- list(gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA_MTDVL,gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA_GTF, 
                                                    gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA_TPB,
                                                    gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA_RCC)


#specify model names
models_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA_names <- c('MTDVL','GTF',
                                                          'TPB',
                                                          'RCC') 
                                                       

#calculate AIC of each model
AIC_MAT_noVP_RW_SF_PP_FWT_GR_ParenPa_IPA_VA <- aictab(cand.set = models_MAT_noVP_SF_PP_FWT_GR_IPA_ParenPa_VA_RW, modnames = models_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA_names)
AIC_MAT_noVP_RW_SF_PP_FWT_GR_ParenPa_IPA_VA #sacamos a VP RW SF PP FWT GR IPA ParenPa VA


#Selección final para MAT
gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA
gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_MTDVL
gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa
gls_MAT_noVP_RW_SF_PP_FWT_GR_IPA_ParenPa_VA_MTDVL
#--------------------------------------------------------------------------------------
#stepwise manual no GTF, IPA RCC  otros
gls_MAT_noGTF_IPA_GR <- gls(mean_MAT_gls ~  VP + VA + PP + MTDVL + SF + FWT + ParenPa + TPB + RW , correlation = corBrownian(phy = tree_gls),
                            data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_MAT_noGTF_IPA_VP <- gls(mean_MAT_gls ~ GR+VA+PP+MTDVL+SF+FWT+ParenPa+TPB+RW, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noGTF_IPA_VA <- gls(mean_MAT_gls ~ GR+VP+PP+MTDVL+SF+FWT+ParenPa+TPB+RW, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noGTF_IPA_PP <- gls(mean_MAT_gls ~ GR+VA+VP+MTDVL+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noGTF_IPA_MTDVL <- gls(mean_MAT_gls ~ GR+VA+VP+PP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                               data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noGTF_IPA_SF <- gls(mean_MAT_gls ~ GR+VA+VP+PP+MTDVL+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noGTF_IPA_FWT <- gls(mean_MAT_gls ~ GR+VA+VP+PP+MTDVL+SF+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                             data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noGTF_IPA_ParenPa <- gls(mean_MAT_gls ~ GR+VA+VP+PP+MTDVL+SF+FWT+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                                 data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noGTF_IPA_TPB <- gls(mean_MAT_gls ~ GR+VA+VP+PP+MTDVL+SF+FWT+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                             data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noGTF_IPA_RW <- gls(mean_MAT_gls ~ GR+VA+VP+PP+MTDVL+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_noGTF_IPA_RCC <- gls(mean_MAT_gls ~ GR+VA+VP+PP+MTDVL+SF+FWT+ParenPa+TPB+RW, correlation = corBrownian(phy = tree_gls),
                             data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAT_noGTF_IPA <- list(gls_MAT_noGTF_IPA_GR, gls_MAT_noGTF_IPA_VP, gls_MAT_noGTF_IPA_VA, gls_MAT_noGTF_IPA_PP,
                             gls_MAT_noGTF_IPA_MTDVL, gls_MAT_noGTF_IPA_SF,
                             gls_MAT_noGTF_IPA_FWT,gls_MAT_noGTF_IPA_ParenPa,gls_MAT_noGTF_IPA_TPB,gls_MAT_noGTF_IPA_RW,
                             gls_MAT_noGTF_IPA_RCC)

#specify model names
models_MAT_noGTF_IPA_names <- c('GR','VP', 'VA', 'PP','MTDVL','SF','FWT','ParenPa','TPB','RW',
                                'RCC')

#calculate AIC of each model
AIC_MAT_noGTF_IPA <- aictab(cand.set = models_MAT_noGTF_IPA, modnames = models_MAT_noGTF_IPA_names)
AIC_MAT_noGTF_IPA #sacamos a GTF IPA y RCC
#----------------------------------------------------------------------------------------
gls_MAT_GR <- gls(mean_MAT_gls ~ Growth_Rings, correlation = corBrownian(phy = tree_gls),
                  data = traits_MAP, method = "ML")

gls_MAT_VP <- gls(mean_MAT_gls ~ Vessel_porosity, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_MAT_VA <- gls(mean_MAT_gls ~ Vessel_arangament, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_MAT_PP <- gls(mean_MAT_gls ~ Perforation_plates, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_MAT_IPA <- gls(mean_MAT_gls ~ Intervessel_pits_arregement, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML")

gls_MAT_MTDVL <- gls(mean_MAT_gls ~ Mean_tangential_diameter_of_vessel_lumina, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML")

gls_MAT_GTF <- gls(mean_MAT_gls ~ Ground_tissue_fibres, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML", na.action = na.roughfix)

gls_MAT_SF <- gls(mean_MAT_gls ~ Septa_in_fibers, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_MAT_FWT <- gls(mean_MAT_gls ~ Fibre_wall_thickness, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML")

gls_MAT_ParenPa <- gls(mean_MAT_gls ~ Parenchyma_Paratracheal, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML")

gls_MAT_TPB <- gls(mean_MAT_gls ~ Type_of_parenchyma_banding, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML")

gls_MAT_RW <- gls(mean_MAT_gls ~ Ray_width, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_MAT_RCC <- gls(mean_MAT_gls ~ Rays_cellular_composition, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML",  na.action = na.roughfix)

models_MAT_IND <- list(gls_MAT_GR, gls_MAT_VP, gls_MAT_VA, gls_MAT_PP,
                       gls_MAT_IPA, gls_MAT_MTDVL, gls_MAT_GTF, gls_MAT_SF,
                       gls_MAT_FWT,gls_MAT_ParenPa,gls_MAT_TPB,gls_MAT_RW,
                       gls_MAT_RCC)

#specify model names
models_MAT_IND_names <- c('GR','VP', 'VA', 'PP','IPA','MTDVL','GTF','SF','FWT','ParenPa','TPB','RW',
                          'RCC')

#calculate AIC of each model
AIC_MAT_IND <- aictab(cand.set = models_MAT_IND, modnames = models_MAT_IND_names)
AIC_MAT_IND
#-----------------------------------------------------------------------------------
gls_PDQ_GR <- gls(mean_PDQ_gls ~ Growth_Rings, correlation = corBrownian(phy = tree_gls),
                  data = traits_MAP, method = "ML")

gls_PDQ_VP <- gls(mean_PDQ_gls ~ Vessel_porosity, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_PDQ_VA <- gls(mean_PDQ_gls ~ Vessel_arangament, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_PDQ_PP <- gls(mean_PDQ_gls ~ Perforation_plates, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_PDQ_IPA <- gls(mean_PDQ_gls ~ Intervessel_pits_arregement, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML")

gls_PDQ_MTDVL <- gls(mean_PDQ_gls ~ Mean_tangential_diameter_of_vessel_lumina, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML")

gls_PDQ_GTF <- gls(mean_PDQ_gls ~ Ground_tissue_fibres, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_SF <- gls(mean_PDQ_gls ~ Septa_in_fibers, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_PDQ_FWT <- gls(mean_PDQ_gls ~ Fibre_wall_thickness, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML")

gls_PDQ_ParenPa <- gls(mean_PDQ_gls ~ Parenchyma_Paratracheal, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML")

gls_PDQ_TPB <- gls(mean_PDQ_gls ~ Type_of_parenchyma_banding, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML")

gls_PDQ_RW <- gls(mean_PDQ_gls ~ Ray_width, correlation = corBrownian(phy = tree_gls),
                  data = traits_gls, method = "ML")

gls_PDQ_RCC <- gls(mean_PDQ_gls ~ Rays_cellular_composition, correlation = corBrownian(phy = tree_gls),
                   data = traits_gls, method = "ML",  na.action = na.roughfix)

models_PDQ_IND <- list(gls_PDQ_GR, gls_PDQ_VP, gls_PDQ_VA, gls_PDQ_PP,
                       gls_PDQ_IPA, gls_PDQ_MTDVL, gls_PDQ_GTF, gls_PDQ_SF,
                       gls_PDQ_FWT,gls_PDQ_ParenPa,gls_PDQ_TPB,gls_PDQ_RW,
                       gls_PDQ_RCC)

#specify model names
models_PDQ_IND_names <- c('GR','VP', 'VA', 'PP','IPA','MTDVL','GTF','SF','FWT','ParenPa','TPB','RW',
                          'RCC')

#calculate AIC of each model
AIC_PDQ_IND <- aictab(cand.set = models_PDQ_IND, modnames = models_PDQ_IND_names)
AIC_PDQ_IND
#----------------------------------------------------------------------------------------
#stepwise manual para PDQ
gls_PDQ_noGR <- gls(mean_PDQ_gls ~  VP+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_PDQ_noVP <- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noVA <- gls(mean_PDQ_gls ~ GR+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noPP <- gls(mean_PDQ_gls ~ GR+VA+VP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noIPA <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+MTDVL+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noMTDVL <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+FP+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noGTF <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+SF+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noSF <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+FWT+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noFWT <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+ParenPa+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noParenPa <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+TPB+RW+RCC, correlation = corBrownian(phy = tree_gls),
                         data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noTPB <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noRW <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noRCC <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+TPB+RW, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML",  na.action = na.roughfix)

models_PDQ_no <- list(gls_PDQ_noGR, gls_PDQ_noVP, gls_PDQ_noVA, gls_PDQ_noPP,
                      gls_PDQ_noIPA, gls_PDQ_noMTDVL, gls_PDQ_noGTF, gls_PDQ_noSF,
                      gls_PDQ_noFWT,gls_PDQ_noParenPa,gls_PDQ_noTPB,gls_PDQ_noRW,
                      gls_PDQ_noRCC)

#specify model names
models_PDQ_no_names <- c('GR','VP', 'VA', 'PP','IPA','MTDVL','GTF','SF','FWT','ParenPa','TPB','RW',
                         'RCC')

#calculate AIC of each model
AIC_PDQ_no <- aictab(cand.set = models_PDQ_no, modnames = models_PDQ_no_names)
AIC_PDQ_no #sacamos a TPB

#-------------------------------------------------------------------------------------
#stepwise manual para PDQ SIN TPB
gls_PDQ_noGR_TPB <- gls(mean_PDQ_gls ~  VP+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_PDQ_noVP_TPB <- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noVA_TPB <- gls(mean_PDQ_gls ~ GR+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noPP_TPB <- gls(mean_PDQ_gls ~ GR+VA+VP+IPA+MTDVL+FP+SF+FWT+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noIPA_TPB <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+MTDVL+FP+SF+FWT+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noMTDVL_TPB <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+FP+SF+FWT+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noGTF_TPB <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+SF+FWT+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noSF_TPB <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+FWT+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noFWT_TPB <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noParenPa_TPB <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+RW+RCC, correlation = corBrownian(phy = tree_gls),
                         data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noRW_TPB <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noRCC_TPB <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+FWT+ParenPa+RW, correlation = corBrownian(phy = tree_gls),
                     data = traits_gls, method = "ML",  na.action = na.roughfix)

models_PDQ_no_TPB <- list(gls_PDQ_noGR_TPB, gls_PDQ_noVP_TPB, gls_PDQ_noVA_TPB, gls_PDQ_noPP_TPB,
                      gls_PDQ_noIPA_TPB, gls_PDQ_noMTDVL_TPB, gls_PDQ_noGTF_TPB, gls_PDQ_noSF_TPB,
                      gls_PDQ_noFWT_TPB,gls_PDQ_noParenPa_TPB, gls_PDQ_noRW_TPB,
                      gls_PDQ_noRCC_TPB)

#specify model names
models_PDQ_no_TPB_names <- c('GR','VP', 'VA', 'PP','IPA','MTDVL','GTF','SF','FWT','ParenPa','RW',
                         'RCC')

#calculate AIC of each model
AIC_PDQ_no_TPB <- aictab(cand.set = models_PDQ_no_TPB, modnames = models_PDQ_no_TPB_names)
AIC_PDQ_no_TPB #sacamos a FWT
#-----------------------------------------------------------------------------------------
#stepwise manual para PDQ SIN TPB FWT
gls_PDQ_noGR_TPB_FWT <- gls(mean_PDQ_gls ~  VP+VA+PP+IPA+MTDVL+FP+SF+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                        data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_PDQ_noVP_TPB_FWT <- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noVA_TPB_FWT <- gls(mean_PDQ_gls ~ GR+VP+PP+IPA+MTDVL+FP+SF+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noPP_TPB_FWT <- gls(mean_PDQ_gls ~ GR+VA+VP+IPA+MTDVL+FP+SF+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noIPA_TPB_FWT <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+MTDVL+FP+SF+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                         data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noMTDVL_TPB_FWT <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+FP+SF+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                           data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noGTF_TPB_FWT <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+SF+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                         data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noSF_TPB_FWT <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+ParenPa+RW+RCC, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noParenPa_TPB_FWT <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+RW+RCC, correlation = corBrownian(phy = tree_gls),
                             data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noRW_TPB_FWT <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noRCC_TPB_FWT <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+ParenPa+RW, correlation = corBrownian(phy = tree_gls),
                         data = traits_gls, method = "ML",  na.action = na.roughfix)

models_PDQ_no_TPB_FWT <- list(gls_PDQ_noGR_TPB_FWT, gls_PDQ_noVP_TPB_FWT, gls_PDQ_noVA_TPB_FWT, gls_PDQ_noPP_TPB_FWT,
                          gls_PDQ_noIPA_TPB_FWT, gls_PDQ_noMTDVL_TPB_FWT, gls_PDQ_noGTF_TPB_FWT, gls_PDQ_noSF_TPB_FWT,
                          gls_PDQ_noParenPa_TPB_FWT, gls_PDQ_noRW_TPB_FWT,
                          gls_PDQ_noRCC_TPB_FWT)

#specify model names
models_PDQ_no_TPB_FWT_names <- c('GR','VP', 'VA', 'PP','IPA','MTDVL','GTF','SF','ParenPa','RW',
                             'RCC')

#calculate AIC of each model
AIC_PDQ_no_TPB_FWT <- aictab(cand.set = models_PDQ_no_TPB_FWT, modnames = models_PDQ_no_TPB_FWT_names)
AIC_PDQ_no_TPB_FWT #sacamos a RW
#---------------------------------------------------------------------------------------
#stepwise manual para PDQ SIN TPB FWT RW
gls_PDQ_noGR_TPB_FWT_RW <- gls(mean_PDQ_gls ~  VP+VA+PP+IPA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                            data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_PDQ_noVP_TPB_FWT_RW <- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noVA_TPB_FWT_RW <- gls(mean_PDQ_gls ~ GR+VP+PP+IPA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noPP_TPB_FWT_RW <- gls(mean_PDQ_gls ~ GR+VA+VP+IPA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noIPA_TPB_FWT_RW <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                             data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noMTDVL_TPB_FWT_RW <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                               data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noGTF_TPB_FWT_RW <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                             data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noSF_TPB_FWT_RW <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                            data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noParenPa_TPB_FWT_RW <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+RCC, correlation = corBrownian(phy = tree_gls),
                                 data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noRCC_TPB_FWT_RW <- gls(mean_PDQ_gls ~ GR+VA+VP+PP+IPA+MTDVL+FP+SF+ParenPa, correlation = corBrownian(phy = tree_gls),
                             data = traits_gls, method = "ML",  na.action = na.roughfix)

models_PDQ_no_TPB_FWT_RW <- list(gls_PDQ_noGR_TPB_FWT_RW, gls_PDQ_noVP_TPB_FWT_RW, gls_PDQ_noVA_TPB_FWT_RW, gls_PDQ_noPP_TPB_FWT_RW,
                              gls_PDQ_noIPA_TPB_FWT_RW, gls_PDQ_noMTDVL_TPB_FWT_RW, gls_PDQ_noGTF_TPB_FWT_RW, gls_PDQ_noSF_TPB_FWT_RW,
                              gls_PDQ_noParenPa_TPB_FWT_RW, 
                              gls_PDQ_noRCC_TPB_FWT_RW)

#specify model names
models_PDQ_no_TPB_FWT_RW_names <- c('GR','VP', 'VA', 'PP','IPA','MTDVL','GTF','SF','ParenPa',
                                 'RCC')

#calculate AIC of each model
AIC_PDQ_no_TPB_FWT_RW <- aictab(cand.set = models_PDQ_no_TPB_FWT_RW, modnames = models_PDQ_no_TPB_FWT_RW_names)
AIC_PDQ_no_TPB_FWT_RW #sacamos a VP
#------------------------------------------------------------------------------------
#stepwise manual para PDQ SIN TPB FWT RW VP
gls_PDQ_noGR_TPB_FWT_RW_VP <- gls(mean_PDQ_gls ~  VA+PP+IPA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                               data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_PDQ_noVA_TPB_FWT_RW_VP <- gls(mean_PDQ_gls ~ GR+PP+IPA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                               data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noPP_TPB_FWT_RW_VP <- gls(mean_PDQ_gls ~ GR+VA+IPA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                               data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noIPA_TPB_FWT_RW_VP <- gls(mean_PDQ_gls ~ GR+VA+PP+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noMTDVL_TPB_FWT_RW_VP <- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                  data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noGTF_TPB_FWT_RW_VP<- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+MTDVL+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noSF_TPB_FWT_RW_VP <- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                               data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noParenPa_TPB_FWT_RW_VP <- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+RCC, correlation = corBrownian(phy = tree_gls),
                                    data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noRCC_TPB_FWT_RW_VP <- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+MTDVL+FP+SF+ParenPa, correlation = corBrownian(phy = tree_gls),
                                data = traits_gls, method = "ML",  na.action = na.roughfix)

models_PDQ_no_TPB_FWT_RW_VP <- list(gls_PDQ_noGR_TPB_FWT_RW_VP, gls_PDQ_noVA_TPB_FWT_RW_VP, gls_PDQ_noPP_TPB_FWT_RW_VP,
                                 gls_PDQ_noIPA_TPB_FWT_RW_VP, gls_PDQ_noMTDVL_TPB_FWT_RW_VP, gls_PDQ_noGTF_TPB_FWT_RW_VP, gls_PDQ_noSF_TPB_FWT_RW_VP,
                                 gls_PDQ_noParenPa_TPB_FWT_RW_VP, 
                                 gls_PDQ_noRCC_TPB_FWT_RW_VP)

#specify model names
models_PDQ_no_TPB_FWT_RW_VP_names <- c('GR','VA', 'PP','IPA','MTDVL','GTF','SF','ParenPa',
                                    'RCC')

#calculate AIC of each model
AIC_PDQ_no_TPB_FWT_RW_VP <- aictab(cand.set = models_PDQ_no_TPB_FWT_RW_VP, modnames = models_PDQ_no_TPB_FWT_RW_VP_names)
AIC_PDQ_no_TPB_FWT_RW_VP #sacamos a PP
#--------------------------------------------------------------------------------------
#stepwise manual para PDQ SIN TPB FWT RW VP PP
gls_PDQ_noGR_TPB_FWT_RW_VP_PP <- gls(mean_PDQ_gls ~  VA+IPA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                  data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_PDQ_noVA_TPB_FWT_RW_VP_PP <- gls(mean_PDQ_gls ~ GR+IPA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                  data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noIPA_TPB_FWT_RW_VP_PP <- gls(mean_PDQ_gls ~ GR+VA+MTDVL+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                   data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noMTDVL_TPB_FWT_RW_VP_PP <- gls(mean_PDQ_gls ~ GR+VA+IPA+FP+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noGTF_TPB_FWT_RW_VP_PP <- gls(mean_PDQ_gls ~ GR+VA+IPA+MTDVL+SF+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                  data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noSF_TPB_FWT_RW_VP_PP <- gls(mean_PDQ_gls ~ GR+VA+IPA+MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                  data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noParenPa_TPB_FWT_RW_VP_PP <- gls(mean_PDQ_gls ~ GR+VA+IPA+MTDVL+FP+SF+RCC, correlation = corBrownian(phy = tree_gls),
                                       data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noRCC_TPB_FWT_RW_VP_PP <- gls(mean_PDQ_gls ~ GR+VA+IPA+MTDVL+FP+SF+ParenPa, correlation = corBrownian(phy = tree_gls),
                                   data = traits_gls, method = "ML",  na.action = na.roughfix)

models_PDQ_no_TPB_FWT_RW_VP_PP <- list(gls_PDQ_noGR_TPB_FWT_RW_VP_PP, gls_PDQ_noVA_TPB_FWT_RW_VP_PP, 
                                       gls_PDQ_noIPA_TPB_FWT_RW_VP_PP, gls_PDQ_noMTDVL_TPB_FWT_RW_VP_PP, gls_PDQ_noGTF_TPB_FWT_RW_VP_PP, gls_PDQ_noSF_TPB_FWT_RW_VP_PP,
                                       gls_PDQ_noParenPa_TPB_FWT_RW_VP_PP, 
                                       gls_PDQ_noRCC_TPB_FWT_RW_VP_PP)

#specify model names
models_PDQ_no_TPB_FWT_RW_VP_PP_names <- c('GR','VA',
                                       'IPA','MTDVL','GTF','SF',
                                       'ParenPa',
                                       'RCC')

#calculate AIC of each model
AIC_PDQ_no_TPB_FWT_RW_VP_PP <- aictab(cand.set = models_PDQ_no_TPB_FWT_RW_VP_PP, modnames = models_PDQ_no_TPB_FWT_RW_VP_PP_names)
AIC_PDQ_no_TPB_FWT_RW_VP_PP #sacamos a SF
#-----------------------------------------------------------------------------------------
#stepwise manual para PDQ SIN TPB FWT RW VP PP SF
gls_PDQ_noGR_TPB_FWT_RW_VP_PP_SF <- gls(mean_PDQ_gls ~  VA+PP+IPA+MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                     data = traits_MAP, method = "ML", na.action = na.roughfix)

gls_PDQ_noVA_TPB_FWT_RW_VP_PP_SF <- gls(mean_PDQ_gls ~ GR+PP+IPA+MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noIPA_TPB_FWT_RW_VP_PP_SF <- gls(mean_PDQ_gls ~ GR+VA+PP+MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                      data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noMTDVL_TPB_FWT_RW_VP_PP_SF <- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noGTF_TPB_FWT_RW_VP_PP_SF <- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+MTDVL+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                      data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noSF_TPB_FWT_RW_VP_PP_SF <- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+MTDVL+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                     data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noParenPa_TPB_FWT_RW_VP_PP_SF <- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+MTDVL+FP+RCC, correlation = corBrownian(phy = tree_gls),
                                          data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noRCC_TPB_FWT_RW_VP_PP_SF <- gls(mean_PDQ_gls ~ GR+VA+PP+IPA+MTDVL+FP+ParenPa, correlation = corBrownian(phy = tree_gls),
                                      data = traits_gls, method = "ML",  na.action = na.roughfix)

models_PDQ_no_TPB_FWT_RW_VP_PP_SF <- list(gls_PDQ_noGR_TPB_FWT_RW_VP_PP_SF, gls_PDQ_noVA_TPB_FWT_RW_VP_PP_SF, 
                                       gls_PDQ_noIPA_TPB_FWT_RW_VP_PP_SF, gls_PDQ_noMTDVL_TPB_FWT_RW_VP_PP_SF, gls_PDQ_noGTF_TPB_FWT_RW_VP_PP_SF, 
                                       gls_PDQ_noParenPa_TPB_FWT_RW_VP_PP_SF, 
                                       gls_PDQ_noRCC_TPB_FWT_RW_VP_PP_SF)

#specify model names
models_PDQ_no_TPB_FWT_RW_VP_PP_SF_names <- c('GR','VA',
                                          'IPA','MTDVL','GTF',
                                          'ParenPa',
                                          'RCC')

#calculate AIC of each model
AIC_PDQ_no_TPB_FWT_RW_VP_PP_SF <- aictab(cand.set = models_PDQ_no_TPB_FWT_RW_VP_PP_SF, modnames = models_PDQ_no_TPB_FWT_RW_VP_PP_SF_names)
AIC_PDQ_no_TPB_FWT_RW_VP_PP_SF #sacamos a GR
#---------------------------------------------------------------------------------------
#stepwise manual para PDQ SIN TPB FWT RW VP PP SF GR
gls_PDQ_noVA_TPB_FWT_RW_VP_PP_SF_GR <- gls(mean_PDQ_gls ~ PP+IPA+MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noIPA_TPB_FWT_RW_VP_PP_SF_GR <- gls(mean_PDQ_gls ~ VA+PP+MTDVL+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                         data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noMTDVL_TPB_FWT_RW_VP_PP_SF_GR <- gls(mean_PDQ_gls ~ VA+PP+IPA+FP+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                           data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noGTF_TPB_FWT_RW_VP_PP_SF_GR <- gls(mean_PDQ_gls ~ VA+PP+IPA+MTDVL+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                         data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noSF_TPB_FWT_RW_VP_PP_SF_GR <- gls(mean_PDQ_gls ~ VA+PP+IPA+MTDVL+ParenPa+RCC, correlation = corBrownian(phy = tree_gls),
                                        data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noParenPa_TPB_FWT_RW_VP_PP_SF_GR <- gls(mean_PDQ_gls ~ VA+PP+IPA+MTDVL+FP+RCC, correlation = corBrownian(phy = tree_gls),
                                             data = traits_gls, method = "ML", na.action = na.roughfix)

gls_PDQ_noRCC_TPB_FWT_RW_VP_PP_SF_GR <- gls(mean_PDQ_gls ~ VA+PP+IPA+MTDVL+FP+ParenPa, correlation = corBrownian(phy = tree_gls),
                                         data = traits_gls, method = "ML",  na.action = na.roughfix)

models_PDQ_no_TPB_FWT_RW_VP_PP_SF_GR <- list(gls_PDQ_noVA_TPB_FWT_RW_VP_PP_SF_GR, 
                                          gls_PDQ_noIPA_TPB_FWT_RW_VP_PP_SF_GR, gls_PDQ_noMTDVL_TPB_FWT_RW_VP_PP_SF_GR, gls_PDQ_noGTF_TPB_FWT_RW_VP_PP_SF_GR, 
                                          gls_PDQ_noParenPa_TPB_FWT_RW_VP_PP_SF_GR, 
                                          gls_PDQ_noRCC_TPB_FWT_RW_VP_PP_SF_GR)

#specify model names
models_PDQ_no_TPB_FWT_RW_VP_PP_SF_GR_names <- c('VA',
                                             'IPA','MTDVL','GTF',
                                             'ParenPa',
                                             'RCC')

#calculate AIC of each model
AIC_PDQ_no_TPB_FWT_RW_VP_PP_SF_GR <- aictab(cand.set = models_PDQ_no_TPB_FWT_RW_VP_PP_SF_GR,  modnames = models_PDQ_no_TPB_FWT_RW_VP_PP_SF_GR_names)
AIC_PDQ_no_TPB_FWT_RW_VP_PP_SF_GR #sacamos a GR
#----------------------------------------------------------------------------------------


write.xlsx(AIC_MAP_No, "C:/Users/PERSONAL/Downloads/Tesis/Matrices/Matriz8/AIC_MAP_NO.xlsx", 
                       password = NULL)
write.xlsx(AIC_MAT_No, "C:/Users/PERSONAL/Downloads/Tesis/Matrices/Matriz8/AIC_MAT_NO.xlsx", 
                       password = NULL)

write.xlsx(AIC_PDQ_No, "C:/Users/PERSONAL/Downloads/Tesis/Matrices/Matriz8/AIC_PDQ_NO.xlsx", 
           password = NULL)
#---------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
#Midiendo el lambda de un caracter
#con nlme
lambda_GR_gls <- gls(Growth_Rings ~ 1, correlation = corBrownian(phy = tree_gls),
                           data = traits_gls, method = "ML", na.action = na.roughfix)
summary(lambda_GR_gls)

#con caper #NOTA, no funcionó porque es un factor
tree_gls$node.label<-NULL
GR <- comparative.data(phy = tree_gls, data = traits_gls,
                            names.col = sp, vcv = TRUE,
                            na.omit = FALSE, warn.dropped = TRUE)

lambda_GR_pgls <- pgls(Growth_Rings ~ 1, data = GR, lambda = "ML")

#Esto no funcionó, porque sólo es para caracteres binarios
brunchMod <- brunch(mean_MAP_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament+Perforation_plates+
                      Intervessel_pits_arregement+Mean_tangential_diameter_of_vessel_lumina+
                      Ground_tissue_fibres+Septa_in_fibers+Fibre_wall_thickness+Parenchyma_Paratracheal+
                      Type_of_parenchyma_banding+Ray_width+Rays..cellular_composition, data=GR)
brunchMod_result <- caic.table(brunchMod)
brunchMod_result

brunchMod_result1 <- caic.diagnostics(brunchMod, outlier=2)

#----------------------------------------------------------------------------------------
#PGLS poster

pgls_MAP01 <- gls(mean_MAP_gls ~ Growth_Rings+Vessel_porosity, correlation = corBrownian(phy = tree_gls),
                        data = traits_exa, method = "ML")
pgls_MAP02 <- gls(mean_MAP_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_MAP03 <- gls(mean_MAP_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament+
                    Type_of_parenchyma_banding, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_MAP04 <- gls(mean_MAP_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament+
                    Type_of_parenchyma_banding+Septa_in_fibers, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_MAP05 <- gls(mean_MAP_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament+
                    Type_of_parenchyma_banding+Septa_in_fibers+Mean_tangential_diameter_of_vessel_lumina, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_MAP06 <- gls(mean_MAP_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament+
                    Type_of_parenchyma_banding+Septa_in_fibers+Mean_tangential_diameter_of_vessel_lumina
                 +Parenchyma_Paratracheal, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")

models_MAP <- list(pgls_MAP01, pgls_MAP02 ,pgls_MAP03,pgls_MAP04,
                      pgls_MAP05, pgls_MAP06)
                     

#specify model names
models_MAP_names <- c('1','2', '3', '4','5','6')


#calculate AIC of each model
AIC_MAP <- aictab(cand.set = models_MAP, modnames = models_MAP_names)
AIC_MAP

#------------------------------------------------------------------------------------
pgls_MAT01 <- gls(mean_MAT_gls ~ Growth_Rings+Vessel_porosity, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_MAT02 <- gls(mean_MAT_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_MAT03 <- gls(mean_MAT_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament+
                    Type_of_parenchyma_banding, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_MAT04 <- gls(mean_MAT_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament+
                    Type_of_parenchyma_banding+Septa_in_fibers, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_MAT05 <- gls(mean_MAT_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament+
                    Type_of_parenchyma_banding+Septa_in_fibers+Mean_tangential_diameter_of_vessel_lumina, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_MAT06 <- gls(mean_MAT_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament+
                    Type_of_parenchyma_banding+Septa_in_fibers+Mean_tangential_diameter_of_vessel_lumina
                  +Parenchyma_Paratracheal, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")

models_MAT <- list(pgls_MAT01, pgls_MAT02 ,pgls_MAT03,pgls_MAT04,
                   pgls_MAT05, pgls_MAT06)


#specify model names
models_MAT_names <- c('1','2', '3', '4','5','6')


#calculate AIC of each model
AIC_MAT <- aictab(cand.set = models_MAT, modnames = models_MAT_names)
AIC_MAT
#-----------------------------------------------------------------------------------------
pgls_PDQ01 <- gls(mean_PDQ_gls ~ Growth_Rings+Vessel_porosity, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_PDQ02 <- gls(mean_PDQ_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_PDQ03 <- gls(mean_PDQ_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament+
                    Type_of_parenchyma_banding, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_PDQ04 <- gls(mean_PDQ_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament+
                    Type_of_parenchyma_banding+Septa_in_fibers, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_PDQ05 <- gls(mean_PDQ_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament+
                    Type_of_parenchyma_banding+Septa_in_fibers+Mean_tangential_diameter_of_vessel_lumina, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")
pgls_PDQ06 <- gls(mean_PDQ_gls ~ Growth_Rings+Vessel_porosity+Vessel_arangament+
                    Type_of_parenchyma_banding+Septa_in_fibers+Mean_tangential_diameter_of_vessel_lumina
                  +Parenchyma_Paratracheal, correlation = corBrownian(phy = tree_gls),
                  data = traits_exa, method = "ML")

models_PDQ <- list(pgls_PDQ01, pgls_PDQ02 ,pgls_PDQ03,pgls_PDQ04,
                   pgls_PDQ05, pgls_PDQ06)


#specify model names
models_PDQ_names <- c('1','2', '3', '4','5','6')


#calculate AIC of each model
AIC_PDQ <- aictab(cand.set = models_PDQ, modnames = models_PDQ_names)
AIC_PDQ

write.xlsx(AIC_MAP, "C:/Users/PERSONAL/Downloads/Tesis/Matrices/Matriz8/AIC_MAP.xlsx", 
           password = NULL)
write.xlsx(AIC_MAT, "C:/Users/PERSONAL/Downloads/Tesis/Matrices/Matriz8/AIC_MAT.xlsx", 
           password = NULL)

write.xlsx(AIC_PDQ, "C:/Users/PERSONAL/Downloads/Tesis/Matrices/Matriz8/AIC_PDQ.xlsx", 
           password = NULL)
