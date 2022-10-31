#Ensayo medir señal filogenética

#asiganamos path donde tenemos los archivos
setwd("C:/Users/PERSONAL/Downloads/Tesis/Ensayo señal filogenética")
install.packages("remotes")#for install remotes packager

install.packages("picante")
install.packages("ape")
install.packages("adephylo")
install.packages("ade4")
install.packages("phylobase")
install.packages("geiger")
install.packages("phytools")
install.packages("RRphylo") #Contains polytomies fixer
install.packages("caper") #pgls plot
install.packages("phyloint") # to measure phylosignal in categorical data
source(code.R)# for delta signal

library(picante)
library(ape)
library(adephylo)
library(ade4)
library(phylobase)
library(geiger)
library(phytools)
library(RRphylo)
library(caper)
library(phyloint)

install.packages(https://github.com/stoufferlab/phyloint/tree/master/R)
#----------------------------------------------------------------------------------------#
#Ejemplo con mis datos
traits_exa<-read.csv2("Matriz_8_22-06-2022.csv")

#elimino columnas que no necesito
traits_exa$gen <- NULL
traits_exa$fam <- NULL

#cargo mmi filogenia
tree_exa <- read.tree('Figure.1c.phy')

#hago coincidir mis datos con el orden de las tips de la filogenia
traits_exa <-traits_exa[match(tree_exa$tip.label, traits_exa$sp),]

#para enraizar la filogenia
tree_exa <-multi2di(tree_exa)

#Reparo las politomías del árbol
tree_exa <- fix.poly(tree_exa,type="resolve")


#Transformación de columnas de chr a int
  traits_exa$Growth_Rings = as.integer(traits_exa$Growth_Rings)
  traits_exa$Vessel_porosity = as.integer(traits_exa$Vessel_porosity)
  traits_exa$Vessel_arangement = as.integer(traits_exa$Vessel_arangement)
  traits_exa$Perforation_plates = as.integer(traits_exa$Perforation_plates)
  traits_exa$Intervessel_pits_arregement = as.integer(traits_exa$Intervessel_pits_arregement)
  traits_exa$Mean_tangential_diameter_of_vessel_lumina = as.integer(traits_exa$Mean_tangential_diameter_of_vessel_lumina)
  traits_exa$Ground_tissue_fibres = as.integer(traits_exa$Ground_tissue_fibres)
  traits_exa$Septa_in_fibers = as.integer(traits_exa$Septa_in_fibers)
  traits_exa$Fibre_wall_thickness = as.integer(traits_exa$Fibre_wall_thickness)
  traits_exa$Parenchyma_Paratracheal = as.integer(traits_exa$Parenchyma_Paratracheal)
  traits_exa$Type_of_parenchyma_banding = as.integer(traits_exa$Type_of_parenchyma_banding)
  traits_exa$Ray_width = as.integer(traits_exa$Ray_width)
  traits_exa$Rays..cellular_composition = as.integer(traits_exa$Rays..cellular_composition)

#Transformación de columnas de chr a factor
traits_exa$Growth_Rings = as.factor(traits_exa$Growth_Rings)
traits_exa$Vessel_porosity = as.factor(traits_exa$Vessel_porosity)
traits_exa$Vessel_arangement = as.factor(traits_exa$Vessel_arangement)
traits_exa$Perforation_plates = as.factor(traits_exa$Perforation_plates)
traits_exa$Intervessel_pits_arregement = as.factor(traits_exa$Intervessel_pits_arregement)
traits_exa$Mean_tangential_diameter_of_vessel_lumina = as.factor(traits_exa$Mean_tangential_diameter_of_vessel_lumina)
traits_exa$Ground_tissue_fibres = as.factor(traits_exa$Ground_tissue_fibres)
traits_exa$Septa_in_fibers = as.factor(traits_exa$Septa_in_fibers)
traits_exa$Fibre_wall_thickness = as.factor(traits_exa$Fibre_wall_thickness)
traits_exa$Parenchyma_Paratracheal = as.factor(traits_exa$Parenchyma_Paratracheal)
traits_exa$Type_of_parenchyma_banding = as.factor(traits_exa$Type_of_parenchyma_banding)
traits_exa$Ray_width = as.factor(traits_exa$Ray_width)
traits_exa$Rays..cellular_composition = as.factor(traits_exa$Rays..cellular_composition)

#reemplazar na
traits_exa[is.na(traits_exa)] <- 0

#Creo vectores con cada caracter por aparte
Growth_Rings <- traits_exa[,2]
Vessel_porosity <- traits_exa[,3]
Vessel_arangament <- traits_exa[,4]
Perforation_plates <- traits_exa[,5]
Intervessel_pits_arregement <- traits_exa[,6]
Mean_tangential_diameter_of_vessel_lumina <- traits_exa[,7]
Ground_tissue_fibres <- traits_exa[,8]
Septa_in_fibers <- traits_exa[,9]
Fibre_wall_thickness <- traits_exa[,10]
Parenchyma_Paratracheal <- traits_exa[,11]
Type_of_parenchyma_banding <- traits_exa[,12]
Ray_width <- traits_exa[,13] 
Rays..cellular_composition <- traits_exa[,14]


#----------------------------------------------------------------------------------------

#Midiendo fitDiscrete

#Saco name species en una columna
sp <- traits_exa$sp 
#Asigno los nombres de la lista de especies a los datos de Growth rings
names(Growth_Rings) <- sp
names(Vessel_porosity) <- sp
names(Vessel_arangament) <- sp
names(Perforation_plates) <- sp
names(Intervessel_pits_arregement) <- sp
names(Mean_tangential_diameter_of_vessel_lumina) <- sp
names(Ground_tissue_fibres) <- sp
names(Septa_in_fibers) <- sp
names(Fibre_wall_thickness) <- sp
names(Parenchyma_Paratracheal) <- sp
names(Type_of_parenchyma_banding) <- sp
names(Ray_width) <- sp
names(Rays..cellular_composition) <- sp

#Mido la señal 
fit_sig_exa_GR <- fitDiscrete(tree_exa,Growth_Rings, model="ER", transform="lambda" )
fit_sig_exa_VP <- fitDiscrete(tree_exa,Vessel_porosity, model="ER", transform="lambda" )
fit_sig_exa_VA <- fitDiscrete(tree_exa,Vessel_arangament, model="ER", transform="lambda" )
fit_sig_exa_PP <- fitDiscrete(tree_exa,Perforation_plates, model="ER", transform="lambda" )
fit_sig_exa_IPA <- fitDiscrete(tree_exa,Intervessel_pits_arregement, model="ER", transform="lambda" )
fit_sig_exa_MTDVL <- fitDiscrete(tree_exa,Mean_tangential_diameter_of_vessel_lumina, model="ER", transform="lambda" )
fit_sig_exa_GTF <- fitDiscrete(tree_exa,Ground_tissue_fibres, model="ER", transform="lambda" )
fit_sig_exa_SF <- fitDiscrete(tree_exa,Septa_in_fibers, model="ER", transform="lambda" )
fit_sig_exa_FWT <- fitDiscrete(tree_exa,Fibre_wall_thickness, model="ER", transform="lambda" )
fit_sig_exa_ParenPa <- fitDiscrete(tree_exa,Parenchyma_Paratracheal, model="ER", transform="lambda" )
fit_sig_exa_TPB <- fitDiscrete(tree_exa,Type_of_parenchyma_banding, model="ER", transform="lambda" )
fit_sig_exa_RW <- fitDiscrete(tree_exa,Ray_width, model="ER", transform="lambda" )
fit_sig_exa_RCC <- fitDiscrete(tree_exa,Rays..cellular_composition, model="ER", transform="lambda" )

#creamos vectores con cada uno de los valores que me interesan
trait_k <- c(fit_sig_exa_GR$opt$k,fit_sig_exa_VP$opt$k, fit_sig_exa_VA$opt$k, 
             fit_sig_exa_PP$opt$k,fit_sig_exa_IPA$opt$k,fit_sig_exa_MTDVL$opt$k,
             fit_sig_exa_GTF$opt$k,fit_sig_exa_SF$opt$k, fit_sig_exa_FWT$opt$k,
             fit_sig_exa_ParenPa$opt$k,fit_sig_exa_TPB$opt$k, fit_sig_exa_RW$opt$k,
             fit_sig_exa_RCC$opt$k)

trait_lambda <- c(fit_sig_exa_GR$opt$lambda,fit_sig_exa_VP$opt$lambda, fit_sig_exa_VA$opt$lambda, 
                  fit_sig_exa_PP$opt$lambda,fit_sig_exa_IPA$opt$lambda,fit_sig_exa_MTDVL$opt$lambda,
                  fit_sig_exa_GTF$opt$lambda,fit_sig_exa_SF$opt$lambda, fit_sig_exa_FWT$opt$lambda,
                  fit_sig_exa_ParenPa$opt$lambda,fit_sig_exa_TPB$opt$lambda, fit_sig_exa_RW$opt$lambda,
                  fit_sig_exa_RCC$opt$lambda)

trait_lnL <- c(fit_sig_exa_GR$opt$lnL,fit_sig_exa_VP$opt$lnL, fit_sig_exa_VA$opt$lnL, 
               fit_sig_exa_PP$opt$lnL,fit_sig_exa_IPA$opt$lnL,fit_sig_exa_MTDVL$opt$lnL,
               fit_sig_exa_GTF$opt$lnL,fit_sig_exa_SF$opt$lnL, fit_sig_exa_FWT$opt$lnL,
               fit_sig_exa_ParenPa$opt$lnL,fit_sig_exa_TPB$opt$lnL, fit_sig_exa_RW$opt$lnL,
               fit_sig_exa_RCC$opt$lnL)

trait_aic <- c(fit_sig_exa_GR$opt$aic,fit_sig_exa_VP$opt$aic, fit_sig_exa_VA$opt$aic, 
               fit_sig_exa_PP$opt$aic,fit_sig_exa_IPA$opt$aic,fit_sig_exa_MTDVL$opt$aic,
               fit_sig_exa_GTF$opt$aic,fit_sig_exa_SF$opt$aic, fit_sig_exa_FWT$opt$aic,
               fit_sig_exa_ParenPa$opt$aic,fit_sig_exa_TPB$opt$aic, fit_sig_exa_RW$opt$aic,
               fit_sig_exa_RCC$opt$aic)

trait_aicc <- c(fit_sig_exa_GR$opt$aicc,fit_sig_exa_VP$opt$aicc, fit_sig_exa_VA$opt$aicc, 
                fit_sig_exa_PP$opt$aicc,fit_sig_exa_IPA$opt$aicc,fit_sig_exa_MTDVL$opt$aicc,
                fit_sig_exa_GTF$opt$aicc,fit_sig_exa_SF$opt$aicc, fit_sig_exa_FWT$opt$aicc,
                fit_sig_exa_ParenPa$opt$aicc,fit_sig_exa_TPB$opt$aicc, fit_sig_exa_RW$opt$aicc,
                fit_sig_exa_RCC$opt$aicc)

#creamos un dataframe
result_fit_trait <- cbind(trait_name_A,trait_lambda, trait_lnL, trait_aic, trait_aicc)
result_fit_trait <- as.data.frame(result_fit_trait)
result_fit_trait$trait_lambda <- as.numeric(result_fit_trait$trait_lambda)
result_fit_trait$trait_lnL <- as.numeric(result_fit_trait$trait_lnL)
result_fit_trait$trait_aic <- as.numeric(result_fit_trait$trait_aic)
result_fit_trait$trait_aicc <- as.numeric(result_fit_trait$trait_aicc)

#ordenamos el dataframe con respecto a la señal filogenética
result_fit_trait <- result_fit_trait[order(result_fit_trait$trait_lambda),]
result_fit_trait

write.xlsx(result_fit_trait, "C:/Users/PERSONAL/Downloads/Tesis/Matrices/Matriz8/phy_signal.xlsx", 
           password = NULL)

#ESTO ESTÁ PENDIENTE

#Para verificar el valor de lambda,

tree_exa_L0<-lambdaTree(tree_exa, lambda = 0)
plot(tree_exa_L0)
#L0
fit_sig_exa_GR_L0 <- fitDiscrete(tree_exa_L0,Growth_Rings, model="ER")

#L1
fit_sig_exa_GR_L1 <- fitDiscrete(tree_exa,Growth_Rings, model="ER")

#Calcular log likelihood ratio  L0
GR_LLR0 <- -2*(fit_sig_exa_GR_L0$opt$lnl - fit_sig_exa_GR$opt$lnl)

#Calcular log likelihood ratio  L1
GR_LLR1 <- -2*(fit_sig_exa_GR_L1$opt$lnl - fit_sig_exa_GR$opt$lnl)

#Calcular chi cuadrado L0
chi_GR_0 <- pchisq(GR_LLR0, df=1,lower.tail = FALSE)

#Calcular chi cuadrado L1
chi_GR_1 <- pchisq(GR_LLR1, df=1,lower.tail = FALSE)

lambda_GL_L0<-fitContinuous(phy = primatetree_L0, data = log(primatedata$GestationLen_d),
                            data.names = primatedata$Binomial, model = "BM")

#-----------------------------------------------------------------------------------------
#Phylogenetic signal- delta
library(ape)
source("Code.R")
