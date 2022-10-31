#hacer knn
library(tidyverse)
install.packages("wesanderson") #paleta colores
library(wesanderson)
install.packages("vioplot")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
library(grid)

#construir dataframe con mis variables climáticas y cada uno de mis carcateres

data_GR <- data.frame(mean_MAP_gls,mean_MAT_gls,mean_PDQ_gls,Growth_Rings)
data_VP <- data.frame(mean_MAP_gls,mean_MAT_gls,mean_PDQ_gls,Vessel_porosity)
data_VA <- data.frame(mean_MAP_gls,mean_MAT_gls,mean_PDQ_gls,Vessel_arangament)
data_PP <- data.frame(mean_MAP_gls,mean_MAT_gls,mean_PDQ_gls,Perforation_plates)
data_IPA <- data.frame(mean_MAP_gls,mean_MAT_gls,mean_PDQ_gls,Intervessel_pits_arregement)
data_MTDVL <- data.frame(mean_MAP_gls,mean_MAT_gls,mean_PDQ_gls,Mean_tangential_diameter_of_vessel_lumina)
data_GTF <- data.frame(mean_MAP_gls,mean_MAT_gls,mean_PDQ_gls,Ground_tissue_fibres)
data_SF <- data.frame(mean_MAP_gls,mean_MAT_gls,mean_PDQ_gls,Septa_in_fibers)
data_FWT <- data.frame(mean_MAP_gls,mean_MAT_gls,mean_PDQ_gls,Fibre_wall_thickness)
data_ParenPa <- data.frame(mean_MAP_gls,mean_MAT_gls,mean_PDQ_gls,Parenchyma_Paratracheal)
data_TPB <- data.frame(mean_MAP_gls,mean_MAT_gls,mean_PDQ_gls,Type_of_parenchyma_banding)
data_RW <- data.frame(mean_MAP_gls,mean_MAT_gls,mean_PDQ_gls,Ray_width)
data_RCC <- data.frame(mean_MAP_gls,mean_MAT_gls,mean_PDQ_gls,Rays_cellular_composition)

#indicamos que lea nuestro caracter como factores
data_GR$Growth_Rings <- as.factor(data_GR$Growth_Rings)
data_VP$Vessel_porosity <- as.factor(data_VP$Vessel_porosity)
data_VA$Vessel_arangament <- as.factor(data_VA$Vessel_arangament)
data_PP$Perforation_plates <- as.factor(data_PP$Perforation_plates)
data_IPA$Intervessel_pits_arregement <- as.factor(data_IPA$Intervessel_pits_arregement)
data_MTDVL$Mean_tangential_diameter_of_vessel_lumina <- as.factor(data_MTDVL$Mean_tangential_diameter_of_vessel_lumina)
data_GTF$Ground_tissue_fibres <- as.factor(data_GTF$Ground_tissue_fibres)
data_SF$Septa_in_fibers <- as.factor(data_SF$Septa_in_fibers)
data_FWT$Fibre_wall_thickness <- as.factor(data_FWT$Fibre_wall_thickness)
data_ParenPa$Parenchyma_Paratracheal <- as.factor(data_ParenPa$Parenchyma_Paratracheal)
data_TPB$Type_of_parenchyma_banding <- as.factor(data_TPB$Type_of_parenchyma_banding)
data_RW$Ray_width <- as.factor(data_RW$Ray_width)
data_RCC$Rays_cellular_composition <- as.factor(data_RCC$Rays_cellular_composition)

#gráficos violin
#definimos el tamaño de los titulos de eje
My_Theme = theme(
  axis.title.x = element_text(size = 8),
  axis.title.y = element_text(size = 8))
ensayo <- ggplot(data_GR, aes(x=Growth_Rings, y=mean_MAP_gls, fill= Growth_Rings)) +
  labs(x=" ", y = "MAP (mm)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(varwidth=TRUE, fill="white", color ="#838B8B", alpha= 0.3) + scale_fill_brewer(palette="PuBu") + geom_jitter(position = position_jitter(width = .01)) +
  theme(legend.position = "none") 

#plots de violín para Growth Rings

plot_vio_MAP_GR <- ggplot(data_GR, aes(x=Growth_Rings, y=mean_MAP_gls, fill= Growth_Rings)) +
  labs(x=" ", y = "MAP (mm)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_MAT_GR <- ggplot(data_GR, aes(x=Growth_Rings, y=mean_MAT_gls, fill= Growth_Rings)) +
  labs(x=" ", y = "MAT (°C)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_PDQ_GR <- ggplot(data_GR, aes(x=Growth_Rings, y=mean_PDQ_gls, fill= Growth_Rings)) +
  labs(x="Anillos de Crecimiento", y = "PDQ (mm)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

#plots de violín para vessel porosity
plot_vio_MAP_VP <- ggplot(data_VP, aes(x=Vessel_porosity, y=mean_MAP_gls, fill= Vessel_porosity)) +
  labs(x=" ", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_MAT_VP <- ggplot(data_VP, aes(x=Vessel_porosity, y=mean_MAT_gls, fill= Vessel_porosity)) +
  labs(x=" ", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_PDQ_VP <- ggplot(data_VA, aes(x=Vessel_porosity, y=mean_PDQ_gls, fill= Vessel_porosity)) +
  labs(x="Porosidad de los Vasos", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

#plots de violín para vessel arrangament
plot_vio_MAP_VA <- ggplot(data_VA, aes(x=Vessel_arangament, y=mean_MAP_gls, fill= Vessel_arangament)) +
  labs(x=" ", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_MAT_VA <- ggplot(data_VA, aes(x=Vessel_arangament, y=mean_MAT_gls, fill= Vessel_arangament)) +
  labs(x=" ", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_PDQ_VA <- ggplot(data_VP, aes(x=Vessel_arangament, y=mean_PDQ_gls, fill= Vessel_arangament)) +
  labs(x="Arreglo de los Vasos", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

img_GR_VP_VA <-grid.arrange(plot_vio_MAP_GR, plot_vio_MAP_VP,plot_vio_MAP_VA, 
             plot_vio_MAT_GR,plot_vio_MAT_VP,plot_vio_MAT_VA,
             plot_vio_PDQ_GR,plot_vio_PDQ_VP, plot_vio_PDQ_VA, nrow = 3, ncol= 3, 
             heights = c(1,1,1), widths = c(0.6,0.8,1))
#-----------------------------------------------------------------------------------------
#plots de violín para Perforation plates
plot_vio_MAP_PP <- ggplot(data_PP, aes(x=Perforation_plates, y=mean_MAP_gls, fill= Perforation_plates)) +
  labs(x=" ", y = "MAP (mm)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_MAT_PP <- ggplot(data_PP, aes(x=Perforation_plates, y=mean_MAT_gls, fill= Perforation_plates)) +
  labs(x=" ", y = "MAT (°C)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_PDQ_PP <- ggplot(data_PP, aes(x=Perforation_plates, y=mean_PDQ_gls, fill= Perforation_plates)) +
  labs(x="Placas de Perforación", y = "PDQ (mm)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

#plots de violín para Intervessel pits arregement
plot_vio_MAP_IPA<- ggplot(data_IPA, aes(x=Intervessel_pits_arregement, y=mean_MAP_gls, fill= Intervessel_pits_arregement)) +
  labs(x=" ", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_MAT_IPA <- ggplot(data_IPA, aes(x=Intervessel_pits_arregement, y=mean_MAT_gls, fill= Intervessel_pits_arregement)) +
  labs(x=" ", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_PDQ_IPA <- ggplot(data_IPA, aes(x=Intervessel_pits_arregement, y=mean_PDQ_gls, fill= Intervessel_pits_arregement)) +
  labs(x="Arreglo de Punteaduras Intervasculares", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

#plots de violín para Mean_tangential_diameter_of_vessel_lumina
plot_vio_MAP_MTDVL <- ggplot(data_MTDVL, aes(x=Mean_tangential_diameter_of_vessel_lumina, y=mean_MAP_gls, fill= Mean_tangential_diameter_of_vessel_lumina)) +
  labs(x=" ", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_MAT_MTDVL <- ggplot(data_MTDVL, aes(x=Mean_tangential_diameter_of_vessel_lumina, y=mean_MAT_gls, fill= Mean_tangential_diameter_of_vessel_lumina)) +
  labs(x=" ", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_PDQ_MTDVL <- ggplot(data_MTDVL, aes(x=Mean_tangential_diameter_of_vessel_lumina, y=mean_PDQ_gls, fill= Mean_tangential_diameter_of_vessel_lumina)) +
  labs(x="Diámetro de los Vasos", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

img_PP_IPA_MTDVL <-grid.arrange(plot_vio_MAP_PP, plot_vio_MAP_IPA,plot_vio_MAP_MTDVL, 
                            plot_vio_MAT_PP,plot_vio_MAT_IPA,plot_vio_MAT_MTDVL,
                            plot_vio_PDQ_PP,plot_vio_PDQ_IPA, plot_vio_PDQ_MTDVL, nrow = 3, ncol= 3, 
                            heights = c(1,1,1), widths = c(0.8,0.8,1))
#----------------------------------------------------------------------------------------
#plots de violín para Ground_tissue_fibres

plot_vio_MAP_GTF <- ggplot(data_GTF , aes(x=Ground_tissue_fibres, y=mean_MAP_gls, fill= Ground_tissue_fibres)) +
  labs(x=" ", y = "MAP (mm)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_MAT_GTF  <- ggplot(data_GTF , aes(x=Ground_tissue_fibres, y=mean_MAT_gls, fill= Ground_tissue_fibres)) +
  labs(x=" ", y = "MAT (°C)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_PDQ_GTF  <- ggplot(data_GTF , aes(x=Ground_tissue_fibres, y=mean_PDQ_gls, fill= Ground_tissue_fibres)) +
  labs(x="Punteaduras en Fibras", y = "PDQ (mm)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

#plots de violín para Septa_in_fibers
plot_vio_MAP_SF <- ggplot(data_SF, aes(x=Septa_in_fibers, y=mean_MAP_gls, fill= Septa_in_fibers)) +
  labs(x=" ", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_MAT_SF <- ggplot(data_SF, aes(x=Septa_in_fibers, y=mean_MAT_gls, fill= Septa_in_fibers)) +
  labs(x=" ", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_PDQ_SF <- ggplot(data_SF, aes(x=Septa_in_fibers, y=mean_PDQ_gls, fill= Septa_in_fibers)) +
  labs(x="Septos en Fibras", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

#plots de violín para Fibre_wall_thickness
plot_vio_MAP_FWT <- ggplot(data_FWT, aes(x=Fibre_wall_thickness, y=mean_MAP_gls, fill= Fibre_wall_thickness)) +
  labs(x=" ", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_MAT_FWT <- ggplot(data_FWT, aes(x=Fibre_wall_thickness, y=mean_MAT_gls, fill= Fibre_wall_thickness)) +
  labs(x=" ", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_PDQ_FWT <- ggplot(data_FWT, aes(x=Fibre_wall_thickness, y=mean_PDQ_gls, fill= Fibre_wall_thickness)) +
  labs(x="Grosor de las Fibras", y = " ") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.08, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

img_GTF_SF_FWT <-grid.arrange(plot_vio_MAP_GTF, plot_vio_MAP_SF,plot_vio_MAP_FWT, 
                            plot_vio_MAT_GTF,plot_vio_MAT_SF,plot_vio_MAT_FWT,
                            plot_vio_PDQ_GTF,plot_vio_PDQ_SF, plot_vio_PDQ_FWT, nrow = 3, ncol= 3, 
                            heights = c(1,1,1), widths = c(0.6,0.8,1))
#-------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
plot_vio_MAP_VP <- ggplot(data_VP, aes(x=Vessel_porosity, y=mean_MAP_gls, fill=Vessel_porosity)) + 
  labs(x="Porosidad de los Vasos", y = "MAP (mm)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.05, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none")

plot_vio_MAP_VA <- ggplot(data_VP, aes(x=Vessel_arangament, y=mean_MAP_gls, fill=Vessel_arangament)) + 
  labs(x="Arreglo de los Vasos", y = "MAP (mm)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.05, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none") 

plot_vio_MAP_PP <- ggplot(data_PP, aes(x=Perforation_plates, y=mean_MAP_gls, fill=Perforation_plates)) + 
  labs(x="Placas de Perforación", y = "MAP (mm)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.05, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none")

plot_vio_MAP_IPA <- ggplot(data_IPA, aes(x=Intervessel_pits_arregement, y=mean_MAP_gls, fill=Intervessel_pits_arregement)) + 
  labs(x="Arreglo Punteaduras Intervasculares", y = "MAP (mm)") + My_Theme +geom_violin(color="gray") + 
  geom_boxplot(width=0.05, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none")

plot_vio_MAP_MTDVL <- ggplot(data_MTDVL, aes(x=Mean_tangential_diameter_of_vessel_lumina, y=mean_MAP_gls, fill=Mean_tangential_diameter_of_vessel_lumina)) + 
  labs(x="Diámetro de los Vasos", y = "MAP (mm)") + My_Theme + geom_violin(color="gray") + 
  geom_boxplot(width=0.05, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none")

prueba <- plot_vio_MAP_MTDVL <- ggplot(data_MTDVL, aes(x=Mean_tangential_diameter_of_vessel_lumina, y=mean_MAP_gls, fill=Mean_tangential_diameter_of_vessel_lumina)) + 
   labs(x="Diámetro de los Vasos", y = "MAP (mm)") + My_Theme +geom_violin(color="gray") + 
  geom_boxplot(width=0.05, fill="white", color ="#838B8B") + scale_fill_brewer(palette="PuBu") + 
  theme(legend.position = "none")



#correlaciones gráficos MAP
plot_cor_MAP_GR <- boxplot(mean_MAP_gls~Growth_Rings, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                      xlab="Growth Rings", ylab="MAP", ylim= c(0,3000))
plot_cor_MAP_VP <- boxplot(mean_MAP_gls~Vessel_porosity, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                      xlab="Vessel Porosity", ylab="MAP", ylim= c(0,3000))
plot_cor_MAP_VA <- boxplot(mean_MAP_gls~Vessel_arangament, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Vessel Arangement", ylab="MAP", ylim= c(0,3000))
plot_cor_MAP_PP <- boxplot(mean_MAP_gls~Perforation_plates, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Perforation Plates", ylab="MAP", ylim= c(0,3000))
plot_cor_MAP_IPA <- boxplot(mean_MAP_gls~Intervessel_pits_arregement, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Intervessel Pits Arangement", ylab="MAP", ylim= c(0,3000))
plot_cor_MAP_MTDVL <- boxplot(mean_MAP_gls~Mean_tangential_diameter_of_vessel_lumina, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                            xlab="Mean Tangential Diameter of Vessel", ylab="MAP", ylim= c(0,3000))
plot_cor_MAP_GTF <- boxplot(mean_MAP_gls~Ground_tissue_fibres, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                              xlab="Ground Tissue Fiber", ylab="MAP", ylim= c(0,3000))
plot_cor_MAP_SF <- boxplot(mean_MAP_gls~Septa_in_fibers, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                            xlab="Septa in Fiber", ylab="MAP", ylim= c(0,3000))
plot_cor_MAP_FWT <- boxplot(mean_MAP_gls~Fibre_wall_thickness, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Fibre Wall Thickness", ylab="MAP", ylim= c(0,3000))
plot_cor_MAP_ParenPa <- boxplot(mean_MAP_gls~Parenchyma_Paratracheal, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Parenchyma Paratracheal", ylab="MAP", ylim= c(0,3000))
plot_cor_MAP_TPB <- boxplot(mean_MAP_gls~Type_of_parenchyma_banding, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Type of Parenchyma Banding", ylab="MAP", ylim= c(0,3000))
plot_cor_MAP_RW <- boxplot(mean_MAP_gls~Ray_width, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Ray Width", ylab="MAP", ylim= c(0,3000))
plot_cor_MAP_RCC <- boxplot(mean_MAP_gls~Rays_cellular_composition, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Rays Cellular Composition", ylab="MAP", ylim= c(0,3000))

#Correlaciones gráficos MAT
plot_cor_MAT_GR <- boxplot(mean_MAT_gls~Growth_Rings, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Growth Rings", ylab="MAT", ylim= c(15,30))
plot_cor_MAT_VP <- boxplot(mean_MAT_gls~Vessel_porosity, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Vessel Porosity", ylab="MAT", ylim= c(15,30))
plot_cor_MAT_VA <- boxplot(mean_MAT_gls~Vessel_arangament, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Vessel Arangement", ylab="MAT", ylim= c(15,30))
plot_cor_MAT_PP <- boxplot(mean_MAT_gls~Perforation_plates, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Perforation Plates", ylab="MAT", ylim= c(15,30))
plot_cor_MAT_IPA <- boxplot(mean_MAT_gls~Intervessel_pits_arregement, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                            xlab="Intervessel Pits Arangement", ylab="MAT", ylim= c(15,30))
plot_cor_MAT_MTDVL <- boxplot(mean_MAT_gls~Mean_tangential_diameter_of_vessel_lumina, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                              xlab="Mean Tangential Diameter of Vessel", ylab="MAT", ylim= c(15,30))
plot_cor_MAT_GTF <- boxplot(mean_MAT_gls~Ground_tissue_fibres, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                            xlab="Ground Tissue Fiber", ylab="MAT", ylim= c(15,30))
plot_cor_MAT_SF <- boxplot(mean_MAT_gls~Septa_in_fibers, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Septa in Fiber", ylab="MAT", ylim= c(15,30))
plot_cor_MAT_FWT <- boxplot(mean_MAT_gls~Fibre_wall_thickness, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                            xlab="Fibre Wall Thickness", ylab="MAT", ylim= c(15,30))
plot_cor_MAT_ParenPa <- boxplot(mean_MAT_gls~Parenchyma_Paratracheal, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                                xlab="Parenchyma Paratracheal", ylab="MAT", ylim= c(15,30))
plot_cor_MAT_TPB <- boxplot(mean_MAT_gls~Type_of_parenchyma_banding, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                            xlab="Type of Parenchyma Banding", ylab="MAT", ylim= c(15,30))
plot_cor_MAT_RW <- boxplot(mean_MAT_gls~Ray_width, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                           xlab="Ray Width", ylab="MAT", ylim= c(15,30))
plot_cor_MAT_RCC <- boxplot(mean_MAT_gls~Rays_cellular_composition, data=traits_gls, varwidth=TRUE, col=palette("Dark 2"),
                            xlab="Rays Cellular Composition", ylab="MAT", ylim= c(15,30))





#correlaciones gráficos MAT
plot_cor_MAT_GR <- boxplot(mean_MAT_gls~Growth_Rings, data=traits_gls, width=proportion_GR, col=c("orange", "blue"))

dat <- data.frame(Growth_Rings,mean_MAP_gls)

ggplot(aes(x=dat_GR, y=dat_MAP)) + 
  geom_boxplot(width=proportion_GR,lwd=1.5, aes(color=dat_GR)) +
  geom_jitter(width=0.15,aes(color=dat_GR))+
  labs(subtitle="Correlation MAP and Growth Rings")

cor_MAP_VP <- boxplot(mean_MAP_gls~Vessel_porosity, data=traits_gls)
cor_MAP_VA <- boxplot(mean_MAP_gls~Vessel_arangament, data=traits_gls)
cor_MAP_PP <- boxplot(mean_MAP_gls~Perforation_plates, data=traits_gls)
cor_MAP_IPA <- boxplot(mean_MAP_gls~Intervessel_pits_arregement, data=traits_gls)
cor_MAP_MTDVL <- boxplot(mean_MAP_gls~Mean_tangential_diameter_of_vessel_lumina, data=traits_gls)
cor_MAP_GTF <- boxplot(mean_MAP_gls~Ground_tissue_fibres, data=traits_gls)
cor_MAP_SF <- boxplot(mean_MAP_gls~Septa_in_fibers, data=traits_gls)
cor_MAP_FWT <- boxplot(mean_MAP_gls~Fibre_wall_thickness, data=traits_gls)
cor_MAP_ParenPa <- boxplot(mean_MAP_gls~Parenchyma_Paratracheal, data=traits_gls)
cor_MAP_TPB <- boxplot(mean_MAP_gls~Type_of_parenchyma_banding, data=traits_gls)
cor_MAP_RW <- boxplot(mean_MAP_gls~Ray_width, data=traits_gls)
cor_MAP_RCC <- boxplot(mean_MAP_gls~Rays_cellular_composition, data=traits_gls)

result_kruskal <- data.frame(cor_MAP_GR, cor_MAP_VP,  )
install.packages("plotly")
library(plotly)

indep <- data.frame(mean_MAP_gls, mean_MAT_gls, mean_PDQ_gls)
indep_names <- list("Precipitacion Total", "Temperatura","Precipitacion en sequia")
dep <- data.frame(Growth_Rings, Vessel_porosity)
dep_names <- list("Anillos")

library(plotly)
par(mfrow=c(2,3))
for (i in 1:ncol(indep)) {
  for (j in 1:ncol(dep)) {
    newfig <- boxplot(indep[,i] ~ dep[,j])
  }
}

fig <- subplot(plots, nrows=3)

for (i in 1:3) {
  for(j in 1:1){
    boxplot(indep[i], dep[j])
  }
}
  
  

#correlaciones p-value 
kruskal.test(mean_MAP_gls, Growth_Rings)


