library(tidyverse)
library(ggplot2)
library(dplyr)
library(psych)
library(devtools)
library(ggbiplot)
install.packages("reshape")
install.packages("dplyr") 
install.packages("tidyverse")
install.packages("psych")
install.packages("devtools")
library("devtools")
install_github("vqv/ggbiplot")


mbl <- read.csv("./Mlbb_Heroes+ReyerosMarcos.csv", header = TRUE, sep = ",")

rownames(mbl) <- mbl$Name

borrar <- c("Mana_Regen","Title", "Voice_Line","Release_Date", "Primary_Role","Secondary_Role","Lane", "Mag_Damage", "Name")

mbl2 <- mbl[ , !names(mbl) %in% borrar]


pca_mbl <- prcomp(mbl2, center = TRUE, scale = TRUE)

names(pca_mbl)

summary(pca_mbl)

##pca_data <- data.frame(
##  Modelo = rownames(pca_mbl$x),
##  x = pca_mbl$x[,1],
##  y = pca_mbl$x[,2]
##)

##ggplot(data = pca_data, aes(x,y, label = Modelo)) +
##  geom_text() +
##  xlab(paste0("CP1:", pca_var_pct[1],"%")) +
##  ylab(paste0("CP2:", pca_var_pct[2],"%")) +
##  theme_bw()+
##  ggtitle("GRAFICO PCA")

pca_final <- principal(mbl2, nfactors = 2 , rotate= "none")

pca_final

Nombres <- mbl$Name

ggbiplot(pca_mbl, elipse = TRUE, labels = rownames(mbl2), groups = mbl$Primary_Role)


