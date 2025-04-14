library(dplyr)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggfortify)



#load("PCA_data.RData")
data <- read.csv("PHY_WQ_dataset_PCA.csv")
#Update species name
data$Genus.Name <- gsub("Aphanizomenon issatschenkoi", "Cuspidothrix issatschenkoi", data$Genus.Name)
data$Genus.Name <- gsub("Nitzschia closterium", "Cylindrotheca closterium", data$Genus.Name)



df <- data
df <- na.omit(df)

#Calculate TN TP molecular ratio
df$TNTP <- (df$TN/14)/(df$TP/31)

#Calculate mean of environmental condition for each species
d_mean <- data.frame(aggregate(cbind(TN, TP, SAL, TEMP, AMM, NIT,FRP,OP,ON, SIL, TURB,BIOVOL, TNTP)~ Genus.Name, data = df, function(x) c(mean = mean(x), na.rm = TRUE))) 

#Creat dataframe for species' habitat 
df_pca <- data.frame(
  Spp = d_mean$Genus.Name,
  TN  = d_mean$TN[,"mean"],
  TP  = d_mean$TP[,"mean"],
  SAL  = d_mean$SAL[,"mean"],
  TEMP  = d_mean$TEMP[,"mean"],
  AMM  = d_mean$AMM[,"mean"],
  NIT  = d_mean$NIT[,"mean"],
  FRP  = d_mean$FRP[,"mean"],
  OP  = d_mean$OP[,"mean"],
  ON  = d_mean$ON[,"mean"],
  SIL  = d_mean$SIL[,"mean"],
  TURB  = d_mean$TURB[,"mean"],
  TNTP  = d_mean$TNTP[,"mean"]
  
)
# remove OP and ON
pca <- df_pca[,2:13] %>% select(-OP,- ON)

#Start PCA
res.pca <- prcomp(pca, scale. = TRUE)


# 
# res.pca
# Standard deviations (1, .., p=10):
#   [1] 2.03143575 1.50714528 0.98502483 0.92964754 0.82754249 0.75032196 0.50808522 0.46646724 0.19906682 0.06390578
# 
# Rotation (n x k) = (10 x 10):
#   PC1         PC2          PC3          PC4         PC5         PC6        PC7          PC8          PC9         PC10
# TN    0.44016056 -0.23916256 -0.122174883  0.007899597  0.10470674 -0.17097777 -0.2569492  0.179132387 -0.396697956 -0.662112801
# TP   -0.08698591 -0.60344591 -0.036469080 -0.018184377 -0.14440971 -0.13789432  0.1960226  0.688049178  0.226362591  0.153883909
# SAL  -0.08454718  0.20671735 -0.886082477 -0.072837965 -0.39003306 -0.05993929  0.0526737  0.022896203 -0.018669424 -0.017503732
# TEMP -0.27970616 -0.03765258 -0.008057966 -0.752290409  0.23404444 -0.48223910 -0.2404048 -0.079639116 -0.000232945  0.053597577
# AMM   0.39962617 -0.12464854 -0.070951663 -0.362643706  0.15975044  0.07293363  0.7762799 -0.223841942 -0.068103794  0.004872895
# NIT   0.47352572 -0.10183750 -0.125125796  0.028310949  0.07298148 -0.04357640 -0.3017103  0.009628869 -0.349552607  0.726918512
# FRP   0.02090603 -0.52600357  0.100077696 -0.210372570 -0.56965641  0.26188442 -0.2221121 -0.469911354  0.054843951 -0.048588476
# SIL  -0.22991101 -0.37403292 -0.155456260  0.498665567  0.18709983 -0.51952134  0.1522802 -0.447847576 -0.064825648  0.039223133
# TURB -0.23442854 -0.30228433 -0.356607773 -0.033250002  0.59112190  0.58852040 -0.1598555 -0.038889351  0.047591277 -0.016742874
# TNTP  0.47024230  0.02530211 -0.128430202  0.044935518  0.14660619 -0.15344099 -0.2080069 -0.120577147  0.809161172 -0.046188904

# Functions to make circle
gg_circle <- function(r, xc, yc, color="black", fill=NA, ...) {
  x <- xc + r*cos(seq(0, pi, length.out=100))
  ymax <- yc + r*sin(seq(0, pi, length.out=100))
  ymin <- yc + r*sin(seq(0, -pi, length.out=100))
  annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...)
}


#Plot PCA

PCA_quater <- autoplot(res.pca, data = df_pca, colour = "blue", loadings = TRUE,  loadings.colour = 'black',
                       loadings.label.size = 5,loadings.label.colour = "black",
                       loadings.label = TRUE, ylim =  c(-0.4,0.2), xlim = c(-0.3, 0.4))+ 
  theme_minimal() +
  theme(legend.position = c(0.83, 0.73),
        legend.background = element_rect(fill = "white", color = "black"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 11),
        legend.key = element_rect(fill = "white", color = NA),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text( color="black", 
                                    size=12, angle=0),
        axis.text.y = element_text( color="black", 
                                    size=12, angle=0),
        
        plot.margin = unit(c(1,1,1,1), "cm"))+
  gg_circle(r=0.1, xc=0.23, yc=-0.06, color="blue", fill="blue", alpha=0.1)+
  gg_circle(r=0.09, xc=-0.01, yc= -0.26, color="red", fill="red", alpha=0.1)+
  gg_circle(r=0.12, xc=-0.15, yc= -0.1, color="green", fill="green", alpha=0.1)


PCA_quater

# tiff("PCA_revision.tiff", width = 120, height = 110, units = "mm", res = 2000)
# PCA_quater
# dev.off()

#ggsave("PCA.jpg", PCA_quater, width = 4.5, height = 4, dpi = 600)
