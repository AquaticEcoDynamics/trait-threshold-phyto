library(clustMixType)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(cowplot)
library(gridExtra)
library(ggpubr)
library(patchwork)
library(tibble)

data_all <- read.csv("Classification/K_prototype_classification_input_all_trait.csv") 

data <- read.csv("K_prototype_input.csv") %>% select(-TN10, -TN90)

data$NIT10 <- data_all$NIT10
data$NIT90 <- data_all$NIT90

df <- data.frame(data, row.names = 1)
df$Organism <- as.factor(df$Organism)
df$Movement <- as.factor(df$Movement)
df$Trophic <- as.factor(df$Trophic)
df$N_fixation <- as.factor(df$N_fixation)
df$Silica <- as.factor(df$Silica)
set.seed(7)

cluster <- kproto(df, k = 5, lambda = NULL, nstart = 25)
# cluster$withinss
cluster$size
summary(cluster)

df$cluster <- cluster$cluster
df$Clusters <- ifelse(df$cluster == 3, "A", 
                      ifelse(df$cluster == 5, "D",
                             ifelse(df$cluster == 2, "E",
                                    ifelse(df$cluster == 4, "B","C"))))
df$cluster <- as.factor(df$cluster)
df$Clusters <- as.factor(df$Clusters)


trait_check <- read.csv("Trait_check.csv")
df$Group <- trait_check$Group
write.csv(df,"K_prototype_clusters_output_revised_test_B.csv")
save.image(file = "TestB.RData")


##TEST C


data_all <- read.csv("K_prototype_classification_input_all_trait.csv") 

data <- read.csv("K_prototype_input.csv") 

data$NIT10 <- data_all$NIT10
data$NIT90 <- data_all$NIT90
data$AMM10 <- data_all$AMM10
data$AMM90 <- data_all$AMM90
data$SIL10 <- data_all$SIL10
data$SIL90 <- data_all$SIL90


df <- data.frame(data, row.names = 1)
df$Organism <- as.factor(df$Organism)
df$Movement <- as.factor(df$Movement)
df$Trophic <- as.factor(df$Trophic)
df$N_fixation <- as.factor(df$N_fixation)
df$Silica <- as.factor(df$Silica)
set.seed(7)

cluster <- kproto(df, k = 5, lambda = NULL, nstart = 25)
# cluster$withinss
cluster$size
summary(cluster)

df$cluster <- cluster$cluster
df$Clusters <- ifelse(df$cluster == 3, "B", 
                      ifelse(df$cluster == 5, "B",
                             ifelse(df$cluster == 2, "C",
                                    ifelse(df$cluster == 4, "D","A"))))
df$cluster <- as.factor(df$cluster)
df$Clusters <- as.factor(df$Clusters)


trait_check <- read.csv("Trait_check.csv")
df$Group <- trait_check$Group
write.csv(df,"K_prototype_clusters_output_revised_test_C.csv")
save.image(file = "TestC.RData")


##TEST D



data_all <- read.csv("K_prototype_classification_input_all_trait.csv") 
str(data_all)
data <- read.csv("K_prototype_input.csv") 



data$NIT10 <- data_all$NIT10
data$NIT90 <- data_all$NIT90
data$AMM10 <- data_all$AMM10
data$AMM90 <- data_all$AMM90
data$SIL10 <- data_all$SIL10
data$SIL90 <- data_all$SIL90
data$FRP10 <- data_all$FRP10
data$FRP90 <- data_all$FRP90

new_data <- read.csv("Threshold_all_trait.csv")

data$SAL10 <- new_data$SAL10
data$SAL90 <- new_data$SAL90

# data$TURB10 <- new_data$TURB10
# data$TURB90 <- new_data$TURB90

df <- data.frame(data, row.names = 1)
df$Organism <- as.factor(df$Organism)
df$Movement <- as.factor(df$Movement)
df$Trophic <- as.factor(df$Trophic)
df$N_fixation <- as.factor(df$N_fixation)
df$Silica <- as.factor(df$Silica)
set.seed(7)

cluster <- kproto(df, k = 5, lambda = NULL, nstart = 25)
# cluster$withinss
cluster$size
summary(cluster)

df$cluster <- cluster$cluster
df$Clusters <- ifelse(df$cluster == 3, "E", 
                      ifelse(df$cluster == 5, "B",
                             ifelse(df$cluster == 2, "C",
                                    ifelse(df$cluster == 4, "D","A"))))
df$cluster <- as.factor(df$cluster)
df$Clusters <- as.factor(df$Clusters)


trait_check <- read.csv("Trait_check.csv")
df$Group <- trait_check$Group
write.csv(df,"K_prototype_clusters_output_revised_test_D.csv")
save.image(file = "TestD.RData")
