#Stage 4 - Classificaiton. 

#CLassification output can be seen here: load("Classification_output.RData")

#in this stage, we used output from the stage 3.


library(clustMixType)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(gridExtra)
library(ggpubr)
library(patchwork)
library(reshape2)

# classification_input <- read.csv("K_prototype_classification_input_change.csv")
#save(classification_input, file = "K_prototype_classification_input.RData")
#Read input data frame: classification_input
load("K_prototype_classification_input.RData") #or read file

classification_input <- read.csv("K_prototype_input.csv")

data <- classification_input
df <- data.frame(data, row.names = 1)
df$Organism <- as.factor(df$Organism)
df$Movement <- as.factor(df$Movement)
df$Trophic <- as.factor(df$Trophic)
df$N_fixation <- as.factor(df$N_fixation)
df$Silica <- as.factor(df$Silica)
set.seed(7)


# Elbow method to select the best number of clusters.

Es <- numeric(10)
for(i in 1:10){
  kpres <- kproto(df, k = i, nstart = 5)
  Es[i] <- kpres$tot.withinss
}
plot(1:10, Es, type = "b", ylab = "Objective Function", xlab = "Clusters", pch = 16)
# ggsave("k_number.png",  dpi=600)
# Based on this plot, k = 5 is the best number of cluster

#Now, run K-prototype with k = 5

cluster <- kproto(df, k = 5, lambda = NULL, nstart = 25)
# cluster$withinss


#load("Classification_output.RData")

df$cluster <- cluster$cluster


# The order of clusters may change each run, so ensure that group names are correctly matched to their corresponding clusters.
df$Clusters <- ifelse(df$cluster == 3, "C", 
                      ifelse(df$cluster == 5, "D",
                             ifelse(df$cluster == 2, "E",
                                    ifelse(df$cluster == 4, "B","A"))))
df$cluster <- as.factor(df$cluster)
df$Clusters <- as.factor(df$Clusters)



df_TN1 <- df
df_TN1$cluster <- as.factor(df_TN1$cluster)
df_TN1$Clusters <- as.factor(df_TN1$Clusters)

df_TN1$BioVol_um3 <- df_TN1$BioVol_mm3*10^9
ggplot(df_TN1, aes(x = Clusters, y = BioVol_um3, color = Clusters)) +  # ggplot function
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 9000), breaks = c(0, 4500, 9000)) + 
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold.italic"),
    axis.text.x = element_text(color = "black", size = 12, angle = 0),
    axis.text.y = element_text(color = "black", size = 12, angle = 0),
    plot.margin = unit(c(0.0, 0, 0, 0), "cm")
  ) +
  xlab("") + 
  ylab(expression(C_VOL ~ (µm^3)))


#add taxonomic group and other environmental threshold to df from other files.
#load final data frame (after adding those information)
#load("Classification_output_with_other_thresholds.RData")
load("Classification_trait_thresholds.RData")

df_TN1 <- df
df_TN1$cluster <- as.factor(df_TN1$cluster)
df_TN1$Clusters <- as.factor(df_TN1$Clusters)


df_TN <- df_TN1 %>% 
  select(TN10,TN90,Clusters)
df_TN_long <- melt(df_TN, id = "Clusters") 
n <- ggplot(df_TN_long, aes(x = variable, y = value, color = Clusters)) +  # ggplot function
  geom_boxplot()+ 
  theme(legend.title=element_blank(),legend.position = "None")+
  xlab("")+ scale_y_continuous(limits = c(0, 4), breaks = c(0, 2, 4)) +
  ylab (expression(TN~( mg/L ))) +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold.italic"),
    axis.text.x = element_text( color="black", 
                                size=12, angle=0),
    axis.text.y = element_text( color="black", 
                                size=12, angle=0),
    plot.margin = unit(c(0.0,0,0,0), "cm"))
# +
#   annotate("text", x = 0.8, y = 3.5, label = "(C)",size = 5)

n
df_TP <- df_TN1 %>% 
  select(TP10,TP90,Clusters)
df_TP_long <- melt(df_TP, id = "Clusters") 
p <- ggplot(df_TP_long, aes(x = variable, y = value, color = Clusters)) +  # ggplot function
  geom_boxplot()+scale_y_continuous(limits = c(0, 0.1), breaks = c(0, 0.05, 0.1)) +
  theme(legend.title=element_blank(),legend.position = "None")+
  xlab("")+ 
  ylab (expression(TP~( mg/L ))) +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold.italic"),
    axis.text.x = element_text( color="black", 
                                size=12, angle=0),
    axis.text.y = element_text( color="black", 
                                size=12, angle=0),
    plot.margin = unit(c(0.0,0,0,0), "cm"))
# +
#   annotate("text", x = 0.8, y = 0.085, label = "(E)",size = 5)
p
df_TN_TP <- df_TN1 %>% 
  select(TN_TP10,TN_TP90,Clusters)
colnames(df_TN_TP) <- c("TNTP10", "TNTP90", "Clusters")
df_TN_TP_long <- melt(df_TN_TP, id = "Clusters") 
r <- ggplot(df_TN_TP_long, aes(x = variable, y = value, color = Clusters)) +  # ggplot function
  geom_boxplot()+ scale_y_continuous(limits = c(0, 400), breaks = c(0, 200, 400)) +
  theme(legend.title=element_blank(),legend.position = "None")+
  xlab("")+ 
  ylab ("TNTP") +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text( color="black", 
                                size=12, angle=0),
    axis.text.y = element_text( color="black", 
                                size=12, angle=0),
    plot.margin = unit(c(0.0,0,0,0), "cm"))
# +
#   annotate("text", x = 0.8, y = 270, label = "(G)",size = 5)
r
df_TEMP <- df_TN1 %>% 
  select(TEMP10,TEMP90,Clusters)
df_TEMP_long <- melt(df_TEMP, id = "Clusters") 
t <- ggplot(df_TEMP_long, aes(x = variable, y = value, color = Clusters)) +  # ggplot function
  geom_boxplot()+ scale_y_continuous(limits = c(0, 40), breaks = c(0, 20, 40)) +
  theme(legend.title=element_blank(),legend.position = "None")+
  xlab("")+ 
  ylab (expression(paste(
    "TEMP (°C)"))) +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold.italic"),
    axis.text.x = element_text( color="black", 
                                size=12, angle=0),
    axis.text.y = element_text( color="black", 
                                size=12, angle=0),
    plot.margin = unit(c(0.0,0,0,0), "cm"))
# +
#   annotate("text", x = 0.8, y = 27.5, label = "(H)",size = 5)
t
df_AMM <- df_TN1 %>% 
  select(AMM10,AMM90,Clusters)
df_AMM_long <- melt(df_AMM, id = "Clusters") 
a <- ggplot(df_AMM_long, aes(x = variable, y = value, color = Clusters)) +  # ggplot function
  geom_boxplot()+ scale_y_continuous(limits = c(0, 0.04), breaks = c(0, 0.02, 0.04)) +
  theme(legend.title=element_blank(),legend.position = "None")+
  xlab("")+ 
  ylab (expression(AMM~( mg/L ))) +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold.italic"),
    axis.text.x = element_text( color="black", 
                                size=12, angle=0),
    axis.text.y = element_text( color="black", 
                                size=12, angle=0),
    plot.margin = unit(c(0.0,0,0,0), "cm"))
# +
#   annotate("text", x = 0.8, y = 0.035, label = "(D)",size = 5)

a
df_NIT <- df_TN1 %>% 
  select(NIT10,NIT90,Clusters)
df_NIT_long <- melt(df_NIT, id = "Clusters") 
ni <- ggplot(df_NIT_long, aes(x = variable, y = value, color = Clusters)) +  # ggplot function
  geom_boxplot()+theme(legend.title=element_blank(), legend.position = "None")+
  scale_y_continuous(limits = c(0, 4), breaks = c(0, 2, 4)) +
  xlab("")+ 
  ylab (expression(NIT~( mg/L ))) +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold.italic"),
    axis.text.x = element_text( color="black", 
                                size=12, angle=0),
    axis.text.y = element_text( color="black", 
                                size=12, angle=0),
    plot.margin = unit(c(0.0,0,0,0), "cm"))
# +
#   annotate("text", x = 0.8, y = 2.6, label = "(B)",size = 5)
ni
df_SIL <- df_TN1 %>% 
  select(SIL10,SIL90,Clusters)
df_SIL_long <- melt(df_SIL, id = "Clusters") 
s <- ggplot(df_SIL_long, aes(x = variable, y = value, color = Clusters)) +  # ggplot function
  geom_boxplot()+theme(legend.title=element_blank(), legend.position = "None")+
  scale_y_continuous(limits = c(0, 50), breaks = c(0, 25, 50)) +
  xlab("")+ 
  ylab (expression(SIL~( mg/L ))) +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold.italic"),
    axis.text.x = element_text( color="black", 
                                size=12, angle=0),
    axis.text.y = element_text( color="black", 
                                size=12, angle=0),
    plot.margin = unit(c(0.0,0,0,0), "cm"))
s


df_TN1$BioVol_um3 <- df_TN1$BioVol_mm3*10^9


b <- ggplot(df_TN1, aes(x = Clusters, y = BioVol_um3, color = Clusters)) +  # ggplot function
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 9000), breaks = c(0, 4500, 9000)) + 
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold.italic"),
    axis.text.x = element_text(color = "black", size = 12, angle = 0),
    axis.text.y = element_text(color = "black", size = 12, angle = 0),
    plot.margin = unit(c(0.0, 0, 0, 0), "cm")
  ) +
  xlab("") + 
  ylab(expression(C_VOL ~ (µm^3)))
# +
#   annotate("text", x = 1.2, y = 8000, label = "(A)",size = 5)
b
p1 <- (b/n/p/r)
p2 <- (ni/a/s/t)
f <- p1|p2
f



#COMPARE DIFFERENCE BETWEEN CLUSTERS
library(PMCMRplus)
# Normal distribution test
shapiro.test(df$TP90)
shapiro.test(df$TN90)
shapiro.test(df$TN_TP90)
shapiro.test(df$TEMP90)
shapiro.test(df$AMM90)
shapiro.test(df$NIT90)
shapiro.test(df$FRP90)
shapiro.test(df$SIL90)

shapiro.test(df$TP10)
shapiro.test(df$TN10)
shapiro.test(df$TN_TP10)
shapiro.test(df$TEMP10)
shapiro.test(df$AMM10)
shapiro.test(df$NIT10)
shapiro.test(df$FRP10)
shapiro.test(df$SIL10)

shapiro.test(df$BioVol_mm3)

#p-value of all the shapiro test < 0.05, data was not normal distribution, 
# Therefore, non-parametric test was selected

kruskal.test(TP90 ~ Clusters, data = df) 
kruskal.test(TN90 ~ Clusters, data = df) 
kruskal.test(TN_TP90 ~ Clusters, data = df) 
kruskal.test(NIT90 ~ Clusters, data = df)
kruskal.test(AMM90 ~ Clusters, data = df)
kruskal.test(FRP90 ~ Clusters, data = df) 
kruskal.test(SIL90 ~ Clusters, data = df) 
kruskal.test(TEMP90 ~ Clusters, data = df)

kruskal.test(TP10 ~ Clusters, data = df) 
kruskal.test(TN10 ~ Clusters, data = df) 
kruskal.test(TN_TP10 ~ Clusters, data = df)
kruskal.test(NIT10 ~ Clusters, data = df) 
kruskal.test(AMM10 ~ Clusters, data = df) 
kruskal.test(FRP10 ~ Clusters, data = df)
kruskal.test(SIL10 ~ Clusters, data = df) 
kruskal.test(TEMP10 ~ Clusters, data = df)

library(PMCMRplus)

df$Clusters <- as.factor(df$Clusters)
kruskal.test(BioVol_mm3 ~ Clusters, data = df) 

kwAllPairsConoverTest(BioVol_mm3 ~ Clusters, data = df,
                      p.adjust.method = "holm")


kwAllPairsConoverTest(TN90 ~ Clusters, data = df,
                      p.adjust.method = "holm")



kwAllPairsConoverTest(NIT90 ~ Clusters, data = df,
                      p.adjust.method = "holm")



kwAllPairsConoverTest(TNTP90 ~ Clusters, data = df,
                      p.adjust.method = "holm")


kwAllPairsConoverTest(AMM90 ~ Clusters, data = df,
                      p.adjust.method = "holm")

kwAllPairsConoverTest(FRP90 ~ Clusters, data = df,
                      p.adjust.method = "holm")


kwAllPairsConoverTest(SIL90 ~ Clusters, data = df,
                      p.adjust.method = "holm")


#DUNN TEST

library(FSA)
dunnTest(BioVol_mm3 ~ Clusters, data = df,, method="bonferron")

dunnTest(TN90 ~ Clusters, data = df,, method="bonferron")

dunnTest(TNTP90 ~ Clusters, data = df,, method="bonferron")

dunnTest(NIT90 ~ Clusters, data = df,, method="bonferron")

dunnTest(AMM90 ~ Clusters, data = df,, method="bonferron")

dunnTest(FRP90 ~ Clusters, data = df,, method="bonferron")

dunnTest(SIL90 ~ Clusters, data = df,, method="bonferron")

dunnTest(TP10 ~ Clusters, data = df,, method="bonferron")

dunnTest(TN10 ~ Clusters, data = df,, method="bonferron")

dunnTest(TN_TP10 ~ Clusters, data = df,, method="bonferron")

dunnTest(NIT10 ~ Clusters, data = df,, method="bonferron")

dunnTest(AMM10 ~ Clusters, data = df,, method="bonferron")

dunnTest(SIL10 ~ Clusters, data = df,, method="bonferron")


#TEST TO COMPARE CONTRIBUTION OF TAXONOMIC GROUPS TO FUNCTIONAL GROUPS
# COMPARE WITH CLUSTER COMPOSITION

# Load required libraries
library(ggstatsplot)
library(ggplot2)
library(dplyr)
library(gridExtra)

# Assuming data_all is your dataset
data_all = df
# List of groups to iterate over
groups <- unique(data_all$Clusters)

# Initialize an empty list to store results
results <- data.frame(Group = character(), 
                      TN90_pvalue = numeric(), 
                      TP90_pvalue = numeric(), 
                      TN_TP90_pvalue = numeric(),
                      stringsAsFactors = FALSE)

# Loop through each group
for (group in groups) {
  
  # Filter data for the current group
  data_filter <- data_all %>%
    filter(Clusters == group)
  
  # Perform Kruskal-Wallis tests
  TN90_test <- kruskal.test(TN90 ~ Group, data = data_filter)
  TP90_test <- kruskal.test(TP90 ~ Group, data = data_filter)
  TN_TP90_test <- kruskal.test(TN_TP90 ~ Group, data = data_filter)
  
  # Store p-values in the results dataframe
  results <- rbind(results, data.frame(Group = group,
                                       TN90_pvalue = TN90_test$p.value,
                                       TP90_pvalue = TP90_test$p.value,
                                       TN_TP90_pvalue = TN_TP90_test$p.value))
}

# Export the results to a CSV file
#write.csv(results, "kruskal_test_pvalues.csv", row.names = FALSE)


# Load required libraries
library(ggstatsplot)
library(ggplot2)
library(dplyr)
library(gridExtra)

# Assuming data_all is your dataset
data_all = df
# List of groups to iterate over
groups <- unique(data_all$Clusters)

# Initialize an empty list to store results
results <- data.frame(Group = character(), 
                      TN90_pvalue = numeric(), 
                      TP90_pvalue = numeric(), 
                      TN_TP90_pvalue = numeric(),
                      stringsAsFactors = FALSE)

# Loop through each group
for (group in groups) {
  
  # Filter data for the current group
  data_filter <- data_all %>%
    filter(Clusters == group)
  
  # Perform Kruskal-Wallis tests
  TN90_test <- kruskal.test(TN90 ~ Group, data = data_filter)
  TP90_test <- kruskal.test(TP90 ~ Group, data = data_filter)
  TN_TP90_test <- kruskal.test(TNTP90 ~ Group, data = data_filter)
  
  # Store p-values in the results dataframe
  results <- rbind(results, data.frame(Group = group,
                                       TN90_pvalue = TN90_test$p.value,
                                       TP90_pvalue = TP90_test$p.value,
                                       TN_TP90_pvalue = TN_TP90_test$p.value))
}


# results of p-values for comparing mean of all taxonomic groups contributing to each functional group.
# We expect p-values > 0.05 to indicate there is no significant difference. It mean K-prototype picked up
# species of the taxonomic groups have the similar or the same threshold values to build functional groups

# Export the results to a CSV file

#write.csv(results, "kruskal_test_pvalues.csv", row.names = FALSE)


#TEST TO COMPARE CONTRIBUTION OF TAXONOMIC GROUPS TO FUNCTIONAL GROUPS
# One taxonomic group contributed to many functional groups. We compare the taxonomic groups of different functional groups to answer
# the question are there any differences between taxonomic groups of different functional groups.
library(car)

data_all <- df
# Get unique groups (clusters)
groups <- unique(data_all$Clusters)
group_taxa <- unique(data_all$Group)
group_taxa

# Initialize an empty dataframe to store results
results_levene <- data.frame(Cluster = character(), 
                             TN90_pvalue = numeric(), 
                             TP90_pvalue = numeric(), 
                             TN_TP90_pvalue = numeric(),
                             stringsAsFactors = FALSE)

# Loop through each group
for (group in groups) {
  
  # Filter data for the current group
  data_filter <- data_all %>%
    filter(Clusters == group) %>% 
    select(TN90, TP90, TNTP90, Clusters)
  
  data_filter_1 <- data_all %>%
    filter(Clusters == group)
  in_group <- unique(data_filter_1$Group)
  
  
  data_filter <- data_filter %>%
    rename(Group = Clusters)
  
  # Extract TN90, TP90, and TN_TP90 values for the current group
  TN90_values <- data_filter$TN90
  TP90_values <- data_filter$TP90
  TN_TP90_values <- data_filter$TNTP90
  
  # Extract TN90, TP90, and TN_TP90 values for all groups
  
  groups_to_exclude <- c("Other", "Cryptophyta", "Chloromonadophyta", "Euglenophyta (Euglenoid)")
  
  data_groups <- data_all %>%
    filter(Group %in% in_group) %>%
    select(TN90, TP90, TNTP90, Group)
  
  
  
  # Combine data_filter and data_groups
  final_data <- rbind(data_filter %>% select(TN90, TP90, TNTP90, Group), data_groups)
  
  # Add a column indicating whether the row is from the cluster or the group
  #final_data$Source <- c(data_filter$Clusters, data_groups$Group)
  # Initialize variables for p-values
  TN90_pvalue <- NA
  TP90_pvalue <- NA
  TN_TP90_pvalue <- NA
  
  # Check if there are more than one unique value in the Source column
  if (length(unique(final_data$Group)) > 1) {
    # Perform Levene's tests if there is more than one unique value
    TN90_levene <- leveneTest(TN90 ~ Group, data = final_data)
    TP90_levene <- leveneTest(TP90 ~ Group, data = final_data)
    TN_TP90_levene <- leveneTest(TNTP90 ~ Group, data = final_data)
    
    # Store p-values
    TN90_pvalue <- TN90_levene$`Pr(>F)`[1]
    TP90_pvalue <- TP90_levene$`Pr(>F)`[1]
    TN_TP90_pvalue <- TN_TP90_levene$`Pr(>F)`[1]
  }
  
  # Append results to the dataframe
  results_levene <- rbind(results_levene, data.frame(Cluster = group,
                                                     TN90_pvalue = TN90_pvalue,
                                                     TP90_pvalue = TP90_pvalue,
                                                     TN_TP90_pvalue = TN_TP90_pvalue))
  
  
  # Print summary of results
  print(results_levene)
}
#COMPARE CONTRIBUTION OF TAXONOMIC GROUP CONTRIBUTION

groups <- unique(data_all$Group)

# Initialize an empty dataframe to store results
results <- data.frame(Group = character(), 
                      TN90_pvalue = character(), 
                      TP90_pvalue = character(), 
                      TN_TP90_pvalue = character(),
                      stringsAsFactors = FALSE)

# Loop through each group
for (group in groups) {
  
  # Filter data for the current group
  data_filter <- data_all %>%
    filter(Group == group)
  
  # Initialize variables for p-values
  TN90_pvalue <- "samegroup"
  TP90_pvalue <- "samegroup"
  TN_TP90_pvalue <- "samegroup"
  
  # Check if there are more than one unique value in the Clusters column
  if (length(unique(data_filter$Clusters)) > 1) {
    # Perform Kruskal-Wallis tests if there is more than one unique value
    TN90_test <- kruskal.test(TN90 ~ Clusters, data = data_filter)
    TP90_test <- kruskal.test(TP90 ~ Clusters, data = data_filter)
    TN_TP90_test <- kruskal.test(TNTP90 ~ Clusters, data = data_filter)
    
    # Store p-values
    TN90_pvalue <- TN90_test$p.value
    TP90_pvalue <- TP90_test$p.value
    TN_TP90_pvalue <- TN_TP90_test$p.value
  }
  
  # Append results to the dataframe
  results <- rbind(results, data.frame(Group = group,
                                       TN90_pvalue = as.character(TN90_pvalue),
                                       TP90_pvalue = as.character(TP90_pvalue),
                                       TN_TP90_pvalue = as.character(TN_TP90_pvalue)))
}
print(results)


#HERE, we expected p-values < 0.05 indicated significant difference between groups within a taxonomic group. It means K-prototype picked up
# species have similar threshold, therefore, even they are the same taxonomic groups, but they were assigned into different functional because 
# significant differerence in threshold values.



# Export the results to a CSV file
#write.csv(results, "kruskal_test_pvalues_Taxonomic_contribution.csv", row.names = FALSE)


# The final test is COMPARING THE VARIANCE OF THRESHOLD BETWEEN PAIRS OF FUNCTIONAL GROUPS AND TAXONOMIC GROUPS
# if functional group show less variance, it will be better than taxonomic groups.

library(car)
library(dplyr)
library(ggstatsplot)
library(ggplot2)
library(gridExtra)
# Load the dataset
#df = read.csv("K_prototype_clusters_output.csv")
# df <- df %>%
#   rename(TNTP90 = TN_TP90)
# df <- df %>%
#   rename(TNTP10 = TN_TP10)
data_all <- df%>% 
  filter(Group %in% c("Chlorophyta (Green algae)", "Bacilariophyta (Diatom)", "Cyanophyta (Blue green)", "Dinophyta"))

# Get unique clusters (groups)

clusters <- unique(data_all$Clusters)
group_taxa <- unique(data_all$Group)

# Initialize an empty dataframe to store results
results_levene <- data.frame(Cluster = character(), 
                             Taxa = character(),
                             TN90_pvalue = numeric(), 
                             TP90_pvalue = numeric(), 
                             TN_TP90_pvalue = numeric(),
                             stringsAsFactors = FALSE)

# Loop through each cluster
for (i in clusters) {
  
  # Filter data for the current cluster
  data_cluster <- data_all %>%
    filter(Clusters == i) %>% select(TN90, TP90,TNTP90, Clusters)%>%
    rename(Group = Clusters)  # Rename 'Clusters' column to 'Group'
  
  # Loop through each group taxa
  for (u in group_taxa) {
    
    # Filter data for the current group taxa
    data_taxon <- data_all %>%
      filter(Group == u) %>% select(TN90, TP90,TNTP90, Group)
    
    # Skip if there are not enough data points
    if (nrow(data_taxon) < 2) {
      next
    }
    
    combined_data <- bind_rows(data_cluster, data_taxon)
    
    levene_testTP <- leveneTest(TP90 ~ Group, data = combined_data)
    levene_testTN <- leveneTest(TN90 ~ Group, data = combined_data)
    levene_testTNTP <- leveneTest(TNTP90 ~ Group, data = combined_data)
    TP90_pvalue <- levene_testTP$`Pr(>F)`[1]
    TN90_pvalue <- levene_testTN$`Pr(>F)`[1]
    TNTP90_pvalue <- levene_testTNTP$`Pr(>F)`[1]
    
    
    
    # Store results in the dataframe
    results_levene <- rbind(results_levene, 
                            data.frame(Cluster = i,
                                       Taxa = u,
                                       TN90_pvalue = TN90_pvalue,
                                       TP90_pvalue = TP90_pvalue,
                                       TN_TP90_pvalue = TNTP90_pvalue))
    #THIS IS FOR PLOTTING
    plot1 <- ggbetweenstats(
      data = combined_data,
      x = Group,
      y = TP90,type = "nonparametric",
      title = paste("Group:", i,u, "- TP90")
    )
    
    plot2 <- ggbetweenstats(
      data = combined_data,
      x = Group,
      y = TN90,
      title = paste("Group:", i,u, "- TN90")
    )
    
    plot3 <- ggbetweenstats(
      data = combined_data,
      x = Group,
      y = TNTP90,
      title = paste("Group:", i,u, "- TN_TP90")
    )
  } # End of group_taxa loop
  
  
  
  # Combine the plots horizontally
  plot <- grid.arrange(plot1, plot2, plot3, nrow = 1, ncol = 3)
  
  plot
  
} # End of clusters loop

# Print the final results
print(results_levene)

#write.csv(results_levene, "Levene_test.csv", row.names = FALSE)




