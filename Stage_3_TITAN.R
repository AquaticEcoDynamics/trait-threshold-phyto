#STAGE 3 - TITAN ANALYSIS, this stage includes Threshold indicator taxa analysis then check the purity of threshold

#To run TITAN, species with less than 4 occurences needs to be removed in input data 
#Data occurence should be transformed into a log10 scale (with cell number + 1) 
#Note: TITAN running is very time consumming, it is better to run TITAn for each environmental seperately

library(TITAN2)
library(dplyr)

# #Import Input for TITAN
#load("TITAN_data.Rdata") 
data <- read.csv("HN_PHY_WQ_DATA_log_1.csv")

colnames(data)
#Taxa community: 15 - 191
#Create subset community data

taxa <- data[,c(15:191)]
colnames(taxa)


# TN TITAN ANALYSIS (1)

#Creat subset of Environmental varibales: TN (mg/L)
TN = data$TN
summary(TN)

#Creat new data frame
df <-  data.frame(TN, taxa)
df <-  na.omit(df)

TN_taxa  = df
TN = TN_taxa$TN
species <- colnames(TN_taxa)
species
taxa = TN_taxa[,c(2:178)] 

#After remove NA values of environmental data, occurence of spp might be changed, therefore, need to check the occurence again
#Identify spp occured less than 4 times in new dataframe 

for (i in colnames(taxa)){
  occurences <- sum(taxa[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}

# print spp
# [1] "T.Anabaena.bergii"
# [1] "T.Dolichospermum.cf.circinale"

TN_data.TITAN = TN_taxa %>% 
  select(-T.Anabaena.bergii, -T.Dolichospermum.cf.circinale )
#check occurrences again

colnames(TN_data.TITAN)
taxa.TITAN = TN_data.TITAN[,c(2:176)]

#double check spp occured less than 4 times
for (i in colnames(taxa.TITAN)){
  occurences <- sum(taxa.TITAN[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}
#Good, no low occurrence species
#Now let's run TITAN

TN.TITAN <- TN_data.TITAN$TN
taxa.TITAN
#TITAN Calculation
glades.titan_TN <- titan(TN.TITAN, taxa.TITAN)
#great, let's wait for the results 5 mins/bootstrap => 500 bootstraps will be done in 2 days

save.image("TITAN_TN.RData")
# savehistory("TITAN_TN_All_spp.Rhistory")





#
#
#
# TP TITAN ANALYSIS (2)
load("TITAN_data.Rdata")
#Creat subset of Environmental varibales: TP (mg/L)
TP = data$TP
summary(TP)
taxa <- data[,c(15:191)]

#Creat new data frame
df <-  data.frame(TP, taxa)
df <-  na.omit(df)

TP_taxa = df
TP = TP_taxa$TP
species <- colnames(TP_taxa)
taxa = TP_taxa[,c(2:178)] 

#After remove NA values of environmental data, occurence of spp might be changed, therefore, need to check the occurence again
#Identify spp occured less than 4 times in new dataframe 

for (i in colnames(taxa)){
  occurences <- sum(taxa[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}

# [1] "T.Anabaena.bergii"
# [1] "T.Dolichospermum.cf.circinale"

TP_data.TITAN <- TP_taxa %>% 
  select(-T.Anabaena.bergii, -T.Dolichospermum.cf.circinale )
#check occurrences again

colnames(TP_data.TITAN)
taxa.TITAN = TP_data.TITAN[,c(2:176)]

for (i in colnames(taxa.TITAN)){
  occurences <- sum(taxa.TITAN[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}
##Good, no low occurrence species
#Now let's run TITAN

TP.TITAN <- TP_data.TITAN$TP
taxa.TITAN
#TITAN Calculation
glades.titan_TP <- titan(TP.TITAN, taxa.TITAN)

save.image("TITAN_TP.RData")
# savehistory("TITAN_TP_All_spp.Rhistory")



#
#
#
# TNTP TITAN ANALYSIS (3)
load("TITAN_data.Rdata")
#Creat subset of Environmental varibales: TNTP
TN_TP_Ratio = data$TN_TP_Ratio
summary(TN_TP_Ratio)

hist(TN_TP_Ratio)
taxa <- data[,c(15:191)]
#Creat new data frame
df <-  data.frame(TN_TP_Ratio, taxa)
df <-  na.omit(df)

TN_TP_Ratio_taxa = df
TN_TP_Ratio = TN_TP_Ratio_taxa$TN_TP_Ratio
species <- colnames(TN_TP_Ratio_taxa)
species
taxa = TN_TP_Ratio_taxa[,c(2:176)] 

#After remove NA values of environmental data, occurence of spp might be changed, therefore, need to check the occurence again
#Identify spp occured less than 4 times in new dataframe 
for (i in colnames(taxa)){
  occurences <- sum(taxa[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}

TN_TP_Ratio_data.TITAN <- TN_TP_Ratio_taxa %>% 
  select(-T.Anabaena.bergii, -T.Dolichospermum.cf.circinale)

#check occurrences again

colnames(TN_TP_Ratio_data.TITAN)
taxa.TITAN = TN_TP_Ratio_data.TITAN[,c(2:176)]

for (i in colnames(taxa.TITAN)){
  occurences <- sum(taxa.TITAN[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}
##Good, no low occurrence species
#Now let's run TITAN

TN_TP_Ratio.TITAN <- TN_TP_Ratio_data.TITAN$TN_TP_Ratio
taxa.TITAN
#TITAN Calculation
glades.titan_TN_TP_Ratio <- titan(TN_TP_Ratio.TITAN, taxa.TITAN)

save.image("TITAN_TN_TP_Ratio.RData")
# savehistory("TITAN_TN_TP_Ratio_All_spp.Rhistory")


#
#
#
# TEMPERATURE TITAN ANALYSIS (4)
load("TITAN_data.Rdata")
#Creat subset of Environmental varibales: TEMP
TEMP = data$TEMP
summary(TEMP)

hist(TEMP)
taxa <-  data[,c(15:191)]

df <-  data.frame(TEMP, taxa)
df <-  na.omit(df)

TEMP_taxa = df
TEMP = TEMP_taxa$TEMP
taxa = TEMP_taxa[,c(2:178)] 

#After remove NA values of environmental data, occurence of spp might be changed, therefore, need to check the occurence again
#Identify spp occured less than 4 times in new dataframe 

for (i in colnames(taxa)){
  occurences <- sum(taxa[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}

TEMP_data.TITAN <- TEMP_taxa %>% 
  select(-T.Anabaena.bergii, -T.Dolichospermum.cf.circinale)

#check occurrences again

colnames(TEMP_data.TITAN)
taxa.TITAN = TEMP_data.TITAN[,c(2:176)]

for (i in colnames(taxa.TITAN)){
  occurences <- sum(taxa.TITAN[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}
##Good, no low occurrence species
#Now let's run TITAN

TEMP.TITAN <- TEMP_data.TITAN$TEMP
taxa.TITAN
#TITAN Calculation
glades.titan_TEMP <- titan(TEMP.TITAN, taxa.TITAN)

save.image("TITAN_TEMP_All_spp.RData")
# savehistory("TITAN_TEMP.Rhistory")


#
#
#
# AMMONIUM TITAN ANALYSIS (5)

load("TITAN_data.Rdata")
AMM = data$AMM
summary(AMM)
taxa <-  data[,c(15:191)]
df <-  data.frame(AMM, taxa)
df <-  na.omit(df)

AMM_taxa = df
AMM = AMM_taxa$AMM
taxa = AMM_taxa[,c(2:178)] 

for (i in colnames(taxa)){
  occurences <- sum(taxa[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}
# [1] "T.Anabaena.aphanizomenoides"
# [1] 2
# [1] "T.Anabaena.bergii"
# [1] 2
# [1] "T.Dolichospermum.cf.circinale"

AMM_data.TITAN <- AMM_taxa %>% 
  select(-T.Anabaena.aphanizomenoides, -T.Anabaena.bergii, -T.Dolichospermum.cf.circinale )
#check occurrences again

colnames(AMM_data.TITAN)
taxa.TITAN = AMM_data.TITAN[,c(2:175)]

for (i in colnames(taxa.TITAN)){
  occurences <- sum(taxa.TITAN[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}
##Good, no low occurrence species
#Now let's run TITAN

AMM.TITAN <- AMM_data.TITAN$AMM
taxa.TITAN
#TITAN Calculation
glades.titan_AMM <- titan(AMM.TITAN, taxa.TITAN)
#great, let's wait for the results 5 mins/bootstrap => 500 bootstraps will be done in 2 days
save.image("TITAN_AMM.RData")
# savehistory("TITAN_AMM_All_spp.Rhistory")


#
#
#
# NIT TITAN ANALYSIS (6)

load("TITAN_data.Rdata")
NIT = data$NIT
summary(NIT)
taxa <-  data[,c(15:191)]
df <-  data.frame(NIT, taxa)
df <-  na.omit(df)

NIT_taxa = df
NIT = NIT_taxa$NIT
colnames(NIT_taxa)
taxa = NIT_taxa[,c(2:178)] 


for (i in colnames(taxa)){
  occurences <- sum(taxa[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}
# [1] "T.Anabaena.aphanizomenoides"
# [1] 2
# [1] "T.Anabaena.bergii"
# [1] 2
# [1] "T.Dolichospermum.cf.circinale"

NIT_data.TITAN <- NIT_taxa %>% 
  select(-T.Anabaena.aphanizomenoides, -T.Anabaena.bergii, -T.Dolichospermum.cf.circinale )
#check occurrences again

colnames(NIT_data.TITAN)
taxa.TITAN = NIT_data.TITAN[,c(2:175)]

for (i in colnames(taxa.TITAN)){
  occurences <- sum(taxa.TITAN[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}
##Good, no low occurrence species
#Now let's run TITAN

NIT.TITAN <- NIT_data.TITAN$NIT

#TITAN Calculation
glades.titan_NIT <- titan(NIT.TITAN, taxa.TITAN)
save.image("TITAN_NIT.RData")
# savehistory("TITAN_NIT_All_spp.Rhistory")

#
#
#
# FRP TITAN ANALYSIS (7)

load("TITAN_data.Rdata")
#Create subset community data

taxa <- data[,c(15:191)]
colnames(taxa)
#Creat subset of Environmental varibales: FRP (mg/L)
FRP = data$FRP
summary(FRP)
#Creat new data frame
df <-  data.frame(FRP, taxa)
df <-  na.omit(df)
FRP_taxa = df
FRP = FRP_taxa$FRP
species <- colnames(FRP_taxa)
taxa = FRP_taxa[,c(2:178)] 


for (i in colnames(taxa)){
  occurences <- sum(taxa[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}

# [1] "T.Anabaena.aphanizomenoides"
# [1] 2
# [1] "T.Anabaena.bergii"
# [1] 0
# [1] "T.Aphanizomenonaceae"
# [1] 0
# [1] "T.Dolichospermum.cf.circinale"
# [1] 0
# [1] "T.Dolichospermum.circinale"
# [1] 3
# [1] "T.Radiocystis"
# [1] 1
# [1] "Anagnostidinema"
# [1] 0
# [1] "Haptophyte"
# [1] 0
# [1] "Monoraphidium.cf"
# [1] 3
# [1] "Peridinoid"
# [1] 3
# [1] "Prorocentrum.non.toxic"
# [1] 3
# [1] "Dimorphococcus"
# [1] 2
# [1] "T.Gymnodinoid"

FRP_data.TITAN <- FRP_taxa %>% 
  select(-T.Anabaena.aphanizomenoides, -T.Anabaena.bergii, -T.Aphanizomenonaceae, - T.Dolichospermum.cf.circinale,
         -T.Dolichospermum.circinale, -T.Radiocystis, -Anagnostidinema, -Anagnostidinema, -Haptophyte, -Monoraphidium.cf, 
         -Peridinoid,-Dimorphococcus, -T.Gymnodinoid  , -Prorocentrum.non.toxic)
#check occurrences again

colnames(FRP_data.TITAN)
taxa.TITAN = FRP_data.TITAN[,c(2:165)]

for (i in colnames(taxa.TITAN)){
  occurences <- sum(taxa.TITAN[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}
##Good, no low occurrence species
#Now let's run TITAN

FRP.TITAN <- FRP_data.TITAN$FRP
taxa.TITAN
#TITAN Calculation
glades.titan_FRP <- titan(FRP.TITAN, taxa.TITAN)
#great, let's wait for the results 5 mins/bootstrap => 500 bootstraps will be done in 2 days
save.image("TITAN_FRP.RData")
# savehistory("TITAN_FRP_All_spp.Rhistory")


#
#
#
# SALINITY TITAN ANALYSIS (8)

load("TITAN_data.Rdata")

taxa <- data[,c(15:191)]
SAL = data$SAL #unit : psu
df <-  data.frame(SAL, taxa)
df <-  na.omit(df)

SAL_taxa = df

SAL = SAL_taxa$SAL

species <- colnames(SAL_taxa)

taxa = SAL_taxa[,c(2:178)]

for (i in colnames(taxa)){
  
  occurences <- sum(taxa[[i]]>0)
  
  if (occurences < 4){
    
    print(occurences)
    
    print(i)
    
  }
  
}



# [1] "T.Anabaena.aphanizomenoides"
# [1] 1
# [1] "T.Anabaena.bergii"
# [1] 0
# [1] "T.Aphanizomenonaceae"
# [1] 1
# [1] "T.Dolichospermum.cf.circinale"
# [1] 1
# [1] "Anagnostidinema"
# [1] 0
# [1] "Monoraphidium.cf"
# [1] 3
# [1] "Dimorphococcus"



SAL_data.TITAN <- SAL_taxa %>%
  
  select(-T.Anabaena.aphanizomenoides, -T.Anabaena.bergii, - T.Aphanizomenonaceae, 
         -T.Dolichospermum.cf.circinale, -Anagnostidinema, -Monoraphidium.cf, -Dimorphococcus)

#check occurrences again



colnames(SAL_data.TITAN)

taxa.SAL.TITAN = SAL_data.TITAN[,c(2:171)]



for (i in colnames(taxa.SAL.TITAN)){
  
  occurences <- sum(taxa.SAL.TITAN[[i]]>0)
  
  if (occurences < 4){
    
    print(occurences)
    
    print(i)
    
  }
  
}

##Good, no low occurrence species

#Now let's run TITAN



SAL.TITAN <- SAL_data.TITAN$SAL

taxa.SAL.TITAN

glades.titan_SAL <- titan(SAL.TITAN, taxa.SAL.TITAN)
save.image("TITAN_SAL.RData")
# savehistory("TITAN_SAL_All_spp.Rhistory")



#
#
#
# SILICA TITAN ANALYSIS (9)

load("TITAN_data.Rdata")

SIL = data$SIL
summary(SIL)
taxa <-  data[,c(15:191)]
df <-  data.frame(SIL, taxa)
df <-  na.omit(df)

SIL_taxa = df
SIL = SIL_taxa$SIL
species <- colnames(SIL_taxa)
taxa = SIL_taxa[,c(2:178)] 


for (i in colnames(taxa)){
  occurences <- sum(taxa[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}

# [1] 2
# [1] "T.Anabaena.bergii"
# [1] 0
# [1] "T.Aphanizomenonaceae"
# [1] 0
# [1] "T.Dolichospermum.cf.circinale"
# [1] 0
# [1] "T.Dolichospermum.circinale"
# [1] 2
# [1] "T.Radiocystis"
# [1] 0
# [1] "Anagnostidinema"
# [1] 0
# [1] "Haptophyte"
# [1] 0
# [1] "Monoraphidium.cf"
# [1] 3
# [1] "Westella.cf"
# [1] 2
# [1] "Dimorphococcus"
# [1] 2
# [1] "Quadrigula"
# [1] 0
# [1] "Rhabdogloea"
# > 

SIL_data.TITAN <- SIL_taxa %>% 
  select(-T.Anabaena.bergii, -T.Aphanizomenonaceae,- T.Dolichospermum.cf.circinale,
         -T.Dolichospermum.circinale,-T.Radiocystis,-Anagnostidinema,-Haptophyte,-Monoraphidium.cf,
         -Westella.cf,-Dimorphococcus,-Quadrigula,-Rhabdogloea)
#check occurrences again

colnames(SIL_data.TITAN)
taxa.TITAN = SIL_data.TITAN[,c(2:166)]

for (i in colnames(taxa.TITAN)){
  occurences <- sum(taxa.TITAN[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}
##Good, no low occurrence species
#Now let's run TITAN

SIL.TITAN <- SIL_data.TITAN$SIL
taxa.TITAN
#TITAN Calculation
glades.titan_SIL <- titan(SIL.TITAN, taxa.TITAN)
save.image("TITAN_SIL.RData")
# savehistory("TITAN_SIL_All_spp.Rhistory")


#
#
#
# TURBILITY TITAN ANALYSIS (10)

load("TITAN_data.Rdata")
TURB = data$TURB
summary(TURB)
taxa <-  data[,c(15:191)]
df <-  data.frame(TURB, taxa)
df <-  na.omit(df)
TURB_taxa = df
TURB = TURB_taxa$TURB
species <- colnames(TURB_taxa)
taxa = TURB_taxa[,c(2:178)] 


for (i in colnames(taxa)){
  occurences <- sum(taxa[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}

TURB_data.TITAN <- TURB_taxa %>% 
  select(-T.Anabaena.bergii, -T.Aphanizomenonaceae, -T.Dolichospermum.cf.circinale)
#check occurrences again

colnames(TURB_data.TITAN)
taxa.TITAN = TURB_data.TITAN[,c(2:175)]

for (i in colnames(taxa.TITAN)){
  occurences <- sum(taxa.TITAN[[i]]>0)
  if (occurences < 4){
    print(occurences)
    print(i)
  }
}
##Good, no low occurrence species
#Now let's run TITAN

TURB.TITAN <- TURB_data.TITAN$TURB
taxa.TITAN
#TITAN Calculation
glades.titan_TURB <- titan(TURB.TITAN, taxa.TITAN)

save.image("TITAN_TURB.RData")
# savehistory("TITAN_TURB_All_spp.Rhistory")

#DONE TITAN FOR 10 VARIABLES

save(glades.titan_AMM, file = "TITAN_AMM.RData")
save(glades.titan_FRP, file = "TITAN_FRP.RData")
save(glades.titan_NIT, file = "TITAN_NIT.RData")
save(glades.titan_SAL, file = "TITAN_SAL.RData")
save(glades.titan_SIL, file = "TITAN_SIL.RData")
save(glades.titan_TEMP, file = "TITAN_TEMP.RData")
save(glades.titan_TN, file = "TITAN_TN.RData")
save(glades.titan_TP, file = "TITAN_TP.RData")
save(glades.titan_TURB, file = "TITAN_TURB.RData")
save(glades.titan_TN_TP_Ratio, file = "TITAN_TN_TP_Ratio.RData")
save.image(file = "TITAN_ouput.RData")
# 
# 
library(tibble)
library(clustMixType)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(patchwork)



#EXTRACT CHANGE POINT THRESHOLD DATA
#Load data



load("TITAN_AMM.RData")
load("TITAN_FRP.RData")
load("TITAN_NIT.RData")
load("TITAN_SAL.RData")
load("TITAN_SIL.RData")
load("TITAN_TEMP.RData")
load("TITAN_TN.RData")
load("TITAN_TP.RData")
load("TITAN_TURB.RData")
load("TITAN_TN_TP_Ratio.RData")




##TN (1)
spp_data_TN <- glades.titan_TN$sppmax
spp_data_TN <- data.frame(spp_data_TN)

colnames(spp_data_TN)#Only need spp, change point 10 and 90
spp_data_TN <- spp_data_TN[,c(9,11)]
spp_data_TN <- spp_data_TN %>% 
  rownames_to_column(var = "Taxa")
colnames(spp_data_TN)
spp_data_TN <- spp_data_TN %>% 
  rename(Taxa = Taxa,
         TN10 = X10.,
         TN90 = X90.)

colnames(spp_data_TN)

##TP (2)
spp_data_TP <- glades.titan_TP$sppmax
spp_data_TP <- data.frame(spp_data_TP)

colnames(spp_data_TP)#Only need spp, change point 10 and 90
spp_data_TP <- spp_data_TP[,c(9,11)]
spp_data_TP <- spp_data_TP %>% 
  rownames_to_column(var = "Taxa")
colnames(spp_data_TP)
spp_data_TP <- spp_data_TP %>% 
  rename(Taxa = Taxa,
         TP10 = X10.,
         TP90 = X90.)

colnames(spp_data_TP)

##TN_TP_Ratio (3)
spp_data_TN_TP <- glades.titan_TN_TP_Ratio$sppmax
spp_data_TN_TP <- data.frame(spp_data_TN_TP)

colnames(spp_data_TN_TP)#Only need spp, change point 10 and 90
spp_data_TN_TP <- spp_data_TN_TP[,c(9,11)]
spp_data_TN_TP <- spp_data_TN_TP %>% 
  rownames_to_column(var = "Taxa")
colnames(spp_data_TN_TP)
spp_data_TN_TP <- spp_data_TN_TP %>% 
  rename(Taxa = Taxa,
         TN_TP10 = X10.,
         TN_TP90 = X90.)

colnames(spp_data_TN_TP)



#TEMP (4)
spp_data_TEMP <- glades.titan_TEMP$sppmax
spp_data_TEMP <- data.frame(spp_data_TEMP)

colnames(spp_data_TEMP)#Only need spp, change point 10 and 90
spp_data_TEMP <- spp_data_TEMP[,c(9,11)]
spp_data_TEMP <- spp_data_TEMP %>% 
  rownames_to_column(var = "Taxa")
colnames(spp_data_TEMP)
spp_data_TEMP <- spp_data_TEMP %>% 
  rename(Taxa = Taxa,
         TEMP10 = X10.,
         TEMP90 = X90.)

colnames(spp_data_TEMP)

#AMM (5)

spp_data_AMM <- glades.titan_AMM$sppmax
spp_data_AMM <- data.frame(spp_data_AMM)

colnames(spp_data_AMM)#Only need spp, change point 10 and 90
spp_data_AMM <- spp_data_AMM[,c(9,11)]
spp_data_AMM <- spp_data_AMM %>% 
  rownames_to_column(var = "Taxa")
colnames(spp_data_AMM)
spp_data_AMM <- spp_data_AMM %>% 
  rename(Taxa = Taxa,
         AMM10 = X10.,
         AMM90 = X90.)

head(spp_data_AMM)

#NIT (6)

spp_data_NIT <- glades.titan_NIT$sppmax
spp_data_NIT <- data.frame(spp_data_NIT)

colnames(spp_data_NIT)#Only need spp, change point 10 and 90
spp_data_NIT <- spp_data_NIT[,c(9,11)]
spp_data_NIT <- spp_data_NIT %>% 
  rownames_to_column(var = "Taxa")
colnames(spp_data_NIT)
spp_data_NIT <- spp_data_NIT %>% 
  rename(Taxa = Taxa,
         NIT10 = X10.,
         NIT90 = X90.)

head(spp_data_NIT)

##FRP (7)
spp_data_frp <- glades.titan_FRP$sppmax
spp_data_frp <- data.frame(spp_data_frp)

colnames(spp_data_frp)#Only need spp, change point 10 and 90
spp_data_frp <- spp_data_frp[,c(9,11)]
spp_data_frp <- spp_data_frp %>% 
  rownames_to_column(var = "Taxa")
colnames(spp_data_frp)
spp_data_frp <- spp_data_frp %>% 
  rename(Taxa = Taxa,
         FRP10 = X10.,
         FRP90 = X90.)

head(spp_data_frp)

#SAL (8)
spp_data_sal <- glades.titan_SAL$sppmax
spp_data_sal <- data.frame(spp_data_sal)

colnames(spp_data_sal)#Only need spp, change point 10 and 90
spp_data_sal <- spp_data_sal[,c(9,11)]
spp_data_sal <- spp_data_sal %>% 
  rownames_to_column(var = "Taxa")
colnames(spp_data_sal)
spp_data_sal <- spp_data_sal %>% 
  rename(Taxa = Taxa,
         SAL10 = X10.,
         SAL90 = X90.)

head(spp_data_sal)
#TURB (9)
spp_data_turb <- glades.titan_TURB$sppmax
spp_data_turb <- data.frame(spp_data_turb)

colnames(spp_data_turb)#Only need spp, change point 10 and 90
spp_data_turb <- spp_data_turb[,c(9,11)]
spp_data_turb <- spp_data_turb %>% 
  rownames_to_column(var = "Taxa")
colnames(spp_data_turb)
spp_data_turb <- spp_data_turb %>% 
  rename(Taxa = Taxa,
         TURB10 = X10.,
         TURB90 = X90.)

head(spp_data_turb)

#SIL (10)
spp_data_SIL <- glades.titan_SIL$sppmax
spp_data_SIL <- data.frame(spp_data_SIL)

colnames(spp_data_SIL)#Only need spp, change point 10 and 90
spp_data_SIL <- spp_data_SIL[,c(9,11)]
spp_data_SIL <- spp_data_SIL %>% 
  rownames_to_column(var = "Taxa")
colnames(spp_data_SIL)
spp_data_SIL <- spp_data_SIL %>% 
  rename(Taxa = Taxa,
         SIL10 = X10.,
         SIL90 = X90.)

head(spp_data_SIL)


#EXPORT DATA FOR K-PROTOTYPE CLASSIFICATION WITH 4 MAIN FACTORS  (TN, TP, TNTP, and TEMPERATURE AS THE RESULTS OF MCA AND PCA)
merged_data <- spp_data_TP %>%
  full_join(spp_data_TN, by = "Taxa") %>%
  full_join(spp_data_TN_TP, by = "Taxa") %>%
  full_join(spp_data_TEMP, by = "Taxa") %>%
  full_join(spp_data_AMM, by = "Taxa") %>%
  full_join(spp_data_NIT, by = "Taxa") %>%
  full_join(spp_data_frp, by = "Taxa") %>%
  full_join(spp_data_SIL, by = "Taxa") %>%
  full_join(spp_data_sal, by = "Taxa") %>%
  full_join(spp_data_turb, by = "Taxa") 

data <- na.omit(merged_data)

clustering_data <- data[,c(1:9)]

write.csv(clustering_data,"Clustering_data_4.csv")
write.csv(data,"Clustering_data_all.csv")

save(clustering_data, file = "Clustering_data.RData")

#DONE TITAN ANALYSIS

# UPDATE TITAN WITH Cuspidothrix.issatschenkoi_new and Cylindrotheca.closterium_new (as reviwer suggestion)

library(dplyr)
library(TITAN2)

load("TITAN_data_revised.RData")

data <- data_ori %>% select("Cuspidothrix.issatschenkoi" , "Aphanizomenon.issatschenkoi", "Nitzschia.closterium", "Cylindrotheca.closterium")



colnames(data)

columns_with_chenkoi <- grep("chenkoi", colnames(data), value = TRUE)

# Print the result
columns_with_chenkoi

data$Cuspidothrix.issatschenkoi_new <- data$Aphanizomenon.issatschenkoi+data$Cuspidothrix.issatschenkoi
data$Cylindrotheca.closterium_new <- data$Nitzschia.closterium + data$Cylindrotheca.closterium

data_new <- data %>% select(Cuspidothrix.issatschenkoi_new, Cylindrotheca.closterium_new)
data <- data_new

taxa.TITAN <- data
data_E <- data_ori

TN.TITAN <- data$TN

glades.titan_TN <- titan(TN.TITAN, taxa.TITAN)
save.image("TITAN_TN_new.RData")
#load("TITAN_TN_new.RData")
save(glades.titan_TN, file = "TITAN_TN_revised.RData")

spp <- glades.titan_TN$sppmax

TP.TITAN <- data$TP
glades.titan_TP <- titan(TP.TITAN, taxa.TITAN)

save.image("TITAN_TP_new.RData")
#load("TITAN_TP_new.RData")
save(glades.titan_TP, file = "TITAN_TP_revised.RData")

TNTP.TITAN <- data$TN_TP_Ratio
glades.titan_TNTP <- titan(TNTP.TITAN, taxa.TITAN)


save.image("TITAN_TNTP_new.RData")
#load("TITAN_TNTP_new.RData")
save(glades.titan_TNTP, file = "TITAN_TN_TP_Ratio_revised.RData")

TEMP.TITAN <- data$TEMP

temp_data <- data.frame(TEMP.TITAN, taxa.TITAN)
temp_data_na <- na.omit(temp_data)

TEMP.TITAN <- temp_data_na$TEMP
taxa.TITAN <- temp_data_na[, c(2:3)]

glades.titan_TEMP <- titan(TEMP.TITAN, taxa.TITAN)
save.image("TITAN_TEMP_new.RData")
#load("TITAN_TEMP_new.RData")
save(glades.titan_TEMP, file = "TITAN_TEMP_revised.RData")

# savehistory("TITAN_new_T.Rhistory")

data <- glades.titan_TEMP$sppmax


FRP.TITAN <- data$FRP
frp_data <- data.frame(FRP.TITAN,taxa.TITAN)
frp_data_na <- na.omit(frp_data)

FRP.TITAN <- frp_data_na$FRP
taxa.TITAN <- frp_data_na[, c(2:3)]

glades.titan_FRP <- titan(FRP.TITAN, taxa.TITAN)
save.image("TITAN_FRP_new.RData")
#load("TITAN_FRP_new.RData")
save(glades.titan_FRP, file = "TITAN_FRP_revised.RData")


taxa.TITAN <- data
NIT.TITAN <- data$NIT
NIT_data <- data.frame(NIT.TITAN,taxa.TITAN)
NIT_data_na <- na.omit(NIT_data)

NIT.TITAN <- NIT_data_na$NIT
taxa.TITAN <- NIT_data_na[, c(2:3)]

glades.titan_NIT <- titan(NIT.TITAN, taxa.TITAN)
save.image("TITAN_NIT_new.RData")
#load("TITAN_NIT_new.RData")
save(glades.titan_NIT, file = "TITAN_NIT_revised.RData")


taxa.TITAN <- data
SAL.TITAN <- data$SAL
SAL_data <- data.frame(SAL.TITAN,taxa.TITAN)
SAL_data_na <- na.omit(SAL_data)

SAL.TITAN <- SAL_data_na$SAL
taxa.TITAN <- SAL_data_na[, c(2:3)]

glades.titan_SAL <- titan(SAL.TITAN, taxa.TITAN)
save.image("TITAN_SAL_new.RData")
#load("TITAN_SAL_new.RData")
save(glades.titan_SAL, file = "TITAN_SAL_revised.RData")



taxa.TITAN <- data
AMM.TITAN <- data$AMM
AMM_data <- data.frame(AMM.TITAN,taxa.TITAN)
AMM_data_na <- na.omit(AMM_data)

AMM.TITAN <- AMM_data_na$AMM
taxa.TITAN <- AMM_data_na[, c(2:3)]

glades.titan_AMM <- titan(AMM.TITAN, taxa.TITAN)
save.image("TITAN_AMM_new.RData")
#load("TITAN_AMM_new.RData")
save(glades.titan_AMM, file = "TITAN_AMM_revised.RData")


taxa.TITAN <- data
SIL.TITAN <- data_E$SIL
SIL_data <- data.frame(SIL.TITAN,taxa.TITAN)
SIL_data_na <- na.omit(SIL_data)

SIL.TITAN <- SIL_data_na$SIL
taxa.TITAN <- SIL_data_na[, c(2:3)]

glades.titan_SIL <- titan(SIL.TITAN, taxa.TITAN)
save.image("TITAN_SIL_new.RData")
#load("TITAN_SIL_new.RData")
save(glades.titan_SIL, file = "TITAN_SIL_revised.RData")


taxa.TITAN <- data
TURB.TITAN <- data_E$TURB
TURB_data <- data.frame(TURB.TITAN,taxa.TITAN)
TURB_data_na <- na.omit(TURB_data)

TURB.TITAN <- TURB_data_na$SIL
taxa.TITAN <- TURB_data_na[, c(2:3)]

glades.titan_TURB <- titan(TURB.TITAN, taxa.TITAN)
save.image("TITAN_TURB_new.RData")
#load("TITAN_TURB_new.RData")
save(glades.titan_TURB, file = "TITAN_TURB_revised.RData")

#Then (manually) update the new thresholds of the 2 species into TITAN output file for clustering_data
#Then add trait information to the threshold files.
#The Final data frame can be seen in the next stage (classification) under name: "classification_input"


####

#CHECK TITAN PURITY

library(dplyr)
library(tibble)

# #Load TITAN outputs to chek purity of thresholds
# 
# #load change point of total phosphorus
# load("TITAN_TP.RData")
# 
# #load change point of total TN
# load("TITAN_TN.RData")
# 
# #load change point of TN TP Ratio
# load("TITAN_TN_TP_RATIO.RData")
# #load change point of total TEMP
# load("TITAN_TEMP.RData")
# 
# # # Load new data threshold 
# # load("TITAN_TN_revised.RData")
# # load("TITAN_TP_revised.RData")
# # load("TITAN_TNTP_revised.RData")
# # load("TITAN_TEMP_revised.RData")
# # 
# 
# threshold <- read.csv("Z:/Hawkesbury/Vuong/Package_1/Paper 1/Script/K_prototype_classification_input.csv")
# data_total <- read.csv("Z:/Hawkesbury/Vuong/Package_1/Data_analysis/CLASSIFICATION/CLustering/CLustering_taxa/Clustering/Lolipop/Cluster_cb.csv") %>%
#   filter(!X %in% c("Aphanizomenon.issatschenkoi",  "Nitzschia.closterium"))

#save.image("TITAN_purity_check.RData")


#Load all data inputs for purity check (TITAN outputs)
#load("TITAN_purity_check.RData") 
#check TN

TN_data <- glades.titan_TN$sppmax
TN_data <- data.frame(TN_data)
TN_p <- TN_data$purity

TP_data <- glades.titan_TP$sppmax
TP_data <- data.frame(TP_data)
TP_p <- TP_data$purity


TNTP_data <- glades.titan_TN_TP_Ratio$sppmax
TNTP_data <- data.frame(TNTP_data)
TNTP_p <- TNTP_data$purity

TEMP_data <- glades.titan_TEMP$sppmax
TEMP_data <- data.frame(TEMP_data)
TEMP_p <- TEMP_data$purity


TEMP_data <- rownames_to_column(TEMP_data, var = "Taxa")
taxa <- TEMP_data$Taxa
purity <- data.frame(taxa,TN_p, TP_p, TNTP_p, TEMP_p)


#threshold <- read.csv("K_prototype_classification_input.csv")

taxa_column <- threshold$Taxa
taxa_list <- unique(taxa_column)
print(taxa_list)





filtered_threshold <- purity %>% filter(taxa %in% taxa_list)

taxa_List <- c(
  "T.Anabaena.bergii", 
  "T.Dolichospermum.cf.circinal", 
  "T.Anabaena.bergii", 
  "T.Dolichospermum.cf.circinale", 
  "T.Anabaena.aphanizomenoides", 
  "T.Anabaena.bergii", 
  "T.Aphanizomenonaceae", 
  "T.Dolichospermum.cf.circinale", 
  "T.Dolichospermum.circinale", 
  "T.Radiocystis", 
  "Anagnostidinema", 
  "Anagnostidinema", 
  "Haptophyte", 
  "Monoraphidium.cf", 
  "Peridinoid", 
  "Dimorphococcus", 
  "T.Gymnodinoid", 
  "Prorocentrum.non.toxic"
)


new_purity <- purity %>% filter(!(taxa %in% taxa_List))



new_purity <- new_purity %>%
  filter(!taxa %in% c("Aphanizomenon.issatschenkoi", "Westella.cf", "Rhabdogloea", "Quadrigula", "Nitzschia.closterium"))

new_purity
#write.csv(new_purity, "new_purity_final.csv", row.names = FALSE)


count_purity_scores <- new_purity %>%
  # Apply the counts for each column
  summarise(
    TN_p_above_095 = sum(TN_p > 0.9),
    TN_p_0.6_095 = sum(TN_p >= 0.6 & TN_p <= 0.9),
    TN_p_below_06 = sum(TN_p < 0.6),
    
    TP_p_above_095 = sum(TP_p > 0.9),
    TP_p_0.6_095 = sum(TP_p >= 0.6 & TP_p <= 0.9),
    TP_p_below_06 = sum(TP_p < 0.6),
    
    TNTP_p_above_095 = sum(TNTP_p > 0.9),
    TNTP_p_0.6_095 = sum(TNTP_p >= 0.6 & TNTP_p <= 0.9),
    TNTP_p_below_06 = sum(TNTP_p < 0.6),
    
    TEMP_p_above_095 = sum(TEMP_p > 0.9),
    TEMP_p_0.6_095 = sum(TEMP_p >= 0.6 & TEMP_p <= 0.9),
    TEMP_p_below_06 = sum(TEMP_p < 0.6)
  )




# Filter taxa where all columns have purity scores > 0.95 and all columns > 0.6
filtered_purity1 <- new_purity %>%
  filter(
    #  TN_p > 0.9 & TP_p > 0.9 & TNTP_p > 0.9 & TEMP_p > 0.9  # All scores > 0.95
    TN_p > 0.6 & TP_p > 0.6 & TNTP_p > 0.6 & TEMP_p > 0.6   # All scores > 0.6
  )
# Filter taxa where all columns have purity scores > 0.95 and all columns > 0.6
filtered_purity2 <- new_purity %>%
  filter(
    TN_p > 0.9 & TP_p > 0.9 & TNTP_p > 0.9 & TEMP_p > 0.9  # All scores > 0.95
    #TN_p > 0.6 & TP_p > 0.6 & TNTP_p > 0.6 & TEMP_p > 0.6   # All scores > 0.6
  )


filtered_purity3 <- new_purity %>%
  filter(
    # TN_p > 0.9 & TP_p > 0.9 & TNTP_p > 0.9 & TEMP_p > 0.9  # All scores > 0.95
    TN_p < 0.6 & TP_p < 0.6 & TNTP_p < 0.6 & TEMP_p < 0.6   # All scores > 0.6
  )

# Print the filtered taxa
print(filtered_purity1)
print(filtered_purity2)
print(filtered_purity3)
#where at least three out of the four columns 
filtered_purity <- new_purity %>%
  filter(rowSums(select(., TN_p, TP_p, TNTP_p, TEMP_p) > 0.9) >= 3)

# Print to check
print(filtered_purity)


filtered_purity <- new_purity %>%
  filter(
    rowSums(select(., TN_p, TP_p, TNTP_p, TEMP_p) > 0.9) >= 3 &
      rowSums(select(., TN_p, TP_p, TNTP_p, TEMP_p) > 0.6) == 4
  )

# Print to check
print(filtered_purity)


library(dplyr)
library(ggplot2)
library(gridExtra)

purity <- new_purity
#purity <- read.csv("Z:/Hawkesbury/Vuong/Package_1/Paper 1/Revision/Test_TITAN/new_purity_final.csv")

# Replace text in the 'taxa' column of the 'purity' dataframe
purity$taxa <- gsub("Ach\\.N\\.Athidium", "Achnanthidium", purity$taxa)
purity$taxa <- gsub("AchNAthes", "Achnanthes", purity$taxa)
purity$taxa <- gsub("Coccoid.Blue.Green.Picoplankton", "Blue.green.sp1", purity$taxa)
purity$taxa <- gsub("Green.cell", "Green.algae.sp3", purity$taxa)
#purity$taxa <- sort(purity$taxa)
purity <- purity[order(purity$taxa), ]



# data_total <- read.csv("Z:/Hawkesbury/Vuong/Package_1/Data_analysis/CLASSIFICATION/CLustering/CLustering_taxa/Clustering/Lolipop/Cluster_cb.csv") %>%
#   filter(!X %in% c("Aphanizomenon.issatschenkoi",  "Nitzschia.closterium"))

#data_total$X <- sort(data_total$X)
data_total <- data_total[order(data_total$X), ]


purity_data <- data.frame(data_total, purity)

check <- purity_data[,c("X", "taxa")] #all good
purity_data$X <- factor(purity_data$X, levels = purity_data$X[order(purity_data$Group)])

#Update new thresholds
# Load data threshold new
# load("Z:/Hawkesbury/Vuong/Package_1/Paper 1/Revision/Test_TITAN/TITAN_new.RData")
# load("Z:/Hawkesbury/Vuong/Package_1/Paper 1/Revision/Test_TITAN/TITAN_new_T.RData")
# load("Z:/Hawkesbury/Vuong/Package_1/Paper 1/Revision/Test_TITAN/TITAN_TNTP.RData")

load("TITAN_TN_revised.RData")
load("TITAN_TP_revised.RData")
load("TITAN_TN_TP_Ratio_revised.RData")
load("TITAN_TEMP_revised.RData")

new_TP <- data.frame(glades.titan_TP$sppmax) 
new_TN <- data.frame(glades.titan_TN$sppmax) 
new_TNTP <- data.frame(glades.titan_TNTP$sppmax) 
new_TEMP <- data.frame(glades.titan_TEMP$sppmax) 
#add TP
purity_data$TP10[purity_data$X == "Cuspidothrix.issatschenkoi"] <- 0.02
purity_data$TP90[purity_data$X == "Cuspidothrix.issatschenkoi"] <- 0.03

purity_data$TP10[purity_data$X == "Cylindrotheca.closterium"] <- 0.02
purity_data$TP90[purity_data$X == "Cylindrotheca.closterium"] <- 0.031

#add TN
purity_data$TN10[purity_data$X == "Cuspidothrix.issatschenkoi"] <- 0.28
purity_data$TN90[purity_data$X == "Cuspidothrix.issatschenkoi"] <- 0.59

purity_data$TN10[purity_data$X == "Cylindrotheca.closterium"] <- 0.35
purity_data$TN90[purity_data$X == "Cylindrotheca.closterium"] <- 0.41

#add TEMP
purity_data$TEMP10[purity_data$X == "Cuspidothrix.issatschenkoi"] <- 23.3
purity_data$TEMP90[purity_data$X == "Cuspidothrix.issatschenkoi"] <- 26.6

purity_data$TEMP10[purity_data$X == "Cylindrotheca.closterium"] <- 23.4
purity_data$TEMP90[purity_data$X == "Cylindrotheca.closterium"] <- 28.855

#add TNTP
purity_data$TNTP10[purity_data$X == "Cuspidothrix.issatschenkoi"] <- 32.72
purity_data$TNTP90[purity_data$X == "Cuspidothrix.issatschenkoi"] <- 48.24

purity_data$TNTP10[purity_data$X == "Cylindrotheca.closterium"] <- 31.37
purity_data$TNTP90[purity_data$X == "Cylindrotheca.closterium"] <- 43.235
# Update name
purity_data$X <- gsub("Aphanothece", "Anathece ", purity_data$taxa)
purity_data$X <- gsub("Anabaena", "Dolichospermum", purity_data$taxa)

#purity_data$Group <- sort(purity_data$Group )
#
P_TNTP <- ggplot(purity_data) +
  geom_segment( aes(x=X, xend=X, y=TN_TP10, yend=TN_TP90, col = TNTP_p), size = 0.75) +
  geom_point( aes(x=X, y=TN_TP10),  size=1, col = "black" ) +
  geom_point( aes(x=X, y=TN_TP90),  size=1  ,col = "black") +
  # geom_point( aes(x=X, y=mymean),  size=2 ) +
  coord_flip()+
  # theme_ipsum() +
  xlab("") +
  ylab("TNTP")+
  # theme_bw()+
  theme(legend.position="none")+
  scale_y_continuous(limits = c(0, 400), breaks = c(0, 200, 400)) +   # Set y-axis limits
  theme(legend.position = "none",
        axis.text.y = element_text(colour = "black", size = 5),  # Adjust size as needed
        axis.text.x = element_text(colour = "black")) +
  scale_x_discrete(labels = function(x) gsub(".* - ", "", x))+  # Modify x-axis labels+
  scale_color_gradient(low = "blue", high = "red", name = "Purity")  # Set legend title to 'Purity'

P_TNTP
P_TP <- ggplot(purity_data) +
  geom_segment( aes(x=X, xend=X, y=TP10, yend=TP90, col = TP_p), size = 0.75) +
  geom_point( aes(x=X, y=TP10),  size=1 , col = "black") +
  geom_point( aes(x=X, y=TP90),  size=1 ,col = "black") +
  # geom_point( aes(x=X, y=mymean),  size=2 ) +
  coord_flip()+
  # theme_ipsum() +
  xlab("") +
  ylab("TP (mg/L)")+
  # theme_bw()+
  theme(legend.position="none") +
  
  scale_y_continuous(limits = c(0, 0.2), breaks = c(0, 0.1, 0.2)) +   # Set y-axis limits
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black")) +
  scale_x_discrete(labels = function(x) gsub(".* - ", "", x))+
  scale_color_gradient(low = "blue", high = "red", name = "Purity")


P_TP


P_TN <- ggplot(purity_data) +
  geom_segment( aes(x=X, xend=X, y=TN10, yend=TN90, col = TN_p), size = 0.75) +
  geom_point( aes(x=X, y=TN10),  size=1, col = "black" ) +
  geom_point( aes(x=X, y=TN90),  size=1 , col = "black") +
  # geom_point( aes(x=X, y=mymean),  size=2 ) +
  coord_flip()+
  # theme_ipsum() +
  xlab("") +
  ylab("TN (mg/L)")+
  # theme_bw()+
  theme(legend.position="none") +
  
  scale_y_continuous(limits = c(0, 4), breaks = c(0, 2, 4)) +   # Set y-axis limits
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black")) +
  scale_x_discrete(labels = function(x) gsub(".* - ", "", x))+
  scale_color_gradient(low = "blue", high = "red", name = "Purity")

P_TN


P_TEMP <- ggplot(purity_data) +
  geom_segment( aes(x=X, xend=X, y=TEMP10, yend=TEMP90, col = TEMP_p), size = 0.75) +
  geom_point( aes(x=X, y=TEMP10),  size=1, col = "black" ) +
  geom_point( aes(x=X, y=TEMP90),  size=1 , col = "black") +
  # geom_point( aes(x=X, y=mymean),  size=2 ) +
  coord_flip()+
  # theme_ipsum() +
  xlab("") +
  ylab(expression(paste("TEMP ("^"o","C)")))+
  # theme_bw()+
  scale_y_continuous(limits = c(0, 40), breaks = c(0, 20, 40)) +   # Set y-axis limits
  theme(legend.position = "right",
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black")) +
  scale_x_discrete(labels = function(x) gsub(".* - ", "", x)) +
  scale_color_gradient(low = "blue", high = "red", name = "Purity")

P_TEMP
# Combine all plots using grid.arrange or patchwork
p <- (P_TNTP | P_TP | P_TN | P_TEMP) + plot_layout(ncol = 4, widths = unit(rep(1, 4), "null"), heights = unit(rep(1, 1), "null")) &
  theme(plot.margin = margin(0, 0.4, 0, 0, "cm"))  # Adjust margins to reduce gaps 
p 

# tiff("Figure_6_Purity.tiff", width = 183, height = 240, units = "mm", res = 1000)
# print(p)
# dev.off()


# NO BLACK POINT: 

# TNTP plot
P_TNTP <- ggplot(purity_data) +
  geom_segment(aes(x = X, xend = X, y = TN_TP10, yend = TN_TP90, col = TNTP_p), size = 0.75) +
  geom_point(aes(x = X, y = TN_TP10, col = TNTP_p), size = 1) +
  geom_point(aes(x = X, y = TN_TP90, col = TNTP_p), size = 1) +
  coord_flip() +
  xlab("") +
  ylab("TNTP") +
  scale_y_continuous(limits = c(0, 400), breaks = c(0, 200, 400)) +
  theme(legend.position = "none",
        axis.text.y = element_text(colour = "black", size = 5),
        axis.text.x = element_text(colour = "black")) +
  scale_x_discrete(labels = function(x) gsub(".* - ", "", x)) +
  scale_color_gradient(low = "blue", high = "red", name = "Purity")

# TP plot
P_TP <- ggplot(purity_data) +
  geom_segment(aes(x = X, xend = X, y = TP10, yend = TP90, col = TP_p), size = 0.75) +
  geom_point(aes(x = X, y = TP10, col = TP_p), size = 1) +
  geom_point(aes(x = X, y = TP90, col = TP_p), size = 1) +
  coord_flip() +
  xlab("") +
  ylab("TP (mg/L)") +
  scale_y_continuous(limits = c(0, 0.2), breaks = c(0, 0.1, 0.2)) +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black")) +
  scale_x_discrete(labels = function(x) gsub(".* - ", "", x)) +
  scale_color_gradient(low = "blue", high = "red", name = "Purity")

# TN plot
P_TN <- ggplot(purity_data) +
  geom_segment(aes(x = X, xend = X, y = TN10, yend = TN90, col = TN_p), size = 0.75) +
  geom_point(aes(x = X, y = TN10, col = TN_p), size = 1) +
  geom_point(aes(x = X, y = TN90, col = TN_p), size = 1) +
  coord_flip() +
  xlab("") +
  ylab("TN (mg/L)") +
  scale_y_continuous(limits = c(0, 4), breaks = c(0, 2, 4)) +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black")) +
  scale_x_discrete(labels = function(x) gsub(".* - ", "", x)) +
  scale_color_gradient(low = "blue", high = "red", name = "Purity")

# TEMP plot
P_TEMP <- ggplot(purity_data) +
  geom_segment(aes(x = X, xend = X, y = TEMP10, yend = TEMP90, col = TEMP_p), size = 0.75) +
  geom_point(aes(x = X, y = TEMP10, col = TEMP_p), size = 1) +
  geom_point(aes(x = X, y = TEMP90, col = TEMP_p), size = 1) +
  coord_flip() +
  xlab("") +
  ylab(expression(paste("TEMP ("^"o","C)"))) +
  scale_y_continuous(limits = c(0, 40), breaks = c(0, 20, 40)) +
  theme(legend.position = "right",
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black")) +
  scale_x_discrete(labels = function(x) gsub(".* - ", "", x)) +
  scale_color_gradient(low = "blue", high = "red", name = "Purity")

# Combine all plots using grid.arrange or patchwork
library(patchwork)
p <- (P_TNTP | P_TP | P_TN | P_TEMP) +
  plot_layout(ncol = 4) &
  theme(plot.margin = margin(0, 0.4, 0, 0, "cm"))  # Adjust margins to reduce gaps

p

