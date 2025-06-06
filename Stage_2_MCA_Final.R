
#Stage 2 -  Multi-correlation analysis (MCA)

library(jpeg)
library(tidyr)
library(ggplot2)
library(zoo)
library(corrplot)
library(tidyverse)


#MULTICORRELATION ANALYSIS
# Load data for MCA. Data include water quality and  daily total biovolume of phytoplankton.


#load("MCA_data.RData") 

d_mean <- read.csv("MCA_data.csv")
# Values in d_mean is the mean of water quality variables and biovolume in every quater from 2006 to 2018.

cor_matrix <- cor(d_mean, use = "complete.obs") 

# cor_matrix
# TN          TP        TNTP         SAL        TEMP         AMM        NIT        FRP        TURB          SIL         CHLA         VOL
# TN    1.00000000  0.57593757  0.06960831  0.37319253 -0.58497249 -0.24406288  0.5706438  0.5334384 -0.03389155  0.355229986  0.627800557  0.47038466
# TP    0.57593757  1.00000000 -0.65810732  0.03107652 -0.08929313 -0.04191268 -0.1488760  0.8780919  0.24230171  0.348385721  0.701123948  0.62014640
# TNTP  0.06960831 -0.65810732  1.00000000  0.36892708 -0.36912727 -0.20813743  0.5486262 -0.5691135 -0.39702995 -0.215581688 -0.347944238 -0.36972584
# SAL   0.37319253  0.03107652  0.36892708  1.00000000 -0.22899744 -0.25039717  0.4116539 -0.0664994 -0.42373209  0.270853928  0.039715659 -0.07818047
# TEMP -0.58497249 -0.08929313 -0.36912727 -0.22899744  1.00000000  0.27329826 -0.6198670 -0.2450715  0.08989276 -0.054543323 -0.251582615 -0.08247565
# AMM  -0.24406288 -0.04191268 -0.20813743 -0.25039717  0.27329826  1.00000000 -0.2572013 -0.1588657  0.12290793 -0.276984480 -0.155110620 -0.04977617
# NIT   0.57064383 -0.14887601  0.54862619  0.41165385 -0.61986698 -0.25720127  1.0000000 -0.1260731 -0.31908144  0.357967979 -0.215960045 -0.34904394
# FRP   0.53343837  0.87809186 -0.56911351 -0.06649940 -0.24507149 -0.15886567 -0.1260731  1.0000000  0.14283858  0.267743678  0.708275503  0.59416207
# TURB -0.03389155  0.24230171 -0.39702995 -0.42373209  0.08989276  0.12290793 -0.3190814  0.1428386  1.00000000 -0.010883598  0.249687557  0.31519794
# SIL   0.35522999  0.34838572 -0.21558169  0.27085393 -0.05454332 -0.27698448  0.3579680  0.2677437 -0.01088360  1.000000000  0.009377786 -0.15188418
# CHLA  0.62780056  0.70112395 -0.34794424  0.03971566 -0.25158262 -0.15511062 -0.2159600  0.7082755  0.24968756  0.009377786  1.000000000  0.93206144
# VOL   0.47038466  0.62014640 -0.36972584 -0.07818047 -0.08247565 -0.04977617 -0.3490439  0.5941621  0.31519794 -0.151884183  0.932061440  1.00000000

#save plot
tiff("Figure_4_a.tiff", width = 120, height = 120, units = "mm", res = 2000)

# Set the text size for axis labels and tick marks
par(cex.lab = 0.5, cex.axis = 0.5) # Adjust these values as needed for your desired size

# Generate the plot
corrplot.mixed(cor(d_mean),
               lower = "number",
               upper = "circle",
               tl.col = "black",
               tl.pos = "lt", 
               mar = c(1, 1, 1, 1),
               # cl.cex = 0.8,
               # cl.ratio = 0.15, 
               number.cex = 0.7,
               # number.font = 1
)

# Close the TIFF device
dev.off()