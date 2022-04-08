#### Seed Analysis for CBS Undergraduate Research Conference #### 

rm(list=ls())

#library load
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)

#Data
Traits <- read.csv("Data/20220329_Seed-Traits_cleaning.csv")
Cover <- read.csv("Data/Core_All-Years_2021.csv")
Species <- read.csv("Data/McL_80SitesSpeciesTraits_012615.csv")
SB <- read.csv("Data/Seedbank_WateringExp2016_CLEAN.csv")
Plots <- read.csv("Data/WE_Treatment.csv") 

#### Prepping Data ####


SB.Cov <- c(unique(SB$Species), unique(Cover$Species_Name_J12))

Traits <- merge(Species[,1:2], Traits, by.x = "Species_Name", by.y = "Species")

Traits <- Traits[which(Traits$Species_Name_J12 %in% SB.Cov),]

write.csv(Traits, "Data/Trait-subset.csv", row.names = F)

Traits <- read.csv("Data/Trait-subset.csv")
Traits <- Traits[Traits$Use == "use",]

# Summarize by species
SB.sum <- ddply(SB, .(Plot, Species), summarize, n.seeds = sum(n_seedlings))

SB_joined <- left_join(SB.sum, Plots, by = "Plot") %>% select(Plot, Species, Serpentine, n.seeds)

SB_Traits_joined <- merge(SB_joined, Traits[,-c(1,3,4,5,6,9,10,11,13,16)],  by.x = "Species", by.y = "Species_Name")

SB_Traits_joined$Serpentine <- ifelse(SB_Traits_joined$Plot %in% c(1,2,7:14,21,41:46,84), 
                               "HS", # Harsh serpentine
                               ifelse(SB_Traits_joined$Serpentine == "N",
                                      SB_Traits_joined$Serpentine,
                                      "LS")) # Lush serpentine



#### To do ###
# 1. Draw graphs!
# 2. Try to recreate graphs with ggplot2 and ASK marina when you get confused :D
# 3. CWM
 ### Sum all individuals in a plot total seeds
 ### divide number of seeds for a specific species by total (RA)
 ### multiple trait by that RA
 ### take the mean of the plot, functional group (native, invasive, forb, grass, NF, IG)
# Community level ordinations by soil type 
pca <- prcomp(df, scale = T) # only includes quantitative data
summary(pca)
biplot(pca)

#split graph by serpentine type and color by forbs/grass/invasive/natives

### 
