#### Seed Analysis for CBS Undergraduate Research Conference #### 

rm(list=ls())

#library load
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(devtools)
install_github("vqv/ggbiplot")

#Data
Traits <- read.csv("Data/20220329_Seed-Traits_cleaning.csv")
Cover <- read.csv("Data/Core_All-Years_2021.csv")
Species <- read.csv("Data/McL_80SitesSpeciesTraits_012615.csv")
#SB <- read.csv("Data/Seedbank-Data_2015 - Seedbank_WateringExp2016_CLEAN.csv")
SB <- read.csv("Data/Seedbank_WateringExp2016_CLEAN.csv") # Marina's file path
Plots <- read.csv("Data/WE_Treatment.csv") 


#### Prepping Data ####

Plots$Serpentine <- ifelse(Plots$Plot %in% c(1,2,7:14,21,41:46,84), 
                               "HS",
                               ifelse(Plots$Serpentine == "N",
                                      Plots$Serpentine,
                                      "LS"))

SB.Cov <- c(unique(SB$Species), unique(Cover$Species_Name_J12))

Traits <- merge(Species[,1:2], Traits, by.x = "Species_Name", by.y = "Species")

Traits <- Traits[which(Traits$Species_Name_J12 %in% SB.Cov),]
##is any of this code above necessary to keep still?

Traits <- read.csv("Data/Trait-subset.csv")
Traits <- Traits[Traits$Use == "use",]

# Summarize by species
SB.sum <- ddply(SB, .(Plot, Species), summarize, n.seeds = sum(n_seedlings))

SB_joined <- left_join(SB.sum, Plots, by = "Plot") %>% select(Plot, Species, Serpentine, n.seeds)

SB_Traits_joined <- merge(SB_joined, Traits[,-c(1,3,4,5,6,9,10,11,13,16)],  by.x = "Species", by.y = "Species_Name")

#Remove NA data
SB_Traits_joined <- SB_Traits_joined[complete.cases(SB_Traits_joined),]


####CWM####

####_Plot####
Plot.cwm <- SB_Traits_joined            %>% group_by(Plot)                     %>% summarize(
  shape.final.cwm = weighted.mean(shape.final, n.seeds),
  set.time.mpsec.cwm = weighted.mean(set.time.mpsec, n.seeds),
  mass.morph.mg.cwm = weighted.mean(mass.morph.mg, n.seeds),
  wing.loading.cwm = weighted.mean(wing.loading, n.seeds),
  height.cm.cwm = weighted.mean(height.cm, n.seeds),
  ldd.cwm = weighted.mean(ldd, n.seeds)
)

Plot.cwm <- merge(Plot.cwm, Plots[,c(1,3)], by = "Plot")

####_Native/Invasive####
nat.inv.cwm <- SB_Traits_joined %>% group_by(nat.inv) %>% summarize(
  shape.final.cwm = weighted.mean(shape.final, n.seeds),
  set.time.mpsec.cwm = weighted.mean(set.time.mpsec, n.seeds),
  mass.morph.mg.cwm = weighted.mean(mass.morph.mg, n.seeds),
  wing.loading.cwm = weighted.mean(wing.loading, n.seeds),
  height.cm.cwm = weighted.mean(height.cm, n.seeds),
  ldd.cwm = weighted.mean(ldd, n.seeds)
)

####_Forb/Grass####
group.cwm <- SB_Traits_joined %>% group_by(group) %>% summarize(
  shape.final.cwm = weighted.mean(shape.final, n.seeds),
  set.time.mpsec.cwm = weighted.mean(set.time.mpsec, n.seeds),
  mass.morph.mg.cwm = weighted.mean(mass.morph.mg, n.seeds),
  wing.loading.cwm = weighted.mean(wing.loading, n.seeds),
  height.cm.cwm = weighted.mean(height.cm, n.seeds),
  ldd.cwm = weighted.mean(ldd, n.seeds)
)

####PCA####

#Library Load
library(factoextra)
library(ggbiplot)

#PCA
Plot.pca <- prcomp(Plot.cwm[,c(1:7)], scale = TRUE)

summary(Plot.pca)
biplot(Plot.pca)

####_ggbiplot####
Plot.serp<- Plot.cwm$Serpentine

ggbiplot(Plot.pca, obs.scale = 1, var.scale = 1,
         groups = Plot.serp)

####_factoextra####

fviz_pca_var(Plot.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



####Barplots####

#Basic Barplot
Barplot <- ggplot(data = Plot.cwm, mapping = aes(x = Serpentine, y = shape.final.cwm)) + geom_bar(stat="identity")
Barplot

#Stacked Barplot


stack.barplot <- ggplot(data = Plot.cwm, mapping = aes(x = Plot, y = shape.final.cwm, fill = Serpentine)) + geom_bar(stat="identity")
stack.barplot



#### To do ####
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
