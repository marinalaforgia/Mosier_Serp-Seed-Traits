#### Seed Analysis for CBS Undergraduate Research Conference #### 

rm(list=ls())

#library load
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(devtools)
install_github("vqv/ggbiplot", force = T)

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

SB_Traits_joined <- merge(SB_joined, Traits[,-c(1,4,5,6,9,10,11,13,16)],  by.x = "Species", by.y = "Species_Name_J12")

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
nat.inv.cwm <- SB_Traits_joined %>% group_by(nat.inv, Plot) %>% summarize(
  shape.final.cwm = weighted.mean(shape.final, n.seeds),
  set.time.mpsec.cwm = weighted.mean(set.time.mpsec, n.seeds),
  mass.morph.mg.cwm = weighted.mean(mass.morph.mg, n.seeds),
  wing.loading.cwm = weighted.mean(wing.loading, n.seeds),
  height.cm.cwm = weighted.mean(height.cm, n.seeds),
  ldd.cwm = weighted.mean(ldd, n.seeds)
)

nat.inv.cwm <- merge(nat.inv.cwm, Plots[,c(1,3)], by = "Plot")

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
hist(log(Plot.cwm$shape.final.cwm))
hist(sqrt(Plot.cwm$set.time.mpsec.cwm))
hist(log(Plot.cwm$mass.morph.mg.cwm))
hist(log(Plot.cwm$wing.loading.cwm))
hist(Plot.cwm$height.cm.cwm)
hist(sqrt(Plot.cwm$ldd.cwm))

Plot.cwm$shape.final.cwm.log <- log(Plot.cwm$shape.final.cwm)
Plot.cwm$set.time.mpsec.cwm.sqrt <- sqrt(Plot.cwm$set.time.mpsec.cwm)
Plot.cwm$mass.morph.mg.cwm.log <- log(Plot.cwm$mass.morph.mg.cwm)
Plot.cwm$wing.loading.cwm.log <- log(Plot.cwm$wing.loading.cwm)
Plot.cwm$ldd.cwm.sqrt <- sqrt(Plot.cwm$ldd.cwm)

Plot.pca <- prcomp(Plot.cwm[,c(6,9:13)], scale = TRUE)

summary(Plot.pca)
biplot(Plot.pca)

####_ggbiplot####
Plot.serp <- Plot.cwm$Serpentine

ggbiplot(Plot.pca, obs.scale = 1, var.scale = 1,
         groups = Plot.serp)

Plot.cwm <- cbind(Plot.cwm, Plot.pca$x[,1:2])

autoplot(Plot.pca, data = Plot.cwm, loadings = T, loadings.label = T, label = F, loadings.label.vjust = -1.5, loadings.label.hjust = .6, col = 'appendages', shape = 'group') +
  theme_classic()  

####_factoextra####

fviz_pca_var(Plot.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



####Barplots####

#Basic Barplot
library(Rmisc)

Barplot <- ggplot(data = Plot.cwm, mapping = aes(x = Serpentine, y = shape.final.cwm)) + geom_boxplot()
Barplot

ggplot(data = nat.inv.cwm, mapping = aes(x = Serpentine, y = shape.final.cwm, col = nat.inv)) + geom_boxplot()

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
