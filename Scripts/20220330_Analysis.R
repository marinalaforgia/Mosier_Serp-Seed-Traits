#### Seed Analysis for CBS Undergraduate Research Conference #### 

rm(list=ls())

#library load
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggfortify) # ggbiplot is in this package!
library(vegan)

#Data
Traits <- read.csv("Data/20220418_Seed-Traits_cleaning.csv")
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


Traits <- read.csv("Data/Trait-subset_UPDATED.csv")
Traits <- Traits[Traits$Use == "use",]
Traits$fungroup <- paste(Traits$nat.inv, Traits$group)

Traits[which(is.na(Traits$ldd)),]$ldd <- 1 # reasonable assumption based on glance at the species

# Summarize by species
SB.sum <- ddply(SB, .(Plot, Species), summarize, n.seeds = sum(n_seedlings, na.rm = T)) # removes two instances of NA

SB_joined <- left_join(SB.sum, Plots, by = "Plot") %>% select(Plot, Species, Serpentine, n.seeds)

SB_Traits_joined <- merge(SB_joined, Traits[,-c(1,4,5,6,9,10,11,13,16)],  by.x = "Species", by.y = "Species_Name_J12")

#Remove NA data, I dont think this is necessary after the above changes
#SB_Traits_joined <- SB_Traits_joined[complete.cases(SB_Traits_joined),]


####CWM####

####_Plot####
Plot.cwm <- SB_Traits_joined %>% group_by(Plot)  %>% summarize(
  shape.final.cwm = weighted.mean(shape.final, n.seeds),
  set.time.mpsec.cwm = weighted.mean(set.time.mpsec, n.seeds),
  mass.morph.mg.cwm = weighted.mean(mass.morph.mg, n.seeds),
  wing.loading.cwm = weighted.mean(wing.loading, n.seeds),
  height.cm.cwm = weighted.mean(height.cm, n.seeds),
  ldd.cwm = weighted.mean(ldd, n.seeds)
)

Plot.cwm <- merge(Plot.cwm, Plots[,c(1,3)], by = "Plot")

####_Native/Invasive####
nat.inv.cwm <- SB_Traits_joined %>% group_by(nat.inv, Plot, group) %>% summarize(
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

####_Traits####
Traits$shape.final.log <- log(Traits$shape.final)
Traits$set.time.mpsec.sqrt <- sqrt(Traits$set.time.mpsec)
Traits$mass.morph.mg.log <- log(Traits$mass.morph.mg)
Traits$wing.loading.log <- log(Traits$wing.loading)
Traits$ldd.sqrt <- sqrt(Traits$ldd)

Traits.pca <- prcomp(Traits[,c(18,22:26)], scale = TRUE)
biplot(Traits.pca)


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

# maybe try plotting pc3 and pc4
summary(Plot.pca)
biplot(Plot.pca)
Plot.cwm <- cbind(Plot.cwm, Plot.pca$x)

autoplot(Plot.pca, data = Plot.cwm, loadings = T, loadings.label = T, label = F, loadings.label.vjust = -1.5, loadings.label.hjust = .6, col = 'Serpentine') +
  theme_classic() 

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

ggplot(data = nat.inv.cwm, mapping = aes(x = Serpentine, y = shape.final.cwm, col = nat.inv)) + 
  geom_boxplot() +
  facet_wrap(~group)
  

#Stacked Barplot


stack.barplot <- ggplot(data = Plot.cwm, mapping = aes(x = Plot, y = shape.final.cwm, fill = Serpentine)) + geom_bar(stat="identity")
stack.barplot

#### NMDS plot ####
plot.cwm.nmds <- metaMDS(Plot.cwm[,2:7], k = 3) 

stressplot(plot.cwm.nmds)
ordiplot(plot.cwm.nmds, type="n")

ordihull(plot.cwm.nmds, groups = Plot.cwm$Serpentine, draw = "polygon", col = c("red", "green", "blue"), label = T)

# other way of doing NMDS (perhaps more correct)
SB.sum.wide <- pivot_wider(SB_joined, names_from = Species, values_from = n.seeds)
SB.sum.wide[is.na(SB.sum.wide)] <- 0
SB.sum.wide <- merge(SB.sum.wide, Plot.cwm[,c(1:7)], by = "Plot")

SB.sum.nmds <- metaMDS(SB.sum.wide[,3:94], k = 3, trymax = 100) 
stressplot(SB.sum.nmds)

ordiplot(SB.sum.nmds)

ordihull(SB.sum.nmds, groups = SB.sum.wide$Serpentine, draw = "polygon", col = c("red", "green", "blue"), label = T)

env <- SB.sum.wide[,c(95:100)]
en <-envfit(SB.sum.nmds, env, permutations = 9999)
plot(en)
