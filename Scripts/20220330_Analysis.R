#### Seed Analysis for CBS Undergraduate Research Conference #### 

rm(list=ls())

#library load
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(vegan)
library(sjstats)

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

Traits$shape.final.log <- log(Traits$shape.final)
Traits$set.time.mpsec.sqrt <- sqrt(Traits$set.time.mpsec)
Traits$mass.morph.mg.log <- log(Traits$mass.morph.mg)
Traits$wing.loading.log <- log(Traits$wing.loading)
Traits$ldd.sqrt <- sqrt(Traits$ldd)

# Summarize by species
SB.sum <- ddply(SB, .(Plot, Species), summarize, n.seeds = sum(n_seedlings, na.rm = T)) # removes two instances of NA

SB_joined <- left_join(SB.sum, Plots, by = "Plot") %>% select(Plot, Species, Serpentine, n.seeds)

SB_Traits_joined <- merge(SB_joined, Traits[,-c(1,4,5,6,9,10,11,13,16)],  by.x = "Species", by.y = "Species_Name_J12")


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


####Barplots####

#Basic Boxplot
library(Rmisc)

Boxplot <- ggplot(data = Plot.cwm, mapping = aes(x = Serpentine, y = shape.final.cwm)) + geom_boxplot()
Boxplot

ggplot(data = nat.inv.cwm, mapping = aes(x = Serpentine, y = shape.final.cwm, col = nat.inv)) + 
  geom_boxplot() +
  facet_wrap(~group)
  

#Stacked Barplot


stack.barplot <- ggplot(data = Plot.cwm, mapping = aes(x = Plot, y = shape.final.cwm, fill = Serpentine)) + geom_bar(stat="identity")
stack.barplot

# Bar plot
nat.inv.cwm.sum <- nat.inv.cwm %>% 
#  filter(nat.inv.cwm, !(nat.inv == "native" & grass.forb == "grass")) %>%
  group_by(nat.inv, group, Serpentine) %>%
  summarize(
    shape.cwm = mean(shape.final.cwm),
    shape.cwm.se = parameters::standard_error(shape.final.cwm),
    wing.cwm = mean(wing.loading.cwm),
    wing.cwm.se = parameters::standard_error(wing.loading.cwm),
    ldd.cwm = mean(ldd.cwm),
    ldd.cwm.se = parameters::standard_error(ldd.cwm),
    height.cwm = mean(height.cm.cwm),
    height.cwm.se = parameters::standard_error(height.cm.cwm),
    mass.cwm = mean(mass.morph.mg.cwm),
    mass.cwm.se = parameters::standard_error(mass.morph.mg.cwm),
    set.time.cwm = mean(set.time.mpsec.cwm),
    set.time.cwm.se = parameters::standard_error(set.time.mpsec.cwm),
  )

nat.inv.cwm.sum$fun.group <- paste(nat.inv.cwm.sum$nat.inv, nat.inv.cwm.sum$group)
nat.inv.cwm.sum <- filter(nat.inv.cwm.sum, fun.group != "native grass")

ggplot(nat.inv.cwm.sum, aes(x = Serpentine, y = mass.cwm, fill = fun.group)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mass.cwm - mass.cwm.se, ymax = mass.cwm + mass.cwm.se), position = position_dodge(.9), width = 0.2) +
  theme_bw() +
  theme(
    legend.title = element_blank()
  ) +
  labs(y = "CWM Wing Loading (mass/area)")
  
ggplot(nat.inv.cwm.sum, aes(x = Serpentine, y = wing.cwm, fill = fun.group)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = wing.cwm - wing.cwm.se, ymax = wing.cwm + wing.cwm.se), position = position_dodge(.9), width = 0.2)
  
ggplot(nat.inv.cwm.sum, aes(x = Serpentine, y = wing.cwm, fill = fun.group)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = wing.cwm - wing.cwm.se, ymax = wing.cwm + wing.cwm.se), position = position_dodge(.9), width = 0.2)

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

#ordiellipse(SB.sum.nmds, groups = SB.sum.wide$Serpentine, draw = "polygon", col = c("red", "green", "blue"), label = T)

ordihull(SB.sum.nmds, groups = SB.sum.wide$Serpentine, draw = "polygon", col = c("red", "green", "blue"), label = T)

env <- SB.sum.wide[,c(95:100)]
colnames(env) <- c("Shape", "Settling Time", "Mass", "Wing Loading", "Height", "LDD")
en <- envfit(SB.sum.nmds, env, permutations = 9999)
plot(en)
