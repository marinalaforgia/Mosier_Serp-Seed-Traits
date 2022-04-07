#### Seed Analysis for CBS Undergraduate Research Conference #### 

rm(list=ls())

#library load
library(tidyverse)
library(ggplot2)

#Data
Traits<-read.csv("Data/20220329_Seed-Traits_cleaning.csv")
Cover<-read.csv("Data/Core_All-Years_2021.csv")
Species<-read.csv("Data/McL_80SitesSpeciesTraits_012615.csv")
SB<-read.csv("Data/Seedbank-Data_2015 - Seedbank_WateringExp2016_CLEAN.csv")
Plots<-read.csv("Data/WE_Treatment.csv")
#Plot 70 came out as 7#

#### Joining Tables ####

SB_joined <- left_join(SB,Plots, by = "Plot") %>% select(Plot, Species,Serpentine)

SB_Traits_joined <- left_join(Traits,SB_joined, by = "Species")

#Just examining data to understand things
Avena_fatua <- SB_joined %>% filter(Species == "Avena fatua")
