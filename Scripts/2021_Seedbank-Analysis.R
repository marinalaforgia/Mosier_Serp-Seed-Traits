rm(list=ls())
library(plyr)
library(tidyverse)
library(Rmisc)

# these datasets are constant
trait.raw <- read.csv("~//Google Drive/02_McLaughlin_80Sites_Organized/Modified_CombinedFiles/McL_80SitesSpeciesTraits_012615.csv") # traits not averaged across soil types, instead pull out trait by soil type depending on the soil that plot is on
trait.raw <- filter(trait.raw, Annual.Perennial != "Perennial")
trait.raw$fungroup <- paste(trait.raw$Native.Exotic, trait.raw$Annual.Perennial, trait.raw$Grass.Forb.Shrub)

treat <- read.csv("Data/Treatment.csv")[,c(1:3)]
sb <- read.csv("Data/Seed-bank/Seedbank_WateringExp2016_CLEAN.csv")

sb <- merge(sb, treat, by = "Plot")


sb <- filter(sb, notes != "Stopped counting second year")
sb <- filter(sb, notes != "perennial")
sb <- filter(sb, notes != "weed")
sb <- filter(sb, notes != "dead")

sb <- merge(sb, trait.raw[,c(2,23)], by.x = "Species", by.y = "Species_Name_J12")

sb.sum <- ddply(sb, .(Plot, Treatment, Serpentine, fungroup), summarize, seeds = sum(n_seedlings))

sb.sum.ls <- sb.sum[!(sb.sum$seeds < 10 & sb.sum$fungroup == "Exotic Annual Grass"),]

ggplot(sb.sum, aes(x = fungroup, y = seeds)) +
  geom_boxplot() +
  facet_wrap(~Serpentine)

sb.sp.sum <- ddply(sb, .(Plot, Treatment, Serpentine, Species), summarize, seeds = sum(n_seedlings))


ggplot(sb.sum.ls, aes(x = fungroup, y = seeds)) +
  geom_boxplot() +
  facet_wrap(~Serpentine)

core.15 <- read.csv("Data/2015/Core-Plot-2015.csv", strip.white = T)   
core.15.l <- reshape(core.15, varying = colnames(core.15)[2:ncol(core.15)], v.names = "Cover", timevar = "Plot", times = colnames(core.15)[2:ncol(core.15)], direction = "long")[,1:3]
colnames(core.15.l)[1] <- "Species_Name"
core.15.l$Plot <- gsub("\\X", "", core.15.l$Plot)
core.15.l$Year <- 2015
core.15.l <- filter(core.15.l, Cover != 0)
core.15.l <- core.15.l[,c(4,2,1,3)]
core.15.l <- merge(core.15.l, treat, by = "Plot")

core.15.l <- merge(core.15.l, trait.raw[,c(2,23)], by.x = "Species_Name", by.y = "Species_Name_J12")

core.15.l.sum <- ddply(core.15.l, .(Plot, Treatment, Serpentine, fungroup, Year), summarize, Cover = sum(Cover))


ggplot(core.15.l.sum, aes(x = fungroup, y = Cover)) +
  geom_boxplot() +
  facet_wrap(~Serpentine)

m1 <- lm(Cover ~ Serpentine, data = core.16.l.sum[core.16.l.sum$fungroup == "Exotic Annual Grass",])
plot(fitted(m1), resid(m1))
qqnorm(resid(m1)) 
qqline(resid(m1), col = 2, lwd = 2,lty = 2) 
summary(m1)

m1 <- lm(seeds ~ Serpentine, data = sb.sum[sb.sum$fungroup == "Exotic Annual Grass",])
plot(m1)
qqnorm(resid(m1)) 
qqline(resid(m1), col = 2, lwd = 2,lty = 2) 
summary(m1)

core.16 <- read.csv("Data/2016/Core-Plot-2016.csv", strip.white = T)   
core.16.l <- reshape(core.16, varying = colnames(core.16)[2:ncol(core.16)], v.names = "Cover", timevar = "Plot", times = colnames(core.16)[2:ncol(core.16)], direction = "long")[,1:3]
colnames(core.16.l)[1] <- "Species_Name"
core.16.l$Plot <- gsub("\\X", "", core.16.l$Plot)
core.16.l$Year <- 2016
core.16.l <- filter(core.16.l, Cover != 0)
core.16.l <- core.16.l[,c(4,2,1,3)]
core.16.l <- merge(core.16.l, treat, by = "Plot")
core.16.l <- merge(core.16.l, trait.raw[,c(2,23)], by.x = "Species_Name", by.y = "Species_Name_J12")

core.16.l.sum <- ddply(core.16.l, .(Plot, Treatment, Serpentine, fungroup, Year), summarize, Cover = sum(Cover))

ggplot(core.16.l.sum, aes(x = fungroup, y = Cover)) +
  geom_boxplot() +
  facet_wrap(~Serpentine + Treatment) +
  ylim(0,150)

core.l.sum <- rbind(core.15.l.sum, core.16.l.sum)
core.l.sum <- summarySE(core.l.sum, groupvars = c("Year", "fungroup","Serpentine"), measurevar = "Cover")

ggplot(core.l.sum, aes(x = factor(Year), y = Cover, group = fungroup, col = fungroup)) +
  geom_point() +
  geom_errorbar(aes(ymin = Cover - se, ymax = Cover + se), width = 0.2) +
  geom_line() +
  facet_wrap(~Serpentine)

# get rid of really harsh serpentine plots (8,9,10)
core.15.l.sum$prev.cov <- core.15.l.sum$Cover
core.16.l.sum$cur.cov <- core.16.l.sum$Cover
all <- merge(core.15.l.sum[,c(1:4,7)], core.16.l.sum[,c(1:4,7)], by = c("Plot", "Treatment", "Serpentine", "fungroup"))
all <- merge(all, sb.sum, by = c("Plot", "Treatment", "Serpentine", "fungroup"))
