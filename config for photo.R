library(dplyr); library(readxl); library(lubridate)
library(minpack.lm); library(lattice)
library(plantecophys); library(effects); library(photosynthesis); library(units)

# Make sure you install packages

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

############################################## 

###################################################################################
###################################################################################

in_data<-"E:/CSU/JSG"
out_data<-"E:/CSU/JSG"

# Above you will need to edit to your home PC

###################################################################################

setwd(in_data)
df<-read.csv("JSG LRC_2.csv")
df<-subset(df, KEEP == "Y")
levels(as.factor(df$UserIDs_in))
levels(as.factor(df$date))

dat<-subset(df, UserIDs_in == "7")

dat<-subset(dat, CO2R >= 410.380)
dat<-subset(dat, CO2R <= 410.410)



plot(dat$Photo ~ dat$CO2R)

object<-fitaci(dat, varnames = list(ALEAF = "Photo", Tleaf = "Tleaf", Ci = "Ci", PPFD = "PARi"),
               fitmethod = 'default', Tleaf = 25, Tcorrect = T)
object$pars

object

object$GammaStar

object$Photosyn(Ca=393.5186)
# 
# dat$Photo_out/dat$Cond_out

dat$GasEx_gsw_mol.m.U.207B.?.s.U.207B.?
  dat$Photo
dat$CO2S
dat$Tleaf

Kc=exp(38.05-79.43/(1.842471*(32.9591+273.15)));

Ko=exp(20.30-36.38/(1.842471*(32.9591+273.15)));

Km=Kc*(1+(210/Ko));


model Vcmax = (Apred*(CO2S+Km))/(CO2S-Gstar);



Apred=predicted photosynthesis



Stomatal limitation (L) = 1- (observed A/predicted A)



















# 
# df<-read.csv("Backup_One at a time values_WETFERT.csv")
# 
# dum<-subset(df, fert == "Post")
# dum<-subset(df, treatment == "n")
# dum<-subset(df, leafage == "new" & fit.method == "bilinear")
# 
# m1<-lm(vcmax~fert, dum)
# anova(m1)
# summary(m1)
# plot(allEffects(m1))
# 
# m1<-lm(jmax~timepoint, df)
# anova(m1)
# summary(m1)
# plot(allEffects(m1))





