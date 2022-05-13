library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(nlme)
library(MuMIn); library(car)
library(sjPlot)
library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(plotrix); library(suncalc)


library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
setwd("E:/CSU/JSG/Environmental data")
# setwd("~/Desktop")


env<-read.csv("JacksSolarGardenMet_2021.csv")
env$SVP_kPa<-(610.78*(2.71828^(env$AirTemp/(env$AirTemp+238.3)*17.2694)))/1000
env$VPD_kPa<-env$SVP_kPa * (1- (env$RH/100))
env$Date<-as.Date(env$Date, format = "%m/%d/%Y")
env$Precip<-as.numeric(env$Precip)
# env$Timestamp2<-as.POSIXct(paste(env$Date, env$ï..Time), format="%Y-%m-%d %H:%M:%S")
# sun<-getSunlightPosition(date = as.POSIXct(env$Timestamp2, tz = "UTC"), lat = 40.16, lon = -105.1)
# 
# write.csv(sun, "sunpositionJSG.csv")


inseason<-subset(env, growingseason == "Y")
dum<-subset(inseason, light == "1")
morn<-subset(dum, Noonage == "morning")
aftern<-subset(dum, Noonage == "afternoon")
day<-summaryBy(AirTemp+RH+VPD_kPa ~ Date+Noonage+light , FUN = c(min, mean, max), na.rm = T, dum)
am<-summaryBy(AirTemp+RH+VPD_kPa ~ Date+Noonage+light , FUN = c(min, mean, max), na.rm = T, morn)
pm<-summaryBy(AirTemp+RH+VPD_kPa ~ Date+Noonage+light , FUN = c(min, mean, max), na.rm = T, aftern)
rain<-summaryBy(Precip ~ Date+Noonage+light , FUN = c(min, mean, max), na.rm = T, dum)

dookie<-summaryBy(Precip~Date, FUN = sum , na.rm = T, env)

tiff(file = "JSG morning_afternoon ENV 2021.tiff", height = 10, width = 6.5, res = 600, units = "in", compression = "zip+p")
par(mfrow = c(3,2), omi = c(1, 1, 0.1, 0.1), mar = c(1,2.5,0.2,0.5))


plot(AirTemp.mean ~ Date, day, pch = NA, ylim = c(0,40), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2021-05-01"), as.Date("2021-09-01")))


# plot(AirTemp.mean ~ Date, dum, pch = 1, col= "dodgerblue", ylim = c(0, 40), xaxt = "n", yaxt = "n", xlab = "", ylab = "


# points(day$AirTemp.max ~ day$Date, type = "l", col = "sienna3", lty = 1)
# points(day$AirTemp.mean ~ day$Date, type = "l", col = "black", lty = 1)
# points(day$AirTemp.min ~ day$Date, type = "l", col = "dodgerblue", lty = 1)

points(am$AirTemp.mean ~ am$Date, type = "l", col = "sienna3", lty = 1)
points(pm$AirTemp.mean ~ pm$Date, type = "l", col = "mediumorchid3", lty = 1)




legend("topright", ("(a)"), horiz = F, bty='n', cex= 1.5)
mtext(side = 2, expression(italic(T)[air]~(degree*C)), cex = 2, padj = -1.2, outer= F)

legend("bottom", c("Morning", "Afternoon"), col=c("sienna3", "mediumorchid3"), lty = 1, horiz = F, bty='n', cex= 1.8)

# mtext(side = 2, expression(degree*C), cex = 1.5, padj = -2.5, outer= F)
# mtext(side = 1, expression("Time"), cex = 1.5, padj = 1.5, outer = T)

axis.Date(1, env$Date, at = seq(min(env$Date, na.rm = T), max(env$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 0.8, labels = F)
axis(2, at = seq(0,40,10), cex.axis = 1.4, las = 2)

#############################################
###############################


m1<-lm(AirTemp.mean~Noonage, day)
anova(m1)
summary(m1)
# plot(allEffects(m1))
AT=cld(emmeans(m1, ~ Noonage))



xx<-c(-500,500); yy<-c(-500,500)
plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(10,30), xlim=c(0,5))

ATM<-subset(AT, Noonage == "morning")
rect(01, 00, 02, mean(ATM$emmean), col = "aquamarine4", border = "black")

ablineclip(v=1.5, y1=as.numeric(ATM$emmean) + (ATM$SE), y2=as.numeric(ATM$emmean) - (ATM$SE))

ATA<-subset(AT, Noonage == "afternoon")
rect(03, 00, 04, mean(ATA$emmean), col = "lightgoldenrod", border = "black")

ablineclip(v=3.5, y1=as.numeric(ATA$emmean) + (ATA$SE), y2=as.numeric(ATA$emmean) - (ATA$SE))
box()
axis(2, at = seq(10,30,5), las = 2, cex.axis = 1.4)
# mtext(side = 2, expression(italic(T)[air]~(degree*C)), cex = 2, padj = -1.2, outer= F)
legend("topright", ("(b)"), horiz = F, bty='n', cex= 1.5)
legend("topleft", c("Morning", "Afternoon"), col=c("aquamarine4", "lightgoldenrod"), pch= c(15), cex = 1.8, horiz = F, bty='n')
text(3.5, 28, expression("*"),cex=1.6)
#########################

###############################


#############################################


plot(RH.mean ~ Date, day, pch = NA, ylim = c(0,100), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2021-05-01"), as.Date("2021-09-01")))


# points(day$RH.max ~ day$Date, type = "l", col = "sienna3", lty = 1)
# points(day$RH.mean ~ day$Date, type = "l", col = "black", lty = 1)
# points(day$RH.min ~ day$Date, type = "l", col = "dodgerblue", lty = 1)

points(am$RH.mean ~ am$Date, type = "l", col = "sienna3", lty = 1)
points(pm$RH.mean ~ pm$Date, type = "l", col = "mediumorchid3", lty = 1)


mtext(side = 2, expression("RH (%)"), cex = 1.5, padj = -2, outer= F)
# legend("bottomleft", c("Max", "Mean", "Min"), col=c("sienna3", "Black", "dodgerblue"), pch=1, lty = 1, horiz = F, bty='n', cex= 0.8)
# legend("bottom", c("Warmed", "Ambient"), lty = 1:2, horiz = F, bty='n', cex = 0.8)
# mtext(side = 2, expression(degree*C), cex = 1.5, padj = -2.5, outer= F)
# mtext(side = 1, expression("Time"), cex = 1.5, padj = 1.5, outer = T)
legend("topright", ("(c)"), horiz = F, bty='n', cex= 1.5)
axis.Date(1, env$Date, at = seq(min(env$Date, na.rm = T), max(env$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 0.8, labels = F)
axis(2, at = seq(0,100,20), cex.axis = 1.4, las = 2)




##########################################
m1<-lm(RH.mean~Noonage, day)
anova(m1)
summary(m1)
# plot(allEffects(m1))
RH=cld(emmeans(m1, ~ Noonage))


xx<-c(-500,500); yy<-c(-500,500)
plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(10,80), xlim=c(0,5))

# rect(xleft = 0, xright = 6, ybottom = -2000, ytop = 2200, col = "honeydew", bty = "n", border = F)
# box()
RHM<-subset(RH, Noonage == "morning")
rect(01, 00, 02, mean(RHM$emmean), col = "aquamarine4", border = "black")

ablineclip(v=1.5, y1=as.numeric(RHM$emmean) + (RHM$SE), y2=as.numeric(RHM$emmean) - (RHM$SE))

RHA<-subset(RH, Noonage == "afternoon")
rect(03, 00, 04, mean(RHA$emmean), col = "lightgoldenrod", border = "black")

ablineclip(v=3.5, y1=as.numeric(RHA$emmean) + (RHA$SE), y2=as.numeric(RHA$emmean) - (RHA$SE))
box()
axis(2, at = seq(10,100,10), las = 2, cex.axis = 1.2)
# mtext(side = 2, expression("RH (%)"), cex = 1.8, padj = -1.6, outer= F)
legend("topright", ("(d)"), horiz = F, bty='n', cex= 1.5)
text(1.5, 70, expression("*"),cex=1.6)

##########################################################

########################################


plot(VPD_kPa.mean ~ Date, day, pch = NA, ylim = c(0,6), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2021-05-01"), as.Date("2021-09-01")))



# points(day$VPD_kPa.max ~ day$Date, type = "l", col = "sienna3", lty = 1)
# points(day$VPD_kPa.mean ~ day$Date, type = "l", col = "black", lty = 1)
# points(day$VPD_kPa.min ~ day$Date, type = "l", col = "dodgerblue", lty = 1)


points(am$VPD_kPa.mean ~ am$Date, type = "l", col = "sienna3", lty = 1)
points(pm$VPD_kPa.mean ~ pm$Date, type = "l", col = "mediumorchid3", lty = 1)

# legend("bottomleft", c("Max", "Mean", "Min"), col=c("sienna3", "Black", "dodgerblue"), pch=1, lty = 1, horiz = F, bty='n', cex= 0.8)
# legend("bottom", c("Warmed", "Ambient"), lty = 1:2, horiz = F, bty='n', cex = 0.8)


legend("topright", ("(e)"), horiz = F, bty='n', cex= 1.5)
mtext(side = 2, expression("VPD (kPa)"), cex = 1.5, padj = -2, outer= F)
# axis.Date(1, env$Date, at = seq(min(env$Date, na.rm = T), max(env$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 1)
axis(2, at = seq(0,6,1), cex.axis = 1.4, las = 2)
mtext(side = 1, expression("Time (Month/Year)"), cex = 1.8, padj = 3, outer = F)
axis.Date(1, env$Date, at = seq(min(env$Date, na.rm = T), max(env$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 1)


###########################
m1<-lm(VPD_kPa.max~Noonage, day)
anova(m1)
summary(m1)
# plot(allEffects(m1))
VPD=cld(emmeans(m1, ~ Noonage))

xx<-c(-500,500); yy<-c(-500,500)
plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(1,4), xlim=c(0,5))

VPDM<-subset(VPD, Noonage == "morning")
rect(01, 00, 02, mean(VPDM$emmean), col = "aquamarine4", border = "black")

ablineclip(v=1.5, y1=as.numeric(VPDM$emmean) + (VPDM$SE), y2=as.numeric(VPDM$emmean) - (VPDM$SE))

VPDA<-subset(VPD, Noonage == "afternoon")
rect(03, 00, 04, mean(VPDA$emmean), col = "lightgoldenrod", border = "black")

ablineclip(v=3.5, y1=as.numeric(VPDA$emmean) + (VPDA$SE), y2=as.numeric(VPDA$emmean) - (VPDA$SE))
box()
axis(2, at = seq(1,4,1), las = 2, cex.axis = 1.4)

legend("topright", ("(f)"), horiz = F, bty='n', cex= 1.5)
mtext(side = 1, expression("Time of Day"), cex = 2, padj = 1, outer= F)
text(3.5, 3.6, expression("*"),cex=1.6)

#Precip plot
# plot(Precip.sum ~ Date, dookie, pch = NA, ylim = c(0,24), xaxt="n",yaxt="n",xlab="",ylab="", 
#      xlim = c(as.Date("2021-05-01"), as.Date("2021-09-01")))
# 
# 
# 
# points(dookie$Precip.sum ~ dookie$Date, type = "l", col = "grey66", lty = 1)
# 
# rect(xleft = as.Date("2021-07-08"), xright = as.Date("2021-09-06"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
# box()
# 
# 

# legend("topright", ("(d)"), horiz = F, bty='n', cex= 1.5)
# mtext(side = 2, expression("Daily Precip (mm)"), cex = 1.5, padj = -2, outer= F)
# mtext(side = 1, expression("Time (Month/Year)"), cex = 1.8, padj = 2, outer = T)
# axis.Date(1, env$Date, at = seq(min(env$Date, na.rm = T), max(env$Date, na.rm = T),"months"), las = 1, format = "%b %Y", cex.axis = 1)
# axis(2, at = seq(0,24,4), cex.axis = 1.2, las = 2)



# write.csv(dum, "cleanedJSGenv.csv")

dev.off()

#########
###########
##########
###########
###########
###########
# 
# tiff(file = "TOD ENV Bars.tiff", height = 8, width = 6, res = 600, units = "in", compression = "zip+p")
# 
# m1<-lm(RH.mean~Noonage, day)
# anova(m1)
# summary(m1)
# # plot(allEffects(m1))
# RH=cld(emmeans(m1, ~ Noonage))
# 
# m1<-lm(AirTemp.mean~Noonage, day)
# anova(m1)
# summary(m1)
# # plot(allEffects(m1))
# AT=cld(emmeans(m1, ~ Noonage))
# 
# m1<-lm(VPD_kPa.max~Noonage, day)
# anova(m1)
# summary(m1)
# # plot(allEffects(m1))
# VPD=cld(emmeans(m1, ~ Noonage))
# 
# ##########################
# 
# # par(mfrow = c(3,1), omi = c(0.8, 1, 0.4, 0.8), mar = c(1.5,3.5,0.5,3.5))
# 
# ###############################
# 
# xx<-c(-500,500); yy<-c(-500,500)
# plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(10,30), xlim=c(0,5))
# 
# ATM<-subset(AT, Noonage == "morning")
# rect(01, 00, 02, mean(ATM$emmean), col = "aquamarine4", border = "black")
# 
# ablineclip(v=1.5, y1=as.numeric(ATM$emmean) + (ATM$SE), y2=as.numeric(ATM$emmean) - (ATM$SE))
# 
# ATA<-subset(AT, Noonage == "afternoon")
# rect(03, 00, 04, mean(ATA$emmean), col = "lightgoldenrod", border = "black")
# 
# ablineclip(v=3.5, y1=as.numeric(ATA$emmean) + (ATA$SE), y2=as.numeric(ATA$emmean) - (ATA$SE))
# box()
# axis(2, at = seq(10,30,5), las = 2, cex.axis = 1.4)
# mtext(side = 2, expression(italic(T)[air]~(degree*C)), cex = 2, padj = -1.2, outer= F)
# legend("topright", ("(a)"), horiz = F, bty='n', cex= 1.5)
# legend("topleft", c("Morning", "Afternoon"), col=c("aquamarine4", "lightgoldenrod"), pch= c(15), cex = 1.8, horiz = F, bty='n')
# text(3.5, 28, expression("*"),cex=1.6)
# #########################
# xx<-c(-500,500); yy<-c(-500,500)
# plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(10,80), xlim=c(0,5))
# 
# # rect(xleft = 0, xright = 6, ybottom = -2000, ytop = 2200, col = "honeydew", bty = "n", border = F)
# # box()
# RHM<-subset(RH, Noonage == "morning")
# rect(01, 00, 02, mean(RHM$emmean), col = "aquamarine4", border = "black")
# 
# ablineclip(v=1.5, y1=as.numeric(RHM$emmean) + (RHM$SE), y2=as.numeric(RHM$emmean) - (RHM$SE))
# 
# RHA<-subset(RH, Noonage == "afternoon")
# rect(03, 00, 04, mean(RHA$emmean), col = "lightgoldenrod", border = "black")
# 
# ablineclip(v=3.5, y1=as.numeric(RHA$emmean) + (RHA$SE), y2=as.numeric(RHA$emmean) - (RHA$SE))
# box()
# axis(2, at = seq(10,100,10), las = 2, cex.axis = 1.2)
# mtext(side = 2, expression("RH (%)"), cex = 1.8, padj = -1.6, outer= F)
# legend("topright", ("(b)"), horiz = F, bty='n', cex= 1.5)
# text(1.5, 70, expression("*"),cex=1.6)
# ###############################
# xx<-c(-500,500); yy<-c(-500,500)
# plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(1,4), xlim=c(0,5))
# 
# VPDM<-subset(VPD, Noonage == "morning")
# rect(01, 00, 02, mean(VPDM$emmean), col = "aquamarine4", border = "black")
# 
# ablineclip(v=1.5, y1=as.numeric(VPDM$emmean) + (VPDM$SE), y2=as.numeric(VPDM$emmean) - (VPDM$SE))
# 
# VPDA<-subset(VPD, Noonage == "afternoon")
# rect(03, 00, 04, mean(VPDA$emmean), col = "lightgoldenrod", border = "black")
# 
# ablineclip(v=3.5, y1=as.numeric(VPDA$emmean) + (VPDA$SE), y2=as.numeric(VPDA$emmean) - (VPDA$SE))
# box()
# axis(2, at = seq(1,4,1), las = 2, cex.axis = 1.4)
# mtext(side = 2, expression("VPD (kPa)"), cex = 1.6, padj = -1.6, outer= F)
# legend("topright", ("(c)"), horiz = F, bty='n', cex= 1.5)
# mtext(side = 1, expression("Time of Day"), cex = 2, padj = 1, outer= F)
# text(3.5, 3.6, expression("*"),cex=1.6)
# dev.off()
# 
