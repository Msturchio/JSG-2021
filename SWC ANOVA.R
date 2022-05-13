library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans); library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix)

# setwd("/Volumes/The Hive/WETFEED Analysis/Model Outputs/Analysis")
# df<-read.csv("NaddedtoLPR.csv")

setwd("E:/CSU/JSG")
df<-read.csv("Complete SWC.csv")
df<-subset(df, KEEP == "Y")

May<-subset(df, Month == "May")
June<-subset(df, Month == "June")
July<-subset(df, Month == "July")
August<-subset(df, Month == "August")


df$Transect<-as.factor(df$Transect)
df$Direction<-as.factor(substr(df$Transect, 1, 1))
df$Height<-as.factor(substr(df$Transect, 2, 2))
df$Flag<-as.factor(df$Flag)
# df$SWC<-as.factor(df$SWC)
###########################################################################
############################################################################
tiff(file = "raw monthly SWC data.tiff", height = 12, width = 8, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(5,2), omi = c(0.8, 1, 0.4, 0.1), mar = c(2,2.5,1,0.5))

dum<-subset(df, Month == "May" & Height == "8")
plot(dum$SWC ~ dum$Flag)
legend("bottom", c("May"), bty = "n", cex = 1.6)
mtext(side = 3, expression("8 Foot"), cex = 1.6, padj = -1, outer= F)

dum<-subset(df, Month == "May" & Height == "6")
plot(dum$SWC ~ dum$Flag)
legend("bottom", c("May"), bty = "n", cex = 1.6)
mtext(side = 3, expression("6 Foot"), cex = 1.6, padj = -1, outer= F)

dum<-subset(df, Month == "June"  & Height == "8")
plot(dum$SWC ~ dum$Flag)
legend("bottom", c("June"), bty = "n", cex = 1.6)

dum<-subset(df, Month == "June"  & Height == "6")
plot(dum$SWC ~ dum$Flag)
legend("bottom", c("June"), bty = "n", cex = 1.6)

dum<-subset(df, Month == "July"  & Height == "8")
plot(dum$SWC ~ dum$Flag)
legend("bottom", c("July"), bty = "n", cex = 1.6)

dum<-subset(df, Month == "July"  & Height == "6")
plot(dum$SWC ~ dum$Flag)
legend("bottom", c("July"), bty = "n", cex = 1.6)

dum<-subset(df, Month == "August" & Height == "8")
plot(dum$SWC ~ dum$Flag)
legend("bottom", c("August"), bty = "n", cex = 1.6)

dum<-subset(df, Month == "August" & Height == "6")
plot(dum$SWC ~ dum$Flag)
legend("bottom", c("August"), bty = "n", cex = 1.6)

dum<-subset(df, Month == "September" & Height == "8")
plot(dum$SWC ~ dum$Flag)
legend("bottom", c("September"), bty = "n", cex = 1.6)

dum<-subset(df, Month == "September" & Height == "6")
plot(dum$SWC ~ dum$Flag)
legend("bottom", c("September"), bty = "n", cex = 1.6)

mtext(side = 2, expression("SWC%"), cex = 1.6, padj = -1, outer= T)
mtext(side = 1, expression("Flag #"), cex = 1.6, padj = 1, outer= T)


dev.off()

###########################################################################
#########################################################################
df$Date<-as.Date(df$ï..Date, ("%m/%d/%Y"))
df$MoYr<-as.Date(floor_date(df$Date, "month"))
df$MoYr<-as.Date(df$MoYr, "%m/%d/%y")
df$SWC<-as.numeric(df$SWC)
sdf<-summaryBy(SWC ~ MoYr + Height  , FUN = c(mean,std.error), na.rm = T, df)

tiff(file = "Month x Height SWC.tiff", height = 8, width = 8, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(2,3.5,1,0.5))

plotCI(sdf$MoYr, sdf$SWC.mean, sdf$SWC.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(18,32))


dum<-subset(sdf, Height == "6")
plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "springgreen4", ylim = c(18,32))
points(dum$SWC.mean ~ dum$MoYr, col = "springgreen4", lty = 1, type = "l")
# This makes a template

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, Height == "8")
plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "sienna3", ylim = c(18,32))
points(dum$SWC.mean ~ dum$MoYr, col = "sienna3", lty = 1, type = "l")


axis.Date(1, df$MoYr, at = seq(min(df$MoYr), max(df$MoYr), "month"), las=1, cex = 1.3, labels = T)
axis(2, at = seq(18,32,2), las = 2, cex.axis = 1.2)
mtext(side = 2, expression("SWC%"), cex = 1.6, padj = -1, las = 2, outer= T)
mtext(side = 1, expression("Month"), cex = 1.6, padj = 3.5, outer= F)

legend("bottom", c("Height *** ","Month  ***","H x M  **"), bty = "n", cex = 1.4)

legend("top", c("8 foot","6 foot"), col=c("sienna3", "springgreen4"), pch= c(16,16,16), cex = 1.4, horiz = F, bty='n')

dev.off()


###################################################################
####################################################################

m1<-lm(SWC~Height*Month,df)
anova(m1)
plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Height*Month))









m1<-lm(SWC~Height*Flag*Month,df)
anova(m1)
plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Height*Month))


dum<-subset(df, LeafID != "17n" )