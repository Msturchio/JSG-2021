library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans); library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix)

# setwd("/Volumes/The Hive/WETFEED Analysis/Model Outputs/Analysis")
# df<-read.csv("NaddedtoLPR.csv")

setwd("E:/CSU/JSG")



df<-read.csv("JSG_Biggie.csv")
df<-subset(df, KEEP == "Y")


df$Transect<-as.factor(df$Transect)
df$Direction<-as.factor(substr(df$Transect, 1, 1))
df$Height<-as.factor(substr(df$Transect, 2, 2))
df$Flag<-as.factor(df$Flag)

df$Date<-as.Date(df$ï..Date, ("%m/%d/%Y"))
df$MoYr<-as.Date(floor_date(df$Date, "month"))
df$MoYr<-as.Date(df$MoYr, "%m/%d/%y")
df$SWC<-as.numeric(df$SWC)
df$Transect<-as.factor(df$Transect)
df$Direction<-as.factor(substr(df$Transect, 1, 1))
df$Height<-as.factor(substr(df$Transect, 2, 2))
df$Position<-as.factor(df$Position)
df$Flag<-as.numeric(df$Flag)
df$Location<-as.factor(df$Location)


light<-summaryBy(PPFD ~ Location + TOD, FUN = c(mean,std.error), na.rm = T, df)
light$Location<-as.numeric(light$Location)
# light$PPFD<-as.numeric(light$PPFD)


mornin<-subset(light, TOD == "Ten")
noon<-subset(light, TOD == "One")
afternoon<-subset(light, TOD == "Four")

lightmean<-summaryBy(PPFD.mean ~ Location, FUN = c(mean,std.error), na.rm = T, light)
lightmeandf<-summaryBy(PPFD ~ Location, FUN = c(mean,std.error), na.rm = T, df)
lightmeandf$Location<-as.numeric(lightmeandf$Location)

tiff(file = "Mean Daily Light shading.tiff", height = 10, width = 12, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(2,3.5,1,0.5))
plotCI(mornin$Location, mornin$PPFD.mean, mornin$PPFD.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="", cex = 2, pch=16, col = "tomato2", xlim = c(0,16), ylim = c(0,1900))
par(new=T)
plotCI(mornin$Location, noon$PPFD.mean, mornin$PPFD.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="", cex = 2, pch=16, col = "black", xlim = c(0,16), ylim = c(0,1900))
par(new=T)
plotCI(mornin$Location, afternoon$PPFD.mean, mornin$PPFD.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="", cex = 2, pch=16, col = "violetred4", xlim = c(0,16), ylim = c(0,1900))

# par(new=T)
# plotCI(lightmean$Location, lightmean$PPFD.mean.mean, lightmean$PPFD.std.error, sfrac = 0,
#        xaxt="n",yaxt="n",xlab="",ylab="", cex = 1, pch=16, col = "aquamarine", xlim = c(0,16), ylim = c(0,1900))
# par(new=T)
# plotCI(lightmeandf$Location, lightmeandf$PPFD.mean, lightmeandf$PPFD.std.error, sfrac = 0,
#        xaxt="n",yaxt="n",xlab="",ylab="", cex = 1, pch=16, col = "goldenrod", xlim = c(0,16), ylim = c(0,1900))


rect(xleft = 0.5, xright = 4.5, ybottom = 1250, ytop = 1300, col = "grey52", bty = "n", border = F)
rect(xleft = 11.5, xright = 15.5, ybottom = 1250, ytop = 1300, col = "grey52", bty = "n", border = F)
rect(xleft = 2.45, xright = 2.55, ybottom = -200, ytop = 1300, col = "grey52", bty = "n", border = F)
rect(xleft = 13.45, xright = 13.55, ybottom = -200, ytop = 1300, col = "grey52", bty = "n", border = F)
rect(xleft = 0.5, xright = 4.5, ybottom = 1290, ytop = 1300, col = "black", bty = "n", border = F)
rect(xleft = 11.5, xright = 15.5, ybottom = 1290, ytop = 1300, col = "black", bty = "n", border = F)
box()


points(mornin$PPFD.mean ~ mornin$Location, col = "tomato2", lty = 1, pch=15, lwd = 4, type = "l")

points(noon$PPFD.mean ~ noon$Location, col = "black", lty = 1, pch=15, lwd = 4, type = "l")

points(afternoon$PPFD.mean ~ afternoon$Location, col = "violetred4", lty = 1, pch = 15, lwd = 4, type = "l")

# points(lightmean$PPFD.mean.mean ~ lightmean$Location, col = "aquamarine", lty = 3, pch = 15, lwd = 2, type = "l")
# 
# points(lightmeandf$PPFD.mean ~ lightmean$Location, col = "goldenrod", lty = 4, pch = 15, lwd = 2, type = "l")

axis(2, at = seq(0,1800,200), las = 2, cex.axis = 1.8)
axis(1, at = seq(1,15,1), las =1, cex.axis = 1.5)

mtext(side = 2, expression(PPFD~(mu*mol~m^-2~s^-1)), cex = 1.8, padj = -0.75, outer= T) 
mtext(side = 1, expression("Location"), cex = 1.8, padj = 3.2, outer= F)

# legend("bottom", c("Height *** ","Month  ***","H x M  **"), bty = "n", cex = 1.4)

legend("bottom", c("10am","1pm","4pm"), col=c("tomato2", "black", "violetred4"), pch = 16, lwd = 3, lty = c(1,1,1,1), cex = 1.7, horiz = F, bty='n')

dev.off()

