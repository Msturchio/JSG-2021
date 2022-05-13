library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans); library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix)

setwd("E:/CSU/JSG")
df<-read.csv("Complete SWC_loco.csv")
df<-subset(df, KEEP == "Y")

# May<-subset(df, Month == "May")
# June<-subset(df, Month == "June")
# July<-subset(df, Month == "July")
# August<-subset(df, Month == "August")


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
# df$Position<-as.factor(df$Position2)
# df$Position<-as.factor(df$Position3)

sdf<-summaryBy(SWC ~ Location + Date , FUN = c(mean,std.error), na.rm = T, df)
sdf$Location<-as.numeric(sdf$Location)

tiff(file = "SWC Location x date.tiff", height = 10, width = 10, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 0.4, 0.4, 0.1), mar = c(1.5,3.5,0.5,0.5))

dum<-subset(sdf, Date == "2021-05-03")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "dodgerblue2", ylim = c(6,50))

rect(xleft = 0.5, xright = 4.5, ybottom = 45, ytop = 45.5, col = "grey52", bty = "n", border = F)
rect(xleft = 11.5, xright = 15.5, ybottom = 45, ytop = 45.5, col = "grey52", bty = "n", border = F)
rect(xleft = 0.5, xright = 4.5, ybottom = 45.4, ytop = 45.5, col = "black", bty = "n", border = F)
rect(xleft = 11.5, xright = 15.5, ybottom = 45.4, ytop = 45.5, col = "black", bty = "n", border = F)
rect(xleft = 2.45, xright = 2.55, ybottom = -2, ytop = 45, col = "grey52", bty = "n", border = F)
rect(xleft = 13.45, xright = 13.55, ybottom = -2, ytop = 45, col = "grey52", bty = "n", border = F)
box()

points(dum$SWC.mean ~ dum$Location, col = "dodgerblue2", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, Date == "2021-05-19")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "dodgerblue2", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "dodgerblue2", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Date == "2021-05-28")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "dodgerblue2", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "dodgerblue2", lty = 1, type = "l")

######################
par(new=T)
dum<-subset(sdf, Date == "2021-06-02")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "dodgerblue2", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "dodgerblue2", lty = 1, type = "l")
##################
par(new=T)
dum<-subset(sdf, Date == "2021-06-08")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "dodgerblue2", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "dodgerblue2", lty = 1, type = "l")

#############
par(new=T)
dum<-subset(sdf, Date == "2021-06-18")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "dodgerblue2", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "dodgerblue2", lty = 1, type = "l")

#############
par(new=T)
dum<-subset(sdf, Date == "2021-06-22")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "dodgerblue2", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "dodgerblue2", lty = 1, type = "l")

#############
par(new=T)
dum<-subset(sdf, Date == "2021-07-06")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=5, col = "firebrick", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "firebrick", lty = 1, type = "l")

#############
par(new=T)
dum<-subset(sdf, Date == "2021-07-09")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=5, col = "firebrick", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "firebrick", lty = 1, type = "l")

#############
par(new=T)
dum<-subset(sdf, Date == "2021-07-13")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=5, col = "firebrick", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "firebrick", lty = 1, type = "l")

#############
par(new=T)
dum<-subset(sdf, Date == "2021-07-16")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=5, col = "firebrick", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "firebrick", lty = 1, type = "l")

#############
par(new=T)
dum<-subset(sdf, Date == "2021-07-23")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=5, col = "firebrick", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "firebrick", lty = 1, type = "l")

#############
par(new=T)
dum<-subset(sdf, Date == "2021-08-02")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=5, col = "firebrick", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "firebrick", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, Date == "2021-08-17")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=5, col = "firebrick", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "firebrick", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, Date == "2021-08-25")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=5, col = "firebrick", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "firebrick", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, Date == "2021-08-30")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=5, col = "firebrick", ylim = c(6,50))

points(dum$SWC.mean ~ dum$Location, col = "firebrick", lty = 1, type = "l")


# par(new=T)
# dum<-subset(sdf, MoYr == "2021-09-01")
# # plot(dum$SWC.mean ~ dum$Location)
# 
# plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
#        xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "cyan", ylim = c(16,36))
# 
# points(dum$SWC.mean ~ dum$Location, col = "cyan", lty = 1, type = "l")


##############

# axis.Date(1, df$MoYr, at = seq(min(df$MoYr), max(df$MoYr), "month"), las=1, cex = 1.6, labels = T)
axis(1, at = seq(0,16,1), las = 1, cex.axis = 1.6)
axis(2, at = seq(5,50,10), las = 2, cex.axis = 1.6)
mtext(side = 2, expression("SWC%"), cex = 1.7, padj = 0, outer= T)
mtext(side = 1, expression("Location"), cex = 1.8, padj = 3, outer= F)
legend("top", c("Early Season (May & June)","Late Season (July & August)"), col=c("dodgerblue2", "firebrick"), pch= c(16,5), cex = 1.8, horiz = F, bty='n')

dev.off()
# 
# sdf$MoYr<-as.factor(sdf$MoYr)
# sdf$Location<-as.factor(sdf$Location)
# m1<-lm(SWC.mean~Location*MoYr,sdf)
# anova(m1)
# plot(allEffects(m1))
# summary(m1)
# d1=cld(emmeans(m1, ~ Location))