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
# df$Position<-as.factor(df$Position2)
# df$Position<-as.factor(df$Position3)
dum<-subset(df, Month != "September")

sdf<-summaryBy(SWC ~ MoYr + Location , FUN = c(mean,std.error), na.rm = T, dum)

tiff(file = "SWC by location over time.tiff", height = 10, width = 10, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.4), mar = c(1.5,3.5,0.5,0.5))


dum<-subset(sdf, Location == "1" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16,cex = 2,  col = "peru", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "peru", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Location == "2" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16,cex = 2, col = "violetred", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "violetred", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, Location == "3" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16,cex = 2, col = "tomato", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "tomato", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Location == "4" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16,cex = 2, col = "sienna", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "sienna", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Location == "5" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16,cex = 2, col = "blue", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Location == "6" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 2,col = "cadetblue", ylim = c(16,36))


points(dum$SWC.mean ~ dum$MoYr, col = "cadetblue", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Location == "7" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16,cex = 2, col = "cornflowerblue", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "cornflowerblue", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Location == "8" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16,cex = 2, col = "cyan", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "cyan", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, Location == "9" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16,cex = 2, col = "deepskyblue", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "deepskyblue", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Location == "10" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16,cex = 2, col = "darkcyan", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "darkcyan", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, Location == "11" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16,cex = 2, col = "dodgerblue", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "dodgerblue", lty = 1, type = "l")
##############


axis.Date(1, env$Date, at = seq(min(df$Date, na.rm = T), max(df$Date, na.rm = T),"months"), las = 1, format = "%b %Y", cex.axis = 1.5)
axis(2, at = seq(16,36,4), las = 2, cex.axis = 1.6)
mtext(side = 2, expression("SWC%"), cex = 2.4, padj = -0.5, outer= T)
mtext(side = 1, expression("Time (Month/Year)"), cex = 2, padj = 2.5, outer= F)
legend("top", c("1","2", "3", "4","5"), col=c("peru", "violetred", "tomato", "sienna","blue"), title = "Location #" , pch= c(16), cex = 1.7, horiz = F, bty='n')
legend("topright", c("6","7", "8", "9", "10", "11"), col=c( "cadetblue", "cornflowerblue" ,"cyan","deepskyblue" ,"darkcyan", "dodgerblue"), pch= c(16), cex = 1.7, horiz = F, bty='n')


dev.off()

df$MoYr<-as.factor(df$MoYr)
m1<-lm(SWC~Transect*MoYr,df)
anova(m1)
plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Transect*MoYr))

