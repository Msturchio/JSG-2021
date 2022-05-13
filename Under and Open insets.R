
library(car); library(lubridate); library(lme4);
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

sdf<-summaryBy(SWC ~ Season + Spot , FUN = c(mean,std.error), na.rm = T, df)
sdf$Location<-as.numeric(sdf$Location)
sdf$Season<-as.factor(sdf$Season)
sdf$Spot<-as.factor(sdf$Spot)

tiff(file = "SWC seasonal inset.tiff", height = 8, width = 8, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,3.5,0.5,0.5))


xx<-c(-500,500); yy<-c(-500,500)

##################################################################
# Box1: Big chunks for spar

plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(20,30), xlim=c(0,16))

eu<-subset(sdf, Season == "early" & Spot == "under")
eo<-subset(sdf, Season == "early" & Spot == "open")
lu<-subset(sdf, Season == "late" & Spot == "under")
lo<-subset(sdf, Season == "late" & Spot == "open")



rect(02, 00, 04, mean(eu$SWC.mean), col = "dodgerblue4", border = "black")
rect(04, 00, 06, mean(eo$SWC.mean), col = "dodgerblue4", border = "black")
rect(06, 00, 08, mean(lu$SWC.mean), col = "firebrick", border = "black")
rect(08, 00, 10, mean(lo$SWC.mean), col = "firebrick", border = "black")
rect(02, 00, 04, mean(eu$SWC.mean), density = 10, lwd = 1, col = "white" , border = "black")
rect(06, 00, 08, mean(lu$SWC.mean), density = 10, lwd = 1, col = "white" , border = "black")


ablineclip(v=3, y1=as.numeric(mean(eu$SWC.mean) + (eu$SWC.std.error)), y2=as.numeric(mean(eu$SWC.mean) - (eu$SWC.std.error)))
ablineclip(v=5, y1=as.numeric(mean(eo$SWC.mean) + (eo$SWC.std.error)), y2=as.numeric(mean(eo$SWC.mean) - (eo$SWC.std.error)))
ablineclip(v=7, y1=as.numeric(mean(lu$SWC.mean) + (lu$SWC.std.error)), y2=as.numeric(mean(lu$SWC.mean) - (lu$SWC.std.error)))
ablineclip(v=9, y1=as.numeric(mean(lo$SWC.mean) + (lo$SWC.std.error)), y2=as.numeric(mean(lo$SWC.mean) - (lo$SWC.std.error)))


box()

axis(2, at = seq(20,30,1), las = 2, cex.axis = 1.4, labels = T)

legend("topleft", c("(b)"), bty = "n", cex = 1.5)
# legend("topright", c("Nitrogen","Phosphorus","Control"), bty = "n",
#        col = c("dodgerblue", "forestgreen","black"), pch = 15)
# 
# 
# legend("topright", density = c(0,20,50), cex = 0.98, bty = "n",
#        c(expression(New~Pre-fert), expression(New~Post-fert), expression(Old~Post-fert)))
mtext(side = 2, expression("SWC%"), cex = 1.8, padj = 1.2, las = 2, outer= T)

legend("right", c("Open", "Under Panel"), density=c(0,30), cex = 1.7, horiz = F, bty='n')
# legend("topright", c("Cool Season Under", "Cool Season Open", "Warm Season Under", "Warm Season Open"), col=c("dodgerblue4", "dodgerblue4", "firebrick" , "firebrick"), pch = c(16,16,16,16), density = c(0,30,0,30), cex = 1.7, horiz = F, bty='n')
legend("topright", c("Cool Season", "Warm Season"), col = c("dodgerblue4", "firebrick"), pch= 16,  cex = 1.7, horiz = F, bty='n')
dev.off()
#########################################################################
tiff(file = "SWC overall inset.tiff", height = 8, width = 8, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,3.5,0.5,0.5))


xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(20,28), xlim=c(0,10))

u<-subset(sdf, Spot == "under")
o<-subset(sdf, Spot == "open")

rect(02, 00, 04, mean(u$SWC.mean), col = "darkgreen", border = "darkblue")
rect(04, 00, 06, mean(o$SWC.mean), col = "darkgreen", border = "darkblue")
rect(02, 00, 04, mean(u$SWC.mean), density = 10, lwd = 1, col = "white" , border = "black")
ablineclip(v=3, y1=as.numeric(mean(u$SWC.mean) + (u$SWC.std.error)), y2=as.numeric(mean(u$SWC.mean) - (u$SWC.std.error)))
ablineclip(v=5, y1=as.numeric(mean(o$SWC.mean) + (o$SWC.std.error)), y2=as.numeric(mean(o$SWC.mean) - (o$SWC.std.error)))

box()

axis(2, at = seq(20,28,1), las = 2, cex.axis = 1.4, labels = T)

legend("topleft", c("(b)"), bty = "n", cex = 1.5)
# legend("topright", c("Nitrogen","Phosphorus","Control"), bty = "n",
#        col = c("dodgerblue", "forestgreen","black"), pch = 15)
# 
# 
# legend("topright", density = c(0,20,50), cex = 0.98, bty = "n",
#        c(expression(New~Pre-fert), expression(New~Post-fert), expression(Old~Post-fert)))
mtext(side = 2, expression("SWC%"), cex = 1.8, padj = 1.2, las = 2, outer= T)

legend("right", c("Open", "Under Panel"), density=c(0,30), cex = 1.7, horiz = F, bty='n')
# legend("topright", c("Cool Season Under", "Cool Season Open", "Warm Season Under", "Warm Season Open"), col=c("dodgerblue4", "dodgerblue4", "firebrick" , "firebrick"), pch = c(16,16,16,16), density = c(0,30,0,30), cex = 1.7, horiz = F, bty='n')
# legend("topright", c("Cool Season", "Warm Season"), col = c("dodgerblue4", "firebrick"), pch= 16,  cex = 1.7, horiz = F, bty='n')
dev.off()