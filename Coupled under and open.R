setwd("E:/CSU/JSG")
df<-read.csv("Complete SWC_3.csv")
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
df$Position<-as.factor(df$Position2)
df$Position<-as.factor(df$Position3)

sdf<-summaryBy(SWC ~ MoYr + Position2 + Position3, FUN = c(mean,std.error), na.rm = T, df)

par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1,2.5,0.5,0.5))


dum<-subset(sdf, Position2 == "OC" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "palegreen", ylim = c(8,40))

points(dum$SWC.mean ~ dum$MoYr, col = "palegreen", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Position2 == "OE" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "palegreen1", ylim = c(8,40))

points(dum$SWC.mean ~ dum$MoYr, col = "palegreen1", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, Position2 == "OW" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "palegreen2", ylim = c(8,40))

points(dum$SWC.mean ~ dum$MoYr, col = "palegreen2", lty = 1, type = "l")



par(new=T)
dum<-subset(sdf, Position2 == "PE" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "springgreen2", ylim = c(8,40))

points(dum$SWC.mean ~ dum$MoYr, col = "springgreen2", lty = 2, type = "l")



par(new=T)
dum<-subset(sdf, Position2 == "PW" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "springgreen4", ylim = c(8,40))

points(dum$SWC.mean ~ dum$MoYr, col = "springgreen4", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, Position3 == "PC" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "springgreen4", ylim = c(8,40))

points(dum$SWC.mean ~ dum$MoYr, col = "springgreen4", lty = 2, type = "l")
##############

axis.Date(1, df$MoYr, at = seq(min(df$MoYr), max(df$MoYr), "month"), las=1, cex = 1.3, labels = T)
axis(1, at = seq(1,32,1), las = 1, cex.axis = 0.8)
axis(2, at = seq(8,40,4), las = 2, cex.axis = 0.8)
mtext(side = 2, expression("SWC%"), cex = 1.6, padj = -1, las = 2, outer= F)
mtext(side = 1, expression("Flag"), cex = 1.6, padj = 2.5, outer= F)


dev.off()

