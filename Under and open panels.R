setwd("E:/CSU/JSG")
df<-read.csv("Complete SWC.csv")
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

sdf<-summaryBy(SWC ~ MoYr + Position , FUN = c(mean,std.error), na.rm = T, df)

tiff(file = "SWC by position over time.tiff", height = 10, width = 10, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,3.5,0.5,0.5))


dum<-subset(sdf, Position == "OC" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "cyan", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "cyan", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Position == "OEC" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "deepskyblue", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "deepskyblue", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, Position == "OEE" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "dodgerblue", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "dodgerblue", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Position == "OEM" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "darkcyan", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "darkcyan", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Position == "OWE" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "blue", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Position == "OWC" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "cadetblue", ylim = c(16,36))


points(dum$SWC.mean ~ dum$MoYr, col = "cadetblue", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Position == "OWM" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "cornflowerblue", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "cornflowerblue", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Position == "PEC" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "peru", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "peru", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, Position == "PEE" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "sienna", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "sienna", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, Position == "PWC" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "violetred", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "violetred", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, Position == "PWE" )

plotCI(dum$MoYr, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "tomato", ylim = c(16,36))

points(dum$SWC.mean ~ dum$MoYr, col = "tomato", lty = 1, type = "l")
##############

axis.Date(1, df$MoYr, at = seq(min(df$MoYr), max(df$MoYr), "month"), las=1, cex = 1.6, labels = T)
axis(2, at = seq(16,36,4), las = 2, cex.axis = 1.6)
mtext(side = 2, expression("SWC%"), cex = 1.7, padj = 1, las = 2, outer= T)
mtext(side = 1, expression("Month"), cex = 1.8, padj = 2.5, outer= F)
legend("topright", c("OC","OEC", "OEE", "OEM", "OWE", "OWC", "OWM", "PEC", "PEE", "PWC", "PWE"), col=c("cyan", "deepskyblue", "dodgerblue", "darkcyan", "blue", "cadetblue", "cornflowerblue" ,"peru","sienna" ,"violetred", "tomato"), pch= c(16), cex = 1.4, horiz = F, bty='n')

dev.off()

df$MoYr<-as.factor(df$MoYr)
m1<-lm(SWC~Transect*MoYr,df)
anova(m1)
plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Transect*MoYr))

