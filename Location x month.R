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

sdf<-summaryBy(SWC ~ Location + MoYr , FUN = c(mean,std.error), na.rm = T, df)
sdf$Location<-as.numeric(sdf$Location)

tiff(file = "SWC Location location location.tiff", height = 10, width = 10, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,3.5,0.5,0.5))

dum<-subset(sdf, MoYr == "2021-05-01")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "cyan", ylim = c(16,36))

points(dum$SWC.mean ~ dum$Location, col = "cyan", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, MoYr == "2021-06-01")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "deepskyblue", ylim = c(16,36))

points(dum$SWC.mean ~ dum$Location, col = "deepskyblue", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, MoYr == "2021-07-01")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "darkcyan", ylim = c(16,36))

points(dum$SWC.mean ~ dum$Location, col = "darkcyan", lty = 1, type = "l")


par(new=T)
dum<-subset(sdf, MoYr == "2021-08-01")
# plot(dum$SWC.mean ~ dum$Location)

plotCI(dum$Location, dum$SWC.mean, dum$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, col = "cornflowerblue", ylim = c(16,36))

points(dum$SWC.mean ~ dum$Location, col = "cornflowerblue", lty = 1, type = "l")


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
axis(1, at = seq(0,16,1), las = 1, cex.axis = 1.4)
axis(2, at = seq(16,36,2), las = 2, cex.axis = 1.6)
mtext(side = 2, expression("SWC%"), cex = 1.7, padj = 1, las = 2, outer= T)
mtext(side = 1, expression("Location"), cex = 1.8, padj = 2.5, outer= F)
legend("topright", c("May","June", "July", "August"), col=c("cyan", "deepskyblue", "darkcyan", "cornflowerblue"), pch= c(16), cex = 1.4, horiz = F, bty='n')

dev.off()

sdf$MoYr<-as.factor(sdf$MoYr)
sdf$Location<-as.factor(sdf$Location)
m1<-lm(SWC.mean~Location*MoYr,sdf)
anova(m1)
plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Location))