
library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans)
dev.off()

df<-read.csv("JSG_Big Boi.csv")
df<-subset(df, KEEP == "Y")
df<-subset(df, Season != "bad" & KEEP == "Y" & TOD == "One")


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
df$Flag<-as.factor(df$Flag)
df$Location<-as.factor(df$Location)
df$grass_mass<-as.factor(df$grass_mass)
df$forb_mass<-as.factor(df$forb_mass)
df$ANPP<-df$ANPP*10
df$ANPP<-as.numeric(df$ANPP)
# dum<-subset(df, TOD == "One" & Season != "late")

moisture<-summaryBy(SWC ~ Location, FUN = c(mean,std.error), na.rm = T, df)
productivity<-summaryBy(ANPP ~ Position*Height , FUN = c(mean,std.error), na.rm = T, df)
productivity$Position<-as.factor(productivity$Position)
productivity$Height<-as.factor(productivity$Height)
JSG<-summaryBy(SWC+ANPP ~ Flag*Transect*Height, FUN = c(mean,std.error), na.rm = T, df)

Transect<-summaryBy(SWC+ANPP ~ Flag*Transect*Month, FUN = c(mean,std.error), na.rm = T, df)


May<-subset(Transect, Month == "May")
June<-subset(Transect, Month == "June")
July<-subset(Transect, Month == "July")
August<-subset(Transect, Month == "August")



bylocation<-summaryBy(SWC+ANPP ~ Location*Month*Season, FUN = c(mean,std.error), na.rm = T, df)
byloco<-summaryBy(SWC+ANPP ~ Location*Season, FUN = c(mean,std.error), na.rm = T, df)
bylocation$Location<-as.numeric(bylocation$Location)
byloco$Location<-as.numeric(byloco$Location)
annual<-summaryBy(SWC+ANPP ~ Location,  FUN = c(mean,std.error), na.rm = T, df)
annual$Location<-as.numeric(annual$Location)

May<-subset(bylocation, Month == "May")
June<-subset(bylocation, Month == "June")
July<-subset(bylocation, Month == "July")
August<-subset(bylocation, Month == "August")

Coolseason<-subset(byloco, Season == "early")
Warmseason<-subset(byloco, Season == "late")


############
###########
############

tiff(file = "SWCANPP shading.tiff", height = 8, width = 10, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.8), mar = c(1.5,3.5,0.5,3.5))


# par(new=T)
plotCI(bylocation$Location, bylocation$SWC.mean, bylocation$SWC.mean.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, cex = 1.7, col = "black", ylim = c(18,34))

rect(xleft = 0.5, xright = 4.5, ybottom = -2, ytop = 30, col = "grey95", bty = "n", border = F)
rect(xleft = 11.5, xright = 15.5, ybottom = -2, ytop = 30, col = "grey95", bty = "n", border = F)
rect(xleft = 0.5, xright = 4.5, ybottom = 29.5, ytop = 30, col = "slategray1", bty = "n", border = F)
rect(xleft = 11.5, xright = 15.5, ybottom = 29.5, ytop = 30, col = "slategray1", bty = "n", border = F)
rect(xleft = 2.45, xright = 2.55, ybottom = -2, ytop = 30, col = "slategray1", bty = "n", border = F)
rect(xleft = 13.45, xright = 13.55, ybottom = -2, ytop = 30, col = "slategray1", bty = "n", border = F)
box()

par(new=T)
plotCI(May$Location, May$ANPP.mean, May$ANPP.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 1.7, col = "palegreen4", ylim = c(400,800))
points(May$ANPP.mean ~ May$Location, col = "palegreen4", lty = 1, lwd = 2, type = "l")
axis(4, at = seq(400,800,50), las = 2, cex.axis = 1.2)
par(new=T)
plotCI(annual$Location, annual$SWC.mean, annual$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 1.7, col = "black", ylim = c(18,34))


points(annual$SWC.mean ~ annual$Location, col = "black", lty = 1, lwd = 2, type = "l")



# dev.off()
axis(1, at = seq(1,15,1), las = 1, cex.axis = 1.2)

mtext(side = 4, expression(""), cex = 1.3, padj = 1, las = 2, outer= T)
mtext(side = 1, expression("Location"), cex = 1.8, padj = 2.5, outer= F)
axis(2, at = seq(18,34,2), las = 2, cex.axis = 1.4)
mtext(side = 2, expression("Soil water content (%)"), cex = 1.8, padj = 0.2, outer= T)
mtext(side = 4, expression(ANPP~g~m^-2), cex = 1.8, padj = 0.5, outer= T)

legend("topleft", c("Mean annual SWC%"), col=c("black"), pch= c(19), cex = 1.5, horiz = F, bty='n')
legend("topright", c("ANPP"), col=c("palegreen4"), pch= c(19), cex = 1.5, horiz = F, bty='n')
# legend("top", c("PV Coverage at solar noon (1pm)"), col=c("azure3"), pch= c(15), cex = 1.5, horiz = F, bty='n')

dev.off()





