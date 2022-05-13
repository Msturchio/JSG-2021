
library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans)
dev.off()
setwd("E:/CSU/JSG")
df<-read.csv("JSG_Big Boi.csv")
dtf<-subset(df, KEEP == "Y")
dft<-subset(dtf, Season != "bad")
dft$Date<-as.Date(dft$ï..Date, ("%m/%d/%Y"))
df<-subset(dtf, Season != "bad" & KEEP == "Y" & TOD == "One")


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
# dft$Direction<-as.factor(substr(dft$Transect, 1, 1))
# dft$Height<-as.factor(substr(dft$Transect, 2, 2))
df$Position<-as.factor(df$Position)
df$Flag<-as.factor(df$Flag)
df$Location<-as.factor(df$Location)
df$grass_mass<-df$grass_mass*10
df$forb_mass<-df$forb_mass*10
df$grass_mass<-as.numeric(df$grass_mass)
df$forb_mass<-as.numeric(df$forb_mass)
df$ANPP<-df$ANPP*10
df$ANPP<-as.numeric(df$ANPP)

sdf<-summaryBy(ANPP ~ Location , FUN = c(mean,std.error), na.rm = T, df)
sdfm<-summaryBy(SWC ~ Location , FUN = c(mean,std.error), na.rm = T, df)
sdfl<-summaryBy(PPFD ~ Location, FUN = c(mean,std.error), na.rm = T, dtf)

foranova<-subset(df, Date == "2021-05-28" & TOD == "One")
foranova2<-subset(foranova, Location != "12")
foranova3<-subset(foranova2, Location != "13")
foranova4<-subset(foranova3, Location != "14")
foranova5<-subset(foranova4, Location != "15")
# foranova<-subset(dft, Date == "2021-05-28")
# 
# foranovappfd<-subset(foranova, )


sdf$Location<-as.numeric(sdf$Location)
sdfm$Location<-as.numeric(sdfm$Location)
sdfl$Location<-as.numeric(sdfl$Location)

ANPP1<-subset(sdf, Location == "1")
ANPP2<-subset(sdf,  Location == "2")
ANPP3<-subset(sdf,  Location == "3")
ANPP4<-subset(sdf,  Location == "4")
ANPP5<-subset(sdf,  Location == "5")
ANPP6<-subset(sdf,  Location == "6")
ANPP7<-subset(sdf,  Location == "7")
ANPP8<-subset(sdf,  Location == "8")
ANPP9<-subset(sdf,  Location == "9")
ANPP10<-subset(sdf,  Location == "10")
ANPP11<-subset(sdf,  Location == "11")
ANPP12<-subset(sdf,  Location == "12")
ANPP13<-subset(sdf, Location == "13")
ANPP14<-subset(sdf,  Location == "14")
ANPP15<-subset(sdf,  Location == "15")

tiff(file = "JSG 2021 igure 5.tiff", height = 8, width = 12, res = 600, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.8), mar = c(1.5,2.5,0.5,8))

xx<-c(-500,500); yy<-c(-500,500)
plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(400,800), xlim=c(1,15))

rect(xleft = 0, xright = 16, ybottom = -2000, ytop = 2200, col = "honeydew", bty = "n", border = F)
box()
# 
# rect(xleft = 11.5, xright = 16, ybottom = -2000, ytop = 2200, col = "azure3", bty = "n", border = F)
# box()
# rect(xleft = 0.5, xright = 4.5, ybottom = -2, ytop = 700, col = "grey95", bty = "n", border = F)
# rect(xleft = 11.5, xright = 15.5, ybottom = -2, ytop = 700, col = "grey95", bty = "n", border = F)
# rect(xleft = 0.5, xright = 4.5, ybottom = 695, ytop = 700, col = "slategray1", bty = "n", border = F)
# rect(xleft = 11.5, xright = 15.5, ybottom = 695, ytop = 700, col = "slategray1", bty = "n", border = F)
# rect(xleft = 2.45, xright = 2.55, ybottom = -2, ytop = 700, col = "slategray1", bty = "n", border = F)
# rect(xleft = 13.45, xright = 13.55, ybottom = -2, ytop = 700, col = "slategray1", bty = "n", border = F)
# rect(xleft = 0.5, xright = 4.5, ybottom = 698, ytop = 700, col = "black", bty = "n", border = F)
# rect(xleft = 11.5, xright = 15.5, ybottom = 698, ytop = 700, col = "black", bty = "n", border = F)
rect(xleft = 0.5, xright = 4.5, ybottom = 742, ytop = 750, col = "grey52", bty = "n", border = F)
rect(xleft = 11.5, xright = 15.5, ybottom = 742, ytop = 750, col = "grey52", bty = "n", border = F)
rect(xleft = 2.45, xright = 2.55, ybottom = -2, ytop = 750, col = "grey52", bty = "n", border = F)
rect(xleft = 13.45, xright = 13.55, ybottom = -2, ytop = 750, col = "grey52", bty = "n", border = F)
rect(xleft = 0.5, xright = 4.5, ybottom = 747, ytop = 750, col = "black", bty = "n", border = F)
rect(xleft = 11.5, xright = 15.5, ybottom = 747, ytop = 750, col = "black", bty = "n", border = F)
box()

rect(00.75, 00, 01.25, mean(ANPP1$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=1, y1=as.numeric(mean(ANPP1$ANPP.mean) + (ANPP1$ANPP.std.error)), y2=as.numeric(mean(ANPP1$ANPP.mean) - (ANPP1$ANPP.std.error)))

rect(01.75, 00, 02.25, mean(ANPP2$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=2, y1=as.numeric(mean(ANPP2$ANPP.mean) + (ANPP2$ANPP.std.error)), y2=as.numeric(mean(ANPP2$ANPP.mean) - (ANPP2$ANPP.std.error)))

rect(02.75, 00, 03.25, mean(ANPP3$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=3, y1=as.numeric(mean(ANPP3$ANPP.mean) + (ANPP3$ANPP.std.error)), y2=as.numeric(mean(ANPP3$ANPP.mean) - (ANPP3$ANPP.std.error)))

rect(03.75, 00, 04.25, mean(ANPP4$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=4, y1=as.numeric(mean(ANPP4$ANPP.mean) + (ANPP4$ANPP.std.error)), y2=as.numeric(mean(ANPP4$ANPP.mean) - (ANPP4$ANPP.std.error)))

rect(04.75, 00, 05.25, mean(ANPP5$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=5, y1=as.numeric(mean(ANPP5$ANPP.mean) + (ANPP5$ANPP.std.error)), y2=as.numeric(mean(ANPP5$ANPP.mean) - (ANPP5$ANPP.std.error)))

rect(05.75, 00, 06.25,mean(ANPP6$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=6, y1=as.numeric(mean(ANPP6$ANPP.mean) + (ANPP6$ANPP.std.error)), y2=as.numeric(mean(ANPP6$ANPP.mean) - (ANPP6$ANPP.std.error)))

rect(06.75, 00, 07.25, mean(ANPP7$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=7, y1=as.numeric(mean(ANPP7$ANPP.mean) + (ANPP7$ANPP.std.error)), y2=as.numeric(mean(ANPP7$ANPP.mean) - (ANPP7$ANPP.std.error)))

rect(07.75, 00, 08.25, mean(ANPP8$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=8, y1=as.numeric(mean(ANPP8$ANPP.mean) + (ANPP8$ANPP.std.error)), y2=as.numeric(mean(ANPP8$ANPP.mean) - (ANPP8$ANPP.std.error)))

rect(08.75, 00, 09.25, mean(ANPP9$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=9, y1=as.numeric(mean(ANPP9$ANPP.mean) + (ANPP9$ANPP.std.error)), y2=as.numeric(mean(ANPP9$ANPP.mean) - (ANPP9$ANPP.std.error)))

rect(09.75, 00, 10.25, mean(ANPP10$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=10, y1=as.numeric(mean(ANPP10$ANPP.mean) + (ANPP10$ANPP.std.error)), y2=as.numeric(mean(ANPP10$ANPP.mean) - (ANPP10$ANPP.std.error)))

rect(10.75, 00, 11.25, mean(ANPP11$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=11, y1=as.numeric(mean(ANPP11$ANPP.mean) + (ANPP11$ANPP.std.error)), y2=as.numeric(mean(ANPP11$ANPP.mean) - (ANPP11$ANPP.std.error)))

rect(11.75, 00, 12.25, mean(ANPP12$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=12, y1=as.numeric(mean(ANPP12$ANPP.mean) + (ANPP12$ANPP.std.error)), y2=as.numeric(mean(ANPP12$ANPP.mean) - (ANPP12$ANPP.std.error)))

rect(12.75, 00, 13.25, mean(ANPP13$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=13, y1=as.numeric(mean(ANPP13$ANPP.mean) + (ANPP13$ANPP.std.error)), y2=as.numeric(mean(ANPP13$ANPP.mean) - (ANPP13$ANPP.std.error)))

rect(13.75, 00, 14.25, mean(ANPP14$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=14, y1=as.numeric(mean(ANPP14$ANPP.mean) + (ANPP14$ANPP.std.error)), y2=as.numeric(mean(ANPP14$ANPP.mean) - (ANPP14$ANPP.std.error)))

rect(14.75, 00, 15.25, mean(ANPP15$ANPP.mean), col = "darkseagreen3", border = "black")

ablineclip(v=15, y1=as.numeric(mean(ANPP15$ANPP.mean) + (ANPP15$ANPP.std.error)), y2=as.numeric(mean(ANPP15$ANPP.mean) - (ANPP15$ANPP.std.error)))



axis(2, at = seq(400,800,100), las = 2, cex.axis = 1.4)
mtext(side = 2, expression(ANPP~g~m^-2), cex = 1.8, padj = - 0.5, outer= T)


# corners = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
# par(xpd = TRUE) #Draw outside plot area
# text(expression(PPFD~(mu*mol~m^-2~s^-1) = corners[2]+.5, y = mean(corners[3:4])))
# text(x = corners[2]+.5, y = mean(corners[3:4]), "Strength", srt = 270)


mtext(side = 4, expression("Soil water content (%)"), cex = 1.7, padj = -3, outer= T)
legend("topright", c("SWC%"), col=c("skyblue"),lty = 1, lwd = 2, cex = 1.5, horiz = F, bty='n')
legend("topleft", c("ANPP"), col=c("darkseagreen3"), pch= c(15), cex = 1.5, horiz = F, bty='n')
legend("top", c("PPFD"), col=c("grey16"), lty = 1,lwd = 2, cex = 1.5, horiz = F, bty='n')

#####################

par(new=T)

plotCI(sdfm$Location, sdfm$SWC.mean, sdfm$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=0.1, cex = 0.1, col = "skyblue", ylim = c(20,34))
points(sdfm$SWC.mean ~ sdfm$Location, col = "skyblue", lty = 1, lwd = 4, type = "l")
axis(4, at = seq(20,36,2), las = 2, cex.axis = 1.5)

par(new=T)
plotCI(sdfl$Location, sdfl$PPFD.mean, sdfl$PPFD.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 0.5, col = "grey16", ylim = c(00,1800))
points(sdfl$PPFD.mean ~ sdfl$Location, col = "grey16", lty = 1, lwd = 4, type = "l")

par(new=T)
plotCI(lightmean$Location, lightmean$PPFD.mean.mean, lightmean$PPFD.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="", cex = 0.2, pch=16, col = "black", xlim = c(1,15), ylim = c(0,1900))
points(lightmean$PPFD.mean.mean ~ lightmean$Location, col = "black", lty = 1, pch = 15, lwd = 4, type = "l")
# axis(4, at = seq(0,1800,200), las = 2, cex.axis = 1.3)

mtext(side = 4, expression(PPFD~(mu*mol~m^-2~s^-1)), cex = 1.8, padj = 1.5, outer= T)


axis(1, at = seq(1,15,1), las = 1, cex.axis = 1.2)
mtext(side = 1, expression("Location"), cex = 1.8, padj = 2.5, outer= F)


dev.off()
###########
##########
###########

dum<-subset(df, Location != "12")
dum2<-subset(dum, Location != "13")
dum3<-subset(dum2, Location != "14")
dum4<-subset(dum3, Location != "15")
dum5<-subset(dum4, Month != "September")


m1<-lm(ANPP~Location*Height*Direction, foranova5)
anova(m1)
summary(m1)
plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Location))

write.csv(d1, "SMmeans.csv")
