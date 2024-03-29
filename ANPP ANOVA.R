
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

df$Date<-as.Date(df$�..Date, ("%m/%d/%Y"))
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


# ANPPandSWC<-summaryBy(ANPP + SWC ~ Location, FUN = c(mean,std.error), na.rm = T, df)
# light$Location<-as.numeric(light$Location)
tiff(file = "month relationships.tiff", height = 12, width = 5, res = 800, units = "in", compression = "zip+p")
par(mfrow = c(5,1), omi = c(1, 0.5, 0.25, 0.1), mar = c(0.25,6,1,0.5))


plot(ANPP.mean ~ SWC.mean , JSG, pch = 16, col= "steelblue2",  xlim = c(14,40), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

m1<-lm(ANPP.mean~SWC.mean, JSG)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
ablineclip(m2,x1=min(JSG$SWC.mean,na.rm = TRUE),x2=max(JSG$SWC.mean,na.rm = TRUE), lwd=2, col = "grey60")
axis(1, at = seq(14,40,2), cex.axis = 1.2, labels = F)
axis(2, at = seq(200,1000,100), las = 2, cex.axis = 1, labels = T)
legend("topleft", c("Mean Annual SWC%"), bty = "n", cex =2)



plot(ANPP.mean ~ SWC.mean , May, pch = 16, col= "steelblue2",  xlim = c(14,40), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
m1<-lm(ANPP.mean~SWC.mean, May)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
ablineclip(m2,x1=min(May$SWC.mean,na.rm = TRUE),x2=max(May$SWC.mean,na.rm = TRUE), lwd=2, col = "darkblue")
axis(1, at = seq(14,40,2), cex.axis = 1.2, labels = F)
axis(2, at = seq(200,1000,100), las = 2, cex.axis = 1, labels = T)
legend("topleft", c("May"), bty = "n", cex =2)


plot(ANPP.mean ~ SWC.mean , June, pch = 16, col= "steelblue2",  xlim = c(14,40), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
m1<-lm(ANPP.mean~SWC.mean, June)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
ablineclip(m2,x1=min(June$SWC.mean,na.rm = TRUE),x2=max(June$SWC.mean,na.rm = TRUE), lwd=2, col = "purple")
axis(1, at = seq(14,40,2), cex.axis = 1.2, labels = F)
axis(2, at = seq(200,1000,100), las = 2, cex.axis = 1, labels = T)
legend("topleft", c("June"), bty = "n", cex =2)




plot(ANPP.mean ~ SWC.mean , July, pch = 16, col= "steelblue2",  xlim = c(14,40), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
m1<-lm(ANPP.mean~SWC.mean, July)
anova(m1)
summary(m1)
# p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
# new<-as.data.frame(p1$data)
# m2<-lm(predicted~x, new)
# coef(lm(predicted~x, new))
# ablineclip(m2,x1=min(July$SWC.mean,na.rm = TRUE),x2=max(July$SWC.mean,na.rm = TRUE), lwd=2, col = "grey60")
axis(1, at = seq(14,40,2), cex.axis = 1.2, labels = F)
axis(2, at = seq(200,1000,100), las = 2, cex.axis = 1, labels = T)

legend("topleft", c("July"), bty = "n", cex =2)


plot(ANPP.mean ~ SWC.mean , August, pch = 16, col= "steelblue2",  xlim = c(18,38), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
m1<-lm(ANPP.mean~SWC.mean, August)
anova(m1)
summary(m1)
# p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
# new<-as.data.frame(p1$data)
# m2<-lm(predicted~x, new)
# coef(lm(predicted~x, new))
# ablineclip(m2,x1=min(August$SWC.mean,na.rm = TRUE),x2=max(August$SWC.mean,na.rm = TRUE), lwd=2, col = "grey60")
axis(1, at = seq(14,40,2), cex.axis = 1.2, labels = T)
axis(2, at = seq(200,1000,100), las = 2, cex.axis = 1, labels = T)
legend("topleft", c("August"), bty = "n", cex =2)

mtext(side = 2, expression(ANPP~g~m^-2), cex = 2.2, padj = 0, outer= T)
mtext(side = 1, expression("SWC%"), cex = 2, padj = 2, outer = F)
dev.off()

# axis(1, at = seq(14,40,2), cex.axis = 1.2, labels = T)
# axis(2, at = seq(200,1000,100), las = 2, cex.axis = 1)





#Important ANOVA
OWE<-subset(productivity, Position == "OWE")
OWE$Height<-as.numeric(OWE$Height)
m1<-lm(ANPP.mean~Height,OWE)
anova(m1)

productivity$Height<-factor(productivity$Height)
m1<-lm(ANPP.mean~Height*Position,JSG)
anova(m1)
summary(m1)
plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Position))



p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
ablineclip(m2,x1=min(JSG$SWC.mean,na.rm = TRUE),x2=max(JSG$SWC.mean,na.rm = TRUE), lwd=2, col = "grey60")


m2<-lm(SWC~Location,df)
anova(m1)
summary(m1)



m1<-lm(ANPP~SWC,df)
anova(m1)
summary(m1)

plot(allEffects(m1))

plot(ANPP.mean~SWC.mean.mean,sdf)
plot(ANPP.mean~Location, productivity)
plot()
plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Location))
d1=cld(emmeans(m1, ~ Height*Month))

df<-read.csv("ANPP.csv")



##################################################################
# Box1: Big chunks for spar


mornin<-subset(light, TOD == "Ten")
noon<-subset(light, TOD == "One")
afternoon<-subset(light, TOD == "Four")


mornin1<-subset(light, TOD == "Ten" & Location == "1")
mornin2<-subset(light, TOD == "Ten" & Location == "2")
mornin3<-subset(light, TOD == "Ten" & Location == "3")
mornin4<-subset(light, TOD == "Ten" & Location == "4")
mornin5<-subset(light, TOD == "Ten" & Location == "5")
mornin6<-subset(light, TOD == "Ten" & Location == "6")
mornin7<-subset(light, TOD == "Ten" & Location == "7")
mornin8<-subset(light, TOD == "Ten" & Location == "8")
mornin9<-subset(light, TOD == "Ten" & Location == "9")
mornin10<-subset(light, TOD == "Ten" & Location == "10")
mornin11<-subset(light, TOD == "Ten" & Location == "11")
mornin12<-subset(light, TOD == "Ten" & Location == "12")
mornin13<-subset(light, TOD == "Ten" & Location == "13")
mornin14<-subset(light, TOD == "Ten" & Location == "14")
mornin15<-subset(light, TOD == "Ten" & Location == "15")

noon1<-subset(light, TOD == "One" & Location == "1")
noon2<-subset(light, TOD == "One" & Location == "2")
noon3<-subset(light, TOD == "One" & Location == "3")
noon4<-subset(light, TOD == "One" & Location == "4")
noon5<-subset(light, TOD == "One" & Location == "5")
noon6<-subset(light, TOD == "One" & Location == "6")
noon7<-subset(light, TOD == "One" & Location == "7")
noon8<-subset(light, TOD == "One" & Location == "8")
noon9<-subset(light, TOD == "One" & Location == "9")
noon10<-subset(light, TOD == "One" & Location == "10")
noon11<-subset(light, TOD == "One" & Location == "11")
noon12<-subset(light, TOD == "One" & Location == "12")
noon13<-subset(light, TOD == "One" & Location == "13")
noon14<-subset(light, TOD == "One" & Location == "14")
noon15<-subset(light, TOD == "One" & Location == "15")

afternoon1<-subset(light, TOD == "Four" & Location == "1")
afternoon2<-subset(light, TOD == "Four" & Location == "2")
afternoon3<-subset(light, TOD == "Four" & Location == "3")
afternoon4<-subset(light, TOD == "Four" & Location == "4")
afternoon5<-subset(light, TOD == "Four" & Location == "5")
afternoon6<-subset(light, TOD == "Four" & Location == "6")
afternoon7<-subset(light, TOD == "Four" & Location == "7")
afternoon8<-subset(light, TOD == "Four" & Location == "8")
afternoon9<-subset(light, TOD == "Four" & Location == "9")
afternoon10<-subset(light, TOD == "Four" & Location == "10")
afternoon11<-subset(light, TOD == "Four" & Location == "11")
afternoon12<-subset(light, TOD == "Four" & Location == "12")
afternoon13<-subset(light, TOD == "Four" & Location == "13")
afternoon14<-subset(light, TOD == "Four" & Location == "14")
afternoon15<-subset(light, TOD == "Four" & Location == "15")



tiff(file = "PPFD SWC.tiff", height = 8, width = 12, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.8), mar = c(1.5,3.5,0.5,3.5))

xx<-c(-500,500); yy<-c(-500,500)
plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,2000), xlim=c(1,15))

rect(xleft = 0, xright = 4.5, ybottom = -2000, ytop = 2200, col = "azure3", bty = "n", border = F)
box()

rect(xleft = 11.5, xright = 16, ybottom = -2000, ytop = 2200, col = "azure3", bty = "n", border = F)
box()


rect(0.5, 00, 01.5, mean(afternoon1$PPFD.mean), col = "lightgoldenrod2", border = "black")
rect(0.5, 00, 01.5, mean(noon1$PPFD.mean), col = "goldenrod2", border = "black")
rect(0.5, 00, 01.5, mean(mornin1$PPFD.mean), col = "navajowhite2", border = "black")

rect(01.5, 00, 02.5, mean(afternoon2$PPFD.mean), col = "lightgoldenrod2", border = "black")
rect(01.5, 00, 02.5, mean(mornin2$PPFD.mean), col = "navajowhite2", border = "black")
rect(01.5, 00, 02.5, mean(noon2$PPFD.mean), col = "goldenrod2", border = "black")

rect(02.5, 00, 03.5, mean(mornin3$PPFD.mean), col = "navajowhite2", border = "black")
rect(02.5, 00, 03.5, mean(afternoon3$PPFD.mean), col = "lightgoldenrod2", border = "black")
rect(02.5, 00, 03.5, mean(noon3$PPFD.mean), col = "goldenrod2", border = "black")

rect(03.5, 00, 04.5, mean(mornin4$PPFD.mean), col = "navajowhite2", border = "black")
rect(03.5, 00, 04.5, mean(noon4$PPFD.mean), col = "goldenrod2", border = "black")
rect(03.5, 00, 04.5, mean(afternoon4$PPFD.mean), col = "lightgoldenrod2", border = "black")


rect(04.5, 00, 05.5, mean(noon5$PPFD.mean), col = "goldenrod2", border = "black")
rect(04.5, 00, 05.5, mean(mornin5$PPFD.mean), col = "navajowhite2", border = "black")
rect(04.5, 00, 05.5, mean(afternoon5$PPFD.mean), col = "lightgoldenrod2", border = "black")

rect(05.5, 00, 06.5, mean(noon6$PPFD.mean), col = "goldenrod2", border = "black")
rect(05.5, 00, 06.5, mean(mornin6$PPFD.mean), col = "navajowhite2", border = "black")
rect(05.5, 00, 06.5, mean(afternoon6$PPFD.mean), col = "lightgoldenrod2", border = "black")


rect(06.5, 00, 07.5, mean(noon7$PPFD.mean), col = "goldenrod2", border = "black")
rect(06.5, 00, 07.5, mean(mornin7$PPFD.mean), col = "navajowhite2", border = "black")
rect(06.5, 00, 07.5, mean(afternoon7$PPFD.mean), col = "lightgoldenrod2", border = "black")


rect(07.5, 00, 08.5, mean(noon8$PPFD.mean), col = "goldenrod2", border = "black")
rect(07.5, 00, 08.5, mean(afternoon8$PPFD.mean), col = "lightgoldenrod2", border = "black")
rect(07.5, 00, 08.5, mean(mornin8$PPFD.mean), col = "navajowhite2", border = "black")

rect(08.5, 00, 09.5, mean(noon9$PPFD.mean), col = "goldenrod2", border = "black")
rect(08.5, 00, 09.5, mean(afternoon9$PPFD.mean), col = "lightgoldenrod2", border = "black")
rect(08.5, 00, 09.5, mean(mornin9$PPFD.mean), col = "navajowhite2", border = "black")


rect(09.5, 00, 10.5, mean(noon10$PPFD.mean), col = "goldenrod2", border = "black")
rect(09.5, 00, 10.5, mean(afternoon10$PPFD.mean), col = "lightgoldenrod2", border = "black")
rect(09.5, 00, 10.5, mean(mornin10$PPFD.mean), col = "navajowhite2", border = "black")

rect(10.5, 00, 11.5, mean(noon11$PPFD.mean), col = "goldenrod2", border = "black")
rect(10.5, 00, 11.5, mean(afternoon11$PPFD.mean), col = "lightgoldenrod2", border = "black")
rect(10.5, 00, 11.5, mean(mornin11$PPFD.mean), col = "navajowhite2", border = "black")

rect(11.5, 00, 12.5, mean(afternoon12$PPFD.mean), col = "lightgoldenrod2", border = "black")
rect(11.5, 00, 12.5, mean(noon12$PPFD.mean), col = "goldenrod2", border = "black")
rect(11.5, 00, 12.5, mean(mornin12$PPFD.mean), col = "navajowhite2", border = "black")

rect(12.5, 00, 13.5, mean(afternoon13$PPFD.mean), col = "lightgoldenrod2", border = "black")
rect(12.5, 00, 13.5, mean(mornin13$PPFD.mean), col = "navajowhite2", border = "black")
rect(12.5, 00, 13.5, mean(noon13$PPFD.mean), col = "goldenrod2", border = "black")

rect(13.5, 00, 14.5, mean(mornin14$PPFD.mean), col = "navajowhite2", border = "black")
rect(13.5, 00, 14.5, mean(afternoon14$PPFD.mean), col = "lightgoldenrod2", border = "black")
rect(13.5, 00, 14.5, mean(noon14$PPFD.mean), col = "goldenrod2", border = "black")

rect(14.5, 00, 15.5, mean(mornin15$PPFD.mean), col = "navajowhite2", border = "black")
rect(14.5, 00, 15.5, mean(noon15$PPFD.mean), col = "goldenrod2", border = "black")
rect(14.5, 00, 15.5, mean(afternoon15$PPFD.mean), col = "lightgoldenrod2", border = "black")

##############################################################################
axis(1, at = seq(1,15,1), las = 1, cex.axis = 1)
axis(2, at = seq(0,2000,200), las = 2, cex.axis = 1.2)
mtext(side = 2, expression("PPFD"), cex = 1.3, padj = 1, las = 2, outer= T)
mtext(side = 1, expression("Location"), cex = 1.8, padj = 2.5, outer= F)

legend("topleft", c("PPFD at 10am","PPFD at 1pm","PPFD at 4pm"), col=c("navajowhite2", "goldenrod2", "lightgoldenrod2"), pch= c(15,15,15), cex = 1.5, horiz = F, bty='n')
#################################################################################
##INSERT SWC DATA

#####################
#####################
##################
######################
######################
#################



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

tiff(file = "SWC and ANPP by cold season location.tiff", height = 8, width = 12, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.8), mar = c(1.5,3.5,0.5,3.5))


# par(new=T)
plotCI(bylocation$Location, bylocation$SWC.mean, bylocation$SWC.mean.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, cex = 1.7, col = "dodgerblue4", ylim = c(18,34))

plotCI(May$Location, May$ANPP.mean, May$ANPP.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 1.7, col = "palegreen4", ylim = c(400,800))
points(May$ANPP.mean ~ May$Location, col = "palegreen4", lty = 1, lwd = 2, type = "l")
axis(4, at = seq(400,800,50), las = 2, cex.axis = 1.2)
par(new=T)
plotCI(Coolseason$Location, Coolseason$SWC.mean, Coolseason$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 1.7, col = "dodgerblue4", ylim = c(18,34))

points(Coolseason$SWC.mean ~ Coolseason$Location, col = "dodgerblue4", lty = 1, lwd = 2, type = "l")




# dev.off()
axis(1, at = seq(1,15,1), las = 1, cex.axis = 1.2)

mtext(side = 4, expression(""), cex = 1.3, padj = 1, las = 2, outer= T)
mtext(side = 1, expression("Location"), cex = 1.8, padj = 2.5, outer= F)
axis(2, at = seq(18,34,2), las = 2, cex.axis = 1.4)
mtext(side = 2, expression("SWC%"), cex = 1.3, padj = 1, las = 2, outer= T)
mtext(side = 4, expression(ANPP~g~m^-2), cex = 1.8, padj = 0.5, outer= T)

legend("topleft", c("Cool season SWC%"), col=c("dodgerblue4"), pch= c(19), cex = 1.5, horiz = F, bty='n')
legend("topright", c("ANPP"), col=c("palegreen4"), pch= c(19), cex = 1.5, horiz = F, bty='n')
# legend("top", c("PV Coverage at solar noon (1pm)"), col=c("azure3"), pch= c(15), cex = 1.5, horiz = F, bty='n')

dev.off()


tiff(file = "SWC and ANPP by location.tiff", height = 8, width = 12, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.8), mar = c(1.5,3.5,0.5,3.5))


# par(new=T)
plotCI(bylocation$Location, bylocation$SWC.mean, bylocation$SWC.mean.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, cex = 1.7, col = "dodgerblue4", ylim = c(18,34))

plotCI(May$Location, May$ANPP.mean, May$ANPP.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 1.7, col = "palegreen4", ylim = c(400,800))
points(May$ANPP.mean ~ May$Location, col = "palegreen4", lty = 1, lwd = 2, type = "l")
axis(4, at = seq(400,800,50), las = 2, cex.axis = 1.2)
par(new=T)
plotCI(annual$Location, annual$SWC.mean, annual$SWC.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 1.7, col = "dodgerblue4", ylim = c(18,34))


points(annual$SWC.mean ~ annual$Location, col = "dodgerblue4", lty = 1, lwd = 2, type = "l")



# dev.off()
axis(1, at = seq(1,15,1), las = 1, cex.axis = 1.2)

mtext(side = 4, expression(""), cex = 1.3, padj = 1, las = 2, outer= T)
mtext(side = 1, expression("Location"), cex = 1.8, padj = 2.5, outer= F)
axis(2, at = seq(18,34,2), las = 2, cex.axis = 1.4)
mtext(side = 2, expression("SWC%"), cex = 1.3, padj = 1, las = 2, outer= T)
mtext(side = 4, expression(ANPP~g~m^-2), cex = 1.8, padj = 0.5, outer= T)

legend("topleft", c("Mean annual SWC%"), col=c("dodgerblue4"), pch= c(19), cex = 1.5, horiz = F, bty='n')
legend("topright", c("ANPP"), col=c("palegreen4"), pch= c(19), cex = 1.5, horiz = F, bty='n')
# legend("top", c("PV Coverage at solar noon (1pm)"), col=c("azure3"), pch= c(15), cex = 1.5, horiz = F, bty='n')

dev.off()





######################
#####################
#######################
#######################
######################

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

df$Date<-as.Date(df$�..Date, ("%m/%d/%Y"))
df$MoYr<-as.Date(floor_date(df$Date, "month"))
df$MoYr<-as.Date(df$MoYr, "%m/%d/%y")
df$SWC<-as.numeric(df$SWC)
df$Transect<-as.factor(df$Transect)
df$Direction<-as.factor(substr(df$Transect, 1, 1))
df$Height<-as.factor(substr(df$Transect, 2, 2))
df$Position<-as.factor(df$Position)
df$Flag<-as.numeric(df$Flag)
df$Location<-as.factor(df$Location)


sdf<-summaryBy(SWC ~ Location , FUN = c(mean,std.error), na.rm = T, df)
sdf$Location<-as.numeric(sdf$Location)


swc1<-subset(sdf, Location == "1")
swc2<-subset(sdf,  Location == "2")
swc3<-subset(sdf,  Location == "3")
swc4<-subset(sdf,  Location == "4")
swc5<-subset(sdf,  Location == "5")
swc6<-subset(sdf,  Location == "6")
swc7<-subset(sdf,  Location == "7")
swc8<-subset(sdf,  Location == "8")
swc9<-subset(sdf,  Location == "9")
swc10<-subset(sdf,  Location == "10")
swc11<-subset(sdf,  Location == "11")
swc12<-subset(sdf,  Location == "12")
swc13<-subset(sdf, Location == "13")
swc14<-subset(sdf,  Location == "14")
swc15<-subset(sdf,  Location == "15")


tiff(file = "SWC PPFD.tiff", height = 8, width = 12, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.8), mar = c(1.5,3.5,0.5,3.5))

xx<-c(-500,500); yy<-c(-500,500)
plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(20,36), xlim=c(1,15))

# rect(xleft = 0, xright = 4.5, ybottom = -2000, ytop = 2200, col = "azure3", bty = "n", border = F)
# box()
# 
# rect(xleft = 11.5, xright = 16, ybottom = -2000, ytop = 2200, col = "azure3", bty = "n", border = F)
# box()

rect(00, 00, 01.5, mean(swc1$SWC.mean), col = "skyblue4", border = "black")

rect(01.5, 00, 02.5, mean(swc2$SWC.mean), col = "skyblue4", border = "black")

rect(02.5, 00, 03.5, mean(swc3$SWC.mean), col = "skyblue4", border = "black")

rect(03.5, 00, 04.5, mean(swc4$SWC.mean), col = "skyblue4", border = "black")

rect(04.5, 00, 05.5, mean(swc5$SWC.mean), col = "skyblue4", border = "black")

rect(05.5, 00, 06.5, mean(swc6$SWC.mean), col = "skyblue4", border = "black")

rect(06.5, 00, 07.5, mean(swc7$SWC.mean), col = "skyblue4", border = "black")

rect(07.5, 00, 08.5, mean(swc8$SWC.mean), col = "skyblue4", border = "black")

rect(08.5, 00, 09.5, mean(swc9$SWC.mean), col = "skyblue4", border = "black")

rect(09.5, 00, 10.5, mean(swc10$SWC.mean), col = "skyblue4", border = "black")

rect(10.5, 00, 11.5, mean(swc11$SWC.mean), col = "skyblue4", border = "black")

rect(11.5, 00, 12.5, mean(swc12$SWC.mean), col = "skyblue4", border = "black")

rect(12.5, 00, 13.5, mean(swc13$SWC.mean), col = "skyblue4", border = "black")

rect(13.5, 00, 14.5, mean(swc14$SWC.mean), col = "skyblue4", border = "black")

rect(14.5, 00, 16, mean(swc15$SWC.mean), col = "skyblue4", border = "black")

axis(2, at = seq(18,36,2), las = 2, cex.axis = 1.4)
mtext(side = 2, expression("SWC%"), cex = 1.3, padj = 1, las = 2, outer= T)
legend("topleft", c("SWC%"), col=c("skyblue4"), pch= c(15), cex = 1.5, horiz = F, bty='n')
##############################################################################
axis(1, at = seq(1,15,1), las = 1, cex.axis = 1)
mtext(side = 1, expression("Location"), cex = 1.8, padj = 2.5, outer= F)
mtext(side = 4, expression(PPFD~(mu*mol~m^-2~s^-1)), cex = 1.6, padj = 1, outer= T) 
legend("topright", c("Light at 10am","Light at 1pm","Light at 4pm"), col=c("tomato2", "lightpink2", "turquoise"), pch= c(15,15,15), cex = 1.5, horiz = F, bty='n')
##############################################################################################################################

df<-read.csv("JSG_Biggie.csv")
df<-subset(df, KEEP == "Y")


df$Transect<-as.factor(df$Transect)
df$Direction<-as.factor(substr(df$Transect, 1, 1))
df$Height<-as.factor(substr(df$Transect, 2, 2))
df$Flag<-as.factor(df$Flag)

df$Date<-as.Date(df$�..Date, ("%m/%d/%Y"))
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

par(new=T)
plotCI(mornin$Location, mornin$PPFD.mean, mornin$PPFD.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="", cex = 0.0001, col = "tomato2", ylim = c(200,2000))

points(mornin$PPFD.mean ~ mornin$Location, col = "tomato2", lty = 1, pch=15, lwd = 5, type = "l")

points(noon$PPFD.mean ~ noon$Location, col = "lightpink2", lty = 1, pch=15, lwd = 5, type = "l")

points(afternoon$PPFD.mean ~ afternoon$Location, col = "turquoise", lty = 1, pch = 15, lwd = 5, type = "l")

axis(4, at = seq(0,2000,200), las = 2, cex.axis = 1.2)

dev.off()