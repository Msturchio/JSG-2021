
library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans) ; library(sjPlot)
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


# ANPPandSWC<-summaryBy(ANPP + SWC ~ Location, FUN = c(mean,std.error), na.rm = T, df)
# light$Location<-as.numeric(light$Location)
tiff(file = "SWC ANPP montly relationships.tiff", height = 12, width = 4, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(4,1), omi = c(1, 0.5, 0.25, 0.1), mar = c(0.5,6,1,0.5))


# plot(ANPP.mean ~ SWC.mean , JSG, pch = 16, col= "steelblue2",  xlim = c(14,40), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
# 
# m1<-lm(ANPP.mean~SWC.mean, JSG)
# anova(m1)
# summary(m1)
# p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
# new<-as.data.frame(p1$data)
# m2<-lm(predicted~x, new)
# coef(lm(predicted~x, new))
# ablineclip(m2,x1=min(JSG$SWC.mean,na.rm = TRUE),x2=max(JSG$SWC.mean,na.rm = TRUE), lwd=2, col = "grey60")
# axis(1, at = seq(14,40,2), cex.axis = 1.2, labels = F)
# axis(2, at = seq(200,1000,100), las = 2, cex.axis = 1, labels = T)
# legend("topleft", c("Mean Annual SWC%"), bty = "n", cex =2)



plot(ANPP.mean ~ SWC.mean , May, pch = 16, col= "gray88",  xlim = c(14,40), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
m1<-lm(ANPP.mean~SWC.mean, May)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
text(21, 250, expression( y ~'= 7.76x + 337'),cex=1.2)
ablineclip(m2,x1=min(May$SWC.mean,na.rm = TRUE),x2=max(May$SWC.mean,na.rm = TRUE), lwd=2, col = "black")
axis(1, at = seq(14,40,4), cex.axis = 1.5, labels = F)
axis(2, at = seq(200,1000,200), las = 2, cex.axis = 2, labels = T)
legend("topleft", c("May"), bty = "n", cex =3)
text(34, 250, expression(italic(r)^2~'= 0.09'),cex=2.2)
legend("topright", c("(c)"), cex = 2, horiz = F, bty='n')

plot(ANPP.mean ~ SWC.mean , June, pch = 16, col= "gray88",  xlim = c(14,40), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
m1<-lm(ANPP.mean~SWC.mean, June)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
text(21, 600, expression(italic(R)[18]~'= 0.86Â±0.03'),cex=0.65)
ablineclip(m2,x1=min(June$SWC.mean,na.rm = TRUE),x2=max(June$SWC.mean,na.rm = TRUE), lwd=2, col = "black")
axis(1, at = seq(14,40,4), cex.axis = 1.5, labels = F)
axis(2, at = seq(200,1000,200), las = 2, cex.axis = 2, labels = T)
legend("topleft", c("June"), bty = "n", cex =3)
text(34, 250, expression(italic(r)^2~'= 0.06'),cex=2.2)
legend("topright", c("(d)"), cex = 2, horiz = F, bty='n')


plot(ANPP.mean ~ SWC.mean , July, pch = 16, col= "gray88",  xlim = c(14,40), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
m1<-lm(ANPP.mean~SWC.mean, July)
anova(m1)
summary(m1)
# p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
# new<-as.data.frame(p1$data)
# m2<-lm(predicted~x, new)
# coef(lm(predicted~x, new))
# ablineclip(m2,x1=min(July$SWC.mean,na.rm = TRUE),x2=max(July$SWC.mean,na.rm = TRUE), lwd=2, col = "grey60")
axis(1, at = seq(14,40,4), cex.axis = 1.5, labels = F)
axis(2, at = seq(200,1000,200), las = 2, cex.axis = 2, labels = T)
legend("topleft", c("July"), bty = "n", cex =3)
legend("topright", c("(e)"), cex = 2, horiz = F, bty='n')


plot(ANPP.mean ~ SWC.mean , August, pch = 16, col= "gray88",  xlim = c(18,38), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
m1<-lm(ANPP.mean~SWC.mean, August)
anova(m1)
summary(m1)
# p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
# new<-as.data.frame(p1$data)
# m2<-lm(predicted~x, new)
# coef(lm(predicted~x, new))
# ablineclip(m2,x1=min(August$SWC.mean,na.rm = TRUE),x2=max(August$SWC.mean,na.rm = TRUE), lwd=2, col = "grey60")
axis(1, at = seq(14,40,4), cex.axis = 1.5, labels = T)
axis(2, at = seq(200,1000,200), las = 2, cex.axis = 2, labels = T)
legend("topleft", c("August"), bty = "n", cex =3)
legend("topright", c("(f)"), cex = 2, horiz = F, bty='n')


mtext(side = 2, expression(Aboveground~net~primary~productivity~(ANPP~g~m^-2)), cex = 2, padj = 0, outer= T)
mtext(side = 1, expression("Soil Water Content (%)"), cex = 1.5, padj = 2, outer = F)
dev.off()




#################
###########
##############
###########


tiff(file = "SWC ANPP montly horizontal.tiff", height = 4, width = 12, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,4), omi = c(1, 1, 0.25, 0.1), mar = c(0.5,1,1,0.5))


# plot(ANPP.mean ~ SWC.mean , JSG, pch = 16, col= "steelblue2",  xlim = c(14,40), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
# 
# m1<-lm(ANPP.mean~SWC.mean, JSG)
# anova(m1)
# summary(m1)
# p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
# new<-as.data.frame(p1$data)
# m2<-lm(predicted~x, new)
# coef(lm(predicted~x, new))
# ablineclip(m2,x1=min(JSG$SWC.mean,na.rm = TRUE),x2=max(JSG$SWC.mean,na.rm = TRUE), lwd=2, col = "grey60")
# axis(1, at = seq(14,40,2), cex.axis = 1.2, labels = F)
# axis(2, at = seq(200,1000,100), las = 2, cex.axis = 1, labels = T)
# legend("topleft", c("Mean Annual SWC%"), bty = "n", cex =2)



plot(ANPP.mean ~ SWC.mean , May, pch = 16, col= "gray69",  xlim = c(14,40), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
m1<-lm(ANPP.mean~SWC.mean, May)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
# text(19, 220, expression( y ~'= 7.76x + 337'),cex=1.4)
text(26, 220, expression(italic(r)^2~'= 0.09'~","~ y ~'= 7.76x + 337'),cex=1.8)
ablineclip(m2,x1=min(May$SWC.mean,na.rm = TRUE),x2=max(May$SWC.mean,na.rm = TRUE), lwd=2, col = "black")
axis(1, at = seq(14,40,4), cex.axis = 1.5, labels = T)
axis(2, at = seq(200,1000,200), las = 2, cex.axis = 2, labels = T)
legend("topleft", c("May"), bty = "n", cex =3)
# text(34, 250, expression(italic(r)^2~'= 0.09'),cex=2.2)
legend("topright", c("(c)"), cex = 2.5, horiz = F, bty='n')
mtext(side = 2, expression(ANPP~g~m^-2), cex = 2, padj = -1.5, outer= F)




plot(ANPP.mean ~ SWC.mean , June, pch = 16, col= "gray69",  xlim = c(14,40), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
m1<-lm(ANPP.mean~SWC.mean, June)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
# text(19, 220, expression( y ~'= 6.28x+ 409'),cex=1.4)
text(26, 220, expression(italic(r)^2~'= 0.06'~","~ y ~'= 6.28x + 409'),cex=1.8)
ablineclip(m2,x1=min(June$SWC.mean,na.rm = TRUE),x2=max(June$SWC.mean,na.rm = TRUE), lwd=2, col = "black")
axis(1, at = seq(14,40,4), cex.axis = 1.5, labels = T)
axis(2, at = seq(200,1000,200), las = 2, cex.axis = 2, labels = F)
legend("topleft", c("June"), bty = "n", cex =3)
# text(34, 250, expression(italic(r)^2~'= 0.06'),cex=2.2)
legend("topright", c("(d)"), cex = 2.5, horiz = F, bty='n')


plot(ANPP.mean ~ SWC.mean , July, pch = 16, col= "gray69",  xlim = c(14,40), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
m1<-lm(ANPP.mean~SWC.mean, July)
anova(m1)
summary(m1)
# p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
# new<-as.data.frame(p1$data)
# m2<-lm(predicted~x, new)
# coef(lm(predicted~x, new))
# ablineclip(m2,x1=min(July$SWC.mean,na.rm = TRUE),x2=max(July$SWC.mean,na.rm = TRUE), lwd=2, col = "grey60")
axis(1, at = seq(14,40,4), cex.axis = 1.5, labels = T)
axis(2, at = seq(200,1000,200), las = 2, cex.axis = 2, labels = F)
legend("topleft", c("July"), bty = "n", cex =3)
legend("topright", c("(e)"), cex = 2.5, horiz = F, bty='n')


plot(ANPP.mean ~ SWC.mean , August, pch = 16, col= "gray69",  xlim = c(14,40), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
m1<-lm(ANPP.mean~SWC.mean, August)
anova(m1)
summary(m1)
# p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
# new<-as.data.frame(p1$data)
# m2<-lm(predicted~x, new)
# coef(lm(predicted~x, new))
# ablineclip(m2,x1=min(August$SWC.mean,na.rm = TRUE),x2=max(August$SWC.mean,na.rm = TRUE), lwd=2, col = "grey60")
axis(1, at = seq(14,40,4), cex.axis = 1.5, labels = T)
axis(2, at = seq(200,1000,200), las = 2, cex.axis = 2, labels = F)
legend("topleft", c("August"), bty = "n", cex =3)
legend("topright", c("(f)"), cex = 2.5, horiz = F, bty='n')



mtext(side = 1, expression("Soil Water Content (%)"), cex = 2, padj = 2.2, outer = T)
dev.off()
