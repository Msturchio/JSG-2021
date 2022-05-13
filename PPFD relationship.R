

library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans); library(sjPlot)
dev.off()

df<-read.csv("JSG_Big Boi.csv")
df<-subset(df, KEEP == "Y" & Month == "August" & TOD == "Four" & Season != "bad" & ï..Date == "8/30/2021")
# df<-subset(df, Season != "bad" & KEEP == "Y" & TOD == "Ten")


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
df$ANPP<-as.factor(df$ANPP)
df$PPFD<-as.numeric(df$PPFD)
df$TOD<-as.factor(df$TOD)
# dum<-subset(df, TOD == "One" & Season != "late")

# moisture<-summaryBy(SWC ~ Location, FUN = c(mean,std.error), na.rm = T, df)
# productivity<-summaryBy(ANPP ~ Position*Height , FUN = c(mean,std.error), na.rm = T, df)
# productivity$Position<-as.factor(productivity$Position)
# productivity$Height<-as.factor(productivity$Height)
JSG<-summaryBy(PPFD ~ Flag*Height*Direction*TOD, FUN = c(mean,std.error), na.rm = T, dtf)
JSG2<-summaryBy(PPFD.mean ~ Flag*Height*Direction, FUN = c(mean,std.error), na.rm = T, JSG)
write.csv(JSG2, "JSG2.csv")
# JSG$ANPP.mean<-as.numeric(JSG$ANPP.mean)
# JSG$PPFD.mean<-as.numeric(JSG$PPFD.mean)
# 
# JSGmorning<-subset(JSG, TOD == "Ten")
# JSGnoon<-subset(JSG, TOD == "One")
# JSGafternoon<-subset(JSG, TOD == "Four")

sdf<-read.csv("sdf.csv")
tiff(file = "Relationships.tiff", height = 6, width = 12, res = 600, units = "in", compression = "zip+p")
par(mfrow = c(1,2), omi = c(1, 1, 0.25, 0.1), mar = c(0.25,6,0.5,0.5))

plot(ANPP.mean ~ SWC.mean , sdf, pch = 19, col= "grey69",  xlim = c(18,38), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

m1<-lm(ANPP.mean~SWC.mean, sdf)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("SWC.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
text(28, 220, expression(italic(r)^2~'= 0.05'~","~ y ~'= 7.11x + 377'),cex=1.8)
ablineclip(m2,x1=min(sdf$SWC.mean,na.rm = TRUE),x2=max(sdf$SWC.mean,na.rm = TRUE), lwd=2, col = "black")
axis(1, at = seq(18,38,4), cex.axis = 2, labels = T)
axis(2, at = seq(200,1000,200), las = 2, cex.axis = 1.6, labels = T)

# text(34, 250, expression(italic(r)^2~'= 0.05'),cex=1.8)

mtext(side = 2, expression(ANPP~g~m^-2), cex = 2.6, padj = 0.3, outer= T)
mtext(side = 1, expression("Soil water content (%)"), cex = 2.2, padj = 2, outer= F)
legend("topleft", c("(a)"), cex = 2, horiz = F, bty='n')

################################
plot(ANPP.mean ~ PPFD.mean.mean , sdf, pch = 19, col= "grey69",  xlim = c(0,1600), ylim = c(200,1000), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

m1<-lm(ANPP.mean~PPFD.mean.mean, sdf)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("PPFD.mean.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
text(800, 220, expression(italic(r)^2~'= 0.07'~","~ y ~'= 0.08x + 502'),cex=1.8)
ablineclip(m2,x1=min(sdf$PPFD.mean.mean,na.rm = TRUE),x2=max(sdf$PPFD.mean.mean,na.rm = TRUE), lwd=2, col = "black")
axis(1, at = seq(0,1600,400), cex.axis = 1.6, labels = T)
axis(2, at = seq(200,1000,200), las = 2, cex.axis = 1.3, labels = F)

# text(1300, 250, expression(italic(r)^2~'= 0.07'),cex=1.8)
mtext(side = 1, expression(PPFD~(mu*mol~m^-2~s^-1)), cex = 2.2, padj = 1.75, outer= F)
legend("topleft", c("(b)"), cex = 2, horiz = F, bty='n')
dev.off()