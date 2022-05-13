library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans); library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix)


setwd("E:/CSU/JSG")
df<-read.csv("LRC data v2.csv")
df<-subset(df, KEEP == "Y")


df$Height<-as.factor(df$Height)
df$Transect<-as.factor(df$Transect)
df$Light<-as.factor(df$Light)
df$Direction<-as.factor(df$Direction)

##########################################################


m1<-lm(k_sat~Height*Light,df)
anova(m1)
# plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Height*Light))

tiff(file = "heightbylight.tiff", height = 10, width = 6, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(2,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,2.5,0.5,0.5))


xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(6,24), xlim=c(0,18))

dum<-subset(d1, Light == "light" & Height == "6")
rect(02, 00, 04, mean(dum$emmean), col = "burlywood3", border = "darkblue")
ablineclip(v=3, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "aquamarine4")


dum<-subset(d1, Light == "shade" & Height == "6")
rect(04, 00, 06, mean(dum$emmean), col = "burlywood3", border = "darkblue")
rect(04, 00, 06, mean(dum$emmean), density = 10, lwd = 1, col = "white" , border = "black")
ablineclip(v=5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "aquamarine4")


dum<-subset(d1, Light == "light" & Height == "8")
rect(08, 00, 10, mean(dum$emmean), col = "darkslategrey", border = "darkblue")
ablineclip(v=9, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "aquamarine4")

dum<-subset(d1, Light == "shade" & Height == "8")
rect(10, 00, 12, mean(dum$emmean), col = "darkslategrey", border = "darkblue")
rect(10, 00, 12, mean(dum$emmean), density = 10, lwd = 1, col = "white" , border = "black")
ablineclip(v=11, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "aquamarine4")


box()

axis(2, at = seq(6,24,6), las = 2, cex.axis = 1.4, labels = T)

legend("topleft", c("(a)"), bty = "n", cex = 1.5)

# mtext(side = 2, expression(italic(A)[net]~(mu*mol~m^-2~s^-1)), cex = 1.6, padj = -2, outer= F)

legend("topright", c("Outside of Panel", "Under Panel"), density=c(0,30), cex = 1.4, horiz = F, bty='n')
legend("right", c("6 foot", "8 foot"), col = c("burlywood3", "darkslategrey"), pch = 15, cex = 1.8, horiz = F, bty='n')
mtext(side = 2, expression(italic(A)[net]~(mu*mol~m^-2~s^-1)), cex = 1.8, padj = -2, outer= F)
##############
#############
#############

m1<-lm(phi_j~Height*Light,df)
anova(m1)
# plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Height*Light))


##

plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0.02,0.12), xlim=c(0,18))

dum<-subset(d1, Light == "light" & Height == "6")
rect(02, 00, 04, mean(dum$emmean), col = "burlywood3", border = "darkblue")
ablineclip(v=3, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "aquamarine4")


dum<-subset(d1, Light == "shade" & Height == "6")
rect(04, 00, 06, mean(dum$emmean), col = "burlywood3", border = "darkblue")
rect(04, 00, 06, mean(dum$emmean), density = 10, lwd = 1, col = "white" , border = "black")
ablineclip(v=5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "aquamarine4")


dum<-subset(d1, Light == "light" & Height == "8")
rect(08, 00, 10, mean(dum$emmean), col = "darkslategrey", border = "darkblue")
ablineclip(v=9, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "aquamarine4")

dum<-subset(d1, Light == "shade" & Height == "8")
rect(10, 00, 12, mean(dum$emmean), col = "darkslategrey", border = "darkblue")
rect(10, 00, 12, mean(dum$emmean), density = 10, lwd = 1, col = "white" , border = "black")
ablineclip(v=11, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "aquamarine4")


box()

axis(2, at = seq(0.02,0.12,0.02), las = 2, cex.axis = 1.4, labels = T)

legend("topleft", c("(b)"), bty = "n", cex = 1.5)
mtext(side = 2, expression(italic(phi)[psII]), cex = 3, padj = -1.6, outer= F)




dev.off()