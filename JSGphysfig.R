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

tiff(file = "JSGphys.tiff", height = 10, width = 12, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 1), mar = c(1.5,3.5,0.5,3.5))


xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(6,24), xlim=c(0,14))

dum<-subset(d1, Light == "light" & Height == "6")
rect(02, 00, 03, mean(dum$emmean), col = "royalblue1", border = "royalblue1")
ablineclip(v=2.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")


dum<-subset(d1, Light == "shade" & Height == "6")
rect(03, 00, 04, mean(dum$emmean), col = "royalblue1", border = "royalblue1")
rect(03, 00, 04, mean(dum$emmean), density = 10, lwd = 2, col = "white" , border = "black")
ablineclip(v=3.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")


dum<-subset(d1, Light == "light" & Height == "8")
rect(04, 00, 5, mean(dum$emmean), col = "royalblue4", border = "royalblue4")
ablineclip(v=4.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")

dum<-subset(d1, Light == "shade" & Height == "8")
rect(5, 00, 6, mean(dum$emmean), col = "royalblue4", border = "royalblue4")
rect(5, 00, 6, mean(dum$emmean), density = 10, lwd = 2, col = "white" , border = "black")
ablineclip(v=5.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")


box()

axis(2, at = seq(6,24,6), las = 2, cex.axis = 1.8, labels = T)

# legend("topleft", c("(a)"), bty = "n", cex = 1.5)

# mtext(side = 2, expression(italic(A)[net]~(mu*mol~m^-2~s^-1)), cex = 1.6, padj = -2, outer= F)

legend("top", c("Between Panels", "Beneath Panels"), density=c(0,30), cex = 1.4, horiz = F, bty='n')

mtext(side = 2, expression(italic(A)[net]~(mu*mol~m^-2~s^-1)), cex = 3, padj = -1, outer= F)

legend("topleft",col = c("royalblue1","royalblue4") , pch = 15, cex = 2, bty = "n",
       c(expression(italic(A)[net]~ (6~foot)), expression(italic(A)[net]~ (8~foot))))

legend("topright",col = c("mediumaquamarine","springgreen4") , pch = 15, cex = 2, bty = "n",
       c(expression(italic(phi)[psII]~ (6~foot)), expression(italic(phi)[psII]~ (8~foot))))
##############
#############
#############

m1<-lm(phi_j~Height*Light,df)
anova(m1)
# plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Height*Light))


##
par(new=T)
plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0.02,0.12), xlim=c(0,14))

dum<-subset(d1, Light == "light" & Height == "6")
rect(08, 00, 09, mean(dum$emmean), col = "mediumaquamarine", border = "mediumaquamarine")
ablineclip(v=8.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")


dum<-subset(d1, Light == "shade" & Height == "6")
rect(09, 00, 10, mean(dum$emmean), col = "mediumaquamarine", border = "mediumaquamarine")
rect(09, 00, 10, mean(dum$emmean), density = 10, lwd = 2, col = "white" , border = "black")
ablineclip(v=9.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")


dum<-subset(d1, Light == "light" & Height == "8")
rect(10, 00, 11, mean(dum$emmean), col = "springgreen4", border = "springgreen4")
ablineclip(v=10.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")

dum<-subset(d1, Light == "shade" & Height == "8")
rect(11, 00, 12, mean(dum$emmean), col = "springgreen4", border = "springgreen4")
rect(11, 00, 12, mean(dum$emmean), density = 10, lwd = 2, col = "white" , border = "black")
ablineclip(v=11.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")


box()

axis(4, at = seq(0.02,0.12,0.02), las = 2, cex.axis = 2.2, labels = T)

# legend("topleft", c("(b)"), bty = "n", cex = 1.5)
mtext(side = 4, expression(italic(phi)[psII]), cex = 4, padj = 2.1, outer= F)




dev.off()
