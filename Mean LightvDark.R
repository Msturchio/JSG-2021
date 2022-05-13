library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix);library(minpack.lm); library(mosaic); library(photosynthesis);library(dplyr); library(readxl); library(lubridate)
library(minpack.lm); library(lattice)
library(plantecophys); library(effects)

library(photosynthesis)


setwd("E:/CSU/JSG")


jsg<-read.csv("JSG_meansstderr_clean.csv")


# tiff(file = "Mean lightvdark JSG.tiff", height = 8, width = 8, res = 600, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.75, 1.2, 0.25, 0.1), mar = c(1,1,0.25,0.5))
jsgdf<-summaryBy(Photo_mean  ~ Light * ï..PARi * Foot, FUN = c(mean,std.error), na.rm = T, jsg)




plotCI(jsgdf$Photo_mean.mean, jsgdf$ï..PARi.mean , jsgdf$Photo_mean.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0,16))



dum<-subset(jsgdf, Light == "light")
plot(Photo_mean.mean ~ ï..PARi, dum, pch = NA, col= "orange", xlim = c(0,2000), ylim = c(0,16), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
par(new=T)
dum<-subset(jsgdf, Light == "shade" & ï..PARi != 1200)
plot(Photo_mean.mean ~ ï..PARi, dum, pch = NA, col= "black", xlim = c(0,2000), ylim = c(0,16), xaxt = "n", yaxt = "n", xlab = "", ylab = "")


# rect(xleft = 1200, xright = 600, ybottom = -999, ytop = 999, col = "grey82", bty = "n", border = F)
# box()

dum<-subset(jsgdf, Light == "light" )

points(Photo_mean.mean ~ ï..PARi, dum, pch = 19, col= "orange", xlim = c(0,2000), ylim = c(0,16), xaxt = "n", yaxt = "n", xlab = "", ylab = "", lty = 1, lwd = 3, type = "l")


dum<-subset(jsgdf, Light == "shade" & ï..PARi != 1200)
points(Photo_mean.mean ~ ï..PARi, dum, pch = 19, col= "black", xlim = c(0,2000), ylim = c(0,16), xaxt = "n", yaxt = "n", xlab = "", ylab = "", lty = 1,lwd = 3, type = "l")


axis(1, at = seq(0,2000,400), cex.axis = 1.2)

axis(2, at = seq(0,16,2), las = 1,labels = T)

mtext(side = 2, expression(italic(A)~(mu*mol~m^-2~s^-1)), cex = 1.6, padj = -1, outer = T)

mtext(side = 1, expression(PAR[i]), cex = 1.6, padj = 1.5, outer = T)

legend("right", c("Under Panel","Outside of Panel"), col=c("black", "orange"), pch= c(19,19), cex = 1.6, horiz = F, bty='n')

# legend("bottomright", c(expression(Light~Saturation~of~ italic(A))), col=c("grey82"), pch= c(15), cex = 1.4, horiz = F, bty='n')


# K<-273.15; df$alpha<-df$AirTemp_C.mean+K; df$beta<-K+17
# q10 <- function(x) {0.46468 * (3.1212) ^ (((x+273.15)-290.15)/10)}
# points(q10(seq(18,28.5,1)) ~ seq(18,28.5,1), type = "l", lwd = 2, lty = 3, col = "black")
# dum<-subset(jsgdf, ï..PARi <= 1200)
# 
# 
# reg<-lm(Photo_mean.mean ~ ï..PARi, data = dum)
# 
# coeff=coefficients(reg)
# eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
# points(jsgdf, main=eq, lty = 1, type = "l")
# abline(reg, col="blue" , lty = 2, lwd = 1.5)

# abline(v=1200)
dum<-subset(jsg, Light == "shade")
plot(Photo_mean ~ ï..PARi, dum)

require(ggplot2)
set.seed(5678)
x <- seq(0, 2000, 100)
y <- nrh(x, 35, 0, 0, 2) + rnorm(length(x), 0, 0.5)
dum <- data.frame(x = dum$ï..PARi, y = dum$Photo_mean)
fit <- nls(y ~ SSnrh(x, asym, phi, theta, rd), data = dum)
## Visualize observed and simulated
ggplot(data = dum, aes(x = x, y = y )) + 
  geom_point() + 
  geom_line(aes(y = fitted(fit)))


dum<-subset(jsg, Light == "light")
plot(Photo_mean ~ ï..PARi, dum)

require(ggplot2)
set.seed(5678)
x <- seq(0, 2000, 100)
y <- nrh(x, 35, 0, 0, 2) + rnorm(length(x), 0, 0.5)
dum <- data.frame(x = dum$ï..PARi, y = dum$Photo_mean)
fit <- nls(y ~ SSnrh(x, asym, phi, theta, rd), data = dum)
## Visualize observed and simulated
ggplot(data = dum, aes(x = x, y = y )) + 
  geom_point() + 
  geom_line(aes(y = fitted(fit)))






summary(fit)




model <- nls(YL ~ NLS.YL(Dens, a, i), data = dum)

set.seed(1234)
X <- c(50, 100, 400, 600, 900, 1200, 1500, 2000)
a <- 15; b <- 0.5
Ye <- as.numeric( SSmicmen(X, a, b) )
res <- rnorm(8, 0, 0.1)
Y <- Ye + res

#nls fit
model <- nls(Y ~ SSmicmen(X, a, b))
summary(model)
plot(model)

m1<-lm(Photo_mean~ï..PARi, dum)
anova(m1)
plot(m1)

model <- drm(Y ~ X, fct = DRC.asymReg())
plot(model, log="", main = "Asymptotic regression")
fit

dum<-subset(jsgdf, ï..PARi <= 600)

reg<-lm(Photo_mean.mean ~ ï..PARi, data = dum)

coeff=coefficients(reg)
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
points(jsgdf, main=eq, lty = 1, type = "l")
abline(reg, col="blue" , lty = 2, lwd = 1.5)

# abline(v=600)
# legend("topright", c(expression(italic(R)[mass]*'*',"Trt", italic(R)[m]*'x Trt' )), bty = "n", cex = 1.3)

dev.off()
