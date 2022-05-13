
####################################################################################################


df<-read.csv("JSG_Biggie.csv")
df<-subset(df, KEEP == "Y")


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

sdf<-summaryBy(SWC ~ Location + TOD + Season + PPFD, FUN = c(mean,std.error), na.rm = T, df)
sdf$Location<-as.numeric(sdf$Location)
sdf$PPFD<-as.numeric(sdf$PPFD)

tiff(file = "JSG_Light.tiff", height = 6, width = 12, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,3.5,0.5,0.5))

# dum<-subset(sdf, MoYr == "2021-05-01")
# plot(dum$SWC.mean ~ dum$Location)

dat<-subset(sdf, Season == "late" & TOD == "Ten")
plotCI(dat$Location, dat$PPFD.mean, dat$PPFD.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 0.1, col = "forestgreen", ylim = c(0,1800))

dat<-subset(sdf, Season == "late" & TOD == "Ten")
points(dat$PPFD.mean ~ dat$Location, col = "darkgoldenrod1", lty = 1, lwd = 3, type = "l")

dat<-subset(sdf, Season == "late" & TOD == "One")
points(dat$PPFD.mean ~ dat$Location, col = "firebrick", lty = 1, lwd = 3, type = "l")

dat<-subset(sdf, Season == "late" & TOD == "Four")
points(dat$PPFD.mean ~ dat$Location, col = "purple", lty = 1, lwd = 3, type = "l")






axis(1, at = seq(0,16,1), las = 1, cex.axis = 1.4)
axis(2, at = seq(0,1800,200), las = 2, cex.axis = 1.6)

mtext(side = 2, expression("PPFD"), cex = 1.7, padj = 1, las = 2, outer= T)
mtext(side = 1, expression("Location"), cex = 1.8, padj = 2.5, outer= F)
legend("top", c("Cool Season (May & June)","Warm Season (July & August)"), col=c("dodgerblue4", "firebrick"), pch= c(16), cex = 1.15, horiz = F, bty='n')

dev.off()



sdf$MoYr<-as.factor(sdf$MoYr)
sdf$Location<-as.factor(sdf$Location)

dum<-subset(sdf, Season == "late")
m1<-lm(SWC.mean~PPFD*TOD,dum)
anova(m1)
plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ PPFD))

dum<-subset(sdf, Season == "late" & TOD == "One")
m1<-lm(SWC.mean~PPFD*Location,dum)
anova(m1)
plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ PPFD))