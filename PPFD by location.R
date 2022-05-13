
library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix)

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


light<-summaryBy(PPFD ~ Location + TOD, FUN = c(mean,std.error), na.rm = T, df)
light$Location<-as.numeric(light$Location)
light$PPFD<-as.numeric(light$PPFD)





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



tiff(file = "PPFD location coallated.tiff", height = 8, width = 12, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,3.5,0.5,0.5))

xx<-c(-500,500); yy<-c(-500,500)
plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,2000), xlim=c(0,16))

rect(0.5, 00, 01.5, mean(afternoon1$PPFD.mean), col = "orange", border = "black")
rect(0.5, 00, 01.5, mean(noon1$PPFD.mean), col = "Red", border = "black")
rect(0.5, 00, 01.5, mean(mornin1$PPFD.mean), col = "yellow", border = "black")

rect(01.5, 00, 02.5, mean(afternoon2$PPFD.mean), col = "orange", border = "black")
rect(01.5, 00, 02.5, mean(mornin2$PPFD.mean), col = "yellow", border = "black")
rect(01.5, 00, 02.5, mean(noon2$PPFD.mean), col = "Red", border = "black")

rect(02.5, 00, 03.5, mean(mornin3$PPFD.mean), col = "yellow", border = "black")
rect(02.5, 00, 03.5, mean(afternoon3$PPFD.mean), col = "orange", border = "black")
rect(02.5, 00, 03.5, mean(noon3$PPFD.mean), col = "Red", border = "black")

rect(03.5, 00, 04.5, mean(mornin4$PPFD.mean), col = "yellow", border = "black")
rect(03.5, 00, 04.5, mean(noon4$PPFD.mean), col = "Red", border = "black")
rect(03.5, 00, 04.5, mean(afternoon4$PPFD.mean), col = "orange", border = "black")


rect(04.5, 00, 05.5, mean(noon5$PPFD.mean), col = "Red", border = "black")
rect(04.5, 00, 05.5, mean(mornin5$PPFD.mean), col = "yellow", border = "black")
rect(04.5, 00, 05.5, mean(afternoon5$PPFD.mean), col = "orange", border = "black")

rect(05.5, 00, 06.5, mean(noon6$PPFD.mean), col = "Red", border = "black")
rect(05.5, 00, 06.5, mean(mornin6$PPFD.mean), col = "yellow", border = "black")
rect(05.5, 00, 06.5, mean(afternoon6$PPFD.mean), col = "orange", border = "black")


rect(06.5, 00, 07.5, mean(noon7$PPFD.mean), col = "Red", border = "black")
rect(06.5, 00, 07.5, mean(mornin7$PPFD.mean), col = "yellow", border = "black")
rect(06.5, 00, 07.5, mean(afternoon7$PPFD.mean), col = "orange", border = "black")


rect(07.5, 00, 08.5, mean(noon8$PPFD.mean), col = "Red", border = "black")
rect(07.5, 00, 08.5, mean(afternoon8$PPFD.mean), col = "orange", border = "black")
rect(07.5, 00, 08.5, mean(mornin8$PPFD.mean), col = "yellow", border = "black")

rect(08.5, 00, 09.5, mean(noon9$PPFD.mean), col = "Red", border = "black")
rect(08.5, 00, 09.5, mean(afternoon9$PPFD.mean), col = "orange", border = "black")
rect(08.5, 00, 09.5, mean(mornin9$PPFD.mean), col = "yellow", border = "black")


rect(09.5, 00, 10.5, mean(noon10$PPFD.mean), col = "Red", border = "black")
rect(09.5, 00, 10.5, mean(afternoon10$PPFD.mean), col = "orange", border = "black")
rect(09.5, 00, 10.5, mean(mornin10$PPFD.mean), col = "yellow", border = "black")

rect(10.5, 00, 11.5, mean(noon11$PPFD.mean), col = "Red", border = "black")
rect(10.5, 00, 11.5, mean(afternoon11$PPFD.mean), col = "orange", border = "black")
rect(10.5, 00, 11.5, mean(mornin11$PPFD.mean), col = "yellow", border = "black")

rect(11.5, 00, 12.5, mean(afternoon12$PPFD.mean), col = "orange", border = "black")
rect(11.5, 00, 12.5, mean(noon12$PPFD.mean), col = "Red", border = "black")
rect(11.5, 00, 12.5, mean(mornin12$PPFD.mean), col = "yellow", border = "black")

rect(12.5, 00, 13.5, mean(afternoon13$PPFD.mean), col = "orange", border = "black")
rect(12.5, 00, 13.5, mean(mornin13$PPFD.mean), col = "yellow", border = "black")
rect(12.5, 00, 13.5, mean(noon13$PPFD.mean), col = "Red", border = "black")

rect(13.5, 00, 14.5, mean(mornin14$PPFD.mean), col = "yellow", border = "black")
rect(13.5, 00, 14.5, mean(afternoon14$PPFD.mean), col = "orange", border = "black")
rect(13.5, 00, 14.5, mean(noon14$PPFD.mean), col = "Red", border = "black")

rect(14.5, 00, 15.5, mean(mornin15$PPFD.mean), col = "yellow", border = "black")
rect(14.5, 00, 15.5, mean(noon15$PPFD.mean), col = "Red", border = "black")
rect(14.5, 00, 15.5, mean(afternoon15$PPFD.mean), col = "orange", border = "black")

##############################################################################
axis(1, at = seq(1,15,1), las = 1, cex.axis = 1)
axis(2, at = seq(0,2000,100), las = 2, cex.axis = 1.2)
mtext(side = 2, expression("PPFD"), cex = 1.7, padj = 1, las = 2, outer= T)
mtext(side = 1, expression("Location"), cex = 1.8, padj = 2.5, outer= F)

legend("topright", c("10am","1pm Solar noon","4pm"), col=c("yellow", "red", "orange"), pch= c(15,15,15), cex = 1.35, horiz = F, bty='n')



dev.off()
