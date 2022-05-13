library(dplyr); library(readxl); library(lubridate)
library(minpack.lm); library(lattice)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
############################################## 


############################################################################################
############################################################################################

# Make sure old files are removed from the 'clean' wd
# If old files are in there they'll be pulled and reassigned a Y for KEEP

############################################################################################
############################################################################################

clean<-"E:/CSU/JSG"
out_data<-"E:/CSU/JSG"
raw<-"E:/CSU/JSG"

############################################################################################

setwd(clean)

df<-read.csv("JSG LRC_2.csv") # Edit this file based on graphs
df$Date<-as.Date(df$Date, "%m-%d-%Y")
df$Light<-as.factor(df$Light)
df$UserIDs_in<-as.factor(df$UserIDs_in)
df<-subset(df, KEEP == "Y")

###########################################################################################
###########################################################################################

setwd(raw)

dates<-as.character(unique(df$Date))
setwd(raw)
for (dates in unique(df$Date)){
  dat<-subset(df, df$Date == dates)
  
  pdf(paste("mooken",as.character(unique(df$Light)),".pdf",sep = ""))
  print(xyplot(Photo ~ PARi | df$Foot, data = df, main = paste(as.character(unique(df$Light)))))
  dev.off()
}



1+1 # and done!
# Once all the plots look good, manually add them to the WETFEET MASTER file (in 'out_data' path)



tiff(file = "raw light.tiff", height = 8, width = 8, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,1.5,0.7,0.2))

# par(new=T)
dat<-subset(df, Foot == "8" & Light == "shade" )
plot(Photo ~ PARi, dat, pch = 16, col= "lightskyblue2", xlim = c(0,2000), ylim = c(0,20), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

axis(1, at = seq(0,2000,200), cex.axis = 1.1, labels = F)


# legend("topleft", c("(a)"), bty = "n")

par(new=T)
dat<-subset(df, Foot == "8" & Light == "light" )
plot(Photo ~ PARi, dat, pch = 1, col= "lightskyblue2", xlim = c(0,2000), ylim = c(0,20), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

axis(1, at = seq(0,2000,200), cex.axis = 1.1, labels = F)
axis(2, las = 2, cex.axis = 1.25, labels = F)
legend("bottomright", c("2.4 m light","2.4 m shade", "1.8 m light", "1.8 m shade" ), col = c("lightskyblue2","lightskyblue2", "grey69", "grey69"), pch = c(19,1,19,1), bty = "n", cex = 1.5)



par(new=T)
dat<-subset(df, Foot == "6" & Light == "shade" )
plot(Photo ~ PARi, dat, pch = 16, col= "grey69", xlim = c(0,2000), ylim = c(0,20), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

axis(1, at = seq(0,2000,200), cex.axis = 1 )
axis(2, las = 2, cex.axis = 2)



par(new=T)
dat<-subset(df, Foot == "6" & Light == "light" )
plot(Photo ~ PARi, dat, pch = 1, col= "grey69", xlim = c(0,2000), ylim = c(0,20), xaxt = "n", yaxt = "n", xlab = "", ylab = "")



mtext(side = 2, expression(italic(A)~(mu*mol~m^-2~s^-1)), cex = 2.5, padj = -0.5, outer = T)





mtext(side = 1, expression(PPFD~(mu*mol~m^-2~s^-1)), cex = 2.2, padj = 1.2, outer = T)
# legend("bottomright", c("NS"), bty = "n", cex = 1.5)



dev.off()

