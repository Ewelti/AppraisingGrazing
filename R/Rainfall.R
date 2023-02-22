##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing/rawData")

#################rain
# attach data
rain <- read.csv("Rainfall.csv")
head(rain)

## DOY month dividers: 151.5,181.5,212.5,243.5,273.5 (May-Oct)
 
################
par(mar=c(4,5,0.2,0.2))#,mfrow=c(2,4))
#########Forb C
plot(1, type="n", xlim=c(min(rain$DOY), max(rain$DOY)), ylim=c(0,max(rain$Rainfall_mm)),las=1,ylab="",xlab="")

title(ylab="Daily rainfall (mm)", line=3, cex.lab=1.6)
title(xlab="DOY", line=2.5, cex.lab=1.6)
polygon(x=c(0,151.5,151.5,0),y=c(-20,-20,100000,100000),col=gray(0.9),border=FALSE)
polygon(x=c(181.5,212.5,212.5,181.5),y=c(-20,-20,100000,100000),col=gray(0.9),border=FALSE)
polygon(x=c(243.5,273.5,273.5,243.5),y=c(-20,-20,100000,100000),col=gray(0.9),border=FALSE)
box(lwd=2)
points(rain$Rainfall_mm[rain$Rainfall_mm>0] ~ rain$DOY[rain$Rainfall_mm>0],pch=21,col="dodgerblue",bg="dodgerblue",cex=2)
points(rain$Rainfall_mm ~ rain$DOY,type="l",col="blue4",lwd=2)
