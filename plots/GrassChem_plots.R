##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")

##load libraries
library(lme4)
library(stringr)
library(plotrix)

#################plant chem
# attach data
chem <- read.csv("rawData/PlantChem.csv")
head(chem)
all_subw <- chem[chem$type=="woody",]
all_subg <- as.data.frame(chem[chem$type=="grass",])
all_subf <- chem[chem$type=="forb",]

#means by trt
chem$trt_mo_type <- paste(chem$trt,chem$mo,chem$type)
chemsum <- aggregate(cbind(percC,percN,Ca_ppm,K_ppm,Mg_ppm,Na_ppm,P_ppm,Si_ppm) ~ trt_mo_type, data = chem, FUN = function(x)c(mean = mean(x),se = std.error(x)))
head(chemsum)

chemsum[c('trt', 'month','type')] <- str_split_fixed(chemsum$trt_mo_type, ' ', 3)
chemsum$month <-as.numeric(chemsum$month)
subw <- chemsum[chemsum$type=="woody",]
subg <- as.data.frame(chemsum[chemsum$type=="grass",])
subf <- chemsum[chemsum$type=="forb",]

################
tiff(filename = "plots/GrassChem_individualPlots.tif", width = 9, height = 5, units = 'in', res = 600, compression = 'lzw')

par(mar=c(2.5,5,0.2,0.2),mfrow=c(2,3))

##grass N
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0.5,3),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass %N", line=3, cex.lab=1.6)
##bison
x1 <- all_subg$mo[all_subg$trt=="bison"]
y1 <- all_subg$percN[all_subg$trt=="bison"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "sienna", lwd = 2)
#points
points(subg$percN[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
#points(subg$percN[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subg$month[subg$trt=="bison"], subg$percN[,1][subg$trt=="bison"]-subg$percN[,2][subg$trt=="bison"], subg$month[subg$trt=="bison"], subg$percN[,1][subg$trt=="bison"]+subg$percN[,2][subg$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
x1 <- all_subg$mo[all_subg$trt=="cattle"]
y1 <- all_subg$percN[all_subg$trt=="cattle"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "gray0", lwd = 2)
#points
points(subg$percN[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
#points(subg$percN[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subg$month[subg$trt=="cattle"], subg$percN[,1][subg$trt=="cattle"]-subg$percN[,2][subg$trt=="cattle"], subg$month[subg$trt=="cattle"], subg$percN[,1][subg$trt=="cattle"]+subg$percN[,2][subg$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
x1 <- all_subg$mo[all_subg$trt=="ungrazed"]
y1 <- all_subg$percN[all_subg$trt=="ungrazed"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "dodgerblue", lwd = 2)
#points
points(subg$percN[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
#points(subg$percN[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subg$month[subg$trt=="ungrazed"], subg$percN[,1][subg$trt=="ungrazed"]-subg$percN[,2][subg$trt=="ungrazed"], subg$month[subg$trt=="ungrazed"], subg$percN[,1][subg$trt=="ungrazed"]+subg$percN[,2][subg$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
x1 <- all_subg$mo[all_subg$trt=="trtpd"]
y1 <- all_subg$percN[all_subg$trt=="trtpd"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,8, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "firebrick2", lwd = 2)
#points
points(subg$percN[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(subg$percN[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subg$month[subg$trt=="trtpd"], subg$percN[,1][subg$trt=="trtpd"]-subg$percN[,2][subg$trt=="trtpd"], subg$month[subg$trt=="trtpd"], subg$percN[,1][subg$trt=="trtpd"]+subg$percN[,2][subg$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
x1 <- all_subg$mo[all_subg$trt=="untrtpd"]
y1 <- all_subg$percN[all_subg$trt=="untrtpd"]
mod <- lm(y1 ~ x1)
## predict values across range
newdat <- data.frame(x = seq(6,8, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "goldenrod2", lwd = 2)
#points
points(subg$percN[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
#points(subg$percN[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subg$month[subg$trt=="untrtpd"], subg$percN[,1][subg$trt=="untrtpd"]-subg$percN[,2][subg$trt=="untrtpd"], subg$month[subg$trt=="untrtpd"], subg$percN[,1][subg$trt=="untrtpd"]+subg$percN[,2][subg$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
legend("topright", legend=("A"), bty="n", cex=1.5)

#####################################################
##grass P
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(600,2700),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass ppm P", line=3.5, cex.lab=1.6)
##bison
x1 <- all_subg$mo[all_subg$trt=="bison"]
y1 <- all_subg$P_ppm[all_subg$trt=="bison"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "sienna", lwd = 2, lty = 2)
#points
points(subg$P_ppm[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
#points(subg$P_ppm[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subg$month[subg$trt=="bison"], subg$P_ppm[,1][subg$trt=="bison"]-subg$P_ppm[,2][subg$trt=="bison"], subg$month[subg$trt=="bison"], subg$P_ppm[,1][subg$trt=="bison"]+subg$P_ppm[,2][subg$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
x1 <- all_subg$mo[all_subg$trt=="cattle"]
y1 <- all_subg$P_ppm[all_subg$trt=="cattle"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "gray0", lwd = 2)
#points
points(subg$P_ppm[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
#points(subg$P_ppm[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subg$month[subg$trt=="cattle"], subg$P_ppm[,1][subg$trt=="cattle"]-subg$P_ppm[,2][subg$trt=="cattle"], subg$month[subg$trt=="cattle"], subg$P_ppm[,1][subg$trt=="cattle"]+subg$P_ppm[,2][subg$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
x1 <- all_subg$mo[all_subg$trt=="ungrazed"]
y1 <- all_subg$P_ppm[all_subg$trt=="ungrazed"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "dodgerblue", lwd = 2)
#points
points(subg$P_ppm[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
#points(subg$P_ppm[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subg$month[subg$trt=="ungrazed"], subg$P_ppm[,1][subg$trt=="ungrazed"]-subg$P_ppm[,2][subg$trt=="ungrazed"], subg$month[subg$trt=="ungrazed"], subg$P_ppm[,1][subg$trt=="ungrazed"]+subg$P_ppm[,2][subg$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
x1 <- all_subg$mo[all_subg$trt=="trtpd"]
y1 <- all_subg$P_ppm[all_subg$trt=="trtpd"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,8, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "firebrick2", lwd = 2)
#points
points(subg$P_ppm[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(subg$P_ppm[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subg$month[subg$trt=="trtpd"], subg$P_ppm[,1][subg$trt=="trtpd"]-subg$P_ppm[,2][subg$trt=="trtpd"], subg$month[subg$trt=="trtpd"], subg$P_ppm[,1][subg$trt=="trtpd"]+subg$P_ppm[,2][subg$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
x1 <- all_subg$mo[all_subg$trt=="untrtpd"]
y1 <- all_subg$P_ppm[all_subg$trt=="untrtpd"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,8, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "goldenrod2", lwd = 2, lty = 2)
#points
points(subg$P_ppm[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
#points(subg$P_ppm[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subg$month[subg$trt=="untrtpd"], subg$P_ppm[,1][subg$trt=="untrtpd"]-subg$P_ppm[,2][subg$trt=="untrtpd"], subg$month[subg$trt=="untrtpd"], subg$P_ppm[,1][subg$trt=="untrtpd"]+subg$P_ppm[,2][subg$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
legend("topright", legend=("B"), bty="n", cex=1.5)

############################################################################
##grass K
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(4500,27000),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass ppm K", line=3.5, cex.lab=1.6)
##bison
x1 <- all_subg$mo[all_subg$trt=="bison"]
y1 <- all_subg$K_ppm[all_subg$trt=="bison"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "sienna", lwd = 2, lty = 2)
#points
points(subg$K_ppm[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
#points(subg$K_ppm[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subg$month[subg$trt=="bison"], subg$K_ppm[,1][subg$trt=="bison"]-subg$K_ppm[,2][subg$trt=="bison"], subg$month[subg$trt=="bison"], subg$K_ppm[,1][subg$trt=="bison"]+subg$K_ppm[,2][subg$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
x1 <- all_subg$mo[all_subg$trt=="cattle"]
y1 <- all_subg$K_ppm[all_subg$trt=="cattle"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "gray0", lwd = 2, lty = 2)
#points
points(subg$K_ppm[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
#points(subg$K_ppm[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subg$month[subg$trt=="cattle"], subg$K_ppm[,1][subg$trt=="cattle"]-subg$K_ppm[,2][subg$trt=="cattle"], subg$month[subg$trt=="cattle"], subg$K_ppm[,1][subg$trt=="cattle"]+subg$K_ppm[,2][subg$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
x1 <- all_subg$mo[all_subg$trt=="ungrazed"]
y1 <- all_subg$K_ppm[all_subg$trt=="ungrazed"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "dodgerblue", lwd = 2, lty = 2)
#points
points(subg$K_ppm[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
#points(subg$K_ppm[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subg$month[subg$trt=="ungrazed"], subg$K_ppm[,1][subg$trt=="ungrazed"]-subg$K_ppm[,2][subg$trt=="ungrazed"], subg$month[subg$trt=="ungrazed"], subg$K_ppm[,1][subg$trt=="ungrazed"]+subg$K_ppm[,2][subg$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
x1 <- all_subg$mo[all_subg$trt=="trtpd"]
y1 <- all_subg$K_ppm[all_subg$trt=="trtpd"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,8, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "firebrick2", lwd = 2)
#points
points(subg$K_ppm[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(subg$K_ppm[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subg$month[subg$trt=="trtpd"], subg$K_ppm[,1][subg$trt=="trtpd"]-subg$K_ppm[,2][subg$trt=="trtpd"], subg$month[subg$trt=="trtpd"], subg$K_ppm[,1][subg$trt=="trtpd"]+subg$K_ppm[,2][subg$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
x1 <- all_subg$mo[all_subg$trt=="untrtpd"]
y1 <- all_subg$K_ppm[all_subg$trt=="untrtpd"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,8, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "goldenrod2", lwd = 2, lty = 2)
#points
points(subg$K_ppm[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
#points(subg$K_ppm[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subg$month[subg$trt=="untrtpd"], subg$K_ppm[,1][subg$trt=="untrtpd"]-subg$K_ppm[,2][subg$trt=="untrtpd"], subg$month[subg$trt=="untrtpd"], subg$K_ppm[,1][subg$trt=="untrtpd"]+subg$K_ppm[,2][subg$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
legend("topright", legend=("C"), bty="n", cex=1.5)

#######################################################

##grass Mg
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(400,2500),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass ppm Mg", line=3.5, cex.lab=1.6)
##bison
x1 <- all_subg$mo[all_subg$trt=="bison"]
y1 <- all_subg$Mg_ppm[all_subg$trt=="bison"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "sienna", lwd = 2, lty = 2)
#points
points(subg$Mg_ppm[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
#points(subg$Mg_ppm[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subg$month[subg$trt=="bison"], subg$Mg_ppm[,1][subg$trt=="bison"]-subg$Mg_ppm[,2][subg$trt=="bison"], subg$month[subg$trt=="bison"], subg$Mg_ppm[,1][subg$trt=="bison"]+subg$Mg_ppm[,2][subg$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
x1 <- all_subg$mo[all_subg$trt=="cattle"]
y1 <- all_subg$Mg_ppm[all_subg$trt=="cattle"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "gray0", lwd = 2, lty = 2)
#points
points(subg$Mg_ppm[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
#points(subg$Mg_ppm[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subg$month[subg$trt=="cattle"], subg$Mg_ppm[,1][subg$trt=="cattle"]-subg$Mg_ppm[,2][subg$trt=="cattle"], subg$month[subg$trt=="cattle"], subg$Mg_ppm[,1][subg$trt=="cattle"]+subg$Mg_ppm[,2][subg$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
x1 <- all_subg$mo[all_subg$trt=="ungrazed"]
y1 <- all_subg$Mg_ppm[all_subg$trt=="ungrazed"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "dodgerblue", lwd = 2, lty = 2)
#points
points(subg$Mg_ppm[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
#points(subg$Mg_ppm[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subg$month[subg$trt=="ungrazed"], subg$Mg_ppm[,1][subg$trt=="ungrazed"]-subg$Mg_ppm[,2][subg$trt=="ungrazed"], subg$month[subg$trt=="ungrazed"], subg$Mg_ppm[,1][subg$trt=="ungrazed"]+subg$Mg_ppm[,2][subg$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
x1 <- all_subg$mo[all_subg$trt=="trtpd"]
y1 <- all_subg$Mg_ppm[all_subg$trt=="trtpd"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,8, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "firebrick2", lwd = 2)
#points
points(subg$Mg_ppm[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(subg$Mg_ppm[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subg$month[subg$trt=="trtpd"], subg$Mg_ppm[,1][subg$trt=="trtpd"]-subg$Mg_ppm[,2][subg$trt=="trtpd"], subg$month[subg$trt=="trtpd"], subg$Mg_ppm[,1][subg$trt=="trtpd"]+subg$Mg_ppm[,2][subg$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
x1 <- all_subg$mo[all_subg$trt=="untrtpd"]
y1 <- all_subg$Mg_ppm[all_subg$trt=="untrtpd"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,8, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "goldenrod2", lwd = 2, lty = 2)
#points
points(subg$Mg_ppm[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
#points(subg$Mg_ppm[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subg$month[subg$trt=="untrtpd"], subg$Mg_ppm[,1][subg$trt=="untrtpd"]-subg$Mg_ppm[,2][subg$trt=="untrtpd"], subg$month[subg$trt=="untrtpd"], subg$Mg_ppm[,1][subg$trt=="untrtpd"]+subg$Mg_ppm[,2][subg$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
legend("topright", legend=("D"), bty="n", cex=1.5)

##########################################################################
##grass Na
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(1220,2050),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass ppm Na", line=3.5, cex.lab=1.6)
##bison
x1 <- all_subg$mo[all_subg$trt=="bison"]
y1 <- all_subg$Na_ppm[all_subg$trt=="bison"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "sienna", lwd = 2, lty = 2)
#points
points(subg$Na_ppm[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
#points(subg$Na_ppm[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subg$month[subg$trt=="bison"], subg$Na_ppm[,1][subg$trt=="bison"]-subg$Na_ppm[,2][subg$trt=="bison"], subg$month[subg$trt=="bison"], subg$Na_ppm[,1][subg$trt=="bison"]+subg$Na_ppm[,2][subg$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
x1 <- all_subg$mo[all_subg$trt=="cattle"]
y1 <- all_subg$Na_ppm[all_subg$trt=="cattle"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "gray0", lwd = 2, lty = 2)
#points
points(subg$Na_ppm[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
#points(subg$Na_ppm[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subg$month[subg$trt=="cattle"], subg$Na_ppm[,1][subg$trt=="cattle"]-subg$Na_ppm[,2][subg$trt=="cattle"], subg$month[subg$trt=="cattle"], subg$Na_ppm[,1][subg$trt=="cattle"]+subg$Na_ppm[,2][subg$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
x1 <- all_subg$mo[all_subg$trt=="ungrazed"]
y1 <- all_subg$Na_ppm[all_subg$trt=="ungrazed"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "dodgerblue", lwd = 2, lty = 2)
#points
points(subg$Na_ppm[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
#points(subg$Na_ppm[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subg$month[subg$trt=="ungrazed"], subg$Na_ppm[,1][subg$trt=="ungrazed"]-subg$Na_ppm[,2][subg$trt=="ungrazed"], subg$month[subg$trt=="ungrazed"], subg$Na_ppm[,1][subg$trt=="ungrazed"]+subg$Na_ppm[,2][subg$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
x1 <- all_subg$mo[all_subg$trt=="trtpd"]
y1 <- all_subg$Na_ppm[all_subg$trt=="trtpd"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,8, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "firebrick2", lwd = 2)
#points
points(subg$Na_ppm[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(subg$Na_ppm[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subg$month[subg$trt=="trtpd"], subg$Na_ppm[,1][subg$trt=="trtpd"]-subg$Na_ppm[,2][subg$trt=="trtpd"], subg$month[subg$trt=="trtpd"], subg$Na_ppm[,1][subg$trt=="trtpd"]+subg$Na_ppm[,2][subg$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
x1 <- all_subg$mo[all_subg$trt=="untrtpd"]
y1 <- all_subg$Na_ppm[all_subg$trt=="untrtpd"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,8, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "goldenrod2", lwd = 2, lty = 2)
#points
points(subg$Na_ppm[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
#points(subg$Na_ppm[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subg$month[subg$trt=="untrtpd"], subg$Na_ppm[,1][subg$trt=="untrtpd"]-subg$Na_ppm[,2][subg$trt=="untrtpd"], subg$month[subg$trt=="untrtpd"], subg$Na_ppm[,1][subg$trt=="untrtpd"]+subg$Na_ppm[,2][subg$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
legend("topright", legend=("E"), bty="n", cex=1.5)

##
legend("topleft",legend=c("Ungrazed","Bison","Cattle","Untrt PD","Trt PD"), bty="n", pt.cex=2,cex=1.5, pch=c(23,21,22,25,24), pt.bg=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"),col=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"))
##

#############################################################
##grass Si
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0,1300),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass ppm Si", line=3.5, cex.lab=1.6)
##bison
x1 <- all_subg$mo[all_subg$trt=="bison"]
y1 <- all_subg$Si_ppm[all_subg$trt=="bison"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "sienna", lwd = 2)
#points
points(subg$Si_ppm[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
#points(subg$Si_ppm[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subg$month[subg$trt=="bison"], subg$Si_ppm[,1][subg$trt=="bison"]-subg$Si_ppm[,2][subg$trt=="bison"], subg$month[subg$trt=="bison"], subg$Si_ppm[,1][subg$trt=="bison"]+subg$Si_ppm[,2][subg$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
x1 <- all_subg$mo[all_subg$trt=="cattle"]
y1 <- all_subg$Si_ppm[all_subg$trt=="cattle"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "gray0", lwd = 2)
#points
points(subg$Si_ppm[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
#points(subg$Si_ppm[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subg$month[subg$trt=="cattle"], subg$Si_ppm[,1][subg$trt=="cattle"]-subg$Si_ppm[,2][subg$trt=="cattle"], subg$month[subg$trt=="cattle"], subg$Si_ppm[,1][subg$trt=="cattle"]+subg$Si_ppm[,2][subg$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
x1 <- all_subg$mo[all_subg$trt=="ungrazed"]
y1 <- all_subg$Si_ppm[all_subg$trt=="ungrazed"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,9, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "dodgerblue", lwd = 2)
#points
points(subg$Si_ppm[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
#points(subg$Si_ppm[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subg$month[subg$trt=="ungrazed"], subg$Si_ppm[,1][subg$trt=="ungrazed"]-subg$Si_ppm[,2][subg$trt=="ungrazed"], subg$month[subg$trt=="ungrazed"], subg$Si_ppm[,1][subg$trt=="ungrazed"]+subg$Si_ppm[,2][subg$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
x1 <- all_subg$mo[all_subg$trt=="trtpd"]
y1 <- all_subg$Si_ppm[all_subg$trt=="trtpd"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,8, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "firebrick2", lwd = 2)
#points
points(subg$Si_ppm[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(subg$Si_ppm[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subg$month[subg$trt=="trtpd"], subg$Si_ppm[,1][subg$trt=="trtpd"]-subg$Si_ppm[,2][subg$trt=="trtpd"], subg$month[subg$trt=="trtpd"], subg$Si_ppm[,1][subg$trt=="trtpd"]+subg$Si_ppm[,2][subg$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
x1 <- all_subg$mo[all_subg$trt=="untrtpd"]
y1 <- all_subg$Si_ppm[all_subg$trt=="untrtpd"]
mod <- lm(y1 ~ x1)
summary(mod)
## predict values across range
newdat <- data.frame(x = seq(6,8, length = length(x1)))
newdat <- within(newdat, ypred <- predict(mod, newdat))
## add these predictions as a line to the plot
lines(ypred ~ x1, data = newdat, col = "goldenrod2", lwd = 2)
#points
points(subg$Si_ppm[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
#points(subg$Si_ppm[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subg$month[subg$trt=="untrtpd"], subg$Si_ppm[,1][subg$trt=="untrtpd"]-subg$Si_ppm[,2][subg$trt=="untrtpd"], subg$month[subg$trt=="untrtpd"], subg$Si_ppm[,1][subg$trt=="untrtpd"]+subg$Si_ppm[,2][subg$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
legend("topright", legend=("F"), bty="n", cex=1.5)
##

dev.off()
##
##################################################################
##

##grass Ca
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(400,5500),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass ppm Ca", line=3.5, cex.lab=1.6)
##bison
points(subg$Ca_ppm[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subg$Ca_ppm[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subg$month[subg$trt=="bison"], subg$Ca_ppm[,1][subg$trt=="bison"]-subg$Ca_ppm[,2][subg$trt=="bison"], subg$month[subg$trt=="bison"], subg$Ca_ppm[,1][subg$trt=="bison"]+subg$Ca_ppm[,2][subg$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subg$Ca_ppm[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subg$Ca_ppm[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subg$month[subg$trt=="cattle"], subg$Ca_ppm[,1][subg$trt=="cattle"]-subg$Ca_ppm[,2][subg$trt=="cattle"], subg$month[subg$trt=="cattle"], subg$Ca_ppm[,1][subg$trt=="cattle"]+subg$Ca_ppm[,2][subg$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subg$Ca_ppm[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subg$Ca_ppm[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subg$month[subg$trt=="ungrazed"], subg$Ca_ppm[,1][subg$trt=="ungrazed"]-subg$Ca_ppm[,2][subg$trt=="ungrazed"], subg$month[subg$trt=="ungrazed"], subg$Ca_ppm[,1][subg$trt=="ungrazed"]+subg$Ca_ppm[,2][subg$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subg$Ca_ppm[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subg$Ca_ppm[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subg$month[subg$trt=="trtpd"], subg$Ca_ppm[,1][subg$trt=="trtpd"]-subg$Ca_ppm[,2][subg$trt=="trtpd"], subg$month[subg$trt=="trtpd"], subg$Ca_ppm[,1][subg$trt=="trtpd"]+subg$Ca_ppm[,2][subg$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subg$Ca_ppm[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subg$Ca_ppm[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subg$month[subg$trt=="untrtpd"], subg$Ca_ppm[,1][subg$trt=="untrtpd"]-subg$Ca_ppm[,2][subg$trt=="untrtpd"], subg$month[subg$trt=="untrtpd"], subg$Ca_ppm[,1][subg$trt=="untrtpd"]+subg$Ca_ppm[,2][subg$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

#########grass C
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(37.5,48),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass %C", line=3, cex.lab=1.6)
#title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(subg$percC[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subg$percC[,1][subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subg$month[subg$trt=="bison"], subg$percC[,1][subg$trt=="bison"]-subg$percC[,2][subg$trt=="bison"], subg$month[subg$trt=="bison"], subg$percC[,1][subg$trt=="bison"]+subg$percC[,2][subg$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subg$percC[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subg$percC[,1][subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subg$month[subg$trt=="cattle"], subg$percC[,1][subg$trt=="cattle"]-subg$percC[,2][subg$trt=="cattle"], subg$month[subg$trt=="cattle"], subg$percC[,1][subg$trt=="cattle"]+subg$percC[,2][subg$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subg$percC[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subg$percC[,1][subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subg$month[subg$trt=="ungrazed"], subg$percC[,1][subg$trt=="ungrazed"]-subg$percC[,2][subg$trt=="ungrazed"], subg$month[subg$trt=="ungrazed"], subg$percC[,1][subg$trt=="ungrazed"]+subg$percC[,2][subg$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subg$percC[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subg$percC[,1][subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subg$month[subg$trt=="trtpd"], subg$percC[,1][subg$trt=="trtpd"]-subg$percC[,2][subg$trt=="trtpd"], subg$month[subg$trt=="trtpd"], subg$percC[,1][subg$trt=="trtpd"]+subg$percC[,2][subg$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subg$percC[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subg$percC[,1][subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subg$month[subg$trt=="untrtpd"], subg$percC[,1][subg$trt=="untrtpd"]-subg$percC[,2][subg$trt=="untrtpd"], subg$month[subg$trt=="untrtpd"], subg$percC[,1][subg$trt=="untrtpd"]+subg$percC[,2][subg$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)


