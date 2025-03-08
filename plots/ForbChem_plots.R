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

#means by trt
chem$trt_mo_type <- paste(chem$trt,chem$mo,chem$type)
chemsum <- aggregate(cbind(percC,percN,Ca_ppm,K_ppm,Mg_ppm,Na_ppm,P_ppm,Si_ppm) ~ trt_mo_type, data = chem, FUN = function(x)c(mean = mean(x),se = std.error(x)))
head(chemsum)

chemsum[c('trt', 'month','type')] <- str_split_fixed(chemsum$trt_mo_type, ' ', 3)
chemsum$month <-as.numeric(chemsum$month)
subw <- chemsum[chemsum$type=="woody",]
subf <- as.data.frame(chemsum[chemsum$type=="Forb",])
subf <- chemsum[chemsum$type=="forb",]

################
tiff(filename = "plots/ForbChem_individualPlots.tiff", width = 9, height = 5, units = 'in', res = 600, compression = 'lzw')

par(mar=c(2.5,5,0.2,0.2),mfrow=c(2,3))

##Forb N
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(1,4.3),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Forb %N", line=3, cex.lab=1.6)
##bison
points(subf$percN[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subf$percN[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subf$month[subf$trt=="bison"], subf$percN[,1][subf$trt=="bison"]-subf$percN[,2][subf$trt=="bison"], subf$month[subf$trt=="bison"], subf$percN[,1][subf$trt=="bison"]+subf$percN[,2][subf$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subf$percN[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subf$percN[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subf$month[subf$trt=="cattle"], subf$percN[,1][subf$trt=="cattle"]-subf$percN[,2][subf$trt=="cattle"], subf$month[subf$trt=="cattle"], subf$percN[,1][subf$trt=="cattle"]+subf$percN[,2][subf$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subf$percN[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subf$percN[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subf$month[subf$trt=="ungrazed"], subf$percN[,1][subf$trt=="ungrazed"]-subf$percN[,2][subf$trt=="ungrazed"], subf$month[subf$trt=="ungrazed"], subf$percN[,1][subf$trt=="ungrazed"]+subf$percN[,2][subf$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subf$percN[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subf$percN[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subf$month[subf$trt=="trtpd"], subf$percN[,1][subf$trt=="trtpd"]-subf$percN[,2][subf$trt=="trtpd"], subf$month[subf$trt=="trtpd"], subf$percN[,1][subf$trt=="trtpd"]+subf$percN[,2][subf$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subf$percN[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subf$percN[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subf$month[subf$trt=="untrtpd"], subf$percN[,1][subf$trt=="untrtpd"]-subf$percN[,2][subf$trt=="untrtpd"], subf$month[subf$trt=="untrtpd"], subf$percN[,1][subf$trt=="untrtpd"]+subf$percN[,2][subf$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
legend("topright", legend=("A"), bty="n", cex=1.5)

##Forb P
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(600,3100),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Forb ppm P", line=3.5, cex.lab=1.6)
##bison
points(subf$P_ppm[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subf$P_ppm[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subf$month[subf$trt=="bison"], subf$P_ppm[,1][subf$trt=="bison"]-subf$P_ppm[,2][subf$trt=="bison"], subf$month[subf$trt=="bison"], subf$P_ppm[,1][subf$trt=="bison"]+subf$P_ppm[,2][subf$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subf$P_ppm[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subf$P_ppm[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subf$month[subf$trt=="cattle"], subf$P_ppm[,1][subf$trt=="cattle"]-subf$P_ppm[,2][subf$trt=="cattle"], subf$month[subf$trt=="cattle"], subf$P_ppm[,1][subf$trt=="cattle"]+subf$P_ppm[,2][subf$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subf$P_ppm[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subf$P_ppm[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subf$month[subf$trt=="ungrazed"], subf$P_ppm[,1][subf$trt=="ungrazed"]-subf$P_ppm[,2][subf$trt=="ungrazed"], subf$month[subf$trt=="ungrazed"], subf$P_ppm[,1][subf$trt=="ungrazed"]+subf$P_ppm[,2][subf$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subf$P_ppm[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subf$P_ppm[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subf$month[subf$trt=="trtpd"], subf$P_ppm[,1][subf$trt=="trtpd"]-subf$P_ppm[,2][subf$trt=="trtpd"], subf$month[subf$trt=="trtpd"], subf$P_ppm[,1][subf$trt=="trtpd"]+subf$P_ppm[,2][subf$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subf$P_ppm[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subf$P_ppm[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subf$month[subf$trt=="untrtpd"], subf$P_ppm[,1][subf$trt=="untrtpd"]-subf$P_ppm[,2][subf$trt=="untrtpd"], subf$month[subf$trt=="untrtpd"], subf$P_ppm[,1][subf$trt=="untrtpd"]+subf$P_ppm[,2][subf$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
legend("topright", legend=("B"), bty="n", cex=1.5)

##Forb K
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(4500,48000),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Forb ppm K", line=3.5, cex.lab=1.6)
##bison
points(subf$K_ppm[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subf$K_ppm[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subf$month[subf$trt=="bison"], subf$K_ppm[,1][subf$trt=="bison"]-subf$K_ppm[,2][subf$trt=="bison"], subf$month[subf$trt=="bison"], subf$K_ppm[,1][subf$trt=="bison"]+subf$K_ppm[,2][subf$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subf$K_ppm[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subf$K_ppm[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subf$month[subf$trt=="cattle"], subf$K_ppm[,1][subf$trt=="cattle"]-subf$K_ppm[,2][subf$trt=="cattle"], subf$month[subf$trt=="cattle"], subf$K_ppm[,1][subf$trt=="cattle"]+subf$K_ppm[,2][subf$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subf$K_ppm[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subf$K_ppm[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subf$month[subf$trt=="ungrazed"], subf$K_ppm[,1][subf$trt=="ungrazed"]-subf$K_ppm[,2][subf$trt=="ungrazed"], subf$month[subf$trt=="ungrazed"], subf$K_ppm[,1][subf$trt=="ungrazed"]+subf$K_ppm[,2][subf$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subf$K_ppm[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subf$K_ppm[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subf$month[subf$trt=="trtpd"], subf$K_ppm[,1][subf$trt=="trtpd"]-subf$K_ppm[,2][subf$trt=="trtpd"], subf$month[subf$trt=="trtpd"], subf$K_ppm[,1][subf$trt=="trtpd"]+subf$K_ppm[,2][subf$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subf$K_ppm[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subf$K_ppm[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subf$month[subf$trt=="untrtpd"], subf$K_ppm[,1][subf$trt=="untrtpd"]-subf$K_ppm[,2][subf$trt=="untrtpd"], subf$month[subf$trt=="untrtpd"], subf$K_ppm[,1][subf$trt=="untrtpd"]+subf$K_ppm[,2][subf$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
legend("topright", legend=("C"), bty="n", cex=1.5)

##
##Forb Mg
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(600,6400),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Forb ppm Mg", line=3.5, cex.lab=1.6)
##bison
points(subf$Mg_ppm[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subf$Mg_ppm[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subf$month[subf$trt=="bison"], subf$Mg_ppm[,1][subf$trt=="bison"]-subf$Mg_ppm[,2][subf$trt=="bison"], subf$month[subf$trt=="bison"], subf$Mg_ppm[,1][subf$trt=="bison"]+subf$Mg_ppm[,2][subf$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subf$Mg_ppm[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subf$Mg_ppm[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subf$month[subf$trt=="cattle"], subf$Mg_ppm[,1][subf$trt=="cattle"]-subf$Mg_ppm[,2][subf$trt=="cattle"], subf$month[subf$trt=="cattle"], subf$Mg_ppm[,1][subf$trt=="cattle"]+subf$Mg_ppm[,2][subf$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subf$Mg_ppm[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subf$Mg_ppm[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subf$month[subf$trt=="ungrazed"], subf$Mg_ppm[,1][subf$trt=="ungrazed"]-subf$Mg_ppm[,2][subf$trt=="ungrazed"], subf$month[subf$trt=="ungrazed"], subf$Mg_ppm[,1][subf$trt=="ungrazed"]+subf$Mg_ppm[,2][subf$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subf$Mg_ppm[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subf$Mg_ppm[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subf$month[subf$trt=="trtpd"], subf$Mg_ppm[,1][subf$trt=="trtpd"]-subf$Mg_ppm[,2][subf$trt=="trtpd"], subf$month[subf$trt=="trtpd"], subf$Mg_ppm[,1][subf$trt=="trtpd"]+subf$Mg_ppm[,2][subf$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subf$Mg_ppm[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subf$Mg_ppm[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subf$month[subf$trt=="untrtpd"], subf$Mg_ppm[,1][subf$trt=="untrtpd"]-subf$Mg_ppm[,2][subf$trt=="untrtpd"], subf$month[subf$trt=="untrtpd"], subf$Mg_ppm[,1][subf$trt=="untrtpd"]+subf$Mg_ppm[,2][subf$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
legend("topright", legend=("D"), bty="n", cex=1.5)

##Forb Na
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(1220,6800),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Forb ppm Na", line=3.5, cex.lab=1.6)
##bison
points(subf$Na_ppm[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subf$Na_ppm[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subf$month[subf$trt=="bison"], subf$Na_ppm[,1][subf$trt=="bison"]-subf$Na_ppm[,2][subf$trt=="bison"], subf$month[subf$trt=="bison"], subf$Na_ppm[,1][subf$trt=="bison"]+subf$Na_ppm[,2][subf$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subf$Na_ppm[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subf$Na_ppm[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subf$month[subf$trt=="cattle"], subf$Na_ppm[,1][subf$trt=="cattle"]-subf$Na_ppm[,2][subf$trt=="cattle"], subf$month[subf$trt=="cattle"], subf$Na_ppm[,1][subf$trt=="cattle"]+subf$Na_ppm[,2][subf$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subf$Na_ppm[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subf$Na_ppm[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subf$month[subf$trt=="ungrazed"], subf$Na_ppm[,1][subf$trt=="ungrazed"]-subf$Na_ppm[,2][subf$trt=="ungrazed"], subf$month[subf$trt=="ungrazed"], subf$Na_ppm[,1][subf$trt=="ungrazed"]+subf$Na_ppm[,2][subf$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subf$Na_ppm[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subf$Na_ppm[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subf$month[subf$trt=="trtpd"], subf$Na_ppm[,1][subf$trt=="trtpd"]-subf$Na_ppm[,2][subf$trt=="trtpd"], subf$month[subf$trt=="trtpd"], subf$Na_ppm[,1][subf$trt=="trtpd"]+subf$Na_ppm[,2][subf$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subf$Na_ppm[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subf$Na_ppm[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subf$month[subf$trt=="untrtpd"], subf$Na_ppm[,1][subf$trt=="untrtpd"]-subf$Na_ppm[,2][subf$trt=="untrtpd"], subf$month[subf$trt=="untrtpd"], subf$Na_ppm[,1][subf$trt=="untrtpd"]+subf$Na_ppm[,2][subf$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
legend("topright", legend=("E"), bty="n", cex=1.5)
legend("topleft",legend=c("Ungrazed","Bison","Cattle","Untrt PD","Trt PD"), bty="n", pt.cex=2,cex=1.5, pch=c(23,21,22,25,24), pt.bg=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"),col=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"))
##

##Forb Si
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0,1400),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Forb ppm Si", line=3.5, cex.lab=1.6)
##bison
points(subf$Si_ppm[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subf$Si_ppm[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subf$month[subf$trt=="bison"], subf$Si_ppm[,1][subf$trt=="bison"]-subf$Si_ppm[,2][subf$trt=="bison"], subf$month[subf$trt=="bison"], subf$Si_ppm[,1][subf$trt=="bison"]+subf$Si_ppm[,2][subf$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subf$Si_ppm[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subf$Si_ppm[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subf$month[subf$trt=="cattle"], subf$Si_ppm[,1][subf$trt=="cattle"]-subf$Si_ppm[,2][subf$trt=="cattle"], subf$month[subf$trt=="cattle"], subf$Si_ppm[,1][subf$trt=="cattle"]+subf$Si_ppm[,2][subf$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subf$Si_ppm[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subf$Si_ppm[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subf$month[subf$trt=="ungrazed"], subf$Si_ppm[,1][subf$trt=="ungrazed"]-subf$Si_ppm[,2][subf$trt=="ungrazed"], subf$month[subf$trt=="ungrazed"], subf$Si_ppm[,1][subf$trt=="ungrazed"]+subf$Si_ppm[,2][subf$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subf$Si_ppm[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subf$Si_ppm[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subf$month[subf$trt=="trtpd"], subf$Si_ppm[,1][subf$trt=="trtpd"]-subf$Si_ppm[,2][subf$trt=="trtpd"], subf$month[subf$trt=="trtpd"], subf$Si_ppm[,1][subf$trt=="trtpd"]+subf$Si_ppm[,2][subf$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subf$Si_ppm[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subf$Si_ppm[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subf$month[subf$trt=="untrtpd"], subf$Si_ppm[,1][subf$trt=="untrtpd"]-subf$Si_ppm[,2][subf$trt=="untrtpd"], subf$month[subf$trt=="untrtpd"], subf$Si_ppm[,1][subf$trt=="untrtpd"]+subf$Si_ppm[,2][subf$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
legend("topright", legend=("F"), bty="n", cex=1.5)
##

dev.off()
##
##################################################################
##
#########Forb C
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(34,48.2),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Forb %C", line=3, cex.lab=1.6)
#title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(subf$percC[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subf$percC[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subf$month[subf$trt=="bison"], subf$percC[,1][subf$trt=="bison"]-subf$percC[,2][subf$trt=="bison"], subf$month[subf$trt=="bison"], subf$percC[,1][subf$trt=="bison"]+subf$percC[,2][subf$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subf$percC[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subf$percC[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subf$month[subf$trt=="cattle"], subf$percC[,1][subf$trt=="cattle"]-subf$percC[,2][subf$trt=="cattle"], subf$month[subf$trt=="cattle"], subf$percC[,1][subf$trt=="cattle"]+subf$percC[,2][subf$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subf$percC[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subf$percC[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subf$month[subf$trt=="ungrazed"], subf$percC[,1][subf$trt=="ungrazed"]-subf$percC[,2][subf$trt=="ungrazed"], subf$month[subf$trt=="ungrazed"], subf$percC[,1][subf$trt=="ungrazed"]+subf$percC[,2][subf$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subf$percC[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subf$percC[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subf$month[subf$trt=="trtpd"], subf$percC[,1][subf$trt=="trtpd"]-subf$percC[,2][subf$trt=="trtpd"], subf$month[subf$trt=="trtpd"], subf$percC[,1][subf$trt=="trtpd"]+subf$percC[,2][subf$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subf$percC[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subf$percC[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subf$month[subf$trt=="untrtpd"], subf$percC[,1][subf$trt=="untrtpd"]-subf$percC[,2][subf$trt=="untrtpd"], subf$month[subf$trt=="untrtpd"], subf$percC[,1][subf$trt=="untrtpd"]+subf$percC[,2][subf$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
##

##Forb Ca
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(2000,19000),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Forb ppm Ca", line=3.5, cex.lab=1.6)
##bison
points(subf$Ca_ppm[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subf$Ca_ppm[,1][subf$trt=="bison"] ~ subf$month[subf$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subf$month[subf$trt=="bison"], subf$Ca_ppm[,1][subf$trt=="bison"]-subf$Ca_ppm[,2][subf$trt=="bison"], subf$month[subf$trt=="bison"], subf$Ca_ppm[,1][subf$trt=="bison"]+subf$Ca_ppm[,2][subf$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subf$Ca_ppm[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subf$Ca_ppm[,1][subf$trt=="cattle"] ~ subf$month[subf$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subf$month[subf$trt=="cattle"], subf$Ca_ppm[,1][subf$trt=="cattle"]-subf$Ca_ppm[,2][subf$trt=="cattle"], subf$month[subf$trt=="cattle"], subf$Ca_ppm[,1][subf$trt=="cattle"]+subf$Ca_ppm[,2][subf$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subf$Ca_ppm[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subf$Ca_ppm[,1][subf$trt=="ungrazed"] ~ subf$month[subf$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subf$month[subf$trt=="ungrazed"], subf$Ca_ppm[,1][subf$trt=="ungrazed"]-subf$Ca_ppm[,2][subf$trt=="ungrazed"], subf$month[subf$trt=="ungrazed"], subf$Ca_ppm[,1][subf$trt=="ungrazed"]+subf$Ca_ppm[,2][subf$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subf$Ca_ppm[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subf$Ca_ppm[,1][subf$trt=="trtpd"] ~ subf$month[subf$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subf$month[subf$trt=="trtpd"], subf$Ca_ppm[,1][subf$trt=="trtpd"]-subf$Ca_ppm[,2][subf$trt=="trtpd"], subf$month[subf$trt=="trtpd"], subf$Ca_ppm[,1][subf$trt=="trtpd"]+subf$Ca_ppm[,2][subf$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subf$Ca_ppm[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subf$Ca_ppm[,1][subf$trt=="untrtpd"] ~ subf$month[subf$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subf$month[subf$trt=="untrtpd"], subf$Ca_ppm[,1][subf$trt=="untrtpd"]-subf$Ca_ppm[,2][subf$trt=="untrtpd"], subf$month[subf$trt=="untrtpd"], subf$Ca_ppm[,1][subf$trt=="untrtpd"]+subf$Ca_ppm[,2][subf$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)


