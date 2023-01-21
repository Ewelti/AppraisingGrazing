##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing/rawData")

##load libraries
library(lme4)
library(stringr)
library(plotrix)

#################plant chem
# attach data
chem <- read.csv("PlantChem.csv")
head(chem)

#means by trt
chem$trt_mo_type <- paste(chem$trt,chem$mo,chem$type)
chemsum <- aggregate(cbind(percC,percN,Ca_ppm,K_ppm,Mg_ppm,Na_ppm,P_ppm,Si_ppm) ~ trt_mo_type, data = chem, FUN = function(x)c(mean = mean(x),se = std.error(x)))
head(chemsum)

chemsum[c('trt', 'month','type')] <- str_split_fixed(chemsum$trt_mo_type, ' ', 3)
chemsum$month <-as.numeric(chemsum$month)
subw <- chemsum[chemsum$type=="woody",]
subw <- as.data.frame(chemsum[chemsum$type=="woody",])
subw <- chemsum[chemsum$type=="woody",]

################
par(mar=c(2.5,5,0.2,0.2),mfrow=c(2,4))
#########Woody C
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(15,51),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Woody %C", line=3, cex.lab=1.6)
#title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(subw$percC[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subw$percC[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subw$month[subw$trt=="bison"], subw$percC[,1][subw$trt=="bison"]-subw$percC[,2][subw$trt=="bison"], subw$month[subw$trt=="bison"], subw$percC[,1][subw$trt=="bison"]+subw$percC[,2][subw$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subw$percC[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subw$percC[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subw$month[subw$trt=="cattle"], subw$percC[,1][subw$trt=="cattle"]-subw$percC[,2][subw$trt=="cattle"], subw$month[subw$trt=="cattle"], subw$percC[,1][subw$trt=="cattle"]+subw$percC[,2][subw$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subw$percC[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subw$percC[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subw$month[subw$trt=="ungrazed"], subw$percC[,1][subw$trt=="ungrazed"]-subw$percC[,2][subw$trt=="ungrazed"], subw$month[subw$trt=="ungrazed"], subw$percC[,1][subw$trt=="ungrazed"]+subw$percC[,2][subw$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subw$percC[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subw$percC[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subw$month[subw$trt=="trtpd"], subw$percC[,1][subw$trt=="trtpd"]-subw$percC[,2][subw$trt=="trtpd"], subw$month[subw$trt=="trtpd"], subw$percC[,1][subw$trt=="trtpd"]+subw$percC[,2][subw$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subw$percC[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subw$percC[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subw$month[subw$trt=="untrtpd"], subw$percC[,1][subw$trt=="untrtpd"]-subw$percC[,2][subw$trt=="untrtpd"], subw$month[subw$trt=="untrtpd"], subw$percC[,1][subw$trt=="untrtpd"]+subw$percC[,2][subw$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##Woody N
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0.3,3),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Woody %N", line=3, cex.lab=1.6)
##bison
points(subw$percN[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subw$percN[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subw$month[subw$trt=="bison"], subw$percN[,1][subw$trt=="bison"]-subw$percN[,2][subw$trt=="bison"], subw$month[subw$trt=="bison"], subw$percN[,1][subw$trt=="bison"]+subw$percN[,2][subw$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subw$percN[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subw$percN[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subw$month[subw$trt=="cattle"], subw$percN[,1][subw$trt=="cattle"]-subw$percN[,2][subw$trt=="cattle"], subw$month[subw$trt=="cattle"], subw$percN[,1][subw$trt=="cattle"]+subw$percN[,2][subw$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subw$percN[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subw$percN[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subw$month[subw$trt=="ungrazed"], subw$percN[,1][subw$trt=="ungrazed"]-subw$percN[,2][subw$trt=="ungrazed"], subw$month[subw$trt=="ungrazed"], subw$percN[,1][subw$trt=="ungrazed"]+subw$percN[,2][subw$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subw$percN[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subw$percN[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subw$month[subw$trt=="trtpd"], subw$percN[,1][subw$trt=="trtpd"]-subw$percN[,2][subw$trt=="trtpd"], subw$month[subw$trt=="trtpd"], subw$percN[,1][subw$trt=="trtpd"]+subw$percN[,2][subw$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subw$percN[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subw$percN[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subw$month[subw$trt=="untrtpd"], subw$percN[,1][subw$trt=="untrtpd"]-subw$percN[,2][subw$trt=="untrtpd"], subw$month[subw$trt=="untrtpd"], subw$percN[,1][subw$trt=="untrtpd"]+subw$percN[,2][subw$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##Woody P
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(250,2500),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Woody ppm P", line=3.5, cex.lab=1.6)
##bison
points(subw$P_ppm[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subw$P_ppm[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subw$month[subw$trt=="bison"], subw$P_ppm[,1][subw$trt=="bison"]-subw$P_ppm[,2][subw$trt=="bison"], subw$month[subw$trt=="bison"], subw$P_ppm[,1][subw$trt=="bison"]+subw$P_ppm[,2][subw$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subw$P_ppm[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subw$P_ppm[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subw$month[subw$trt=="cattle"], subw$P_ppm[,1][subw$trt=="cattle"]-subw$P_ppm[,2][subw$trt=="cattle"], subw$month[subw$trt=="cattle"], subw$P_ppm[,1][subw$trt=="cattle"]+subw$P_ppm[,2][subw$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subw$P_ppm[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subw$P_ppm[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subw$month[subw$trt=="ungrazed"], subw$P_ppm[,1][subw$trt=="ungrazed"]-subw$P_ppm[,2][subw$trt=="ungrazed"], subw$month[subw$trt=="ungrazed"], subw$P_ppm[,1][subw$trt=="ungrazed"]+subw$P_ppm[,2][subw$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subw$P_ppm[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subw$P_ppm[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subw$month[subw$trt=="trtpd"], subw$P_ppm[,1][subw$trt=="trtpd"]-subw$P_ppm[,2][subw$trt=="trtpd"], subw$month[subw$trt=="trtpd"], subw$P_ppm[,1][subw$trt=="trtpd"]+subw$P_ppm[,2][subw$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subw$P_ppm[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subw$P_ppm[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subw$month[subw$trt=="untrtpd"], subw$P_ppm[,1][subw$trt=="untrtpd"]-subw$P_ppm[,2][subw$trt=="untrtpd"], subw$month[subw$trt=="untrtpd"], subw$P_ppm[,1][subw$trt=="untrtpd"]+subw$P_ppm[,2][subw$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##Woody K
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(2000,31000),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Woody ppm K", line=3.5, cex.lab=1.6)
##bison
points(subw$K_ppm[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subw$K_ppm[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subw$month[subw$trt=="bison"], subw$K_ppm[,1][subw$trt=="bison"]-subw$K_ppm[,2][subw$trt=="bison"], subw$month[subw$trt=="bison"], subw$K_ppm[,1][subw$trt=="bison"]+subw$K_ppm[,2][subw$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subw$K_ppm[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subw$K_ppm[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subw$month[subw$trt=="cattle"], subw$K_ppm[,1][subw$trt=="cattle"]-subw$K_ppm[,2][subw$trt=="cattle"], subw$month[subw$trt=="cattle"], subw$K_ppm[,1][subw$trt=="cattle"]+subw$K_ppm[,2][subw$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subw$K_ppm[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subw$K_ppm[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subw$month[subw$trt=="ungrazed"], subw$K_ppm[,1][subw$trt=="ungrazed"]-subw$K_ppm[,2][subw$trt=="ungrazed"], subw$month[subw$trt=="ungrazed"], subw$K_ppm[,1][subw$trt=="ungrazed"]+subw$K_ppm[,2][subw$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subw$K_ppm[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subw$K_ppm[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subw$month[subw$trt=="trtpd"], subw$K_ppm[,1][subw$trt=="trtpd"]-subw$K_ppm[,2][subw$trt=="trtpd"], subw$month[subw$trt=="trtpd"], subw$K_ppm[,1][subw$trt=="trtpd"]+subw$K_ppm[,2][subw$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subw$K_ppm[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subw$K_ppm[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subw$month[subw$trt=="untrtpd"], subw$K_ppm[,1][subw$trt=="untrtpd"]-subw$K_ppm[,2][subw$trt=="untrtpd"], subw$month[subw$trt=="untrtpd"], subw$K_ppm[,1][subw$trt=="untrtpd"]+subw$K_ppm[,2][subw$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##
##Woody Ca
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(1500,15000),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Woody ppm Ca", line=3.5, cex.lab=1.6)
##bison
points(subw$Ca_ppm[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subw$Ca_ppm[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subw$month[subw$trt=="bison"], subw$Ca_ppm[,1][subw$trt=="bison"]-subw$Ca_ppm[,2][subw$trt=="bison"], subw$month[subw$trt=="bison"], subw$Ca_ppm[,1][subw$trt=="bison"]+subw$Ca_ppm[,2][subw$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subw$Ca_ppm[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subw$Ca_ppm[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subw$month[subw$trt=="cattle"], subw$Ca_ppm[,1][subw$trt=="cattle"]-subw$Ca_ppm[,2][subw$trt=="cattle"], subw$month[subw$trt=="cattle"], subw$Ca_ppm[,1][subw$trt=="cattle"]+subw$Ca_ppm[,2][subw$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subw$Ca_ppm[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subw$Ca_ppm[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subw$month[subw$trt=="ungrazed"], subw$Ca_ppm[,1][subw$trt=="ungrazed"]-subw$Ca_ppm[,2][subw$trt=="ungrazed"], subw$month[subw$trt=="ungrazed"], subw$Ca_ppm[,1][subw$trt=="ungrazed"]+subw$Ca_ppm[,2][subw$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subw$Ca_ppm[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subw$Ca_ppm[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subw$month[subw$trt=="trtpd"], subw$Ca_ppm[,1][subw$trt=="trtpd"]-subw$Ca_ppm[,2][subw$trt=="trtpd"], subw$month[subw$trt=="trtpd"], subw$Ca_ppm[,1][subw$trt=="trtpd"]+subw$Ca_ppm[,2][subw$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subw$Ca_ppm[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subw$Ca_ppm[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subw$month[subw$trt=="untrtpd"], subw$Ca_ppm[,1][subw$trt=="untrtpd"]-subw$Ca_ppm[,2][subw$trt=="untrtpd"], subw$month[subw$trt=="untrtpd"], subw$Ca_ppm[,1][subw$trt=="untrtpd"]+subw$Ca_ppm[,2][subw$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##Woody Mg
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(400,2500),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Woody ppm Mg", line=3.5, cex.lab=1.6)
##bison
points(subw$Mg_ppm[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subw$Mg_ppm[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subw$month[subw$trt=="bison"], subw$Mg_ppm[,1][subw$trt=="bison"]-subw$Mg_ppm[,2][subw$trt=="bison"], subw$month[subw$trt=="bison"], subw$Mg_ppm[,1][subw$trt=="bison"]+subw$Mg_ppm[,2][subw$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subw$Mg_ppm[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subw$Mg_ppm[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subw$month[subw$trt=="cattle"], subw$Mg_ppm[,1][subw$trt=="cattle"]-subw$Mg_ppm[,2][subw$trt=="cattle"], subw$month[subw$trt=="cattle"], subw$Mg_ppm[,1][subw$trt=="cattle"]+subw$Mg_ppm[,2][subw$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subw$Mg_ppm[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subw$Mg_ppm[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subw$month[subw$trt=="ungrazed"], subw$Mg_ppm[,1][subw$trt=="ungrazed"]-subw$Mg_ppm[,2][subw$trt=="ungrazed"], subw$month[subw$trt=="ungrazed"], subw$Mg_ppm[,1][subw$trt=="ungrazed"]+subw$Mg_ppm[,2][subw$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subw$Mg_ppm[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subw$Mg_ppm[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subw$month[subw$trt=="trtpd"], subw$Mg_ppm[,1][subw$trt=="trtpd"]-subw$Mg_ppm[,2][subw$trt=="trtpd"], subw$month[subw$trt=="trtpd"], subw$Mg_ppm[,1][subw$trt=="trtpd"]+subw$Mg_ppm[,2][subw$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subw$Mg_ppm[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subw$Mg_ppm[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subw$month[subw$trt=="untrtpd"], subw$Mg_ppm[,1][subw$trt=="untrtpd"]-subw$Mg_ppm[,2][subw$trt=="untrtpd"], subw$month[subw$trt=="untrtpd"], subw$Mg_ppm[,1][subw$trt=="untrtpd"]+subw$Mg_ppm[,2][subw$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##Woody Na
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(1000,10000),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Woody ppm Na", line=3.5, cex.lab=1.6)
##bison
points(subw$Na_ppm[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subw$Na_ppm[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subw$month[subw$trt=="bison"], subw$Na_ppm[,1][subw$trt=="bison"]-subw$Na_ppm[,2][subw$trt=="bison"], subw$month[subw$trt=="bison"], subw$Na_ppm[,1][subw$trt=="bison"]+subw$Na_ppm[,2][subw$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subw$Na_ppm[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subw$Na_ppm[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subw$month[subw$trt=="cattle"], subw$Na_ppm[,1][subw$trt=="cattle"]-subw$Na_ppm[,2][subw$trt=="cattle"], subw$month[subw$trt=="cattle"], subw$Na_ppm[,1][subw$trt=="cattle"]+subw$Na_ppm[,2][subw$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subw$Na_ppm[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subw$Na_ppm[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subw$month[subw$trt=="ungrazed"], subw$Na_ppm[,1][subw$trt=="ungrazed"]-subw$Na_ppm[,2][subw$trt=="ungrazed"], subw$month[subw$trt=="ungrazed"], subw$Na_ppm[,1][subw$trt=="ungrazed"]+subw$Na_ppm[,2][subw$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subw$Na_ppm[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subw$Na_ppm[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subw$month[subw$trt=="trtpd"], subw$Na_ppm[,1][subw$trt=="trtpd"]-subw$Na_ppm[,2][subw$trt=="trtpd"], subw$month[subw$trt=="trtpd"], subw$Na_ppm[,1][subw$trt=="trtpd"]+subw$Na_ppm[,2][subw$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subw$Na_ppm[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subw$Na_ppm[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subw$month[subw$trt=="untrtpd"], subw$Na_ppm[,1][subw$trt=="untrtpd"]-subw$Na_ppm[,2][subw$trt=="untrtpd"], subw$month[subw$trt=="untrtpd"], subw$Na_ppm[,1][subw$trt=="untrtpd"]+subw$Na_ppm[,2][subw$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##Woody Si
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0,2000),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Woody ppm Si", line=3.5, cex.lab=1.6)
##bison
points(subw$Si_ppm[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subw$Si_ppm[,1][subw$trt=="bison"] ~ subw$month[subw$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subw$month[subw$trt=="bison"], subw$Si_ppm[,1][subw$trt=="bison"]-subw$Si_ppm[,2][subw$trt=="bison"], subw$month[subw$trt=="bison"], subw$Si_ppm[,1][subw$trt=="bison"]+subw$Si_ppm[,2][subw$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subw$Si_ppm[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subw$Si_ppm[,1][subw$trt=="cattle"] ~ subw$month[subw$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subw$month[subw$trt=="cattle"], subw$Si_ppm[,1][subw$trt=="cattle"]-subw$Si_ppm[,2][subw$trt=="cattle"], subw$month[subw$trt=="cattle"], subw$Si_ppm[,1][subw$trt=="cattle"]+subw$Si_ppm[,2][subw$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subw$Si_ppm[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subw$Si_ppm[,1][subw$trt=="ungrazed"] ~ subw$month[subw$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subw$month[subw$trt=="ungrazed"], subw$Si_ppm[,1][subw$trt=="ungrazed"]-subw$Si_ppm[,2][subw$trt=="ungrazed"], subw$month[subw$trt=="ungrazed"], subw$Si_ppm[,1][subw$trt=="ungrazed"]+subw$Si_ppm[,2][subw$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subw$Si_ppm[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subw$Si_ppm[,1][subw$trt=="trtpd"] ~ subw$month[subw$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subw$month[subw$trt=="trtpd"], subw$Si_ppm[,1][subw$trt=="trtpd"]-subw$Si_ppm[,2][subw$trt=="trtpd"], subw$month[subw$trt=="trtpd"], subw$Si_ppm[,1][subw$trt=="trtpd"]+subw$Si_ppm[,2][subw$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subw$Si_ppm[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subw$Si_ppm[,1][subw$trt=="untrtpd"] ~ subw$month[subw$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subw$month[subw$trt=="untrtpd"], subw$Si_ppm[,1][subw$trt=="untrtpd"]-subw$Si_ppm[,2][subw$trt=="untrtpd"], subw$month[subw$trt=="untrtpd"], subw$Si_ppm[,1][subw$trt=="untrtpd"]+subw$Si_ppm[,2][subw$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topleft",legend=c("Ungrazed","Bison","Cattle","Untrt PD","Trt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,25,24), pt.bg=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"),col=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"))

##
##################################################################
##

