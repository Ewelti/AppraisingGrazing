##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")

##load libraries
library(lme4)
library(stringr)
library(plotrix)

#################plant chem
# attach data
chem <- read.csv("rawData/SoilChem.csv")
head(chem)

#means by trt
chem$trt_mo <- paste(chem$trt,chem$mo)
chemsum <- aggregate(cbind(percC,percN,Ca_ppm,K_ppm,Mg_ppm,Na_ppm,P_ppm) ~ trt_mo, data = chem, FUN = function(x)c(mean = mean(x),se = std.error(x)))
head(chemsum)

chemsum[c('trt', 'month')] <- str_split_fixed(chemsum$trt_mo, ' ', 2)
chemsum$month <-as.numeric(chemsum$month)


################
tiff(filename = "plots/SoilChem_individualPlots.tiff", width = 6, height = 3, units = 'in', res = 600, compression = 'lzw')

par(mar=c(2.5,5,0.2,0.2),mfrow=c(1,3))
#########Soil C
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(1,11),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Soil %C", line=3, cex.lab=1.6)
#title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(chemsum$percC[,1][chemsum$trt=="bison"] ~ chemsum$month[chemsum$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(chemsum$percC[,1][chemsum$trt=="bison"] ~ chemsum$month[chemsum$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(chemsum$month[chemsum$trt=="bison"], chemsum$percC[,1][chemsum$trt=="bison"]-chemsum$percC[,2][chemsum$trt=="bison"], chemsum$month[chemsum$trt=="bison"], chemsum$percC[,1][chemsum$trt=="bison"]+chemsum$percC[,2][chemsum$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(chemsum$percC[,1][chemsum$trt=="cattle"] ~ chemsum$month[chemsum$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(chemsum$percC[,1][chemsum$trt=="cattle"] ~ chemsum$month[chemsum$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(chemsum$month[chemsum$trt=="cattle"], chemsum$percC[,1][chemsum$trt=="cattle"]-chemsum$percC[,2][chemsum$trt=="cattle"], chemsum$month[chemsum$trt=="cattle"], chemsum$percC[,1][chemsum$trt=="cattle"]+chemsum$percC[,2][chemsum$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(chemsum$percC[,1][chemsum$trt=="ungrazed"] ~ chemsum$month[chemsum$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(chemsum$percC[,1][chemsum$trt=="ungrazed"] ~ chemsum$month[chemsum$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(chemsum$month[chemsum$trt=="ungrazed"], chemsum$percC[,1][chemsum$trt=="ungrazed"]-chemsum$percC[,2][chemsum$trt=="ungrazed"], chemsum$month[chemsum$trt=="ungrazed"], chemsum$percC[,1][chemsum$trt=="ungrazed"]+chemsum$percC[,2][chemsum$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(chemsum$percC[,1][chemsum$trt=="trtpd"] ~ chemsum$month[chemsum$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(chemsum$percC[,1][chemsum$trt=="trtpd"] ~ chemsum$month[chemsum$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(chemsum$month[chemsum$trt=="trtpd"], chemsum$percC[,1][chemsum$trt=="trtpd"]-chemsum$percC[,2][chemsum$trt=="trtpd"], chemsum$month[chemsum$trt=="trtpd"], chemsum$percC[,1][chemsum$trt=="trtpd"]+chemsum$percC[,2][chemsum$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(chemsum$percC[,1][chemsum$trt=="untrtpd"] ~ chemsum$month[chemsum$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(chemsum$percC[,1][chemsum$trt=="untrtpd"] ~ chemsum$month[chemsum$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(chemsum$month[chemsum$trt=="untrtpd"], chemsum$percC[,1][chemsum$trt=="untrtpd"]-chemsum$percC[,2][chemsum$trt=="untrtpd"], chemsum$month[chemsum$trt=="untrtpd"], chemsum$percC[,1][chemsum$trt=="untrtpd"]+chemsum$percC[,2][chemsum$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##Soil N
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0,0.9),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Soil %N", line=3, cex.lab=1.6)
##bison
points(chemsum$percN[,1][chemsum$trt=="bison"] ~ chemsum$month[chemsum$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(chemsum$percN[,1][chemsum$trt=="bison"] ~ chemsum$month[chemsum$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(chemsum$month[chemsum$trt=="bison"], chemsum$percN[,1][chemsum$trt=="bison"]-chemsum$percN[,2][chemsum$trt=="bison"], chemsum$month[chemsum$trt=="bison"], chemsum$percN[,1][chemsum$trt=="bison"]+chemsum$percN[,2][chemsum$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(chemsum$percN[,1][chemsum$trt=="cattle"] ~ chemsum$month[chemsum$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(chemsum$percN[,1][chemsum$trt=="cattle"] ~ chemsum$month[chemsum$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(chemsum$month[chemsum$trt=="cattle"], chemsum$percN[,1][chemsum$trt=="cattle"]-chemsum$percN[,2][chemsum$trt=="cattle"], chemsum$month[chemsum$trt=="cattle"], chemsum$percN[,1][chemsum$trt=="cattle"]+chemsum$percN[,2][chemsum$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(chemsum$percN[,1][chemsum$trt=="ungrazed"] ~ chemsum$month[chemsum$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(chemsum$percN[,1][chemsum$trt=="ungrazed"] ~ chemsum$month[chemsum$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(chemsum$month[chemsum$trt=="ungrazed"], chemsum$percN[,1][chemsum$trt=="ungrazed"]-chemsum$percN[,2][chemsum$trt=="ungrazed"], chemsum$month[chemsum$trt=="ungrazed"], chemsum$percN[,1][chemsum$trt=="ungrazed"]+chemsum$percN[,2][chemsum$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(chemsum$percN[,1][chemsum$trt=="trtpd"] ~ chemsum$month[chemsum$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(chemsum$percN[,1][chemsum$trt=="trtpd"] ~ chemsum$month[chemsum$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(chemsum$month[chemsum$trt=="trtpd"], chemsum$percN[,1][chemsum$trt=="trtpd"]-chemsum$percN[,2][chemsum$trt=="trtpd"], chemsum$month[chemsum$trt=="trtpd"], chemsum$percN[,1][chemsum$trt=="trtpd"]+chemsum$percN[,2][chemsum$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(chemsum$percN[,1][chemsum$trt=="untrtpd"] ~ chemsum$month[chemsum$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(chemsum$percN[,1][chemsum$trt=="untrtpd"] ~ chemsum$month[chemsum$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(chemsum$month[chemsum$trt=="untrtpd"], chemsum$percN[,1][chemsum$trt=="untrtpd"]-chemsum$percN[,2][chemsum$trt=="untrtpd"], chemsum$month[chemsum$trt=="untrtpd"], chemsum$percN[,1][chemsum$trt=="untrtpd"]+chemsum$percN[,2][chemsum$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

plot.new()
legend("top",legend=c("Ungrazed","Bison","Cattle","Untrt PD","Trt PD"), bty="n", pt.cex=3,cex=2, pch=c(23,21,22,25,24), pt.bg=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"),col=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"))
##

dev.off()
##
##################################################################
##


#########################################################
##Soil P
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(600,2700),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Soil ppm P", line=3.5, cex.lab=1.6)
##bison
points(chemsum$P_ppm[,1][chemsum$trt=="bison"] ~ chemsum$month[chemsum$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(chemsum$P_ppm[,1][chemsum$trt=="bison"] ~ chemsum$month[chemsum$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(chemsum$month[chemsum$trt=="bison"], chemsum$P_ppm[,1][chemsum$trt=="bison"]-chemsum$P_ppm[,2][chemsum$trt=="bison"], chemsum$month[chemsum$trt=="bison"], chemsum$P_ppm[,1][chemsum$trt=="bison"]+chemsum$P_ppm[,2][chemsum$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(chemsum$P_ppm[,1][chemsum$trt=="cattle"] ~ chemsum$month[chemsum$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(chemsum$P_ppm[,1][chemsum$trt=="cattle"] ~ chemsum$month[chemsum$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(chemsum$month[chemsum$trt=="cattle"], chemsum$P_ppm[,1][chemsum$trt=="cattle"]-chemsum$P_ppm[,2][chemsum$trt=="cattle"], chemsum$month[chemsum$trt=="cattle"], chemsum$P_ppm[,1][chemsum$trt=="cattle"]+chemsum$P_ppm[,2][chemsum$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(chemsum$P_ppm[,1][chemsum$trt=="ungrazed"] ~ chemsum$month[chemsum$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(chemsum$P_ppm[,1][chemsum$trt=="ungrazed"] ~ chemsum$month[chemsum$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(chemsum$month[chemsum$trt=="ungrazed"], chemsum$P_ppm[,1][chemsum$trt=="ungrazed"]-chemsum$P_ppm[,2][chemsum$trt=="ungrazed"], chemsum$month[chemsum$trt=="ungrazed"], chemsum$P_ppm[,1][chemsum$trt=="ungrazed"]+chemsum$P_ppm[,2][chemsum$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(chemsum$P_ppm[,1][chemsum$trt=="trtpd"] ~ chemsum$month[chemsum$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(chemsum$P_ppm[,1][chemsum$trt=="trtpd"] ~ chemsum$month[chemsum$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(chemsum$month[chemsum$trt=="trtpd"], chemsum$P_ppm[,1][chemsum$trt=="trtpd"]-chemsum$P_ppm[,2][chemsum$trt=="trtpd"], chemsum$month[chemsum$trt=="trtpd"], chemsum$P_ppm[,1][chemsum$trt=="trtpd"]+chemsum$P_ppm[,2][chemsum$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(chemsum$P_ppm[,1][chemsum$trt=="untrtpd"] ~ chemsum$month[chemsum$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(chemsum$P_ppm[,1][chemsum$trt=="untrtpd"] ~ chemsum$month[chemsum$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(chemsum$month[chemsum$trt=="untrtpd"], chemsum$P_ppm[,1][chemsum$trt=="untrtpd"]-chemsum$P_ppm[,2][chemsum$trt=="untrtpd"], chemsum$month[chemsum$trt=="untrtpd"], chemsum$P_ppm[,1][chemsum$trt=="untrtpd"]+chemsum$P_ppm[,2][chemsum$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##Soil K
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(4500,27000),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Soil ppm K", line=3.5, cex.lab=1.6)
##bison
points(chemsum$K_ppm[,1][chemsum$trt=="bison"] ~ chemsum$month[chemsum$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(chemsum$K_ppm[,1][chemsum$trt=="bison"] ~ chemsum$month[chemsum$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(chemsum$month[chemsum$trt=="bison"], chemsum$K_ppm[,1][chemsum$trt=="bison"]-chemsum$K_ppm[,2][chemsum$trt=="bison"], chemsum$month[chemsum$trt=="bison"], chemsum$K_ppm[,1][chemsum$trt=="bison"]+chemsum$K_ppm[,2][chemsum$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(chemsum$K_ppm[,1][chemsum$trt=="cattle"] ~ chemsum$month[chemsum$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(chemsum$K_ppm[,1][chemsum$trt=="cattle"] ~ chemsum$month[chemsum$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(chemsum$month[chemsum$trt=="cattle"], chemsum$K_ppm[,1][chemsum$trt=="cattle"]-chemsum$K_ppm[,2][chemsum$trt=="cattle"], chemsum$month[chemsum$trt=="cattle"], chemsum$K_ppm[,1][chemsum$trt=="cattle"]+chemsum$K_ppm[,2][chemsum$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(chemsum$K_ppm[,1][chemsum$trt=="ungrazed"] ~ chemsum$month[chemsum$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(chemsum$K_ppm[,1][chemsum$trt=="ungrazed"] ~ chemsum$month[chemsum$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(chemsum$month[chemsum$trt=="ungrazed"], chemsum$K_ppm[,1][chemsum$trt=="ungrazed"]-chemsum$K_ppm[,2][chemsum$trt=="ungrazed"], chemsum$month[chemsum$trt=="ungrazed"], chemsum$K_ppm[,1][chemsum$trt=="ungrazed"]+chemsum$K_ppm[,2][chemsum$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(chemsum$K_ppm[,1][chemsum$trt=="trtpd"] ~ chemsum$month[chemsum$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(chemsum$K_ppm[,1][chemsum$trt=="trtpd"] ~ chemsum$month[chemsum$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(chemsum$month[chemsum$trt=="trtpd"], chemsum$K_ppm[,1][chemsum$trt=="trtpd"]-chemsum$K_ppm[,2][chemsum$trt=="trtpd"], chemsum$month[chemsum$trt=="trtpd"], chemsum$K_ppm[,1][chemsum$trt=="trtpd"]+chemsum$K_ppm[,2][chemsum$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(chemsum$K_ppm[,1][chemsum$trt=="untrtpd"] ~ chemsum$month[chemsum$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(chemsum$K_ppm[,1][chemsum$trt=="untrtpd"] ~ chemsum$month[chemsum$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(chemsum$month[chemsum$trt=="untrtpd"], chemsum$K_ppm[,1][chemsum$trt=="untrtpd"]-chemsum$K_ppm[,2][chemsum$trt=="untrtpd"], chemsum$month[chemsum$trt=="untrtpd"], chemsum$K_ppm[,1][chemsum$trt=="untrtpd"]+chemsum$K_ppm[,2][chemsum$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##################

##Soil Mg
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(400,2500),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Soil ppm Mg", line=3.5, cex.lab=1.6)
##bison
points(chemsum$Mg_ppm[,1][chemsum$trt=="bison"] ~ chemsum$month[chemsum$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(chemsum$Mg_ppm[,1][chemsum$trt=="bison"] ~ chemsum$month[chemsum$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(chemsum$month[chemsum$trt=="bison"], chemsum$Mg_ppm[,1][chemsum$trt=="bison"]-chemsum$Mg_ppm[,2][chemsum$trt=="bison"], chemsum$month[chemsum$trt=="bison"], chemsum$Mg_ppm[,1][chemsum$trt=="bison"]+chemsum$Mg_ppm[,2][chemsum$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(chemsum$Mg_ppm[,1][chemsum$trt=="cattle"] ~ chemsum$month[chemsum$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(chemsum$Mg_ppm[,1][chemsum$trt=="cattle"] ~ chemsum$month[chemsum$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(chemsum$month[chemsum$trt=="cattle"], chemsum$Mg_ppm[,1][chemsum$trt=="cattle"]-chemsum$Mg_ppm[,2][chemsum$trt=="cattle"], chemsum$month[chemsum$trt=="cattle"], chemsum$Mg_ppm[,1][chemsum$trt=="cattle"]+chemsum$Mg_ppm[,2][chemsum$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(chemsum$Mg_ppm[,1][chemsum$trt=="ungrazed"] ~ chemsum$month[chemsum$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(chemsum$Mg_ppm[,1][chemsum$trt=="ungrazed"] ~ chemsum$month[chemsum$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(chemsum$month[chemsum$trt=="ungrazed"], chemsum$Mg_ppm[,1][chemsum$trt=="ungrazed"]-chemsum$Mg_ppm[,2][chemsum$trt=="ungrazed"], chemsum$month[chemsum$trt=="ungrazed"], chemsum$Mg_ppm[,1][chemsum$trt=="ungrazed"]+chemsum$Mg_ppm[,2][chemsum$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(chemsum$Mg_ppm[,1][chemsum$trt=="trtpd"] ~ chemsum$month[chemsum$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(chemsum$Mg_ppm[,1][chemsum$trt=="trtpd"] ~ chemsum$month[chemsum$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(chemsum$month[chemsum$trt=="trtpd"], chemsum$Mg_ppm[,1][chemsum$trt=="trtpd"]-chemsum$Mg_ppm[,2][chemsum$trt=="trtpd"], chemsum$month[chemsum$trt=="trtpd"], chemsum$Mg_ppm[,1][chemsum$trt=="trtpd"]+chemsum$Mg_ppm[,2][chemsum$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(chemsum$Mg_ppm[,1][chemsum$trt=="untrtpd"] ~ chemsum$month[chemsum$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(chemsum$Mg_ppm[,1][chemsum$trt=="untrtpd"] ~ chemsum$month[chemsum$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(chemsum$month[chemsum$trt=="untrtpd"], chemsum$Mg_ppm[,1][chemsum$trt=="untrtpd"]-chemsum$Mg_ppm[,2][chemsum$trt=="untrtpd"], chemsum$month[chemsum$trt=="untrtpd"], chemsum$Mg_ppm[,1][chemsum$trt=="untrtpd"]+chemsum$Mg_ppm[,2][chemsum$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##Soil Na
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(1220,2050),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Soil ppm Na", line=3.5, cex.lab=1.6)
##bison
points(chemsum$Na_ppm[,1][chemsum$trt=="bison"] ~ chemsum$month[chemsum$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(chemsum$Na_ppm[,1][chemsum$trt=="bison"] ~ chemsum$month[chemsum$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(chemsum$month[chemsum$trt=="bison"], chemsum$Na_ppm[,1][chemsum$trt=="bison"]-chemsum$Na_ppm[,2][chemsum$trt=="bison"], chemsum$month[chemsum$trt=="bison"], chemsum$Na_ppm[,1][chemsum$trt=="bison"]+chemsum$Na_ppm[,2][chemsum$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(chemsum$Na_ppm[,1][chemsum$trt=="cattle"] ~ chemsum$month[chemsum$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(chemsum$Na_ppm[,1][chemsum$trt=="cattle"] ~ chemsum$month[chemsum$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(chemsum$month[chemsum$trt=="cattle"], chemsum$Na_ppm[,1][chemsum$trt=="cattle"]-chemsum$Na_ppm[,2][chemsum$trt=="cattle"], chemsum$month[chemsum$trt=="cattle"], chemsum$Na_ppm[,1][chemsum$trt=="cattle"]+chemsum$Na_ppm[,2][chemsum$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(chemsum$Na_ppm[,1][chemsum$trt=="ungrazed"] ~ chemsum$month[chemsum$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(chemsum$Na_ppm[,1][chemsum$trt=="ungrazed"] ~ chemsum$month[chemsum$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(chemsum$month[chemsum$trt=="ungrazed"], chemsum$Na_ppm[,1][chemsum$trt=="ungrazed"]-chemsum$Na_ppm[,2][chemsum$trt=="ungrazed"], chemsum$month[chemsum$trt=="ungrazed"], chemsum$Na_ppm[,1][chemsum$trt=="ungrazed"]+chemsum$Na_ppm[,2][chemsum$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(chemsum$Na_ppm[,1][chemsum$trt=="trtpd"] ~ chemsum$month[chemsum$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(chemsum$Na_ppm[,1][chemsum$trt=="trtpd"] ~ chemsum$month[chemsum$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(chemsum$month[chemsum$trt=="trtpd"], chemsum$Na_ppm[,1][chemsum$trt=="trtpd"]-chemsum$Na_ppm[,2][chemsum$trt=="trtpd"], chemsum$month[chemsum$trt=="trtpd"], chemsum$Na_ppm[,1][chemsum$trt=="trtpd"]+chemsum$Na_ppm[,2][chemsum$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(chemsum$Na_ppm[,1][chemsum$trt=="untrtpd"] ~ chemsum$month[chemsum$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(chemsum$Na_ppm[,1][chemsum$trt=="untrtpd"] ~ chemsum$month[chemsum$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(chemsum$month[chemsum$trt=="untrtpd"], chemsum$Na_ppm[,1][chemsum$trt=="untrtpd"]-chemsum$Na_ppm[,2][chemsum$trt=="untrtpd"], chemsum$month[chemsum$trt=="untrtpd"], chemsum$Na_ppm[,1][chemsum$trt=="untrtpd"]+chemsum$Na_ppm[,2][chemsum$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)




