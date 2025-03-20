##################################################################
##

##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")

##load libraries
library(lme4)
library(stringr)
library(plotrix)
library(effects)

#################plant chem
##load data
est <- read.csv("outputs/SiteMonthLevel.csv")
head(est)
est$trt <- as.factor(est$trt)

#log scale soil chem
est$ls_C <- log10(est$percC)
est$ls_N <- log10(est$percN)
est$ls_P <- log10(est$P_ppm)
est$ls_K <- log10(est$K_ppm)
est$ls_Mg <- log10(est$Mg_ppm)
est$ls_Na <- log10(est$Na_ppm)

#means by trt
est$trt_mo <- paste(est$trt,est$month)
chemsum <- aggregate(cbind(ls_N,ls_P,ls_K,ls_Mg,ls_Na,ls_C) ~ trt_mo, data = est, FUN = function(x)c(mean = mean(x),se = std.error(x)))
head(chemsum)

chemsum[c('trt', 'month')] <- str_split_fixed(chemsum$trt_mo, ' ', 2)
chemsum$month <-as.numeric(chemsum$month)
chemi <- as.data.frame(chemsum)

#subset by trt
b <- est[est$trt=="bison",]
ca <- est[est$trt=="cattle",]
un <- est[est$trt=="ungrazed",]
pd <- est[est$trt=="untrtpd",]
tp <- est[est$trt=="trtpd",]

####color function for making transparent colors
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color

## Get RGB values for named color
rgb.val <- col2rgb(color)

## Make new color uCng input color as base and alpha set by transparency
t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
             max = 255,
             alpha = (100 - percent) * 255 / 100,
             names = name)

## Save the color
invisible(t.col)
}

################
tiff(filename = "plots/SoilChem_individualPlots.tif", width = 9, height = 5, units = 'in', res = 600, compression = 'lzw')

par(mar=c(2.5,5,0.2,0.2),mfrow=c(2,3))

#############################################################
##soil C
nut <- na.omit(est$ls_C)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(0.05,0.96), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(1.5),log10(2),log10(3),log10(4),log10(6),log10(8)), labels=c(1.5,2,3,4,6,8),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Soil %C", line=3, cex.lab=1.6)
##bison
cm <- lmer(ls_C ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$ls_C[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col=t_col("sienna", perc = 40),bg=t_col("sienna", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("sienna", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$ls_C[,1][chemi$trt=="bison"]-chemi$ls_C[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$ls_C[,1][chemi$trt=="bison"]+chemi$ls_C[,2][chemi$trt=="bison"],col=t_col("sienna", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(ls_C ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$ls_C[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col=t_col("gray0", perc = 40),bg=t_col("gray0", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("gray0", perc = 40),lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$ls_C[,1][chemi$trt=="cattle"]-chemi$ls_C[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$ls_C[,1][chemi$trt=="cattle"]+chemi$ls_C[,2][chemi$trt=="cattle"],col=t_col("gray0", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(ls_C ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$ls_C[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col=t_col("dodgerblue", perc = 40),bg=t_col("dodgerblue", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("dodgerblue", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$ls_C[,1][chemi$trt=="ungrazed"]-chemi$ls_C[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$ls_C[,1][chemi$trt=="ungrazed"]+chemi$ls_C[,2][chemi$trt=="ungrazed"],col=t_col("dodgerblue", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(ls_C ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
##polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$ls_C[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col=t_col("firebrick2", perc = 40),bg=t_col("firebrick2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("firebrick2", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$ls_C[,1][chemi$trt=="trtpd"]-chemi$ls_C[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$ls_C[,1][chemi$trt=="trtpd"]+chemi$ls_C[,2][chemi$trt=="trtpd"],col=t_col("firebrick2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(ls_C ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$ls_C[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col=t_col("goldenrod2", perc = 40),bg=t_col("goldenrod2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("goldenrod2", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$ls_C[,1][chemi$trt=="untrtpd"]-chemi$ls_C[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$ls_C[,1][chemi$trt=="untrtpd"]+chemi$ls_C[,2][chemi$trt=="untrtpd"],col=t_col("goldenrod2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("A"), bty="n", cex=1.5)
###############################################################

##
##soil N
nut <- na.omit(est$ls_N)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(-1,-0.15), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(0.1),log10(0.15),log10(0.2),log10(0.3),log10(0.4),log10(0.6),log10(0.8)), labels=c(0.1,0.15,0.2,0.3,0.4,0.6,0.8),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Soil %N", line=3, cex.lab=1.6)
##bison
cm <- lmer(ls_N ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$ls_N[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col=t_col("sienna", perc = 40),bg=t_col("sienna", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("sienna", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$ls_N[,1][chemi$trt=="bison"]-chemi$ls_N[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$ls_N[,1][chemi$trt=="bison"]+chemi$ls_N[,2][chemi$trt=="bison"],col=t_col("sienna", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(ls_N ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$ls_N[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col=t_col("gray0", perc = 40),bg=t_col("gray0", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("gray0", perc = 40),lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$ls_N[,1][chemi$trt=="cattle"]-chemi$ls_N[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$ls_N[,1][chemi$trt=="cattle"]+chemi$ls_N[,2][chemi$trt=="cattle"],col=t_col("gray0", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(ls_N ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$ls_N[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col=t_col("dodgerblue", perc = 40),bg=t_col("dodgerblue", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("dodgerblue", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$ls_N[,1][chemi$trt=="ungrazed"]-chemi$ls_N[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$ls_N[,1][chemi$trt=="ungrazed"]+chemi$ls_N[,2][chemi$trt=="ungrazed"],col=t_col("dodgerblue", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(ls_N ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$ls_N[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col=t_col("firebrick2", perc = 40),bg=t_col("firebrick2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("firebrick2", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$ls_N[,1][chemi$trt=="trtpd"]-chemi$ls_N[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$ls_N[,1][chemi$trt=="trtpd"]+chemi$ls_N[,2][chemi$trt=="trtpd"],col=t_col("firebrick2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(ls_N ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$ls_N[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col=t_col("goldenrod2", perc = 40),bg=t_col("goldenrod2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("goldenrod2", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$ls_N[,1][chemi$trt=="untrtpd"]-chemi$ls_N[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$ls_N[,1][chemi$trt=="untrtpd"]+chemi$ls_N[,2][chemi$trt=="untrtpd"],col=t_col("goldenrod2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("B"), bty="n", cex=1.5)

#####################################################
##soil P
nut <- na.omit(est$ls_P)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(0.25,1.45), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(1),log10(2),log10(5),log10(10),log10(20),log10(30)), labels=c(1,2,5,10,20,30),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Soil ppm P", line=3, cex.lab=1.6)

##bison
cm <- lmer(ls_P ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$ls_P[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col=t_col("sienna", perc = 40),bg=t_col("sienna", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("sienna", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$ls_P[,1][chemi$trt=="bison"]-chemi$ls_P[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$ls_P[,1][chemi$trt=="bison"]+chemi$ls_P[,2][chemi$trt=="bison"],col=t_col("sienna", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(ls_P ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$ls_P[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col=t_col("gray0", perc = 40),bg=t_col("gray0", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("gray0", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$ls_P[,1][chemi$trt=="cattle"]-chemi$ls_P[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$ls_P[,1][chemi$trt=="cattle"]+chemi$ls_P[,2][chemi$trt=="cattle"],col=t_col("gray0", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(ls_P ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$ls_P[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col=t_col("dodgerblue", perc = 40),bg=t_col("dodgerblue", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("dodgerblue", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$ls_P[,1][chemi$trt=="ungrazed"]-chemi$ls_P[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$ls_P[,1][chemi$trt=="ungrazed"]+chemi$ls_P[,2][chemi$trt=="ungrazed"],col=t_col("dodgerblue", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(ls_P ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$ls_P[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col=t_col("firebrick2", perc = 40),bg=t_col("firebrick2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("firebrick2", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$ls_P[,1][chemi$trt=="trtpd"]-chemi$ls_P[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$ls_P[,1][chemi$trt=="trtpd"]+chemi$ls_P[,2][chemi$trt=="trtpd"],col=t_col("firebrick2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(ls_P ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$ls_P[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col=t_col("goldenrod2", perc = 40),bg=t_col("goldenrod2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("goldenrod2", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$ls_P[,1][chemi$trt=="untrtpd"]-chemi$ls_P[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$ls_P[,1][chemi$trt=="untrtpd"]+chemi$ls_P[,2][chemi$trt=="untrtpd"],col=t_col("goldenrod2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("C"), bty="n", cex=1.5)

############################################################################
##soil K
nut <- na.omit(est$ls_K)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(1.77,2.65), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(50),log10(75),log10(100),log10(150),log10(200),log10(300),log10(400)), labels=c(50,75,100,150,200,300,400),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Soil ppm K", line=3, cex.lab=1.6)

##bison
cm <- lmer(ls_K ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$ls_K[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col=t_col("sienna", perc = 40),bg=t_col("sienna", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("sienna", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$ls_K[,1][chemi$trt=="bison"]-chemi$ls_K[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$ls_K[,1][chemi$trt=="bison"]+chemi$ls_K[,2][chemi$trt=="bison"],col=t_col("sienna", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(ls_K ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$ls_K[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col=t_col("gray0", perc = 40),bg=t_col("gray0", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("gray0", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$ls_K[,1][chemi$trt=="cattle"]-chemi$ls_K[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$ls_K[,1][chemi$trt=="cattle"]+chemi$ls_K[,2][chemi$trt=="cattle"],col=t_col("gray0", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(ls_K ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$ls_K[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col=t_col("dodgerblue", perc = 40),bg=t_col("dodgerblue", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("dodgerblue", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$ls_K[,1][chemi$trt=="ungrazed"]-chemi$ls_K[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$ls_K[,1][chemi$trt=="ungrazed"]+chemi$ls_K[,2][chemi$trt=="ungrazed"],col=t_col("dodgerblue", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(ls_K ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$ls_K[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col=t_col("firebrick2", perc = 40),bg=t_col("firebrick2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("firebrick2", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$ls_K[,1][chemi$trt=="trtpd"]-chemi$ls_K[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$ls_K[,1][chemi$trt=="trtpd"]+chemi$ls_K[,2][chemi$trt=="trtpd"],col=t_col("firebrick2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(ls_K ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$ls_K[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col=t_col("goldenrod2", perc = 40),bg=t_col("goldenrod2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("goldenrod2", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$ls_K[,1][chemi$trt=="untrtpd"]-chemi$ls_K[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$ls_K[,1][chemi$trt=="untrtpd"]+chemi$ls_K[,2][chemi$trt=="untrtpd"],col=t_col("goldenrod2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("D"), bty="n", cex=1.5)

##
legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Untrt PD","Trt PD"), bty="n", pt.cex=2,cex=1.5, pch=c(23,21,22,25,24), pt.bg=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"),col=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"))
##

#######################################################

##soil Mg
nut <- na.omit(est$ls_Mg)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(2.15,2.75), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(100),log10(150),log10(200),log10(250),log10(300),log10(400),log10(500)), labels=c(100,150,200,250,300,400,500),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Soil ppm Mg", line=3, cex.lab=1.6)

##bison
cm <- lmer(ls_Mg ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$ls_Mg[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col=t_col("sienna", perc = 40),bg=t_col("sienna", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("sienna", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$ls_Mg[,1][chemi$trt=="bison"]-chemi$ls_Mg[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$ls_Mg[,1][chemi$trt=="bison"]+chemi$ls_Mg[,2][chemi$trt=="bison"],col=t_col("sienna", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(ls_Mg ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$ls_Mg[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col=t_col("gray0", perc = 40),bg=t_col("gray0", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("gray0", perc = 40),lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$ls_Mg[,1][chemi$trt=="cattle"]-chemi$ls_Mg[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$ls_Mg[,1][chemi$trt=="cattle"]+chemi$ls_Mg[,2][chemi$trt=="cattle"],col=t_col("gray0", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(ls_Mg ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$ls_Mg[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col=t_col("dodgerblue", perc = 40),bg=t_col("dodgerblue", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("dodgerblue", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$ls_Mg[,1][chemi$trt=="ungrazed"]-chemi$ls_Mg[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$ls_Mg[,1][chemi$trt=="ungrazed"]+chemi$ls_Mg[,2][chemi$trt=="ungrazed"],col=t_col("dodgerblue", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(ls_Mg ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$ls_Mg[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col=t_col("firebrick2", perc = 40),bg=t_col("firebrick2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("firebrick2", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$ls_Mg[,1][chemi$trt=="trtpd"]-chemi$ls_Mg[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$ls_Mg[,1][chemi$trt=="trtpd"]+chemi$ls_Mg[,2][chemi$trt=="trtpd"],col=t_col("firebrick2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(ls_Mg ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$ls_Mg[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col=t_col("goldenrod2", perc = 40),bg=t_col("goldenrod2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("goldenrod2", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$ls_Mg[,1][chemi$trt=="untrtpd"]-chemi$ls_Mg[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$ls_Mg[,1][chemi$trt=="untrtpd"]+chemi$ls_Mg[,2][chemi$trt=="untrtpd"],col=t_col("goldenrod2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("E"), bty="n", cex=1.5)

##########################################################################
##soil Na
nut <- na.omit(est$ls_Na)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(1.1,2.5), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(20),log10(50),log10(100),log10(200),log10(300)), labels=c(20,50,100,200,300),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Soil ppm Na", line=3, cex.lab=1.6)
##bison
cm <- lmer(ls_Na ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$ls_Na[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col=t_col("sienna", perc = 40),bg=t_col("sienna", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("sienna", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$ls_Na[,1][chemi$trt=="bison"]-chemi$ls_Na[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$ls_Na[,1][chemi$trt=="bison"]+chemi$ls_Na[,2][chemi$trt=="bison"],col=t_col("sienna", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(ls_Na ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$ls_Na[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col=t_col("gray0", perc = 40),bg=t_col("gray0", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("gray0", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$ls_Na[,1][chemi$trt=="cattle"]-chemi$ls_Na[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$ls_Na[,1][chemi$trt=="cattle"]+chemi$ls_Na[,2][chemi$trt=="cattle"],col=t_col("gray0", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(ls_Na ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$ls_Na[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col=t_col("dodgerblue", perc = 40),bg=t_col("dodgerblue", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("dodgerblue", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$ls_Na[,1][chemi$trt=="ungrazed"]-chemi$ls_Na[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$ls_Na[,1][chemi$trt=="ungrazed"]+chemi$ls_Na[,2][chemi$trt=="ungrazed"],col=t_col("dodgerblue", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(ls_Na ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$ls_Na[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col=t_col("firebrick2", perc = 40),bg=t_col("firebrick2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("firebrick2", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$ls_Na[,1][chemi$trt=="trtpd"]-chemi$ls_Na[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$ls_Na[,1][chemi$trt=="trtpd"]+chemi$ls_Na[,2][chemi$trt=="trtpd"],col=t_col("firebrick2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(ls_Na ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$ls_Na[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col=t_col("goldenrod2", perc = 40),bg=t_col("goldenrod2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col=t_col("goldenrod2", perc = 40),lwd=2, lty=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$ls_Na[,1][chemi$trt=="untrtpd"]-chemi$ls_Na[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$ls_Na[,1][chemi$trt=="untrtpd"]+chemi$ls_Na[,2][chemi$trt=="untrtpd"],col=t_col("goldenrod2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("F"), bty="n", cex=1.5)


dev.off()
##
##################################################################
##

