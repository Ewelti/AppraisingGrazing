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

#log scale forb chem
est$lf_N <- log10(est$f_N)
est$lf_P <- log10(est$f_P)
est$lf_K <- log10(est$f_K)
est$lf_Mg <- log10(est$f_Mg)
est$lf_Na <- log10(est$f_Na)
est$lf_Si <- log10(est$f_Si)
est["lf_Si"][est["lf_Si"] == "-Inf"] <- 0

#means by trt
est$trt_mo <- paste(est$trt,est$month)
chemsum <- aggregate(cbind(lf_N,lf_P,lf_K,lf_Mg,lf_Na,lf_Si) ~ trt_mo, data = est, FUN = function(x)c(mean = mean(x),se = std.error(x)))
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

##color function for making transparent colors
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color

## Get RGB values for named color
rgb.val <- col2rgb(color)

## Make new color using input color as base and alpha set by transparency
t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
             max = 255,
             alpha = (100 - percent) * 255 / 100,
             names = name)

## Save the color
invisible(t.col)
}

################
tiff(filename = "plots/forbChem_individualPlots.tif", width = 9, height = 5, units = 'in', res = 600, compression = 'lzw')

par(mar=c(2.5,5,0.2,0.2),mfrow=c(2,3))

##forb N
nut <- na.omit(est$lf_N)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(-0.06,0.66), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(0.5),log10(1),log10(1.5),log10(2),log10(3),log10(4)), labels=c(0.5,1,1.5,2,3,4),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="forb %N", line=3, cex.lab=1.6)
##bison
cm <- lmer(lf_N ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$lf_N[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="sienna",lwd=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$lf_N[,1][chemi$trt=="bison"]-chemi$lf_N[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$lf_N[,1][chemi$trt=="bison"]+chemi$lf_N[,2][chemi$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(lf_N ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$lf_N[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="gray0",lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$lf_N[,1][chemi$trt=="cattle"]-chemi$lf_N[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$lf_N[,1][chemi$trt=="cattle"]+chemi$lf_N[,2][chemi$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(lf_N ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$lf_N[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="dodgerblue",lwd=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$lf_N[,1][chemi$trt=="ungrazed"]-chemi$lf_N[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$lf_N[,1][chemi$trt=="ungrazed"]+chemi$lf_N[,2][chemi$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(lf_N ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$lf_N[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="firebrick2",lwd=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$lf_N[,1][chemi$trt=="trtpd"]-chemi$lf_N[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$lf_N[,1][chemi$trt=="trtpd"]+chemi$lf_N[,2][chemi$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(lf_N ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$lf_N[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="goldenrod2",lwd=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$lf_N[,1][chemi$trt=="untrtpd"]-chemi$lf_N[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$lf_N[,1][chemi$trt=="untrtpd"]+chemi$lf_N[,2][chemi$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("A"), bty="n", cex=1.5)

#####################################################
##forb P
nut <- na.omit(est$lf_P)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(2.85,3.5), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(500),log10(1000),log10(1500),log10(2000),log10(2500)), labels=c(500,1000,1500,2000,2500),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="forb ppm P", line=3, cex.lab=1.6)

##bison
cm <- lmer(lf_P ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$lf_P[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="sienna",lwd=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$lf_P[,1][chemi$trt=="bison"]-chemi$lf_P[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$lf_P[,1][chemi$trt=="bison"]+chemi$lf_P[,2][chemi$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(lf_P ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$lf_P[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="gray0",lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$lf_P[,1][chemi$trt=="cattle"]-chemi$lf_P[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$lf_P[,1][chemi$trt=="cattle"]+chemi$lf_P[,2][chemi$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(lf_P ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$lf_P[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="dodgerblue",lwd=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$lf_P[,1][chemi$trt=="ungrazed"]-chemi$lf_P[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$lf_P[,1][chemi$trt=="ungrazed"]+chemi$lf_P[,2][chemi$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(lf_P ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$lf_P[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="firebrick2",lwd=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$lf_P[,1][chemi$trt=="trtpd"]-chemi$lf_P[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$lf_P[,1][chemi$trt=="trtpd"]+chemi$lf_P[,2][chemi$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(lf_P ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$lf_P[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="goldenrod2",lwd=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$lf_P[,1][chemi$trt=="untrtpd"]-chemi$lf_P[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$lf_P[,1][chemi$trt=="untrtpd"]+chemi$lf_P[,2][chemi$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("B"), bty="n", cex=1.5)

############################################################################
##forb K
nut <- na.omit(est$lf_K)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(3.7,4.7), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(5000),log10(10000),log10(20000),log10(40000)), labels=c(5000,10000,20000,40000),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="forb ppm K", line=3, cex.lab=1.6)

##bison
cm <- lmer(lf_K ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$lf_K[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="sienna",lwd=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$lf_K[,1][chemi$trt=="bison"]-chemi$lf_K[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$lf_K[,1][chemi$trt=="bison"]+chemi$lf_K[,2][chemi$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(lf_K ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$lf_K[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="gray0",lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$lf_K[,1][chemi$trt=="cattle"]-chemi$lf_K[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$lf_K[,1][chemi$trt=="cattle"]+chemi$lf_K[,2][chemi$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(lf_K ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$lf_K[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="dodgerblue",lwd=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$lf_K[,1][chemi$trt=="ungrazed"]-chemi$lf_K[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$lf_K[,1][chemi$trt=="ungrazed"]+chemi$lf_K[,2][chemi$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(lf_K ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$lf_K[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="firebrick2",lwd=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$lf_K[,1][chemi$trt=="trtpd"]-chemi$lf_K[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$lf_K[,1][chemi$trt=="trtpd"]+chemi$lf_K[,2][chemi$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(lf_K ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$lf_K[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="goldenrod2",lwd=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$lf_K[,1][chemi$trt=="untrtpd"]-chemi$lf_K[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$lf_K[,1][chemi$trt=="untrtpd"]+chemi$lf_K[,2][chemi$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("C"), bty="n", cex=1.5)

#######################################################

##forb Mg
nut <- na.omit(est$lf_Mg)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(2.9,3.8), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(500),log10(1000),log10(2000),log10(4000),log10(6000)), labels=c(500,1000,2000,4000,6000),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="forb ppm Mg", line=3, cex.lab=1.6)

##bison
cm <- lmer(lf_Mg ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$lf_Mg[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="sienna",lwd=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$lf_Mg[,1][chemi$trt=="bison"]-chemi$lf_Mg[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$lf_Mg[,1][chemi$trt=="bison"]+chemi$lf_Mg[,2][chemi$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(lf_Mg ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$lf_Mg[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="gray0",lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$lf_Mg[,1][chemi$trt=="cattle"]-chemi$lf_Mg[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$lf_Mg[,1][chemi$trt=="cattle"]+chemi$lf_Mg[,2][chemi$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(lf_Mg ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$lf_Mg[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="dodgerblue",lwd=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$lf_Mg[,1][chemi$trt=="ungrazed"]-chemi$lf_Mg[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$lf_Mg[,1][chemi$trt=="ungrazed"]+chemi$lf_Mg[,2][chemi$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(lf_Mg ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$lf_Mg[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="firebrick2",lwd=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$lf_Mg[,1][chemi$trt=="trtpd"]-chemi$lf_Mg[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$lf_Mg[,1][chemi$trt=="trtpd"]+chemi$lf_Mg[,2][chemi$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(lf_Mg ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$lf_Mg[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="goldenrod2",lwd=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$lf_Mg[,1][chemi$trt=="untrtpd"]-chemi$lf_Mg[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$lf_Mg[,1][chemi$trt=="untrtpd"]+chemi$lf_Mg[,2][chemi$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("D"), bty="n", cex=1.5)

##########################################################################
##forb Na
nut <- na.omit(est$lf_Na)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(3.1,3.8), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(1000),log10(2000),log10(3000),log10(5000),log10(7000)), labels=c(1000,2000,3000,5000,7000),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="forb ppm Na", line=3, cex.lab=1.6)
##bison
cm <- lmer(lf_Na ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$lf_Na[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="sienna",lwd=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$lf_Na[,1][chemi$trt=="bison"]-chemi$lf_Na[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$lf_Na[,1][chemi$trt=="bison"]+chemi$lf_Na[,2][chemi$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(lf_Na ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$lf_Na[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="gray0",lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$lf_Na[,1][chemi$trt=="cattle"]-chemi$lf_Na[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$lf_Na[,1][chemi$trt=="cattle"]+chemi$lf_Na[,2][chemi$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(lf_Na ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$lf_Na[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="dodgerblue",lwd=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$lf_Na[,1][chemi$trt=="ungrazed"]-chemi$lf_Na[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$lf_Na[,1][chemi$trt=="ungrazed"]+chemi$lf_Na[,2][chemi$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(lf_Na ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$lf_Na[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="firebrick2",lwd=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$lf_Na[,1][chemi$trt=="trtpd"]-chemi$lf_Na[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$lf_Na[,1][chemi$trt=="trtpd"]+chemi$lf_Na[,2][chemi$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(lf_Na ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$lf_Na[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="goldenrod2",lwd=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$lf_Na[,1][chemi$trt=="untrtpd"]-chemi$lf_Na[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$lf_Na[,1][chemi$trt=="untrtpd"]+chemi$lf_Na[,2][chemi$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("E"), bty="n", cex=1.5)

##
legend("topleft",legend=c("Ungrazed","Bison","Cattle","Untrt PD","Trt PD"), bty="n", pt.cex=2,cex=1.5, pch=c(23,21,22,25,24), pt.bg=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"),col=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"))
##

#############################################################
##forb Si
nut <- na.omit(est$lf_Si)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(1.4,3.2), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(50),log10(100),log10(200),log10(500),log10(1000),log10(1500)), labels=c(50,100,200,500,1000,1500),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="forb ppm Si", line=3, cex.lab=1.6)
##bison
cm <- lmer(lf_Si ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$lf_Si[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="sienna",lwd=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$lf_Si[,1][chemi$trt=="bison"]-chemi$lf_Si[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$lf_Si[,1][chemi$trt=="bison"]+chemi$lf_Si[,2][chemi$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(lf_Si ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$lf_Si[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="gray0",lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$lf_Si[,1][chemi$trt=="cattle"]-chemi$lf_Si[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$lf_Si[,1][chemi$trt=="cattle"]+chemi$lf_Si[,2][chemi$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(lf_Si ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$lf_Si[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="dodgerblue",lwd=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$lf_Si[,1][chemi$trt=="ungrazed"]-chemi$lf_Si[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$lf_Si[,1][chemi$trt=="ungrazed"]+chemi$lf_Si[,2][chemi$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(lf_Si ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
##polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$lf_Si[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(mo_e$fit ~ mo_e$month,type="l",col="firebrick2",lwd=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$lf_Si[,1][chemi$trt=="trtpd"]-chemi$lf_Si[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$lf_Si[,1][chemi$trt=="trtpd"]+chemi$lf_Si[,2][chemi$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(lf_Si ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$lf_Si[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="goldenrod2",lwd=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$lf_Si[,1][chemi$trt=="untrtpd"]-chemi$lf_Si[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$lf_Si[,1][chemi$trt=="untrtpd"]+chemi$lf_Si[,2][chemi$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("F"), bty="n", cex=1.5)
##

dev.off()
##
##################################################################
##