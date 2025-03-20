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

#log scale grass chem
est$lg_N <- log10(est$g_N)
est$lg_P <- log10(est$g_P)
est$lg_K <- log10(est$g_K)
est$lg_Mg <- log10(est$g_Mg)
est$lg_Na <- log10(est$g_Na)
est$lg_Si <- log10(est$g_Si)
est["lg_Si"][est["lg_Si"] == "-Inf"] <- 0

#means by trt
est$trt_mo <- paste(est$trt,est$month)
chemsum <- aggregate(cbind(lg_N,lg_P,lg_K,lg_Mg,lg_Na,lg_Si) ~ trt_mo, data = est, FUN = function(x)c(mean = mean(x),se = std.error(x)))
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
tiff(filename = "plots/GrassChem_individualPlots.tif", width = 9, height = 5, units = 'in', res = 600, compression = 'lzw')

par(mar=c(2.5,5,0.2,0.2),mfrow=c(2,3))

##grass N
nut <- na.omit(est$lg_N)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(-0.2,0.48), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(0.5),log10(1),log10(1.5),log10(2),log10(2.5),log10(3)), labels=c(0.5,1,1.5,2,2.5,3),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass %N", line=3, cex.lab=1.6)
##bison
cm <- lmer(lg_N ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$lg_N[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="sienna",lwd=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$lg_N[,1][chemi$trt=="bison"]-chemi$lg_N[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$lg_N[,1][chemi$trt=="bison"]+chemi$lg_N[,2][chemi$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(lg_N ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$lg_N[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="gray0",lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$lg_N[,1][chemi$trt=="cattle"]-chemi$lg_N[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$lg_N[,1][chemi$trt=="cattle"]+chemi$lg_N[,2][chemi$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(lg_N ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$lg_N[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="dodgerblue",lwd=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$lg_N[,1][chemi$trt=="ungrazed"]-chemi$lg_N[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$lg_N[,1][chemi$trt=="ungrazed"]+chemi$lg_N[,2][chemi$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(lg_N ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
##polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$lg_N[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(mo_e$fit ~ mo_e$month,type="l",col="firebrick2",lwd=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$lg_N[,1][chemi$trt=="trtpd"]-chemi$lg_N[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$lg_N[,1][chemi$trt=="trtpd"]+chemi$lg_N[,2][chemi$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(lg_N ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$lg_N[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="goldenrod2",lwd=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$lg_N[,1][chemi$trt=="untrtpd"]-chemi$lg_N[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$lg_N[,1][chemi$trt=="untrtpd"]+chemi$lg_N[,2][chemi$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("A"), bty="n", cex=1.5)

#####################################################
##grass P
nut <- na.omit(est$lg_P)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(2.7,3.38), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(500),log10(1000),log10(1500),log10(2000),log10(2500)), labels=c(500,1000,1500,2000,2500),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass ppm P", line=3, cex.lab=1.6)

##bison
cm <- lmer(lg_P ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$lg_P[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="sienna",lwd=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$lg_P[,1][chemi$trt=="bison"]-chemi$lg_P[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$lg_P[,1][chemi$trt=="bison"]+chemi$lg_P[,2][chemi$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(lg_P ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$lg_P[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="gray0",lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$lg_P[,1][chemi$trt=="cattle"]-chemi$lg_P[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$lg_P[,1][chemi$trt=="cattle"]+chemi$lg_P[,2][chemi$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(lg_P ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$lg_P[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="dodgerblue",lwd=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$lg_P[,1][chemi$trt=="ungrazed"]-chemi$lg_P[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$lg_P[,1][chemi$trt=="ungrazed"]+chemi$lg_P[,2][chemi$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(lg_P ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
##polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$lg_P[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(mo_e$fit ~ mo_e$month,type="l",col="firebrick2",lwd=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$lg_P[,1][chemi$trt=="trtpd"]-chemi$lg_P[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$lg_P[,1][chemi$trt=="trtpd"]+chemi$lg_P[,2][chemi$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(lg_P ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$lg_P[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="goldenrod2",lwd=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$lg_P[,1][chemi$trt=="untrtpd"]-chemi$lg_P[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$lg_P[,1][chemi$trt=="untrtpd"]+chemi$lg_P[,2][chemi$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("B"), bty="n", cex=1.5)

############################################################################
##grass K
nut <- na.omit(est$lg_K)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(3.6,4.4), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(5000),log10(10000),log10(15000),log10(20000),log10(25000)), labels=c(5000,10000,15000,20000,25000),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass ppm K", line=3, cex.lab=1.6)

##bison
cm <- lmer(lg_K ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$lg_K[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="sienna",lwd=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$lg_K[,1][chemi$trt=="bison"]-chemi$lg_K[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$lg_K[,1][chemi$trt=="bison"]+chemi$lg_K[,2][chemi$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(lg_K ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$lg_K[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="gray0",lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$lg_K[,1][chemi$trt=="cattle"]-chemi$lg_K[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$lg_K[,1][chemi$trt=="cattle"]+chemi$lg_K[,2][chemi$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(lg_K ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$lg_K[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="dodgerblue",lwd=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$lg_K[,1][chemi$trt=="ungrazed"]-chemi$lg_K[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$lg_K[,1][chemi$trt=="ungrazed"]+chemi$lg_K[,2][chemi$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(lg_K ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
##polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$lg_K[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(mo_e$fit ~ mo_e$month,type="l",col="firebrick2",lwd=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$lg_K[,1][chemi$trt=="trtpd"]-chemi$lg_K[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$lg_K[,1][chemi$trt=="trtpd"]+chemi$lg_K[,2][chemi$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(lg_K ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$lg_K[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="goldenrod2",lwd=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$lg_K[,1][chemi$trt=="untrtpd"]-chemi$lg_K[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$lg_K[,1][chemi$trt=="untrtpd"]+chemi$lg_K[,2][chemi$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("C"), bty="n", cex=1.5)

#######################################################

##grass Mg
nut <- na.omit(est$lg_Mg)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(2.7,3.5), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(500),log10(1000),log10(1500),log10(2000),log10(2500)), labels=c(500,1000,1500,2000,2500),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass ppm Mg", line=3, cex.lab=1.6)

##bison
cm <- lmer(lg_Mg ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$lg_Mg[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="sienna",lwd=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$lg_Mg[,1][chemi$trt=="bison"]-chemi$lg_Mg[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$lg_Mg[,1][chemi$trt=="bison"]+chemi$lg_Mg[,2][chemi$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(lg_Mg ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$lg_Mg[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="gray0",lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$lg_Mg[,1][chemi$trt=="cattle"]-chemi$lg_Mg[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$lg_Mg[,1][chemi$trt=="cattle"]+chemi$lg_Mg[,2][chemi$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(lg_Mg ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$lg_Mg[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="dodgerblue",lwd=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$lg_Mg[,1][chemi$trt=="ungrazed"]-chemi$lg_Mg[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$lg_Mg[,1][chemi$trt=="ungrazed"]+chemi$lg_Mg[,2][chemi$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(lg_Mg ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
##polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$lg_Mg[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(mo_e$fit ~ mo_e$month,type="l",col="firebrick2",lwd=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$lg_Mg[,1][chemi$trt=="trtpd"]-chemi$lg_Mg[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$lg_Mg[,1][chemi$trt=="trtpd"]+chemi$lg_Mg[,2][chemi$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(lg_Mg ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$lg_Mg[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="goldenrod2",lwd=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$lg_Mg[,1][chemi$trt=="untrtpd"]-chemi$lg_Mg[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$lg_Mg[,1][chemi$trt=="untrtpd"]+chemi$lg_Mg[,2][chemi$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("D"), bty="n", cex=1.5)

##########################################################################
##grass Na
nut <- na.omit(est$lg_Na)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(3.08,3.34), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(1200),log10(1400),log10(1600),log10(1800),log10(2000)), labels=c(1200,1400,1600,1800,2000),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass ppm Na", line=3, cex.lab=1.6)
##bison
cm <- lmer(lg_Na ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$lg_Na[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="sienna",lwd=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$lg_Na[,1][chemi$trt=="bison"]-chemi$lg_Na[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$lg_Na[,1][chemi$trt=="bison"]+chemi$lg_Na[,2][chemi$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(lg_Na ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$lg_Na[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="gray0",lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$lg_Na[,1][chemi$trt=="cattle"]-chemi$lg_Na[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$lg_Na[,1][chemi$trt=="cattle"]+chemi$lg_Na[,2][chemi$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(lg_Na ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$lg_Na[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="dodgerblue",lwd=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$lg_Na[,1][chemi$trt=="ungrazed"]-chemi$lg_Na[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$lg_Na[,1][chemi$trt=="ungrazed"]+chemi$lg_Na[,2][chemi$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(lg_Na ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
##polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$lg_Na[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(mo_e$fit ~ mo_e$month,type="l",col="firebrick2",lwd=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$lg_Na[,1][chemi$trt=="trtpd"]-chemi$lg_Na[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$lg_Na[,1][chemi$trt=="trtpd"]+chemi$lg_Na[,2][chemi$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(lg_Na ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$lg_Na[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="goldenrod2",lwd=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$lg_Na[,1][chemi$trt=="untrtpd"]-chemi$lg_Na[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$lg_Na[,1][chemi$trt=="untrtpd"]+chemi$lg_Na[,2][chemi$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("E"), bty="n", cex=1.5)

##
legend("topleft",legend=c("Ungrazed","Bison","Cattle","Untrt PD","Trt PD"), bty="n", pt.cex=2,cex=1.5, pch=c(23,21,22,25,24), pt.bg=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"),col=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"))
##

#############################################################
##grass Si
nut <- na.omit(est$lg_Si)
min(nut)
max(nut)
plot(1, type="n", xlim=c(5.9,9.1), ylim=c(1.5,3.5), ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(50),log10(100),log10(200),log10(500),log10(1000),log10(2000)), labels=c(50,100,200,500,1000,2000),las=1)
axis(1, at=c(6,7,8,9),cex.axis=1,labels=c("Jun","Jul","Aug","Sept"))
box(lwd=2)
title(ylab="Grass ppm Na", line=3, cex.lab=1.6)
##bison
cm <- lmer(lg_Si ~ month + (1|site), data=b)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(chemi$lg_Si[,1][chemi$trt=="bison"] ~ chemi$month[chemi$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="sienna",lwd=2)
arrows(chemi$month[chemi$trt=="bison"], chemi$lg_Si[,1][chemi$trt=="bison"]-chemi$lg_Si[,2][chemi$trt=="bison"], chemi$month[chemi$trt=="bison"], chemi$lg_Si[,1][chemi$trt=="bison"]+chemi$lg_Si[,2][chemi$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)

##cattle
cm <- lmer(lg_Si ~ month + (1|site), data=ca)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(chemi$lg_Si[,1][chemi$trt=="cattle"] ~ chemi$month[chemi$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="gray0",lwd=2)
arrows(chemi$month[chemi$trt=="cattle"], chemi$lg_Si[,1][chemi$trt=="cattle"]-chemi$lg_Si[,2][chemi$trt=="cattle"], chemi$month[chemi$trt=="cattle"], chemi$lg_Si[,1][chemi$trt=="cattle"]+chemi$lg_Si[,2][chemi$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
cm <- lmer(lg_Si ~ month + (1|site), data=un)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(chemi$lg_Si[,1][chemi$trt=="ungrazed"] ~ chemi$month[chemi$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="dodgerblue",lwd=2)
arrows(chemi$month[chemi$trt=="ungrazed"], chemi$lg_Si[,1][chemi$trt=="ungrazed"]-chemi$lg_Si[,2][chemi$trt=="ungrazed"], chemi$month[chemi$trt=="ungrazed"], chemi$lg_Si[,1][chemi$trt=="ungrazed"]+chemi$lg_Si[,2][chemi$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##trtpd
cm <- lmer(lg_Si ~ month + (1|site), data=tp)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
##polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(chemi$lg_Si[,1][chemi$trt=="trtpd"] ~ chemi$month[chemi$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(mo_e$fit ~ mo_e$month,type="l",col="firebrick2",lwd=2)
arrows(chemi$month[chemi$trt=="trtpd"], chemi$lg_Si[,1][chemi$trt=="trtpd"]-chemi$lg_Si[,2][chemi$trt=="trtpd"], chemi$month[chemi$trt=="trtpd"], chemi$lg_Si[,1][chemi$trt=="trtpd"]+chemi$lg_Si[,2][chemi$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
cm <- lmer(lg_Si ~ month + (1|site), data=pd)
mo_e <- Effect("month", partial.residuals=T, cm)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$month), mo_e$month), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(chemi$lg_Si[,1][chemi$trt=="untrtpd"] ~ chemi$month[chemi$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(mo_e$fit ~ mo_e$month,type="l",col="goldenrod2",lwd=2)
arrows(chemi$month[chemi$trt=="untrtpd"], chemi$lg_Si[,1][chemi$trt=="untrtpd"]-chemi$lg_Si[,2][chemi$trt=="untrtpd"], chemi$month[chemi$trt=="untrtpd"], chemi$lg_Si[,1][chemi$trt=="untrtpd"]+chemi$lg_Si[,2][chemi$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright", legend=("F"), bty="n", cex=1.5)
##

dev.off()
##
##################################################################
##

