##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")

##load libraries
library(lme4)
library(stringr)
library(effects)

#################pasture meter
# attach data
pm <- read.csv("rawData/PastureMeter.csv")
pm$trt <- as.factor(pm$trt)
pm$line <- as.factor(pm$line)
pm$site <- as.factor(pm$site)
head(pm)
pm$rep <- (pm$month-5)

#calculate estimates
pm$l_g_per_m2 <- log10(pm$g_per_m2+1)

pm$trt_mo <- paste(pm$trt,pm$month)
ests <- NULL
for(i in unique(pm$trt_mo)){
  sub <- pm[pm$trt_mo == i, ]
  ests.i <- coef(summary(lmer(l_g_per_m2 ~ 1 + (1|line:site), data = sub, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

ests[c('trt', 'month')] <- str_split_fixed(ests$trt_mo, ' ', 2)
colnames(ests)[2] ="g_per_m2_est"
colnames(ests)[3] ="g_per_m2_SE"
ests$month <-as.numeric(ests$month)
ests$rep <-as.numeric(ests$month-5)
ests$mo_jit <- (ests$rep + c(-0.1,-0.2,0,0.2,0.1))

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

#subset by trt
b <- pm[pm$trt=="bison",]
ca <- pm[pm$trt=="cattle",]
un <- pm[pm$trt=="ungrazed",]
pd <- pm[pm$trt=="untrtpd",]
tp <- pm[pm$trt=="trtpd",]

######################
tiff(filename = "plots/PastureMeter.tif", width = 6, height = 6, units = 'in', res = 600, compression = 'lzw')

par(mar=c(3,4.5,0.2,0.8))
plot(1, type="n", xlim=c(0.9,4.1), ylim=c(1.27,2.25),ylab="",xlab="", xaxt='n', yaxt='n')
axis(2, at=c(log10(17+1),log10(20+1),log10(25+1),log10(30+1),log10(40+1),log10(50+1),log10(60+1),log10(80+1),log10(100+1),log10(120+1),log10(140+1),log10(180+1)), labels=c(17,20,25,30,40,50,60,80,100,120,140,180),las=1)
axis(1, at=c(1,2,3,4),cex.axis=1.1,labels=c("June","July","August","September"))
box(lwd=2)
title(ylab=expression("Dry grams/ m"^2), line=2.2, cex.lab=1.6)

##ungrazed
mo_pmod <- lmer(log10(g_per_m2 +1) ~ poly(rep,2) + (1|line:site), data = un)
mo_e <- Effect("rep", partial.residuals=T, mo_pmod)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$rep), mo_e$rep), c(rev(mo_e$upper), mo_e$lower), col=t_col("dodgerblue", perc = 70), border = NA)
#points
points(ests$g_per_m2_est[ests$trt=="ungrazed"] ~ ests$rep[ests$trt=="ungrazed"],pch=23,col=t_col("dodgerblue", perc = 40),bg=t_col("dodgerblue", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$rep,type="l",col=t_col("dodgerblue", perc = 40),lwd=2)
arrows(ests$rep[ests$trt=="ungrazed"], ests$g_per_m2_est[ests$trt=="ungrazed"]-ests$g_per_m2_SE[ests$trt=="ungrazed"], ests$rep[ests$trt=="ungrazed"], ests$g_per_m2_est[ests$trt=="ungrazed"]+ests$g_per_m2_SE[ests$trt=="ungrazed"],col=t_col("dodgerblue", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##bison
mo_pmod <- lmer(log10(g_per_m2 +1) ~ poly(rep,2) + (1|line:site), data = b)
mo_e <- Effect("rep", partial.residuals=T, mo_pmod)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$rep), mo_e$rep), c(rev(mo_e$upper), mo_e$lower), col=t_col("sienna", perc = 70), border = NA)
#points
points(ests$g_per_m2_est[ests$trt=="bison"] ~ ests$rep[ests$trt=="bison"],pch=21,col=t_col("sienna", perc = 40),bg=t_col("sienna", perc = 40),cex=2)
arrows(ests$rep[ests$trt=="bison"], ests$g_per_m2_est[ests$trt=="bison"]-ests$g_per_m2_SE[ests$trt=="bison"], ests$rep[ests$trt=="bison"], ests$g_per_m2_est[ests$trt=="bison"]+ests$g_per_m2_SE[ests$trt=="bison"],col=t_col("sienna", perc = 40),lwd=2,length=0.05, angle=90, code=3)
points(mo_e$fit ~ mo_e$rep,type="l",col=t_col("sienna", perc = 40),lwd=2)

##cattle
mo_pmod <- lmer(log10(g_per_m2 +1) ~ poly(rep,2) + (1|line:site), data = ca)
mo_e <- Effect("rep", partial.residuals=T, mo_pmod)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$rep), mo_e$rep), c(rev(mo_e$upper), mo_e$lower), col=t_col("gray0", perc = 70), border = NA)
#points
points(ests$g_per_m2_est[ests$trt=="cattle"] ~ ests$rep[ests$trt=="cattle"],pch=22,col=t_col("gray0", perc = 40),bg=t_col("gray0", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$rep,type="l",col=t_col("gray0", perc = 40),lwd=2)
arrows(ests$rep[ests$trt=="cattle"], ests$g_per_m2_est[ests$trt=="cattle"]-ests$g_per_m2_SE[ests$trt=="cattle"], ests$rep[ests$trt=="cattle"], ests$g_per_m2_est[ests$trt=="cattle"]+ests$g_per_m2_SE[ests$trt=="cattle"],col=t_col("gray0", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##untrtpd
mo_pmod <- lmer(log10(g_per_m2 +1) ~ poly(rep,2) + (1|line:site), data = pd)
mo_e <- Effect("rep", partial.residuals=T, mo_pmod)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$rep), mo_e$rep), c(rev(mo_e$upper), mo_e$lower), col=t_col("goldenrod2", perc = 70), border = NA)
#points
points(ests$g_per_m2_est[ests$trt=="untrtpd"] ~ ests$rep[ests$trt=="untrtpd"],pch=25,col=t_col("goldenrod2", perc = 40),bg=t_col("goldenrod2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$rep,type="l",col=t_col("goldenrod2", perc = 40),lwd=2)
arrows(ests$rep[ests$trt=="untrtpd"], ests$g_per_m2_est[ests$trt=="untrtpd"]-ests$g_per_m2_SE[ests$trt=="untrtpd"], ests$rep[ests$trt=="untrtpd"], ests$g_per_m2_est[ests$trt=="untrtpd"]+ests$g_per_m2_SE[ests$trt=="untrtpd"],col=t_col("goldenrod2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

##trtpd
mo_pmod <- lmer(log10(g_per_m2 +1) ~ poly(rep,2) + (1|line:site), data = tp)
mo_e <- Effect("rep", partial.residuals=T, mo_pmod)
mo_e <- data.frame(mo_e)
# add fill
#polygon(c(rev(mo_e$rep), mo_e$rep), c(rev(mo_e$upper), mo_e$lower), col=t_col("firebrick2", perc = 70), border = NA)
#points
points(ests$g_per_m2_est[ests$trt=="trtpd"] ~ ests$rep[ests$trt=="trtpd"],pch=24,col=t_col("firebrick2", perc = 40),bg=t_col("firebrick2", perc = 40),cex=2)
points(mo_e$fit ~ mo_e$rep,type="l",col=t_col("firebrick2", perc = 40),lwd=2)
arrows(ests$rep[ests$trt=="trtpd"], ests$g_per_m2_est[ests$trt=="trtpd"]-ests$g_per_m2_SE[ests$trt=="trtpd"], ests$rep[ests$trt=="trtpd"], ests$g_per_m2_est[ests$trt=="trtpd"]+ests$g_per_m2_SE[ests$trt=="trtpd"],col=t_col("firebrick2", perc = 40),lwd=2,length=0.05, angle=90, code=3)

legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Untrt PD","Trt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,25,24), pt.bg=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"),col=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"))

##
dev.off()
##