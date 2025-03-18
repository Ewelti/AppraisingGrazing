##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")

##load libraries
library(lme4)
library(stringr)
#################pasture meter
# attach data
pm <- read.csv("rawData/PastureMeter.csv")
head(pm)
pm$rep <- (pm$month-5)

hist(pm$g_per_m2)

#calculate estimates
pm$trt_mo <- paste(pm$trt,pm$month)
ests <- NULL
for(i in unique(pm$trt_mo)){
  sub <- pm[pm$trt_mo == i, ]
  ests.i <- coef(summary(lmer(g_per_m2 ~ 1 + (1|line:site), data = sub, )))[1,1:2]
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

######################
tiff(filename = "plots/PastureMeter.tif", width = 6, height = 6, units = 'in', res = 600, compression = 'lzw')

par(mar=c(3,5,0.2,0.8))
plot(1, type="n", xlim=c(0.9,4.1), ylim=c(40,250),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(1,2,3,4),cex.axis=1.1,labels=c("June","July","August","September"))
box(lwd=2)
title(ylab=expression("Dry grams/ m"^2), line=3, cex.lab=1.6)

##bison
x1 <- pm$rep[pm$trt=="bison"]
y1 <- pm$g_per_m2[pm$trt=="bison"]
mod <- lm(y1 ~ poly(x1, 2, raw=TRUE))
summary(mod)
# predicts + interval
newx <- seq(min(x1), max(x1), length.out=720)
preds <- predict(mod, newdata = data.frame(x1=newx), interval = 'confidence')
# add fill
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col=t_col("sienna", perc = 60), border = NA)

# model
#lines(sort(x1), fitted(mod)[order(x1)], col='sienna') 
#points
points(ests$g_per_m2_est[ests$trt=="bison"] ~ ests$rep[ests$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
arrows(ests$rep[ests$trt=="bison"], ests$g_per_m2_est[ests$trt=="bison"]-ests$g_per_m2_SE[ests$trt=="bison"], ests$rep[ests$trt=="bison"], ests$g_per_m2_est[ests$trt=="bison"]+ests$g_per_m2_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
#points(ests$g_per_m2_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],type="l",col="sienna",lwd=2)

##cattle
x1 <- pm$rep[pm$trt=="cattle"]
y1 <- pm$g_per_m2[pm$trt=="cattle"]
mod <- lm(y1 ~ poly(x1, 2, raw=TRUE))
summary(mod)
# predicts + interval
newx <- seq(min(x1), max(x1), length.out=720)
preds <- predict(mod, newdata = data.frame(x1=newx), interval = 'confidence')
# add fill
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col=t_col("gray0", perc = 60), border = NA)
#points
points(ests$g_per_m2_est[ests$trt=="cattle"] ~ ests$rep[ests$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
#points(ests$g_per_m2_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(ests$rep[ests$trt=="cattle"], ests$g_per_m2_est[ests$trt=="cattle"]-ests$g_per_m2_SE[ests$trt=="cattle"], ests$rep[ests$trt=="cattle"], ests$g_per_m2_est[ests$trt=="cattle"]+ests$g_per_m2_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)

##ungrazed
x1 <- pm$rep[pm$trt=="ungrazed"]
y1 <- pm$g_per_m2[pm$trt=="ungrazed"]
mod <- lm(y1 ~ poly(x1, 2, raw=TRUE))
summary(mod)
# predicts + interval
newx <- seq(min(x1), max(x1), length.out=720)
preds <- predict(mod, newdata = data.frame(x1=newx), interval = 'confidence')
# add fill
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col=t_col("dodgerblue", perc = 60), border = NA)
points(ests$g_per_m2_est[ests$trt=="ungrazed"] ~ ests$rep[ests$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
#points(ests$g_per_m2_est[ests$trt=="ungrazed"] ~ ests$mo_jit[ests$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(ests$rep[ests$trt=="ungrazed"], ests$g_per_m2_est[ests$trt=="ungrazed"]-ests$g_per_m2_SE[ests$trt=="ungrazed"], ests$rep[ests$trt=="ungrazed"], ests$g_per_m2_est[ests$trt=="ungrazed"]+ests$g_per_m2_SE[ests$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)

##untrtpd
x1 <- pm$rep[pm$trt=="untrtpd"]
y1 <- pm$g_per_m2[pm$trt=="untrtpd"]
mod <- lm(y1 ~ poly(x1, 2, raw=TRUE))
summary(mod)
# predicts + interval
newx <- seq(min(x1), max(x1), length.out=720)
preds <- predict(mod, newdata = data.frame(x1=newx), interval = 'confidence')
# add fill
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col=t_col("goldenrod2", perc = 60), border = NA)

points(ests$g_per_m2_est[ests$trt=="untrtpd"] ~ ests$rep[ests$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
#points(ests$g_per_m2_est[ests$trt=="untrtpd"] ~ ests$mo_jit[ests$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(ests$rep[ests$trt=="untrtpd"], ests$g_per_m2_est[ests$trt=="untrtpd"]-ests$g_per_m2_SE[ests$trt=="untrtpd"], ests$rep[ests$trt=="untrtpd"], ests$g_per_m2_est[ests$trt=="untrtpd"]+ests$g_per_m2_SE[ests$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##trtpd
x1 <- pm$rep[pm$trt=="trtpd"]
y1 <- pm$g_per_m2[pm$trt=="trtpd"]
mod <- lm(y1 ~ poly(x1, 2, raw=TRUE))
summary(mod)
# predicts + interval
newx <- seq(min(x1), max(x1), length.out=720)
preds <- predict(mod, newdata = data.frame(x1=newx), interval = 'confidence')
# add fill
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col=t_col("firebrick2", perc = 60), border = NA)
points(ests$g_per_m2_est[ests$trt=="trtpd"] ~ ests$rep[ests$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
#points(ests$g_per_m2_est[ests$trt=="trtpd"] ~ ests$rep[ests$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(ests$rep[ests$trt=="trtpd"], ests$g_per_m2_est[ests$trt=="trtpd"]-ests$g_per_m2_SE[ests$trt=="trtpd"], ests$rep[ests$trt=="trtpd"], ests$g_per_m2_est[ests$trt=="trtpd"]+ests$g_per_m2_SE[ests$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)

legend("topright",legend=c("Ungrazed","Bison","Cattle","Untrt PD","Trt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,25,24), pt.bg=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"),col=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"))

##
dev.off()
##