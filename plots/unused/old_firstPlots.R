##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")

##load libraries
library(lme4)
library(stringr)
#################ring counts
# attach data
rc <- read.csv("rawdata/RingCounts.csv")
head(rc)

rc$line <- as.factor(rc$line)
rc$count <- as.numeric(rc$count)*10
rc <- na.omit(rc)

#calculate estimates
rc$trt_mo <- paste(rc$trt,rc$month)
ests <- NULL
for(i in unique(rc$trt_mo)){
  sub <- rc[rc$trt_mo == i, ]
  ests.i <- coef(summary(lmer(count ~ 1 + (1|line:site), data = sub, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

ests[c('trt', 'month')] <- str_split_fixed(ests$trt_mo, ' ', 2)
colnames(ests)[2] ="ghop_per_m2_est"
colnames(ests)[3] ="ghop_per_m2_SE"
ests$month <-as.numeric(ests$month)
ests$mo_jit <- (ests$month + c(-0.1,-0.2,0,0.2,0.1))

########################################
tiff(filename = "plots/PastureMeter.tiff", width = 6, height = 6, units = 'in', res = 600, compression = 'lzw')

par(mar=c(3,5,0.2,0.2))
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0,28),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1.1,labels=c("June","July","August","September"))
box(lwd=2)
title(ylab=expression("Grasshoppers/ m"^2), line=3, cex.lab=1.6)
##bison
points(ests$ghop_per_m2_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(ests$ghop_per_m2_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$ghop_per_m2_est[ests$trt=="bison"]-ests$ghop_per_m2_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$ghop_per_m2_est[ests$trt=="bison"]+ests$ghop_per_m2_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(ests$ghop_per_m2_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(ests$ghop_per_m2_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$ghop_per_m2_est[ests$trt=="cattle"]-ests$ghop_per_m2_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$ghop_per_m2_est[ests$trt=="cattle"]+ests$ghop_per_m2_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(ests$ghop_per_m2_est[ests$trt=="ungrazed"] ~ ests$mo_jit[ests$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(ests$ghop_per_m2_est[ests$trt=="ungrazed"] ~ ests$mo_jit[ests$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(ests$mo_jit[ests$trt=="ungrazed"], ests$ghop_per_m2_est[ests$trt=="ungrazed"]-ests$ghop_per_m2_SE[ests$trt=="ungrazed"], ests$mo_jit[ests$trt=="ungrazed"], ests$ghop_per_m2_est[ests$trt=="ungrazed"]+ests$ghop_per_m2_SE[ests$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(ests$ghop_per_m2_est[ests$trt=="trtpd"] ~ ests$mo_jit[ests$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(ests$ghop_per_m2_est[ests$trt=="trtpd"] ~ ests$mo_jit[ests$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(ests$mo_jit[ests$trt=="trtpd"], ests$ghop_per_m2_est[ests$trt=="trtpd"]-ests$ghop_per_m2_SE[ests$trt=="trtpd"], ests$mo_jit[ests$trt=="trtpd"], ests$ghop_per_m2_est[ests$trt=="trtpd"]+ests$ghop_per_m2_SE[ests$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(ests$ghop_per_m2_est[ests$trt=="untrtpd"] ~ ests$mo_jit[ests$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(ests$ghop_per_m2_est[ests$trt=="untrtpd"] ~ ests$mo_jit[ests$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(ests$mo_jit[ests$trt=="untrtpd"], ests$ghop_per_m2_est[ests$trt=="untrtpd"]-ests$ghop_per_m2_SE[ests$trt=="untrtpd"], ests$mo_jit[ests$trt=="untrtpd"], ests$ghop_per_m2_est[ests$trt=="untrtpd"]+ests$ghop_per_m2_SE[ests$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright",legend=c("Ungrazed","Bison","Cattle","Untrt PD","Trt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,25,24), pt.bg=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"),col=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"))

dev.off()
#################dung counts
# attach data
dung <- read.csv("DungSummary.csv")
head(dung)

par(mar=c(2,5,0.2,0.2),mfrow=c(3,1))
## bison/cattle
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0,1.8),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab=expression("Bison or cattle dung/m"^2), line=3, cex.lab=1.6)
#title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(dung$patty1x1[dung$trt=="bison"] ~ dung$month[dung$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(dung$patty1x1[dung$trt=="bison"] ~ dung$month[dung$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(dung$month[dung$trt=="bison"], dung$patty1x1[dung$trt=="bison"]-dung$patty1x1_se[dung$trt=="bison"], dung$month[dung$trt=="bison"], dung$patty1x1[dung$trt=="bison"]+dung$patty1x1_se[dung$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(dung$patty1x1[dung$trt=="cattle"] ~ dung$month[dung$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(dung$patty1x1[dung$trt=="cattle"] ~ dung$month[dung$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(dung$month[dung$trt=="cattle"], dung$patty1x1[dung$trt=="cattle"]-dung$patty1x1_se[dung$trt=="cattle"], dung$month[dung$trt=="cattle"], dung$patty1x1[dung$trt=="cattle"]+dung$patty1x1_se[dung$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(dung$patty1x1[dung$trt=="ungrazed"] ~ dung$month[dung$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(dung$patty1x1[dung$trt=="ungrazed"] ~ dung$month[dung$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(dung$month[dung$trt=="ungrazed"], dung$patty1x1[dung$trt=="ungrazed"]-dung$patty1x1_se[dung$trt=="ungrazed"], dung$month[dung$trt=="ungrazed"], dung$patty1x1[dung$trt=="ungrazed"]+dung$patty1x1_se[dung$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(dung$patty1x1[dung$trt=="trtpd"] ~ dung$month[dung$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(dung$patty1x1[dung$trt=="trtpd"] ~ dung$month[dung$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(dung$month[dung$trt=="trtpd"], dung$patty1x1[dung$trt=="trtpd"]-dung$patty1x1_se[dung$trt=="trtpd"], dung$month[dung$trt=="trtpd"], dung$patty1x1[dung$trt=="trtpd"]+dung$patty1x1_se[dung$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(dung$patty1x1[dung$trt=="untrtpd"] ~ dung$month[dung$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(dung$patty1x1[dung$trt=="untrtpd"] ~ dung$month[dung$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(dung$month[dung$trt=="untrtpd"], dung$patty1x1[dung$trt=="untrtpd"]-dung$patty1x1_se[dung$trt=="untrtpd"], dung$month[dung$trt=="untrtpd"], dung$patty1x1[dung$trt=="untrtpd"]+dung$patty1x1_se[dung$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##


## browser
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0,0.4),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab=expression("Browser dung piles/m"^2), line=3, cex.lab=1.6)
#title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(dung$browser1x1[dung$trt=="bison"] ~ dung$month[dung$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(dung$browser1x1[dung$trt=="bison"] ~ dung$month[dung$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(dung$month[dung$trt=="bison"], dung$browser1x1[dung$trt=="bison"]-dung$browser1x1_se[dung$trt=="bison"], dung$month[dung$trt=="bison"], dung$browser1x1[dung$trt=="bison"]+dung$browser1x1_se[dung$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(dung$browser1x1[dung$trt=="cattle"] ~ dung$month[dung$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(dung$browser1x1[dung$trt=="cattle"] ~ dung$month[dung$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(dung$month[dung$trt=="cattle"], dung$browser1x1[dung$trt=="cattle"]-dung$browser1x1_se[dung$trt=="cattle"], dung$month[dung$trt=="cattle"], dung$browser1x1[dung$trt=="cattle"]+dung$browser1x1_se[dung$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(dung$browser1x1[dung$trt=="ungrazed"] ~ dung$month[dung$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(dung$browser1x1[dung$trt=="ungrazed"] ~ dung$month[dung$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(dung$month[dung$trt=="ungrazed"], dung$browser1x1[dung$trt=="ungrazed"]-dung$browser1x1_se[dung$trt=="ungrazed"], dung$month[dung$trt=="ungrazed"], dung$browser1x1[dung$trt=="ungrazed"]+dung$browser1x1_se[dung$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(dung$browser1x1[dung$trt=="trtpd"] ~ dung$month[dung$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(dung$browser1x1[dung$trt=="trtpd"] ~ dung$month[dung$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(dung$month[dung$trt=="trtpd"], dung$browser1x1[dung$trt=="trtpd"]-dung$browser1x1_se[dung$trt=="trtpd"], dung$month[dung$trt=="trtpd"], dung$browser1x1[dung$trt=="trtpd"]+dung$browser1x1_se[dung$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(dung$browser1x1[dung$trt=="untrtpd"] ~ dung$month[dung$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(dung$browser1x1[dung$trt=="untrtpd"] ~ dung$month[dung$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(dung$month[dung$trt=="untrtpd"], dung$browser1x1[dung$trt=="untrtpd"]-dung$browser1x1_se[dung$trt=="untrtpd"], dung$month[dung$trt=="untrtpd"], dung$browser1x1[dung$trt=="untrtpd"]+dung$browser1x1_se[dung$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

#legend("topright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

par(mar=c(4,5,0.2,0.2))
##PD poo
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0,80),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab=expression("PD dung /m"^2), line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##trtpd
points(dung$PD1x1[dung$trt=="trtpd"] ~ dung$month[dung$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(dung$PD1x1[dung$trt=="trtpd"] ~ dung$month[dung$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(dung$month[dung$trt=="trtpd"], dung$PD1x1[dung$trt=="trtpd"]-dung$PD1x1_se[dung$trt=="trtpd"], dung$month[dung$trt=="trtpd"], dung$PD1x1[dung$trt=="trtpd"]+dung$PD1x1_se[dung$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(dung$PD1x1[dung$trt=="untrtpd"] ~ dung$month[dung$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(dung$PD1x1[dung$trt=="untrtpd"] ~ dung$month[dung$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(dung$month[dung$trt=="untrtpd"], dung$PD1x1[dung$trt=="untrtpd"]-dung$PD1x1_se[dung$trt=="untrtpd"], dung$month[dung$trt=="untrtpd"], dung$PD1x1[dung$trt=="untrtpd"]+dung$PD1x1_se[dung$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)
##

