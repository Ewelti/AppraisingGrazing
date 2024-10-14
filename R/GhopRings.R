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
head(rc)

#calculate estimates for trt
ests <- NULL
for(i in unique(rc$trt)){
  sub <- rc[rc$trt == i, ]
  ests.i <- coef(summary(lmer(count ~ 1 + (1|line:month:site), data = sub, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests

#############################################
######################################################
#####################################
##


#calculate estimates for month and trt
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
#####

write.csv(ests, "Ghop_monthTrt_summary.csv")

par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0,30),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab=expression("Grasshoppers/ m"^2), line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
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

legend("topright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##
legend("topright",legend=c("Cattle","Ungrazed"), bty="n", pt.cex=2,cex=1.3, pch=c(22,23), pt.bg=c("gray0","dodgerblue"),col=c("gray0","dodgerblue"))
##
legend("topright",legend=c("Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(24,25), pt.bg=c("firebrick2","goldenrod2"),col=c("firebrick2","goldenrod2"))
##



