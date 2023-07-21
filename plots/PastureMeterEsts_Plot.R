##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")

##load libraries
library(lme4)
library(stringr)
#################pasture meter
# attach data
pm <- read.csv("rawData/PastureMeter.csv")
head(pm)

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
ests$mo_jit <- (ests$month + c(-0.1,-0.2,0,0.2,0.1))

########################################
#calculate trt effects on pasture meter
mo <- lmer(g_per_m2  ~  trt + (1|line:month:site),data=pm)
summary(mo)

# extract coefficients
coefs <- data.frame(coef(summary(mo)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
######################
tiff(filename = "plots/PastureMeter.tiff", width = 6, height = 6, units = 'in', res = 600, compression = 'lzw')

par(mar=c(3,5,0.2,0.2))
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(120,280),las=1,ylab="",xlab="", xaxt='n')
axis(1, at=c(6,7,8,9),cex.axis=1.1,labels=c("June","July","August","September"))
box(lwd=2)
title(ylab=expression("Dry grams/ m"^2), line=3, cex.lab=1.6)
##bison
points(ests$g_per_m2_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(ests$g_per_m2_est[ests$trt=="bison"] ~ ests$mo_jit[ests$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(ests$mo_jit[ests$trt=="bison"], ests$g_per_m2_est[ests$trt=="bison"]-ests$g_per_m2_SE[ests$trt=="bison"], ests$mo_jit[ests$trt=="bison"], ests$g_per_m2_est[ests$trt=="bison"]+ests$g_per_m2_SE[ests$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(ests$g_per_m2_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(ests$g_per_m2_est[ests$trt=="cattle"] ~ ests$mo_jit[ests$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(ests$mo_jit[ests$trt=="cattle"], ests$g_per_m2_est[ests$trt=="cattle"]-ests$g_per_m2_SE[ests$trt=="cattle"], ests$mo_jit[ests$trt=="cattle"], ests$g_per_m2_est[ests$trt=="cattle"]+ests$g_per_m2_SE[ests$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(ests$g_per_m2_est[ests$trt=="ungrazed"] ~ ests$mo_jit[ests$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(ests$g_per_m2_est[ests$trt=="ungrazed"] ~ ests$mo_jit[ests$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(ests$mo_jit[ests$trt=="ungrazed"], ests$g_per_m2_est[ests$trt=="ungrazed"]-ests$g_per_m2_SE[ests$trt=="ungrazed"], ests$mo_jit[ests$trt=="ungrazed"], ests$g_per_m2_est[ests$trt=="ungrazed"]+ests$g_per_m2_SE[ests$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(ests$g_per_m2_est[ests$trt=="trtpd"] ~ ests$mo_jit[ests$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(ests$g_per_m2_est[ests$trt=="trtpd"] ~ ests$mo_jit[ests$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(ests$mo_jit[ests$trt=="trtpd"], ests$g_per_m2_est[ests$trt=="trtpd"]-ests$g_per_m2_SE[ests$trt=="trtpd"], ests$mo_jit[ests$trt=="trtpd"], ests$g_per_m2_est[ests$trt=="trtpd"]+ests$g_per_m2_SE[ests$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(ests$g_per_m2_est[ests$trt=="untrtpd"] ~ ests$mo_jit[ests$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(ests$g_per_m2_est[ests$trt=="untrtpd"] ~ ests$mo_jit[ests$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(ests$mo_jit[ests$trt=="untrtpd"], ests$g_per_m2_est[ests$trt=="untrtpd"]-ests$g_per_m2_SE[ests$trt=="untrtpd"], ests$mo_jit[ests$trt=="untrtpd"], ests$g_per_m2_est[ests$trt=="untrtpd"]+ests$g_per_m2_SE[ests$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright",legend=c("Ungrazed","Bison","Cattle","Untrt PD","Trt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,25,24), pt.bg=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"),col=c("dodgerblue","sienna","gray0","goldenrod2","firebrick2"))

dev.off()
##