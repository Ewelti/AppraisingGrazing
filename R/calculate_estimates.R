##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing/rawData")

##load libraries
library(lme4)
library(stringr)
library(plotrix)
#################pasture meter
# attach data
pm <- read.csv("PastureMeter.csv")
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

par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(120,280),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab=expression("Dry grams/ m"^2), line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
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
##

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
subg <- chemsum[chemsum$type=="grass",]
subf <- chemsum[chemsum$type=="forb",]

subg$percC.mean

par(mar=c(2,5,0.2,0.2),mfrow=c(2,4))

plot(1, type="n", xlim=c(5.5,9.5), ylim=c(37,47),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Grass %C", line=3, cex.lab=1.6)
#title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(subg$percC.mean[subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(subg$percC.mean[subg$trt=="bison"] ~ subg$month[subg$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(subg$month[subg$trt=="bison"], subg$percC.mean[subg$trt=="bison"]-subg$percC.se[subg$trt=="bison"], subg$month[subg$trt=="bison"], subg$percC.mean[subg$trt=="bison"]+subg$percC.se[subg$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(subg$percC.mean[subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(subg$percC.mean[subg$trt=="cattle"] ~ subg$month[subg$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(subg$month[subg$trt=="cattle"], subg$percC.mean[subg$trt=="cattle"]-subg$percC.se[subg$trt=="cattle"], subg$month[subg$trt=="cattle"], subg$percC.mean[subg$trt=="cattle"]+subg$percC.se[subg$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(subg$percC.mean[subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(subg$percC.mean[subg$trt=="ungrazed"] ~ subg$month[subg$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(subg$month[subg$trt=="ungrazed"], subg$percC.mean[subg$trt=="ungrazed"]-subg$percC.se[subg$trt=="ungrazed"], subg$month[subg$trt=="ungrazed"], subg$percC.mean[subg$trt=="ungrazed"]+subg$percC.se[subg$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(subg$percC.mean[subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(subg$percC.mean[subg$trt=="trtpd"] ~ subg$month[subg$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(subg$month[subg$trt=="trtpd"], subg$percC.mean[subg$trt=="trtpd"]-subg$percC.se[subg$trt=="trtpd"], subg$month[subg$trt=="trtpd"], subg$percC.mean[subg$trt=="trtpd"]+subg$percC.se[subg$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(subg$percC.mean[subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(subg$percC.mean[subg$trt=="untrtpd"] ~ subg$month[subg$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(subg$month[subg$trt=="untrtpd"], subg$percC.mean[subg$trt=="untrtpd"]-subg$percC.se[subg$trt=="untrtpd"], subg$month[subg$trt=="untrtpd"], subg$percC.mean[subg$trt=="untrtpd"]+subg$percC.se[subg$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0.5,3),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Grass %N", line=3, cex.lab=1.6)
#title(xlab="Month", line=2.5, cex.lab=1.6)

par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0.5,4.5),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Forb %N", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(chem$forb_percN[chem$trt=="bison"] ~ chem$month[chem$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(chem$forb_percN[chem$trt=="bison"] ~ chem$month[chem$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(chem$month[chem$trt=="bison"], chem$forb_percN[chem$trt=="bison"]-chem$forb_percN_se[chem$trt=="bison"], chem$month[chem$trt=="bison"], chem$forb_percN[chem$trt=="bison"]+chem$forb_percN_se[chem$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(chem$forb_percN[chem$trt=="cattle"] ~ chem$month[chem$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(chem$forb_percN[chem$trt=="cattle"] ~ chem$month[chem$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(chem$month[chem$trt=="cattle"], chem$forb_percN[chem$trt=="cattle"]-chem$forb_percN_se[chem$trt=="cattle"], chem$month[chem$trt=="cattle"], chem$forb_percN[chem$trt=="cattle"]+chem$forb_percN_se[chem$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(chem$forb_percN[chem$trt=="ungrazed"] ~ chem$month[chem$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(chem$forb_percN[chem$trt=="ungrazed"] ~ chem$month[chem$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(chem$month[chem$trt=="ungrazed"], chem$forb_percN[chem$trt=="ungrazed"]-chem$forb_percN_se[chem$trt=="ungrazed"], chem$month[chem$trt=="ungrazed"], chem$forb_percN[chem$trt=="ungrazed"]+chem$forb_percN_se[chem$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(chem$forb_percN[chem$trt=="trtpd"] ~ chem$month[chem$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(chem$forb_percN[chem$trt=="trtpd"] ~ chem$month[chem$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(chem$month[chem$trt=="trtpd"], chem$forb_percN[chem$trt=="trtpd"]-chem$forb_percN_se[chem$trt=="trtpd"], chem$month[chem$trt=="trtpd"], chem$forb_percN[chem$trt=="trtpd"]+chem$forb_percN_se[chem$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(chem$forb_percN[chem$trt=="untrtpd"] ~ chem$month[chem$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(chem$forb_percN[chem$trt=="untrtpd"] ~ chem$month[chem$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(chem$month[chem$trt=="untrtpd"], chem$forb_percN[chem$trt=="untrtpd"]-chem$forb_percN_se[chem$trt=="untrtpd"], chem$month[chem$trt=="untrtpd"], chem$forb_percN[chem$trt=="untrtpd"]+chem$forb_percN_se[chem$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##
#################ring counts
# attach data
rc <- read.csv("RingCountSummary.csv")
head(rc)

par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.5,9.5), ylim=c(0,22),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab=expression("Grasshoppers/ m"^2), line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(rc$ringCount[rc$trt=="bison"] ~ rc$month[rc$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(rc$ringCount[rc$trt=="bison"] ~ rc$month[rc$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(rc$month[rc$trt=="bison"], rc$ringCount[rc$trt=="bison"]-rc$ringCount_se[rc$trt=="bison"], rc$month[rc$trt=="bison"], rc$ringCount[rc$trt=="bison"]+rc$ringCount_se[rc$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(rc$ringCount[rc$trt=="cattle"] ~ rc$month[rc$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(rc$ringCount[rc$trt=="cattle"] ~ rc$month[rc$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(rc$month[rc$trt=="cattle"], rc$ringCount[rc$trt=="cattle"]-rc$ringCount_se[rc$trt=="cattle"], rc$month[rc$trt=="cattle"], rc$ringCount[rc$trt=="cattle"]+rc$ringCount_se[rc$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(rc$ringCount[rc$trt=="ungrazed"] ~ rc$month[rc$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(rc$ringCount[rc$trt=="ungrazed"] ~ rc$month[rc$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(rc$month[rc$trt=="ungrazed"], rc$ringCount[rc$trt=="ungrazed"]-rc$ringCount_se[rc$trt=="ungrazed"], rc$month[rc$trt=="ungrazed"], rc$ringCount[rc$trt=="ungrazed"]+rc$ringCount_se[rc$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(rc$ringCount[rc$trt=="trtpd"] ~ rc$month[rc$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(rc$ringCount[rc$trt=="trtpd"] ~ rc$month[rc$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(rc$month[rc$trt=="trtpd"], rc$ringCount[rc$trt=="trtpd"]-rc$ringCount_se[rc$trt=="trtpd"], rc$month[rc$trt=="trtpd"], rc$ringCount[rc$trt=="trtpd"]+rc$ringCount_se[rc$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(rc$ringCount[rc$trt=="untrtpd"] ~ rc$month[rc$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(rc$ringCount[rc$trt=="untrtpd"] ~ rc$month[rc$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(rc$month[rc$trt=="untrtpd"], rc$ringCount[rc$trt=="untrtpd"]-rc$ringCount_se[rc$trt=="untrtpd"], rc$month[rc$trt=="untrtpd"], rc$ringCount[rc$trt=="untrtpd"]+rc$ringCount_se[rc$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("topright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

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

#################dung beetle body sizes
# attach data
dungb <- read.csv("DBsizeSum.csv")
head(dungb)
unique(dungb$spp)
cp <- dungb[which(dungb$spp=='Canthon_praticola'),]
onF <- dungb[which(dungb$spp=='Onthophagus_nuchicornis_female'),]
onM <- dungb[which(dungb$spp=='Onthophagus_nuchicornis_male'),]
##
## Canthon_praticola body
par(mar=c(4,5,0.2,0.2), mfrow=c(2,2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(12,17),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Body length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(cp$body_mm[cp$trt=="bison"] ~ cp$month[cp$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(cp$body_mm[cp$trt=="bison"] ~ cp$month[cp$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(cp$month[cp$trt=="bison"], cp$body_mm[cp$trt=="bison"]-cp$body_mm_se[cp$trt=="bison"], cp$month[cp$trt=="bison"], cp$body_mm[cp$trt=="bison"]+cp$body_mm_se[cp$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(cp$body_mm[cp$trt=="cattle"] ~ cp$month[cp$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(cp$body_mm[cp$trt=="cattle"] ~ cp$month[cp$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(cp$month[cp$trt=="cattle"], cp$body_mm[cp$trt=="cattle"]-cp$body_mm_se[cp$trt=="cattle"], cp$month[cp$trt=="cattle"], cp$body_mm[cp$trt=="cattle"]+cp$body_mm_se[cp$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(cp$body_mm[cp$trt=="ungrazed"] ~ cp$month[cp$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(cp$body_mm[cp$trt=="ungrazed"] ~ cp$month[cp$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(cp$month[cp$trt=="ungrazed"], cp$body_mm[cp$trt=="ungrazed"]-cp$body_mm_se[cp$trt=="ungrazed"], cp$month[cp$trt=="ungrazed"], cp$body_mm[cp$trt=="ungrazed"]+cp$body_mm_se[cp$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(cp$body_mm[cp$trt=="trtpd"] ~ cp$month[cp$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(cp$body_mm[cp$trt=="trtpd"] ~ cp$month[cp$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(cp$month[cp$trt=="trtpd"], cp$body_mm[cp$trt=="trtpd"]-cp$body_mm_se[cp$trt=="trtpd"], cp$month[cp$trt=="trtpd"], cp$body_mm[cp$trt=="trtpd"]+cp$body_mm_se[cp$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(cp$body_mm[cp$trt=="untrtpd"] ~ cp$month[cp$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(cp$body_mm[cp$trt=="untrtpd"] ~ cp$month[cp$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(cp$month[cp$trt=="untrtpd"], cp$body_mm[cp$trt=="untrtpd"]-cp$body_mm_se[cp$trt=="untrtpd"], cp$month[cp$trt=="untrtpd"], cp$body_mm[cp$trt=="untrtpd"]+cp$body_mm_se[cp$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

## Onthophagus nuchicornis female body
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(7.3,8.3),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Female body length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(onF$body_mm[onF$trt=="bison"] ~ onF$month[onF$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(onF$body_mm[onF$trt=="bison"] ~ onF$month[onF$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(onF$month[onF$trt=="bison"], onF$body_mm[onF$trt=="bison"]-onF$body_mm_se[onF$trt=="bison"], onF$month[onF$trt=="bison"], onF$body_mm[onF$trt=="bison"]+onF$body_mm_se[onF$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(onF$body_mm[onF$trt=="cattle"] ~ onF$month[onF$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(onF$body_mm[onF$trt=="cattle"] ~ onF$month[onF$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(onF$month[onF$trt=="cattle"], onF$body_mm[onF$trt=="cattle"]-onF$body_mm_se[onF$trt=="cattle"], onF$month[onF$trt=="cattle"], onF$body_mm[onF$trt=="cattle"]+onF$body_mm_se[onF$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(onF$body_mm[onF$trt=="ungrazed"] ~ onF$month[onF$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(onF$body_mm[onF$trt=="ungrazed"] ~ onF$month[onF$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(onF$month[onF$trt=="ungrazed"], onF$body_mm[onF$trt=="ungrazed"]-onF$body_mm_se[onF$trt=="ungrazed"], onF$month[onF$trt=="ungrazed"], onF$body_mm[onF$trt=="ungrazed"]+onF$body_mm_se[onF$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(onF$body_mm[onF$trt=="trtpd"] ~ onF$month[onF$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(onF$body_mm[onF$trt=="trtpd"] ~ onF$month[onF$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(onF$month[onF$trt=="trtpd"], onF$body_mm[onF$trt=="trtpd"]-onF$body_mm_se[onF$trt=="trtpd"], onF$month[onF$trt=="trtpd"], onF$body_mm[onF$trt=="trtpd"]+onF$body_mm_se[onF$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(onF$body_mm[onF$trt=="untrtpd"] ~ onF$month[onF$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(onF$body_mm[onF$trt=="untrtpd"] ~ onF$month[onF$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(onF$month[onF$trt=="untrtpd"], onF$body_mm[onF$trt=="untrtpd"]-onF$body_mm_se[onF$trt=="untrtpd"], onF$month[onF$trt=="untrtpd"], onF$body_mm[onF$trt=="untrtpd"]+onF$body_mm_se[onF$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

#legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

## Onthophagus nuchicornis male body
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(7.6,8.8),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Male body length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(onM$body_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(onM$body_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(onM$month[onM$trt=="bison"], onM$body_mm[onM$trt=="bison"]-onM$body_mm_se[onM$trt=="bison"], onM$month[onM$trt=="bison"], onM$body_mm[onM$trt=="bison"]+onM$body_mm_se[onM$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(onM$body_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(onM$body_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(onM$month[onM$trt=="cattle"], onM$body_mm[onM$trt=="cattle"]-onM$body_mm_se[onM$trt=="cattle"], onM$month[onM$trt=="cattle"], onM$body_mm[onM$trt=="cattle"]+onM$body_mm_se[onM$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(onM$body_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(onM$body_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(onM$month[onM$trt=="ungrazed"], onM$body_mm[onM$trt=="ungrazed"]-onM$body_mm_se[onM$trt=="ungrazed"], onM$month[onM$trt=="ungrazed"], onM$body_mm[onM$trt=="ungrazed"]+onM$body_mm_se[onM$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(onM$body_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(onM$body_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(onM$month[onM$trt=="trtpd"], onM$body_mm[onM$trt=="trtpd"]-onM$body_mm_se[onM$trt=="trtpd"], onM$month[onM$trt=="trtpd"], onM$body_mm[onM$trt=="trtpd"]+onM$body_mm_se[onM$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(onM$body_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(onM$body_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(onM$month[onM$trt=="untrtpd"], onM$body_mm[onM$trt=="untrtpd"]-onM$body_mm_se[onM$trt=="untrtpd"], onM$month[onM$trt=="untrtpd"], onM$body_mm[onM$trt=="untrtpd"]+onM$body_mm_se[onM$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

#legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

## Onthophagus nuchicornis male horn
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(0.8,1.3),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Horn length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(onM$horn_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(onM$horn_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(onM$month[onM$trt=="bison"], onM$horn_mm[onM$trt=="bison"]-onM$horn_mm_se[onM$trt=="bison"], onM$month[onM$trt=="bison"], onM$horn_mm[onM$trt=="bison"]+onM$horn_mm_se[onM$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(onM$horn_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(onM$horn_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(onM$month[onM$trt=="cattle"], onM$horn_mm[onM$trt=="cattle"]-onM$horn_mm_se[onM$trt=="cattle"], onM$month[onM$trt=="cattle"], onM$horn_mm[onM$trt=="cattle"]+onM$horn_mm_se[onM$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(onM$horn_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(onM$horn_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(onM$month[onM$trt=="ungrazed"], onM$horn_mm[onM$trt=="ungrazed"]-onM$horn_mm_se[onM$trt=="ungrazed"], onM$month[onM$trt=="ungrazed"], onM$horn_mm[onM$trt=="ungrazed"]+onM$horn_mm_se[onM$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(onM$horn_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(onM$horn_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(onM$month[onM$trt=="trtpd"], onM$horn_mm[onM$trt=="trtpd"]-onM$horn_mm_se[onM$trt=="trtpd"], onM$month[onM$trt=="trtpd"], onM$horn_mm[onM$trt=="trtpd"]+onM$horn_mm_se[onM$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(onM$horn_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(onM$horn_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(onM$month[onM$trt=="untrtpd"], onM$horn_mm[onM$trt=="untrtpd"]-onM$horn_mm_se[onM$trt=="untrtpd"], onM$month[onM$trt=="untrtpd"], onM$horn_mm[onM$trt=="untrtpd"]+onM$horn_mm_se[onM$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

#legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##


## Canthon_praticola wing
par(mar=c(4,5,0.2,0.2), mfrow=c(2,2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(5.3,7.6),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Canthon praticola wing length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(cp$wing_mm[cp$trt=="bison"] ~ cp$month[cp$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(cp$wing_mm[cp$trt=="bison"] ~ cp$month[cp$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(cp$month[cp$trt=="bison"], cp$wing_mm[cp$trt=="bison"]-cp$wing_mm_se[cp$trt=="bison"], cp$month[cp$trt=="bison"], cp$wing_mm[cp$trt=="bison"]+cp$wing_mm_se[cp$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(cp$wing_mm[cp$trt=="cattle"] ~ cp$month[cp$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(cp$wing_mm[cp$trt=="cattle"] ~ cp$month[cp$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(cp$month[cp$trt=="cattle"], cp$wing_mm[cp$trt=="cattle"]-cp$wing_mm_se[cp$trt=="cattle"], cp$month[cp$trt=="cattle"], cp$wing_mm[cp$trt=="cattle"]+cp$wing_mm_se[cp$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(cp$wing_mm[cp$trt=="ungrazed"] ~ cp$month[cp$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(cp$wing_mm[cp$trt=="ungrazed"] ~ cp$month[cp$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(cp$month[cp$trt=="ungrazed"], cp$wing_mm[cp$trt=="ungrazed"]-cp$wing_mm_se[cp$trt=="ungrazed"], cp$month[cp$trt=="ungrazed"], cp$wing_mm[cp$trt=="ungrazed"]+cp$wing_mm_se[cp$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(cp$wing_mm[cp$trt=="trtpd"] ~ cp$month[cp$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(cp$wing_mm[cp$trt=="trtpd"] ~ cp$month[cp$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(cp$month[cp$trt=="trtpd"], cp$wing_mm[cp$trt=="trtpd"]-cp$wing_mm_se[cp$trt=="trtpd"], cp$month[cp$trt=="trtpd"], cp$wing_mm[cp$trt=="trtpd"]+cp$wing_mm_se[cp$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(cp$wing_mm[cp$trt=="untrtpd"] ~ cp$month[cp$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(cp$wing_mm[cp$trt=="untrtpd"] ~ cp$month[cp$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(cp$month[cp$trt=="untrtpd"], cp$wing_mm[cp$trt=="untrtpd"]-cp$wing_mm_se[cp$trt=="untrtpd"], cp$month[cp$trt=="untrtpd"], cp$wing_mm[cp$trt=="untrtpd"]+cp$wing_mm_se[cp$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

## Onthophagus nuchicornis female wing
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(2.9,3.3),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Onthophagus nuchicornis female wing length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(onF$wing_mm[onF$trt=="bison"] ~ onF$month[onF$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(onF$wing_mm[onF$trt=="bison"] ~ onF$month[onF$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(onF$month[onF$trt=="bison"], onF$wing_mm[onF$trt=="bison"]-onF$wing_mm_se[onF$trt=="bison"], onF$month[onF$trt=="bison"], onF$wing_mm[onF$trt=="bison"]+onF$wing_mm_se[onF$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(onF$wing_mm[onF$trt=="cattle"] ~ onF$month[onF$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(onF$wing_mm[onF$trt=="cattle"] ~ onF$month[onF$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(onF$month[onF$trt=="cattle"], onF$wing_mm[onF$trt=="cattle"]-onF$wing_mm_se[onF$trt=="cattle"], onF$month[onF$trt=="cattle"], onF$wing_mm[onF$trt=="cattle"]+onF$wing_mm_se[onF$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(onF$wing_mm[onF$trt=="ungrazed"] ~ onF$month[onF$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(onF$wing_mm[onF$trt=="ungrazed"] ~ onF$month[onF$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(onF$month[onF$trt=="ungrazed"], onF$wing_mm[onF$trt=="ungrazed"]-onF$wing_mm_se[onF$trt=="ungrazed"], onF$month[onF$trt=="ungrazed"], onF$wing_mm[onF$trt=="ungrazed"]+onF$wing_mm_se[onF$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(onF$wing_mm[onF$trt=="trtpd"] ~ onF$month[onF$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(onF$wing_mm[onF$trt=="trtpd"] ~ onF$month[onF$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(onF$month[onF$trt=="trtpd"], onF$wing_mm[onF$trt=="trtpd"]-onF$wing_mm_se[onF$trt=="trtpd"], onF$month[onF$trt=="trtpd"], onF$wing_mm[onF$trt=="trtpd"]+onF$wing_mm_se[onF$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(onF$wing_mm[onF$trt=="untrtpd"] ~ onF$month[onF$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(onF$wing_mm[onF$trt=="untrtpd"] ~ onF$month[onF$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(onF$month[onF$trt=="untrtpd"], onF$wing_mm[onF$trt=="untrtpd"]-onF$wing_mm_se[onF$trt=="untrtpd"], onF$month[onF$trt=="untrtpd"], onF$wing_mm[onF$trt=="untrtpd"]+onF$wing_mm_se[onF$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

#legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

## Onthophagus nuchicornis male wing
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(3,3.5),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Onthophagus nuchicornis male wing length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(onM$wing_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(onM$wing_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(onM$month[onM$trt=="bison"], onM$wing_mm[onM$trt=="bison"]-onM$wing_mm_se[onM$trt=="bison"], onM$month[onM$trt=="bison"], onM$wing_mm[onM$trt=="bison"]+onM$wing_mm_se[onM$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(onM$wing_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(onM$wing_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(onM$month[onM$trt=="cattle"], onM$wing_mm[onM$trt=="cattle"]-onM$wing_mm_se[onM$trt=="cattle"], onM$month[onM$trt=="cattle"], onM$wing_mm[onM$trt=="cattle"]+onM$wing_mm_se[onM$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(onM$wing_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(onM$wing_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(onM$month[onM$trt=="ungrazed"], onM$wing_mm[onM$trt=="ungrazed"]-onM$wing_mm_se[onM$trt=="ungrazed"], onM$month[onM$trt=="ungrazed"], onM$wing_mm[onM$trt=="ungrazed"]+onM$wing_mm_se[onM$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(onM$wing_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(onM$wing_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(onM$month[onM$trt=="trtpd"], onM$wing_mm[onM$trt=="trtpd"]-onM$wing_mm_se[onM$trt=="trtpd"], onM$month[onM$trt=="trtpd"], onM$wing_mm[onM$trt=="trtpd"]+onM$wing_mm_se[onM$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(onM$wing_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(onM$wing_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(onM$month[onM$trt=="untrtpd"], onM$wing_mm[onM$trt=="untrtpd"]-onM$wing_mm_se[onM$trt=="untrtpd"], onM$month[onM$trt=="untrtpd"], onM$wing_mm[onM$trt=="untrtpd"]+onM$wing_mm_se[onM$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

#legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##





## Canthon_praticola thorax
par(mar=c(4,5,0.2,0.2), mfrow=c(2,2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(min(cp$thorax_mm),max(cp$thorax_mm)),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Canthon praticola thorax length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(cp$thorax_mm[cp$trt=="bison"] ~ cp$month[cp$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(cp$thorax_mm[cp$trt=="bison"] ~ cp$month[cp$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(cp$month[cp$trt=="bison"], cp$thorax_mm[cp$trt=="bison"]-cp$thorax_mm_se[cp$trt=="bison"], cp$month[cp$trt=="bison"], cp$thorax_mm[cp$trt=="bison"]+cp$thorax_mm_se[cp$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(cp$thorax_mm[cp$trt=="cattle"] ~ cp$month[cp$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(cp$thorax_mm[cp$trt=="cattle"] ~ cp$month[cp$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(cp$month[cp$trt=="cattle"], cp$thorax_mm[cp$trt=="cattle"]-cp$thorax_mm_se[cp$trt=="cattle"], cp$month[cp$trt=="cattle"], cp$thorax_mm[cp$trt=="cattle"]+cp$thorax_mm_se[cp$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(cp$thorax_mm[cp$trt=="ungrazed"] ~ cp$month[cp$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(cp$thorax_mm[cp$trt=="ungrazed"] ~ cp$month[cp$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(cp$month[cp$trt=="ungrazed"], cp$thorax_mm[cp$trt=="ungrazed"]-cp$thorax_mm_se[cp$trt=="ungrazed"], cp$month[cp$trt=="ungrazed"], cp$thorax_mm[cp$trt=="ungrazed"]+cp$thorax_mm_se[cp$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(cp$thorax_mm[cp$trt=="trtpd"] ~ cp$month[cp$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(cp$thorax_mm[cp$trt=="trtpd"] ~ cp$month[cp$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(cp$month[cp$trt=="trtpd"], cp$thorax_mm[cp$trt=="trtpd"]-cp$thorax_mm_se[cp$trt=="trtpd"], cp$month[cp$trt=="trtpd"], cp$thorax_mm[cp$trt=="trtpd"]+cp$thorax_mm_se[cp$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(cp$thorax_mm[cp$trt=="untrtpd"] ~ cp$month[cp$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(cp$thorax_mm[cp$trt=="untrtpd"] ~ cp$month[cp$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(cp$month[cp$trt=="untrtpd"], cp$thorax_mm[cp$trt=="untrtpd"]-cp$thorax_mm_se[cp$trt=="untrtpd"], cp$month[cp$trt=="untrtpd"], cp$thorax_mm[cp$trt=="untrtpd"]+cp$thorax_mm_se[cp$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

## Onthophagus nuchicornis female thorax
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(2.75,3.2),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Onthophagus nuchicornis female thorax length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(onF$thorax_mm[onF$trt=="bison"] ~ onF$month[onF$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(onF$thorax_mm[onF$trt=="bison"] ~ onF$month[onF$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(onF$month[onF$trt=="bison"], onF$thorax_mm[onF$trt=="bison"]-onF$thorax_mm_se[onF$trt=="bison"], onF$month[onF$trt=="bison"], onF$thorax_mm[onF$trt=="bison"]+onF$thorax_mm_se[onF$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(onF$thorax_mm[onF$trt=="cattle"] ~ onF$month[onF$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(onF$thorax_mm[onF$trt=="cattle"] ~ onF$month[onF$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(onF$month[onF$trt=="cattle"], onF$thorax_mm[onF$trt=="cattle"]-onF$thorax_mm_se[onF$trt=="cattle"], onF$month[onF$trt=="cattle"], onF$thorax_mm[onF$trt=="cattle"]+onF$thorax_mm_se[onF$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(onF$thorax_mm[onF$trt=="ungrazed"] ~ onF$month[onF$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(onF$thorax_mm[onF$trt=="ungrazed"] ~ onF$month[onF$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(onF$month[onF$trt=="ungrazed"], onF$thorax_mm[onF$trt=="ungrazed"]-onF$thorax_mm_se[onF$trt=="ungrazed"], onF$month[onF$trt=="ungrazed"], onF$thorax_mm[onF$trt=="ungrazed"]+onF$thorax_mm_se[onF$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(onF$thorax_mm[onF$trt=="trtpd"] ~ onF$month[onF$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(onF$thorax_mm[onF$trt=="trtpd"] ~ onF$month[onF$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(onF$month[onF$trt=="trtpd"], onF$thorax_mm[onF$trt=="trtpd"]-onF$thorax_mm_se[onF$trt=="trtpd"], onF$month[onF$trt=="trtpd"], onF$thorax_mm[onF$trt=="trtpd"]+onF$thorax_mm_se[onF$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(onF$thorax_mm[onF$trt=="untrtpd"] ~ onF$month[onF$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(onF$thorax_mm[onF$trt=="untrtpd"] ~ onF$month[onF$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(onF$month[onF$trt=="untrtpd"], onF$thorax_mm[onF$trt=="untrtpd"]-onF$thorax_mm_se[onF$trt=="untrtpd"], onF$month[onF$trt=="untrtpd"], onF$thorax_mm[onF$trt=="untrtpd"]+onF$thorax_mm_se[onF$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

#legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

## Onthophagus nuchicornis male thorax
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(2.7,3.3),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Onthophagus nuchicornis male thorax length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(onM$thorax_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(onM$thorax_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(onM$month[onM$trt=="bison"], onM$thorax_mm[onM$trt=="bison"]-onM$thorax_mm_se[onM$trt=="bison"], onM$month[onM$trt=="bison"], onM$thorax_mm[onM$trt=="bison"]+onM$thorax_mm_se[onM$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(onM$thorax_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(onM$thorax_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(onM$month[onM$trt=="cattle"], onM$thorax_mm[onM$trt=="cattle"]-onM$thorax_mm_se[onM$trt=="cattle"], onM$month[onM$trt=="cattle"], onM$thorax_mm[onM$trt=="cattle"]+onM$thorax_mm_se[onM$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(onM$thorax_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(onM$thorax_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(onM$month[onM$trt=="ungrazed"], onM$thorax_mm[onM$trt=="ungrazed"]-onM$thorax_mm_se[onM$trt=="ungrazed"], onM$month[onM$trt=="ungrazed"], onM$thorax_mm[onM$trt=="ungrazed"]+onM$thorax_mm_se[onM$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(onM$thorax_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(onM$thorax_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(onM$month[onM$trt=="trtpd"], onM$thorax_mm[onM$trt=="trtpd"]-onM$thorax_mm_se[onM$trt=="trtpd"], onM$month[onM$trt=="trtpd"], onM$thorax_mm[onM$trt=="trtpd"]+onM$thorax_mm_se[onM$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(onM$thorax_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(onM$thorax_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(onM$month[onM$trt=="untrtpd"], onM$thorax_mm[onM$trt=="untrtpd"]-onM$thorax_mm_se[onM$trt=="untrtpd"], onM$month[onM$trt=="untrtpd"], onM$thorax_mm[onM$trt=="untrtpd"]+onM$thorax_mm_se[onM$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

#legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

## Canthon_praticola head
par(mar=c(4,5,0.2,0.2), mfrow=c(2,2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(2.8,4.2),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Canthon praticola head length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(cp$head_mm[cp$trt=="bison"] ~ cp$month[cp$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(cp$head_mm[cp$trt=="bison"] ~ cp$month[cp$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(cp$month[cp$trt=="bison"], cp$head_mm[cp$trt=="bison"]-cp$head_mm_se[cp$trt=="bison"], cp$month[cp$trt=="bison"], cp$head_mm[cp$trt=="bison"]+cp$head_mm_se[cp$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(cp$head_mm[cp$trt=="cattle"] ~ cp$month[cp$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(cp$head_mm[cp$trt=="cattle"] ~ cp$month[cp$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(cp$month[cp$trt=="cattle"], cp$head_mm[cp$trt=="cattle"]-cp$head_mm_se[cp$trt=="cattle"], cp$month[cp$trt=="cattle"], cp$head_mm[cp$trt=="cattle"]+cp$head_mm_se[cp$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(cp$head_mm[cp$trt=="ungrazed"] ~ cp$month[cp$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(cp$head_mm[cp$trt=="ungrazed"] ~ cp$month[cp$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(cp$month[cp$trt=="ungrazed"], cp$head_mm[cp$trt=="ungrazed"]-cp$head_mm_se[cp$trt=="ungrazed"], cp$month[cp$trt=="ungrazed"], cp$head_mm[cp$trt=="ungrazed"]+cp$head_mm_se[cp$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(cp$head_mm[cp$trt=="trtpd"] ~ cp$month[cp$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(cp$head_mm[cp$trt=="trtpd"] ~ cp$month[cp$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(cp$month[cp$trt=="trtpd"], cp$head_mm[cp$trt=="trtpd"]-cp$head_mm_se[cp$trt=="trtpd"], cp$month[cp$trt=="trtpd"], cp$head_mm[cp$trt=="trtpd"]+cp$head_mm_se[cp$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(cp$head_mm[cp$trt=="untrtpd"] ~ cp$month[cp$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(cp$head_mm[cp$trt=="untrtpd"] ~ cp$month[cp$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(cp$month[cp$trt=="untrtpd"], cp$head_mm[cp$trt=="untrtpd"]-cp$head_mm_se[cp$trt=="untrtpd"], cp$month[cp$trt=="untrtpd"], cp$head_mm[cp$trt=="untrtpd"]+cp$head_mm_se[cp$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

## Onthophagus nuchicornis female head
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(1.5,2),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Onthophagus nuchicornis female head length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(onF$head_mm[onF$trt=="bison"] ~ onF$month[onF$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(onF$head_mm[onF$trt=="bison"] ~ onF$month[onF$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(onF$month[onF$trt=="bison"], onF$head_mm[onF$trt=="bison"]-onF$head_mm_se[onF$trt=="bison"], onF$month[onF$trt=="bison"], onF$head_mm[onF$trt=="bison"]+onF$head_mm_se[onF$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(onF$head_mm[onF$trt=="cattle"] ~ onF$month[onF$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(onF$head_mm[onF$trt=="cattle"] ~ onF$month[onF$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(onF$month[onF$trt=="cattle"], onF$head_mm[onF$trt=="cattle"]-onF$head_mm_se[onF$trt=="cattle"], onF$month[onF$trt=="cattle"], onF$head_mm[onF$trt=="cattle"]+onF$head_mm_se[onF$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(onF$head_mm[onF$trt=="ungrazed"] ~ onF$month[onF$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(onF$head_mm[onF$trt=="ungrazed"] ~ onF$month[onF$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(onF$month[onF$trt=="ungrazed"], onF$head_mm[onF$trt=="ungrazed"]-onF$head_mm_se[onF$trt=="ungrazed"], onF$month[onF$trt=="ungrazed"], onF$head_mm[onF$trt=="ungrazed"]+onF$head_mm_se[onF$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(onF$head_mm[onF$trt=="trtpd"] ~ onF$month[onF$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(onF$head_mm[onF$trt=="trtpd"] ~ onF$month[onF$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(onF$month[onF$trt=="trtpd"], onF$head_mm[onF$trt=="trtpd"]-onF$head_mm_se[onF$trt=="trtpd"], onF$month[onF$trt=="trtpd"], onF$head_mm[onF$trt=="trtpd"]+onF$head_mm_se[onF$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(onF$head_mm[onF$trt=="untrtpd"] ~ onF$month[onF$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(onF$head_mm[onF$trt=="untrtpd"] ~ onF$month[onF$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(onF$month[onF$trt=="untrtpd"], onF$head_mm[onF$trt=="untrtpd"]-onF$head_mm_se[onF$trt=="untrtpd"], onF$month[onF$trt=="untrtpd"], onF$head_mm[onF$trt=="untrtpd"]+onF$head_mm_se[onF$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

#legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

## Onthophagus nuchicornis male head
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(1.8,2.1),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Onthophagus nuchicornis male head length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(onM$head_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(onM$head_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(onM$month[onM$trt=="bison"], onM$head_mm[onM$trt=="bison"]-onM$head_mm_se[onM$trt=="bison"], onM$month[onM$trt=="bison"], onM$head_mm[onM$trt=="bison"]+onM$head_mm_se[onM$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(onM$head_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(onM$head_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(onM$month[onM$trt=="cattle"], onM$head_mm[onM$trt=="cattle"]-onM$head_mm_se[onM$trt=="cattle"], onM$month[onM$trt=="cattle"], onM$head_mm[onM$trt=="cattle"]+onM$head_mm_se[onM$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(onM$head_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(onM$head_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(onM$month[onM$trt=="ungrazed"], onM$head_mm[onM$trt=="ungrazed"]-onM$head_mm_se[onM$trt=="ungrazed"], onM$month[onM$trt=="ungrazed"], onM$head_mm[onM$trt=="ungrazed"]+onM$head_mm_se[onM$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(onM$head_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(onM$head_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(onM$month[onM$trt=="trtpd"], onM$head_mm[onM$trt=="trtpd"]-onM$head_mm_se[onM$trt=="trtpd"], onM$month[onM$trt=="trtpd"], onM$head_mm[onM$trt=="trtpd"]+onM$head_mm_se[onM$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(onM$head_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(onM$head_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(onM$month[onM$trt=="untrtpd"], onM$head_mm[onM$trt=="untrtpd"]-onM$head_mm_se[onM$trt=="untrtpd"], onM$month[onM$trt=="untrtpd"], onM$head_mm[onM$trt=="untrtpd"]+onM$head_mm_se[onM$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

#legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##
## Onthophagus nuchicornis male horn
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(0.79,1.25),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Onthophagus nuchicornis male horn length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(onM$horn_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(onM$horn_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(onM$month[onM$trt=="bison"], onM$horn_mm[onM$trt=="bison"]-onM$horn_mm_se[onM$trt=="bison"], onM$month[onM$trt=="bison"], onM$horn_mm[onM$trt=="bison"]+onM$horn_mm_se[onM$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(onM$horn_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(onM$horn_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(onM$month[onM$trt=="cattle"], onM$horn_mm[onM$trt=="cattle"]-onM$horn_mm_se[onM$trt=="cattle"], onM$month[onM$trt=="cattle"], onM$horn_mm[onM$trt=="cattle"]+onM$horn_mm_se[onM$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(onM$horn_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(onM$horn_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(onM$month[onM$trt=="ungrazed"], onM$horn_mm[onM$trt=="ungrazed"]-onM$horn_mm_se[onM$trt=="ungrazed"], onM$month[onM$trt=="ungrazed"], onM$horn_mm[onM$trt=="ungrazed"]+onM$horn_mm_se[onM$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(onM$horn_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(onM$horn_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(onM$month[onM$trt=="trtpd"], onM$horn_mm[onM$trt=="trtpd"]-onM$horn_mm_se[onM$trt=="trtpd"], onM$month[onM$trt=="trtpd"], onM$horn_mm[onM$trt=="trtpd"]+onM$horn_mm_se[onM$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(onM$horn_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(onM$horn_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(onM$month[onM$trt=="untrtpd"], onM$horn_mm[onM$trt=="untrtpd"]-onM$horn_mm_se[onM$trt=="untrtpd"], onM$month[onM$trt=="untrtpd"], onM$horn_mm[onM$trt=="untrtpd"]+onM$horn_mm_se[onM$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

##




## Canthon_praticola forearm
par(mar=c(4,5,0.2,0.2), mfrow=c(2,2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(3,4.4),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Canthon praticola forearm length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(cp$forearm_mm[cp$trt=="bison"] ~ cp$month[cp$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(cp$forearm_mm[cp$trt=="bison"] ~ cp$month[cp$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(cp$month[cp$trt=="bison"], cp$forearm_mm[cp$trt=="bison"]-cp$forearm_mm_se[cp$trt=="bison"], cp$month[cp$trt=="bison"], cp$forearm_mm[cp$trt=="bison"]+cp$forearm_mm_se[cp$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(cp$forearm_mm[cp$trt=="cattle"] ~ cp$month[cp$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(cp$forearm_mm[cp$trt=="cattle"] ~ cp$month[cp$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(cp$month[cp$trt=="cattle"], cp$forearm_mm[cp$trt=="cattle"]-cp$forearm_mm_se[cp$trt=="cattle"], cp$month[cp$trt=="cattle"], cp$forearm_mm[cp$trt=="cattle"]+cp$forearm_mm_se[cp$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(cp$forearm_mm[cp$trt=="ungrazed"] ~ cp$month[cp$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(cp$forearm_mm[cp$trt=="ungrazed"] ~ cp$month[cp$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(cp$month[cp$trt=="ungrazed"], cp$forearm_mm[cp$trt=="ungrazed"]-cp$forearm_mm_se[cp$trt=="ungrazed"], cp$month[cp$trt=="ungrazed"], cp$forearm_mm[cp$trt=="ungrazed"]+cp$forearm_mm_se[cp$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(cp$forearm_mm[cp$trt=="trtpd"] ~ cp$month[cp$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(cp$forearm_mm[cp$trt=="trtpd"] ~ cp$month[cp$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(cp$month[cp$trt=="trtpd"], cp$forearm_mm[cp$trt=="trtpd"]-cp$forearm_mm_se[cp$trt=="trtpd"], cp$month[cp$trt=="trtpd"], cp$forearm_mm[cp$trt=="trtpd"]+cp$forearm_mm_se[cp$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(cp$forearm_mm[cp$trt=="untrtpd"] ~ cp$month[cp$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(cp$forearm_mm[cp$trt=="untrtpd"] ~ cp$month[cp$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(cp$month[cp$trt=="untrtpd"], cp$forearm_mm[cp$trt=="untrtpd"]-cp$forearm_mm_se[cp$trt=="untrtpd"], cp$month[cp$trt=="untrtpd"], cp$forearm_mm[cp$trt=="untrtpd"]+cp$forearm_mm_se[cp$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

## Onthophagus nuchicornis female forearm
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(1.7,2.1),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Onthophagus nuchicornis female forearm length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(onF$forearm_mm[onF$trt=="bison"] ~ onF$month[onF$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(onF$forearm_mm[onF$trt=="bison"] ~ onF$month[onF$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(onF$month[onF$trt=="bison"], onF$forearm_mm[onF$trt=="bison"]-onF$forearm_mm_se[onF$trt=="bison"], onF$month[onF$trt=="bison"], onF$forearm_mm[onF$trt=="bison"]+onF$forearm_mm_se[onF$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(onF$forearm_mm[onF$trt=="cattle"] ~ onF$month[onF$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(onF$forearm_mm[onF$trt=="cattle"] ~ onF$month[onF$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(onF$month[onF$trt=="cattle"], onF$forearm_mm[onF$trt=="cattle"]-onF$forearm_mm_se[onF$trt=="cattle"], onF$month[onF$trt=="cattle"], onF$forearm_mm[onF$trt=="cattle"]+onF$forearm_mm_se[onF$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(onF$forearm_mm[onF$trt=="ungrazed"] ~ onF$month[onF$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(onF$forearm_mm[onF$trt=="ungrazed"] ~ onF$month[onF$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(onF$month[onF$trt=="ungrazed"], onF$forearm_mm[onF$trt=="ungrazed"]-onF$forearm_mm_se[onF$trt=="ungrazed"], onF$month[onF$trt=="ungrazed"], onF$forearm_mm[onF$trt=="ungrazed"]+onF$forearm_mm_se[onF$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(onF$forearm_mm[onF$trt=="trtpd"] ~ onF$month[onF$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(onF$forearm_mm[onF$trt=="trtpd"] ~ onF$month[onF$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(onF$month[onF$trt=="trtpd"], onF$forearm_mm[onF$trt=="trtpd"]-onF$forearm_mm_se[onF$trt=="trtpd"], onF$month[onF$trt=="trtpd"], onF$forearm_mm[onF$trt=="trtpd"]+onF$forearm_mm_se[onF$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(onF$forearm_mm[onF$trt=="untrtpd"] ~ onF$month[onF$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(onF$forearm_mm[onF$trt=="untrtpd"] ~ onF$month[onF$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(onF$month[onF$trt=="untrtpd"], onF$forearm_mm[onF$trt=="untrtpd"]-onF$forearm_mm_se[onF$trt=="untrtpd"], onF$month[onF$trt=="untrtpd"], onF$forearm_mm[onF$trt=="untrtpd"]+onF$forearm_mm_se[onF$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

#legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

## Onthophagus nuchicornis male forearm
par(mar=c(4,5,0.2,0.2))
plot(1, type="n", xlim=c(5.8,8.3), ylim=c(1.7,2.15),las=1,ylab="",xlab="")
box(lwd=2)
title(ylab="Onthophagus nuchicornis male forearm length (mm)", line=3, cex.lab=1.6)
title(xlab="Month", line=2.5, cex.lab=1.6)
##bison
points(onM$forearm_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],pch=21,col="sienna",bg="sienna",cex=2)
points(onM$forearm_mm[onM$trt=="bison"] ~ onM$month[onM$trt=="bison"],type="l",col="sienna",lwd=2)
arrows(onM$month[onM$trt=="bison"], onM$forearm_mm[onM$trt=="bison"]-onM$forearm_mm_se[onM$trt=="bison"], onM$month[onM$trt=="bison"], onM$forearm_mm[onM$trt=="bison"]+onM$forearm_mm_se[onM$trt=="bison"],col="sienna",lwd=2,length=0.05, angle=90, code=3)
##cattle
points(onM$forearm_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],pch=22,col="gray0",bg="gray0",cex=2)
points(onM$forearm_mm[onM$trt=="cattle"] ~ onM$month[onM$trt=="cattle"],type="l",col="gray0",lwd=2)
arrows(onM$month[onM$trt=="cattle"], onM$forearm_mm[onM$trt=="cattle"]-onM$forearm_mm_se[onM$trt=="cattle"], onM$month[onM$trt=="cattle"], onM$forearm_mm[onM$trt=="cattle"]+onM$forearm_mm_se[onM$trt=="cattle"],col="gray0",lwd=2,length=0.05, angle=90, code=3)
##ungrazed
points(onM$forearm_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],pch=23,col="dodgerblue",bg="dodgerblue",cex=2)
points(onM$forearm_mm[onM$trt=="ungrazed"] ~ onM$month[onM$trt=="ungrazed"],type="l",col="dodgerblue",lwd=2)
arrows(onM$month[onM$trt=="ungrazed"], onM$forearm_mm[onM$trt=="ungrazed"]-onM$forearm_mm_se[onM$trt=="ungrazed"], onM$month[onM$trt=="ungrazed"], onM$forearm_mm[onM$trt=="ungrazed"]+onM$forearm_mm_se[onM$trt=="ungrazed"],col="dodgerblue",lwd=2,length=0.05, angle=90, code=3)
##trtpd
points(onM$forearm_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],pch=24,col="firebrick2",bg="firebrick2",cex=2)
points(onM$forearm_mm[onM$trt=="trtpd"] ~ onM$month[onM$trt=="trtpd"],type="l",col="firebrick2",lwd=2)
arrows(onM$month[onM$trt=="trtpd"], onM$forearm_mm[onM$trt=="trtpd"]-onM$forearm_mm_se[onM$trt=="trtpd"], onM$month[onM$trt=="trtpd"], onM$forearm_mm[onM$trt=="trtpd"]+onM$forearm_mm_se[onM$trt=="trtpd"],col="firebrick2",lwd=2,length=0.05, angle=90, code=3)
##untrtpd
points(onM$forearm_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],pch=25,col="goldenrod2",bg="goldenrod2",cex=2)
points(onM$forearm_mm[onM$trt=="untrtpd"] ~ onM$month[onM$trt=="untrtpd"],type="l",col="goldenrod2",lwd=2)
arrows(onM$month[onM$trt=="untrtpd"], onM$forearm_mm[onM$trt=="untrtpd"]-onM$forearm_mm_se[onM$trt=="untrtpd"], onM$month[onM$trt=="untrtpd"], onM$forearm_mm[onM$trt=="untrtpd"]+onM$forearm_mm_se[onM$trt=="untrtpd"],col="goldenrod2",lwd=2,length=0.05, angle=90, code=3)

#legend("bottomright",legend=c("Ungrazed","Bison","Cattle","Trt PD","Untrt PD"), bty="n", pt.cex=2,cex=1.3, pch=c(23,21,22,24,25), pt.bg=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"),col=c("dodgerblue","sienna","gray0","firebrick2","goldenrod2"))
##

##
