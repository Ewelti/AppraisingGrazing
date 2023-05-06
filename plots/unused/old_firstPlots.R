##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")

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

