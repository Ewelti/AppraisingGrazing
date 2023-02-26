##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")

##load libraries
library(lme4)
library(stringr)
library(plotrix)
library(vegan)
library(BiodiversityR)
library("dplyr")
#################pasture meter
pm <- read.csv("rawData/PastureMeter.csv")
#calculate estimates for each site and month
pm$trt_site_mo <- paste(pm$trt,pm$site,pm$month)
ests_s <- NULL
for(i in unique(pm$trt_site_mo)){
  sub <- pm[pm$trt_site_mo == i, ]
  ests.i <- coef(summary(lmer(g_per_m2 ~ 1 + (1|line), data = sub, )))[1,1:2]
  ests.i <- data.frame(trt_site_mo = i, t(ests.i))
  ests_s <- rbind(ests_s, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests_s

ests_s[c('trt','site', 'month')] <- str_split_fixed(ests_s$trt_site_mo, ' ', 3)
colnames(ests_s)[2] ="g_per_m2_est"
colnames(ests_s)[3] ="g_per_m2_SE"
ests_s$month <-as.numeric(ests_s$month)
head(ests_s)
ests_s = subset(ests_s, select = -c(trt_site_mo))

#########clipbiomass
cb <- read.csv("rawData/ClipBiomass.csv")
head(cb)
#calculate estimates for each site and month
cb$site_mo_type <- paste(cb$site,cb$mo,cb$type)
cbsum <- aggregate(cbind(g_m2,plot_perc) ~ site_mo_type, data = cb, FUN = function(x)c(mean = mean(x),se = std.error(x)))
head(cbsum)
cbsum[c('site', 'month','type')] <- str_split_fixed(cbsum$site_mo_type, ' ', 3)

subw <- cbsum[cbsum$type=="woody",]
colnames(subw)[2] ="g_m2_woody"
colnames(subw)[3] ="perc_woody"

subf <- cbsum[cbsum$type=="forb",]
colnames(subf)[2] ="g_m2_forb"
colnames(subf)[3] ="perc_forb"

subg <- cbsum[cbsum$type=="grass",]
colnames(subg)[2] ="g_m2_grass"
colnames(subg)[3] ="perc_grass"

tt <- merge(subw,subf,by=c("site","month"))
ttt <- merge(tt,subg,by=c("site","month"))
head(ttt)
colnames(df)
df = subset(ttt, select = -c(site_mo_type.x,type.x,site_mo_type.y,type.y,site_mo_type,type))
head(df)
ests_sc <- merge(ests_s,df,by=c("site","month"))
head(ests_sc)

######################################## Plant chem
chem <- read.csv("rawData/PlantChem.csv")
head(chem)
colnames(chem)
chem = subset(chem, select = -c(Number,label,yr,month,day,V_ppm,Al_ppm,B_ppm,Cu_ppm,Fe_ppm,Li_ppm,Mn_ppm,Ni_ppm,Pb_ppm,S_ppm,Sr_ppm,Ti_ppm,Zn_ppm))
sub_w <- chem[chem$type=="woody",]
sub_w$site_mo <- paste(sub_w$site,sub_w$mo)
sub_w = subset(sub_w, select = -c(trt,site,mo,type))
row.names(sub_w) <- sub_w[,9]
wc <- sub_w[,1:8]
sub_f <- chem[chem$type=="forb",]
sub_f$site_mo <- paste(sub_f$site,sub_f$mo)
sub_f = subset(sub_f, select = -c(trt,site,mo,type))
row.names(sub_f) <- sub_f[,9]
fc <- sub_f[,1:8]
sub_g <- chem[chem$type=="grass",]
sub_g$site_mo <- paste(sub_g$site,sub_g$mo)
sub_g = subset(sub_g, select = -c(trt,site,mo,type))
row.names(sub_g) <- sub_g[,9]
gc <- sub_g[,1:8]

##woody PCA
prin_comp<-prcomp(wc, scale=T)
summary(prin_comp)
prin_comp
biplot(prin_comp)
w_sc<-scores(prin_comp)[,1:2] #axis scores for first 2 axes
# correlation between original variables and principal components
corr <- cor(wc, prin_comp$x)
round(corr, 3)
write.csv(as.matrix(corr),file="outputs/WoodyChemPCA_correlations.csv")

##forb PCA
prin_comp<-prcomp(fc, scale=T)
summary(prin_comp)
prin_comp
biplot(prin_comp)
f_sc<-scores(prin_comp)[,1] #axis scores for first axis
# correlation between original variables and principal components
corr <- cor(fc, prin_comp$x)
round(corr, 3)
write.csv(as.matrix(corr),file="outputs/ForbChemPCA_correlations.csv")

##grass PCA
prin_comp<-prcomp(gc, scale=T)
summary(prin_comp)
prin_comp
biplot(prin_comp)
g_sc<- (scores(prin_comp)[,1])*-1 #axis scores for first axis ####NOTE: *-1 to make axis represent nutrient rich
# correlation between original variables and principal components
corr <- cor(gc, (prin_comp$x*-1))
round(corr, 3)
write.csv(as.matrix(corr),file="outputs/GrassChemPCA_correlations.csv")
############

#Broken-stick model#####
(ev <- prin_comp$sdev^2)
n <- length (ev)
bsm <- data.frame(j=seq(1:n), p=0)
bsm$p[1] <- 1/n
for (i in 2:n) {
  bsm$p[i] = bsm$p[i-1] + (1/(n+1-i))
}
bsm$p <- 100*bsm$p/n
bsm
barplot(t(cbind(100*ev/sum(ev),bsm$p[n:1])), beside=TRUE, main="Broken stick model", col=c("blue",2), las=2)
legend("topright", c("% eigenvalue", "Broken stick model"), pch=15, col=c("blue",2), bty="n")

##transpose 
##forbs
ff <- as.data.frame(f_sc)
fsc <- cbind(rownames(ff), data.frame(ff, row.names=NULL))
colnames(fsc)[1] ="site_mo"
colnames(fsc)[2] ="forb_PC1"
fsc[c('site', 'month')] <- str_split_fixed(fsc$site_mo,' ', 2)
fsc = subset(fsc, select = -c(site_mo))

##grass
gg <- as.data.frame(g_sc)
gsc <- cbind(rownames(gg), data.frame(gg, row.names=NULL))
colnames(gsc)[1] ="site_mo"
colnames(gsc)[2] ="grass_PC1"
gsc[c('site', 'month')] <- str_split_fixed(gsc$site_mo,' ', 2)
gsc = subset(gsc, select = -c(site_mo))

chem_sc <- merge(gsc,fsc,by=c("site","month"),all.x=T, all.y=T)
ests_sco <- merge(chem_sc,ests_sc,by=c("site","month"),all.x=T, all.y=T)
head(ests_sc)






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


