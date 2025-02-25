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
  ests.i <- coef(summary(lmer(g_per_m2 ~ 1 + (1|line), data = sub)))[1,1:2]
  ests.i <- data.frame(trt_site_mo = i, t(ests.i))
  ests_s <- rbind(ests_s, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests_s

ests_s[c('trt','site', 'month')] <- str_split_fixed(ests_s$trt_site_mo, ' ', 3)
colnames(ests_s)[2] ="g_per_m2_est"
colnames(ests_s)[3] ="g_per_m2_SE"
ests_s$month <-as.numeric(ests_s$month)
head(ests_s)
ests_su = subset(ests_s, select = -c(trt_site_mo))

#################ring counts
rc <- read.csv("rawData/RingCounts.csv")
head(rc)
rc$count <- (as.numeric(rc$count)*10)
#calculate estimates for each site and month
rc$site_mo <- paste(rc$site,rc$month)
ests_rc <- NULL
for(i in unique(rc$site_mo)){
  sub <- rc[rc$site_mo == i, ]
  ests.i <- coef(summary(lmer(count ~ 1 + (1|line), data = sub)))[1,1:2]
  ests.i <- data.frame(site_mo = i, t(ests.i))
  ests_rc <- rbind(ests_rc, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests_rc

ests_rc[c('site', 'month')] <- str_split_fixed(ests_rc$site_mo, ' ', 2)
colnames(ests_rc)[2] ="ghop_m2_est"
colnames(ests_rc)[3] ="ghop_m2_SE"
ests_rc$month <-as.numeric(ests_rc$month)
head(ests_rc)
ests_rc = subset(ests_rc, select = -c(site_mo))
ests_s <- merge(ests_su,ests_rc,by=c("site","month"))

#################PD poo count
pc <- read.csv("rawData/PD_PooCount.csv")
head(pc)
#calculate estimates for each site and month
pc$site_mo <- paste(pc$site,pc$mo)
pc_m <- aggregate(pc[,7], list(pc$site_mo), mean)

pc_m[c('site', 'month')] <- str_split_fixed(pc_m$Group.1, ' ', 2)
colnames(pc_m)[2] ="PD_poo"
pc_m$month <-as.numeric(pc_m$month)
head(pc_m)
pc_m = subset(pc_m, select = -c(Group.1))
ests_sp <- merge(ests_s,pc_m,by=c("site","month"))

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
ests_sc <- merge(ests_sp,df,by=c("site","month"))
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
ests_scor <- merge(chem_sc,ests_sc,by=c("site","month"),all.x=T, all.y=T)
head(ests_scor)
dim(ests_scor)
###########add individual elements
sub_g[c('site', 'month')] <- str_split_fixed(sub_g$site_mo,' ', 2)
sub_g = subset(sub_g, select = -c(site_mo))
rownames(sub_g) <- NULL
colnames(sub_g)[1:8] = c("g_C", "g_N", "g_Ca", "g_K", "g_Mg", "g_Na", "g_P", "g_Si")
sub_f[c('site', 'month')] <- str_split_fixed(sub_f$site_mo,' ', 2)
sub_f = subset(sub_f, select = -c(site_mo))
rownames(sub_f) <- NULL
colnames(sub_f)[1:8] = c("f_C", "f_N", "f_Ca", "f_K", "f_Mg", "f_Na", "f_P", "f_Si")
sub_w[c('site', 'month')] <- str_split_fixed(sub_w$site_mo,' ', 2)
sub_w = subset(sub_w, select = -c(site_mo))
rownames(sub_w) <- NULL
colnames(sub_w)[1:8] = c("w_C", "w_N", "w_Ca", "w_K", "w_Mg", "w_Na", "w_P", "w_Si")

ests_sco <- merge(sub_g,ests_scor,by=c("site","month"),all.x=T, all.y=T)
ests_sco <- merge(sub_f,ests_sco,by=c("site","month"),all.x=T, all.y=T)
ests_sco <- merge(sub_w,ests_sco,by=c("site","month"),all.x=T, all.y=T)
head(ests_sco)
dim(ests_sco)

######################################## soil chem
schem <- read.csv("rawData/SoilChem.csv")
head(schem)
schem = subset(schem, select = -c(Number,yr,month,day,Al_ppm,Co_ppm,Fe_ppm,Mn_ppm,S_ppm,Zn_ppm))
schem$site_mo <- paste(schem$site,schem$mo)
schem$month <- paste(schem$mo)
head(schem)
ests_sco <- merge(schem,ests_sco,by=c("site","month"),all.x=T, all.y=T)
schem = subset(schem, select = -c(site,mo,trt,month))
head(schem)
row.names(schem) <- schem[,8]
sc <- schem[,1:7]
head(sc)

##soil PCA
prin_comp<-prcomp(sc, scale=T)
summary(prin_comp)
prin_comp
biplot(prin_comp)
soil_PC1_posOther<- (scores(prin_comp)[,1])*-1 #axis scores for first axis ####NOTE: *-1 to make axis represent nutrient rich
soil_PC2_posCN<- (scores(prin_comp)[,2])

# correlation between original variables and principal components
corr <- cor(sc, (prin_comp$x))
round(corr, 3)
write.csv(as.matrix(corr),file="outputs/SoilChemPCA_correlations.csv")
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

##
ss1 <- as.data.frame(soil_PC1_posOther)
ss2 <- as.data.frame(soil_PC2_posCN)
ss <- cbind(rownames(ss1), data.frame(ss1, row.names=NULL), data.frame(ss2, row.names=NULL))
colnames(ss)[1] ="site_mo"
ss[c('site', 'month')] <- str_split_fixed(ss$site_mo,' ', 2)
ss = subset(ss, select = -c(site_mo))

ests_scos <- merge(ss,ests_sco,by=c("site","month"),all.x=T, all.y=T)
head(ests_scos)

ests_scos$trt <- paste(ests_scos$trt.x)
ests_scos = subset(ests_scos, select = -c(mo,trt.x))

########################################Temperature
temp <- read.csv("outputs/Temp_summary.csv")
head(temp)

all_ests <- merge(temp,ests_scos,by=c("site","month"),all.x=T, all.y=T)
head(all_ests)
dim(all_ests)
########################################stocking densities
sto <- read.csv("rawdata/stocking.csv")
head(sto)

al_ests <- merge(sto,all_ests,by=c("site"),all.x=T, all.y=T)
head(al_ests)
dim(al_ests)

write.csv(al_ests, "outputs/SiteMonthLevel.csv")

#############################