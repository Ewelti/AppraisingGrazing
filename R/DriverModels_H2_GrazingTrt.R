##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")
##Set working directory (Julie's computer)
setwd("/Users/julierebh/Downloads/AppraisingGrazing")

#########Hypotheses:
##H1) Grazing will increase plant quality with smaller herbivores having larger effects 
##H2) All grazers will decrease ANPP, and insect herbivory will equal that of large grazer herbivory
##H3) Plant nutrient content will be greatest in the early season while biomass will peak mid-season

##load libraries
#install.packages("stringr")
library(lme4)
library(stringr)
############################H2
##all data
pm <- read.csv("rawData/PastureMeter.csv")
st <- read.csv("rawData/stocking.csv")
pm <- merge(st,pm,by=c("site"),all.x=T, all.y=T)
head(pm)
pm$trt <- as.factor(pm$trt)
pm$month <- as.factor(pm$month)
pm$site <- as.factor(pm$site)
pm$line <- as.factor(pm$line)

unique(pm$trt)
#make ungrazed the reference level
pm <- within(pm, trt <- relevel(trt, ref = 4))

#function to add a new column onto the data with scaled vars (with s before their name)
scaleVars <- function(df){
  newd <- plyr::numcolwise(scale)(df)
  names(newd) <- sapply(names(newd),function(x)paste0("s",x))
  cbind(df, newd)
}

#apply function
pm <- scaleVars(pm)
hist(log10(pm$g_per_m2+1))

pmod <- lmer(log10(g_per_m2+1) ~ trt + (1|line:site), data = pm)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

pds <- pm[ which(pm$trt=='untrtpd'), ]
tpds <- pm[ which(pm$trt=='trtpd'), ]
apds <- rbind(pds,tpds)
apds$trt

ipmod <- lmer(log10(g_per_m2+1) ~ sinsecticide + (1|line:site), data = apds)
summary(ipmod)
# extract coefficients
coefs <- data.frame(coef(summary(ipmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

#calculate estimates for trts
ests <- NULL
for(i in unique(pm$trt)){
  sub <- pm[pm$trt == i, ]
  ests.i <- coef(summary(lmer(g_per_m2 ~ 1 + (1|line:site), data = sub, )))[1,1:2]
  ests.i <- data.frame(trt_mo = i, t(ests.i))
  ests <- rbind(ests, ests.i) ; rm(ests.i, sub)
} ; rm(i)
ests
colnames(ests)[2] ="g_per_m2_est"
colnames(ests)[3] ="g_per_m2_SE"
head(ests)


#calculate estimates for mo and trt combos
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
head(ests)

#subset by trt
b <- ests[ests$trt=="bison",]
c <- ests[ests$trt=="cattle",]
un <- ests[ests$trt=="ungrazed",]
pd <- ests[ests$trt=="untrtpd",]
tp <- ests[ests$trt=="trtpd",]

##calculate grasshopper effect on biomass for each month
ghop_ef6 <-(sum(pd$g_per_m2_est[tp$month==6]))-sum((tp$g_per_m2_est[pd$month==6]))
ghop_ef7 <-(pd$g_per_m2_est[tp$month==7])-(tp$g_per_m2_est[pd$month==7])
ghop_ef8 <-(pd$g_per_m2_est[tp$month==8])-(tp$g_per_m2_est[pd$month==8])
ghop_ef9 <-(pd$g_per_m2_est[tp$month==9])-(tp$g_per_m2_est[pd$month==9])

#calculate total grasshopper effect on biomass
totGH_ef <- mean(c(ghop_ef6, ghop_ef7, ghop_ef8, ghop_ef9))

##calculate bison effect on biomass for each month
##average bison stocking density : 0.0148920706666667 bison/acre
##4046.86 sq m in 1 acre

bi_ef6 <-(b$g_per_m2_est[b$month==6])-(un$g_per_m2_est[un$month==6])
bi_ef7 <-(b$g_per_m2_est[b$month==7])-(un$g_per_m2_est[un$month==7])
bi_ef8 <-(b$g_per_m2_est[b$month==8])-(un$g_per_m2_est[un$month==8])
bi_ef9 <-(b$g_per_m2_est[b$month==9])-(un$g_per_m2_est[un$month==9])

#calculate total bison effect on biomass
##########average effect of bison presence per sq m
totBi_ef <- mean(c(bi_ef6, bi_ef7, bi_ef8, bi_ef9))
##########number of acres one bison has
acres_per_bison <- 1/0.0148920706666667
######### number of bison per sq m
bi_per_sq_m <- 0.0148920706666667/4046.86
############# number of meters 1 bison has
m_per_bison <- 1/bi_per_sq_m
########### one bison eats (kg)
tot_bis_eat <- (m_per_bison*totBi_ef)/1000


per_bi_ef_m <- totBi_ef*(0.0148920706666667/4046.86)
per_bi_ef_ac_kg <- ((totBi_ef*4046.86)/0.0148920706666667)/1000

##calculate cattle effect on biomass for each month
##average cattle stocking density : 0.027007931 cattle/acre

ca_ef6 <-(c$g_per_m2_est[c$month==6])-(un$g_per_m2_est[un$month==6])
ca_ef7 <-(c$g_per_m2_est[c$month==7])-(un$g_per_m2_est[un$month==7])
ca_ef8 <-(c$g_per_m2_est[c$month==8])-(un$g_per_m2_est[un$month==8])
ca_ef9 <-(c$g_per_m2_est[c$month==9])-(un$g_per_m2_est[un$month==9])

#calculate total cattle effect on biomass
totCa_ef <- mean(c(ca_ef6, ca_ef7, ca_ef8, ca_ef9))
per_Ca_ef <- totCa_ef*(0.027007931/4046.86)
per_Ca_ef_ac_kg <- ((totCa_ef*4046.86)/0.027007931)/1000

####################################
