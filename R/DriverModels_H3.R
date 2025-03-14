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
############################H3

#############H3 plant biomass
##all data
pm <- read.csv("rawData/PastureMeter.csv")
pm$trt <- as.factor(pm$trt)
pm$month <- as.numeric(pm$month)
head(pm)

#effect of polynomial month on all pasture meter
mo_pmod <- lmer(log10(g_per_m2+1) ~ poly(month,2) + (1|line:site), data = pm)
summary(mo_pmod)
# extract coefficients
coefs <- data.frame(coef(summary(mo_pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
plot((pm$g_per_m2) ~ pm$DOY)
confint(mo_pmod)

#subset by trt
b <- pm[pm$trt=="bison",]
c <- pm[pm$trt=="cattle",]
un <- pm[pm$trt=="ungrazed",]
pd <- pm[pm$trt=="untrtpd",]
tp <- pm[pm$trt=="trtpd",]

##bison
mo_pmod <- lmer(log10(g_per_m2+1) ~ poly(month,2) + (1|line:site), data = b)
coefs <- data.frame(coef(summary(mo_pmod)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(mo_pmod)

##cattle
mo_pmod <- lmer(log10(g_per_m2+1) ~ poly(month,2) + (1|line:site), data = c)
coefs <- data.frame(coef(summary(mo_pmod)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(mo_pmod)

##ungrazed
mo_pmod <- lmer(log10(g_per_m2+1) ~ poly(month,2) + (1|line:site), data = un)
coefs <- data.frame(coef(summary(mo_pmod)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(mo_pmod)

##untrtpd
mo_pmod <- lmer(log10(g_per_m2+1) ~ poly(month,2) + (1|line:site), data = pd)
coefs <- data.frame(coef(summary(mo_pmod)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(mo_pmod)

##trtpd
mo_pmod <- lmer(log10(g_per_m2+1) ~ poly(month,2) + (1|line:site), data = tp)
coefs <- data.frame(coef(summary(mo_pmod)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(mo_pmod)
#################################

##################H3 nutrients
##load data
est <- read.csv("outputs/SiteMonthLevel.csv")
head(est)
est$trt <- as.factor(est$trt)

#log scale grass chem
est$lg_P <- log10(est$g_P)
est$lg_K <- log10(est$g_K)
est$lg_Mg <- log10(est$g_Mg)
est$lg_Na <- log10(est$g_Na)
est$lg_Si <- log10(est$g_Si)
est["lg_Si"][est["lg_Si"] == "-Inf"] <- 0

#log scale forb chem
est$lf_P <- log10(est$f_P)
est$lf_K <- log10(est$f_K)
est$lf_Mg <- log10(est$f_Mg)
est$lf_Na <- log10(est$f_Na)
est$lf_Si <- log10(est$f_Si)


#grass overall
cm <- lm(g_N ~ month, data=est)
summary(cm)
cm <- lm(g_C ~ month, data=est)
summary(cm)
cm <- lm(lg_P ~ month, data=est)
summary(cm)
cm <- lm(lg_K ~ month, data=est)
summary(cm)
cm <- lm(lg_Na ~ month, data=est)
summary(cm)
cm <- lm(lg_Si ~ month, data=est)
summary(cm)
cm <- lm(lg_Mg ~ month, data=est)
summary(cm)

#subset by trt
b <- est[est$trt=="bison",]
c <- est[est$trt=="cattle",]
un <- est[est$trt=="ungrazed",]
pd <- est[est$trt=="untrtpd",]
tp <- est[est$trt=="trtpd",]

#grass and bison
cm <- lm(g_N ~ month, data=b)
summary(cm)
cm <- lm(lg_P ~ month, data=b)
summary(cm)
cm <- lm(lg_K ~ month, data=b)
summary(cm)
cm <- lm(lg_Na ~ month, data=b)
summary(cm)
cm <- lm(lg_Si ~ month, data=b)
summary(cm)


#grass and cattle
cm <- lm(g_N ~ month, data=c)
summary(cm)
cm <- lm(lg_P ~ month, data=c)
summary(cm)
cm <- lm(lg_K ~ month, data=c)
summary(cm)
cm <- lm(lg_Na ~ month, data=c)
summary(cm)
cm <- lm(lg_Si ~ month, data=c)
summary(cm)

#grass and ungrazed
cm <- lm(g_N ~ month, data=un)
summary(cm)
cm <- lm(lg_P ~ month, data=un)
summary(cm)
cm <- lm(lg_K ~ month, data=un)
summary(cm)
cm <- lm(lg_Na ~ month, data=un)
summary(cm)
cm <- lm(lg_Si ~ month, data=un)
summary(cm)

#grass and untrtpd
cm <- lm(g_N ~ month, data=pd)
summary(cm)
cm <- lm(lg_P ~ month, data=pd)
summary(cm)
cm <- lm(lg_K ~ month, data=pd)
summary(cm)
cm <- lm(lg_Na ~ month, data=pd)
summary(cm)
cm <- lm(lg_Si ~ month, data=pd)
summary(cm)

#grass and trtpd
##############not enough data here

#################forb

#forb overall
cm <- lm(f_N ~ month, data=est)
summary(cm)
cm <- lm(f_C ~ month, data=est)
summary(cm)
cm <- lm(lf_P ~ month, data=est)
summary(cm)
cm <- lm(lf_K ~ month, data=est)
summary(cm)
cm <- lm(lf_Na ~ month, data=est)
summary(cm)
cm <- lm(lf_Si ~ month, data=est)
summary(cm)
cm <- lm(lf_Mg ~ month, data=est)
summary(cm)

#forb and bison
cm <- lm(f_N ~ month, data=b)
summary(cm)
cm <- lm(lf_P ~ month, data=b)
summary(cm)
cm <- lm(lf_K ~ month, data=b)
summary(cm)
cm <- lm(lf_Na ~ month, data=b)
summary(cm)

#forb and cattle
cm <- lm(f_N ~ month, data=c)
summary(cm)
cm <- lm(lf_P ~ month, data=c)
summary(cm)
cm <- lm(lf_K ~ month, data=c)
summary(cm)
cm <- lm(lf_Na ~ month, data=c)
summary(cm)

#forb and ungrazed
cm <- lm(f_N ~ month, data=un)
summary(cm)
cm <- lm(lf_P ~ month, data=un)
summary(cm)
cm <- lm(lf_K ~ month, data=un)
summary(cm)
cm <- lm(lf_Na ~ month, data=un)
summary(cm)

#forb and untrtpd
cm <- lm(f_N ~ month, data=pd)
summary(cm)
cm <- lm(lf_P ~ month, data=pd)
summary(cm)
cm <- lm(lf_K ~ month, data=pd)
summary(cm)
cm <- lm(lf_Na ~ month, data=pd)
summary(cm)

#forb and trtpd
cm <- lm(f_N ~ month, data=tp)
summary(cm)
cm <- lm(lf_P ~ month, data=tp)
summary(cm)
cm <- lm(lf_K ~ month, data=tp)
summary(cm)
cm <- lm(lf_Na ~ month, data=tp)
summary(cm)


##