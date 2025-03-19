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
library(MuMIn)

############################H3

#############H3 plant biomass
##all data
pm <- read.csv("rawData/PastureMeter.csv")
pm$trt <- as.factor(pm$trt)
pm$line <- as.factor(pm$line)
pm$site <- as.factor(pm$site)
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
r.squaredGLMM(mo_pmod)

##cattle
mo_pmod <- lmer(log10(g_per_m2+1) ~ poly(month,2) + (1|line:site), data = c)
coefs <- data.frame(coef(summary(mo_pmod)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(mo_pmod)
r.squaredGLMM(mo_pmod)

##ungrazed
mo_pmod <- lmer(log10(g_per_m2+1) ~ poly(month,2) + (1|line:site), data = un)
coefs <- data.frame(coef(summary(mo_pmod)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(mo_pmod)
r.squaredGLMM(mo_pmod)

##untrtpd
mo_pmod <- lmer(log10(g_per_m2+1) ~ poly(month,2) + (1|line:site), data = pd)
coefs <- data.frame(coef(summary(mo_pmod)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(mo_pmod)
r.squaredGLMM(mo_pmod)

##trtpd
mo_pmod <- lmer(log10(g_per_m2+1) ~ poly(month,2) + (1|line:site), data = tp)
coefs <- data.frame(coef(summary(mo_pmod)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(mo_pmod)
r.squaredGLMM(mo_pmod)
