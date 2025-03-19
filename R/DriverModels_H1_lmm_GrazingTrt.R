##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")
##Set working directory (Julie's computer)
setwd("/Users/julierebh/Downloads/AppraisingGrazing")

#########Hypotheses:
##H1) Grazing will increase plant quality with smaller herbivores having larger effects 
##H2) All grazers will decrease ANPP, and insect herbivory will equal that of large grazer herbivory
##H3) Plant nutrient content will be greatest in the early season while biomass will peak mid-season

##load libraries
library(lme4)
library(MuMIn)

############################H1
##load data
est <- read.csv("outputs/SiteMonthLevel.csv")
head(est)

#make ungrazed the reference level
est$trt <- as.factor(est$trt)
est <- within(est, trt <- relevel(trt, ref = 4))

#log scale grass chem
est$lg_N <- log10(est$g_N)
est$lg_P <- log10(est$g_P)
est$lg_K <- log10(est$g_K)
est$lg_Mg <- log10(est$g_Mg)
est$lg_Na <- log10(est$g_Na)
est$lg_Si <- log10(est$g_Si)
est["lg_Si"][est["lg_Si"] == "-Inf"] <- 0

#log scale forb chem
est$lf_N <- log10(est$f_N)
est$lf_P <- log10(est$f_P)
est$lf_K <- log10(est$f_K)
est$lf_Mg <- log10(est$f_Mg)
est$lf_Na <- log10(est$f_Na)
est$lf_Si <- log10(est$f_Si)

#log scale soil chem
hist(log10(est$percC))
est$ls_C <- log10(est$percC)
est$ls_N <- log10(est$percN)
est$ls_P <- log10(est$P_ppm)
est$ls_K <- log10(est$K_ppm)
est$ls_Mg <- log10(est$Mg_ppm)
est$ls_Na <- log10(est$Na_ppm)

#######################grass N
pmod <- lmer(lg_N ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("grassN",6)
coefsN<-coefs
r.squaredGLMM(pmod)

#######################grass P
pmod <- lmer(lg_P ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("grassP",6)
coefsP<-coefs
resp<- rbind(coefsN[2:6,],coefsP[2:6,])
r.squaredGLMM(pmod)

#######################grass K
pmod <- lmer(lg_K ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("grassK",6)
coefsK<-coefs
resp<- rbind(resp,coefsK[2:6,])
r.squaredGLMM(pmod)

#######################grass Mg
pmod <- lmer(lg_Mg ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("grassMg",6)
coefsMg<-coefs
resp<- rbind(resp,coefsMg[2:6,])
r.squaredGLMM(pmod)

#######################grass Na
pmod <- lmer(lg_Na ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("grassNa",6)
coefsNa<-coefs
resp<- rbind(resp,coefsNa[2:6,])
r.squaredGLMM(pmod)

#######################grass Si
pmod <- lmer(lg_Si ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("grassSi",6)
coefsSi<-coefs
resp<- rbind(resp,coefsSi[2:6,])
r.squaredGLMM(pmod)

######################################################################
##############################################################################

#######################forb N
pmod <- lmer(lf_N ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("forbN",6)
coefsN<-coefs
resp<- rbind(resp,coefsN[2:6,])
r.squaredGLMM(pmod)

#######################forb P
pmod <- lmer(lf_P ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("forbP",6)
coefsP<-coefs
resp<- rbind(resp,coefsP[2:6,])
r.squaredGLMM(pmod)

#######################forb K
pmod <- lmer(lf_K ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("forbK",6)
coefsK<-coefs
resp<- rbind(resp,coefsK[2:6,])
r.squaredGLMM(pmod)

#######################forb Mg
pmod <- lmer(lf_Mg ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("forbMg",6)
coefsMg<-coefs
resp<- rbind(resp,coefsMg[2:6,])
r.squaredGLMM(pmod)

#######################forb Na
pmod <- lmer(lf_Na ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("forbNa",6)
coefsNa<-coefs
resp<- rbind(resp,coefsNa[2:6,])
r.squaredGLMM(pmod)

#######################forb Si
pmod <- lmer(lf_Si ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("forbSi",6)
coefsSi<-coefs
resp <- rbind(resp,coefsSi[2:6,])
r.squaredGLMM(pmod)

######################################################################
##############################################################################

#######################soil C
pmod <- lmer(ls_C ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("soilC",6)
coefsC<-coefs
resp <- rbind(resp,coefsC[2:6,])
r.squaredGLMM(pmod)

#######################soil N
pmod <- lmer(ls_N ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("soilN",6)
coefsN<-coefs
resp<- rbind(resp,coefsN[2:6,])
r.squaredGLMM(pmod)

#######################soil P
pmod <- lmer(ls_P ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("soilP",6)
coefsP<-coefs
resp<- rbind(resp,coefsP[2:6,])
r.squaredGLMM(pmod)

#######################soil K
pmod <- lmer(ls_K ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("soilK",6)
coefsK<-coefs
resp<- rbind(resp,coefsK[2:6,])
r.squaredGLMM(pmod)

#######################soil Mg
pmod <- lmer(ls_Mg ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("soilMg",6)
coefsMg<-coefs
resp<- rbind(resp,coefsMg[2:6,])
r.squaredGLMM(pmod)

#######################soil Na
pmod <- lmer(ls_Na ~ trt + month + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("soilNa",6)
coefsNa<-coefs
resp<- rbind(resp,coefsNa[2:6,])
r.squaredGLMM(pmod)

##############################################################
###############################################################################
write.csv(resp, "outputs/ElementalResponses_grazingTrt.csv")
####
##