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

############################H1
##load data
est <- read.csv("outputs/SiteMonthLevel.csv")
head(est)
est$trt <- as.factor(est$trt)


#function to add a new column onto the data with scaled vars (with s before their name)
scaleVars <- function(df){
  newd <- plyr::numcolwise(scale)(df)
  names(newd) <- sapply(names(newd),function(x)paste0("s",x))
  cbind(df, newd)
}

#apply function
est <- scaleVars(est)

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
est$ls_C <- log10(est$percC)
est$ls_N <- log10(est$percN)
est$ls_P <- log10(est$Ca_ppm)
est$ls_K <- log10(est$K_ppm)
est$ls_Mg <- log10(est$Mg_ppm)
est$ls_Na <- log10(est$Na_ppm)

#######################grass N
pmod <- lmer(lg_N ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("grassN",5)
coefsN<-coefs

#######################grass P
pmod <- lmer(lg_P ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("grassP",5)
coefsP<-coefs
resp<- rbind(coefsN[2:5,],coefsP[2:5,])

#######################grass K
pmod <- lmer(lg_K ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("grassK",5)
coefsK<-coefs
resp<- rbind(resp,coefsK[2:5,])

#######################grass Mg
pmod <- lmer(lg_Mg ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("grassMg",5)
coefsMg<-coefs
resp<- rbind(resp,coefsMg[2:5,])

#######################grass Na
pmod <- lmer(lg_Na ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("grassNa",5)
coefsNa<-coefs
resp<- rbind(resp,coefsNa[2:5,])

#######################grass Si
pmod <- lmer(lg_Si ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est, na.action = na.exclude)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("grassSi",5)
coefsSi<-coefs
resp<- rbind(resp,coefsSi[2:5,])
######################################################################
##############################################################################

#######################forb N
pmod <- lmer(lf_N ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("forbN",5)
coefsN<-coefs
resp<- rbind(resp,coefsN[2:5,])

#######################forb P
pmod <- lmer(lf_P ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("forbP",5)
coefsP<-coefs
resp<- rbind(resp,coefsP[2:5,])

#######################forb K
pmod <- lmer(lf_K ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("forbK",5)
coefsK<-coefs
resp<- rbind(resp,coefsK[2:5,])

#######################forb Mg
pmod <- lmer(lf_Mg ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("forbMg",5)
coefsMg<-coefs
resp<- rbind(resp,coefsMg[2:5,])

#######################forb Na
pmod <- lmer(lf_Na ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("forbNa",5)
coefsNa<-coefs
resp<- rbind(resp,coefsNa[2:5,])

#######################forb Si
pmod <- lmer(lf_Si ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("forbSi",5)
coefsSi<-coefs
resp <- rbind(resp,coefsSi[2:5,])
######################################################################
##############################################################################

#######################soil C
pmod <- lmer(ls_C ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("soilC",5)
coefsC<-coefs
resp <- rbind(resp,coefsC[2:5,])

#######################soil N
#pmod <- lmer(ls_N ~ trt+ (1|site), data = est) ##this approach compares everything to bison and does not take into account bison on pd towns
pmod <- lmer(ls_N ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("soilN",5)
coefsN<-coefs
resp<- rbind(resp,coefsN[2:5,])

#######################soil P
pmod <- lmer(ls_P ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("soilP",5)
coefsP<-coefs
resp<- rbind(resp,coefsP[2:5,])

#######################soil K
pmod <- lmer(ls_K ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("soilK",5)
coefsK<-coefs
resp<- rbind(resp,coefsK[2:5,])

#######################soil Mg
pmod <- lmer(ls_Mg ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("soilMg",5)
coefsMg<-coefs
resp<- rbind(resp,coefsMg[2:5,])

#######################soil Na
pmod <- lmer(ls_Na ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|site), data = est)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
coefs$element <-rep("soilNa",5)
coefsNa<-coefs
respo<- rbind(resp,coefsNa[2:5,])

##############################################################
###############################################################################
write.csv(respo, "outputs/ElementalResponses_grazingGradient.csv")
####
##