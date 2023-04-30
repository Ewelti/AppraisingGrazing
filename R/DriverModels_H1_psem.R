##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")

#########Hypotheses:
##H1) Grazing will increase plant quality with smaller herbivores having larger effects 
##H2) All grazers will decrease ANPP, and insect herbivory will equal that of large grazer herbivory
##H3) Plant nutrient content will be greatest in the early season while biomass will peak mid-season

##load libraries
library(lme4)
#install.packages("multcompView")
library("piecewiseSEM")
library("psycho")
library("multcompView")
############################H1
##load data
est <- read.csv("outputs/SiteMonthLevel.csv")
head(est)
est$trt <- as.factor(est$trt)
est$month <- as.factor(est$month)

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

#function to add a new column onto the data with scaled vars (with s before their name)
scaleVars <- function(df){
  newd <- plyr::numcolwise(scale)(df)
  names(newd) <- sapply(names(newd),function(x)paste0("s",x))
  cbind(df, newd)
}

#apply function
est <- scaleVars(est)


## full grass chem model
h1_N <- lmer(g_N ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide  + soil_PC2_posCN + soil_PC1_posOther + (1|month), data = est)
h1_P <- lmer(lg_P ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + soil_PC2_posCN + soil_PC1_posOther + g_N +(1|month), data = est)
h1_K <- lmer(lg_K ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide  + soil_PC2_posCN + soil_PC1_posOther + g_N +(1|month), data = est)
h1_Mg <- lmer(lg_Mg ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + soil_PC2_posCN + soil_PC1_posOther + g_N +(1|month), data = est)
h1_Na <- lmer(lg_Na ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + soil_PC2_posCN + soil_PC1_posOther + g_N +(1|month), data = est)
h1_Si <- lmer(lg_Si~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + soil_PC2_posCN + soil_PC1_posOther +g_N +(1|month), data = est)
s1 <- lmer (soil_PC2_posCN ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + (1|month), data = est)
s2 <- lmer (soil_PC1_posOther ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + soil_PC2_posCN + (1|month), data = est)

##SEM
sem.g <- psem(h1_N, h1_P, h1_K, h1_Mg,h1_Na, h1_Si,s1, s2)
summary(sem.g, standardize = "none", conserve = TRUE)

##################
## reduced grass chem model with month removed when singular
h1_N <- lmer(g_N ~ sPD_pres + soil_PC2_posCN + (1|month), data = est)
h1_P <- lm(lg_P ~ sPD_pres + g_N  , data = est)
h1_K <- lm(lg_K ~ sbison_dens + sPD_pres + lg_P, data = est)
h1_Mg <- lmer(lg_Mg ~ sbison_dens + scattle_dens + g_N +(1|month), data = est)
h1_Na <- lm(lg_Na ~ sbison_dens + lg_Mg, data = est)
h1_Si <- lmer(lg_Si~ sinsecticide + lg_Na + lg_Mg +(1|month), data = est)
s1 <- lm (soil_PC2_posCN ~ sPD_pres + sinsecticide, data = est)
#s2 <- lmer (soil_PC1_posOther ~ (1|month), data = est)

##SEM
sem.rg <- psem(h1_N, h1_P, h1_K, h1_Mg,h1_Na, h1_Si, s1) #, s2)
summary(sem.rg, standardize = "none", conserve = TRUE)

##################

est$month <- as.numeric(est$month)

## reduced grass chem model with month as fixed
################Final grass model- use this one

h1_N <- lm(g_N ~ sPD_pres + soil_PC2_posCN + month, data = est)
h1_P <- lm(lg_P ~ sPD_pres + g_N, data = est)
h1_K <- lm(lg_K ~ sbison_dens + sPD_pres + lg_P, data = est)
h1_Mg <- lm(lg_Mg ~ sbison_dens + scattle_dens + g_N + month, data = est)
h1_Na <- lm(lg_Na ~ sbison_dens + lg_Mg, data = est)
h1_Si <- lm(lg_Si~ sinsecticide + lg_Na + lg_Mg + month, data = est)
s1 <- lm (soil_PC2_posCN ~ sbison_dens + sPD_pres + sinsecticide, data = est)
#s2 <- lmer (soil_PC1_posOther ~ month, data = est)

##SEM
sem.rg <- psem(h1_N, h1_P, h1_K, h1_Mg,h1_Na, h1_Si, s1) #, s2)
summary(sem.rg, standardize = "none", conserve = TRUE)

#########################
## full forb chem model
h1_N <- lmer(f_N ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide  + soil_PC2_posCN + soil_PC1_posOther + (1|month), data = est)
h1_P <- lmer(lf_P ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + soil_PC2_posCN + soil_PC1_posOther + f_N +(1|month), data = est)
h1_K <- lmer(lf_K ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide  + soil_PC2_posCN + soil_PC1_posOther + f_N +(1|month), data = est)
h1_Mg <- lmer(lf_Mg ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + soil_PC2_posCN + soil_PC1_posOther + f_N +(1|month), data = est)
h1_Na <- lmer(lf_Na ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + soil_PC2_posCN + soil_PC1_posOther + f_N +(1|month), data = est)
h1_Si <- lmer(lf_Si~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + soil_PC2_posCN + soil_PC1_posOther + f_N +(1|month), data = est)
s1 <- lmer (soil_PC2_posCN ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + (1|month), data = est)
s2 <- lmer (soil_PC1_posOther ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + soil_PC2_posCN + (1|month), data = est)

##SEM
sem.f <- psem(h1_N, h1_P, h1_K, h1_Mg,h1_Na, h1_Si,s1, s2)
summary(sem.f, standardize = "none", conserve = TRUE)

##################
## reduced forb chem model
h1_N <- lm(f_N ~ scattle_dens + sPD_pres + sinsecticide  + soil_PC2_posCN + month, data = est)
h1_P <- lm(lf_P ~ sinsecticide + f_N +(1|month), data = est)
h1_K <- lm(lf_K ~ f_N + lf_P, data = est)
h1_Mg <- lm(lf_Mg ~ lf_K, data = est)
h1_Na <- lm(lf_Na ~ sinsecticide + lf_K + month, data = est)
s1 <- lm(soil_PC2_posCN ~ sbison_dens + sPD_pres + sinsecticide, data = est)
#s2 <- lm(soil_PC1_posOther ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + soil_PC2_posCN + month, data = est)

##SEM
sem.rf <- psem(h1_N, h1_P, h1_K, h1_Mg,h1_Na,s1)
summary(sem.rf, standardize = "none", conserve = TRUE)
##############################################################