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

############################H2
##all data
st <- read.csv("outputs/SiteMonthLevel.csv")
head(st)

#function to add a new column onto the data with scaled vars (with s before their name)
scaleVars <- function(df){
  newd <- plyr::numcolwise(scale)(df)
  names(newd) <- sapply(names(newd),function(x)paste0("s",x))
  cbind(df, newd)
}

#apply function
st <- scaleVars(st)
hist((st$g_per_m2_est))

#####full model
pmod <- lmer(g_per_m2_est ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + poly(smonth,2) + (1|site), data = st)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
r.squaredGLMM(pmod)

####pd model
pds <- st[ which(pm$trt=='untrtpd'), ]
tpds <- st[ which(pm$trt=='trtpd'), ]
apds <- rbind(pds,tpds)
apds$trt

pmod <- lmer(g_per_m2_est ~ PD_poo + (1|site), data = apds)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

####cattle model
catt <- st[ which(st$trt=='cattle'), ]
catt$trt

pmod <- lmer(g_per_m2_est ~ cattle_dens + (1|site), data = catt)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

####bison model
bi <- st[ which(st$trt=='bison'), ]
bi$trt

pmod <- lmer(g_per_m2_est ~ bison_dens + (1|site), data = bi)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

plot(g_per_m2_est~ghop_m2_est, log="xy", data = st)
plot(g_per_m2_est~PD_poo, data = st)
####################################
