##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")

#########Hypotheses:
##H1) Grazing will increase plant quality with smaller herbivores having larger effects 
##H2) All grazers will decrease ANPP, and insect herbivory will equal that of large grazer herbivory
##H3) Plant nutrient content will be greatest in the early season while biomass will peak mid-season
##H4) Grasshopper densities will track plant quality and thus will be highest in the presence of large grazers

##load libraries
library(lme4)

############################H1
##load data
est <- read.csv("outputs/SiteMonthLevel.csv")
head(est)
est$trt <- as.factor(est$trt)
est$month <- as.factor(est$month)

#make ungrazed the reference level
est <- within(est, trt <- relevel(trt, ref = 4))

#function to add a new column onto the data with scaled vars (with s before their name)
scaleVars <- function(df){
  newd <- plyr::numcolwise(scale)(df)
  names(newd) <- sapply(names(newd),function(x)paste0("s",x))
  cbind(df, newd)
}

#apply function
est <- scaleVars(est)

#############grass nutrient trt model all months
boxplot(est$grass_PC1 ~est$trt)
hist(est$grass_PC1)
##h1_g <- lmer(grass_PC1 ~ trt + soil_PC1_posOther + soil_PC2_posCN + (1|month), data = est) #
h1_g <- lmer(grass_PC1 ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide  + (1|month), data = est)
summary(h1_g)
# extract coefficients
coefs <- data.frame(coef(summary(h1_g)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

###############forb nutrient trt model all months
boxplot(est$forb_PC1 ~est$trt)
hist(est$forb_PC1)
##h1_f <- lmer(forb_PC1 ~ trt + soil_PC1_posOther + soil_PC2_posCN + (1|month), data = est)
h1_f <- lmer(forb_PC1 ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + (1|month), data = est)
summary(h1_f)
# extract coefficients
coefs <- data.frame(coef(summary(h1_f)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

##month subsets
est6 <- est[est$month==6,]
est7 <- est[est$month==7,]
est8 <- est[est$month==8,]
est9 <- est[est$month==9,]

####grass June
g6 <- lm(grass_PC1 ~ trt, data = est6)
summary(g6)
####grass July
g7 <- lm(grass_PC1 ~ trt, data = est7)
summary(g7)
####grass Aug
g8 <- lm(grass_PC1 ~ trt, data = est8)
summary(g8)
####grass Sept
g9 <- lm(grass_PC1 ~ trt, data = est9)
summary(g9)

####forb June
f6 <- lm(forb_PC1 ~ trt, data = est6)
summary(f6)
####forb July
f7 <- lm(forb_PC1 ~ trt, data = est7)
summary(f7)
####forb Aug
f8 <- lm(forb_PC1 ~ trt, data = est8)
summary(f8)
####forb Sept
f9 <- lm(forb_PC1 ~ trt, data = est9)
summary(f9)
################
##################################