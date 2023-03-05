##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")

#########Hypotheses:
##H1) Grazing will increase plant quality with smaller herbivores having larger effects 
##H2) All grazers will decrease ANPP, and insect herbivory will equal that of large grazer herbivory
##H3) Plant nutrient content will be greatest in the early season while biomass will peak mid-season
##H4) Grasshopper densities will track plant quality and thus will be highest in the presence of large grazers

##load libraries
library(lme4)

############################H4
##estimate data
##load data
est <- read.csv("outputs/SiteMonthLevel.csv")
head(est)
est$trt <- as.factor(est$trt)
est$month <- as.factor(est$month)
hist(est$ghop_m2_est)

#Temp_samplingday
gh_n <- glmer(ghop_m2_est ~ grass_PC1 + forb_PC1 + Temp_samplingday + (1|month), family = gaussian(link = "log"), data = est)
coefs <- data.frame(coef(summary(gh_n)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

##month subsets
est6 <- est[est$month==6,]
est7 <- est[est$month==7,]
est8 <- est[est$month==8,]
est9 <- est[est$month==9,]

####month models
g6 <- glm(ghop_m2_est ~ grass_PC1 + forb_PC1, family = gaussian(link = "log"), data = est6)
summary(g6)
g7 <- glm(ghop_m2_est ~ grass_PC1 + forb_PC1, family = gaussian(link = "log"), data = est7)
summary(g7)
g8 <- glm(ghop_m2_est ~ grass_PC1 + forb_PC1, family = gaussian(link = "log"), data = est8)
summary(g8)
g9 <- glm(ghop_m2_est ~ grass_PC1 + forb_PC1, family = gaussian(link = "log"), data = est9)
summary(g9)
###############################
##all data
rc <- read.csv("rawData/RingCounts.csv")
st <- read.csv("rawData/stocking.csv")
rc <- merge(st,rc,by=c("site"),all.x=T, all.y=T)
head(rc)
rc$trt <- as.factor(rc$trt)
rc$month <- as.factor(rc$month)

#make ungrazed the reference level
rc <- within(rc, trt <- relevel(trt, ref = 4))
rc$count <- as.numeric(rc$count)*10
hist(rc$count)

#function to add a new column onto the data with scaled vars (with s before their name)
scaleVars <- function(df){
  newd <- plyr::numcolwise(scale)(df)
  names(newd) <- sapply(names(newd),function(x)paste0("s",x))
  cbind(df, newd)
}

#apply function
rc <- scaleVars(rc)

#ghmod <- glmer(count ~ trt + (1|line:site), family =poisson(link="log"), data = rc)
ghmod <- glmer(count ~ sbison_dens + scattle_dens + sPD_pres + sinsecticide + (1|line:site), family = poisson(link="log"), data = rc)
summary(ghmod)
# extract coefficients
coefs <- data.frame(coef(summary(ghmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

##
