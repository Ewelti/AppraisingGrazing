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
dim(pm)
st <- read.csv("outputs/SiteMonthLevel.csv")
head(st)
pm <- merge(st,pm,by=c("site","month"),all.x=T, all.y=T)
head(pm)
pm$trt <- as.factor(pm$trt)
pm$site <- as.factor(pm$site)
pm$line <- as.factor(pm$line)

#function to add a new column onto the data with scaled vars (with s before their name)
scaleVars <- function(df){
  newd <- plyr::numcolwise(scale)(df)
  names(newd) <- sapply(names(newd),function(x)paste0("s",x))
  cbind(df, newd)
}

#apply function
pm <- scaleVars(pm)
hist(log10(pm$g_per_m2+1))

pmod <- lmer(log10(g_per_m2+1) ~ sbison_dens + scattle_dens + sPD_poo + sghop_m2_est + (1|line:site), data = pm)
summary(pmod)
confint(pmod)
# extract coefficients
coefs <- data.frame(coef(summary(pmod)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
####################################
