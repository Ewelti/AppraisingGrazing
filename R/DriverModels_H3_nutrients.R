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
#################################

##################H3 nutrients
##load data
est <- read.csv("outputs/SiteMonthLevel.csv")
head(est)
est$trt <- as.factor(est$trt)

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

#subset by trt
b <- est[est$trt=="bison",]
ca <- est[est$trt=="cattle",]
un <- est[est$trt=="ungrazed",]
pd <- est[est$trt=="untrtpd",]
tp <- est[est$trt=="trtpd",]

#############grass and bison
#N
cm <- lmer(lg_N ~ month + (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co_b<-co
#P
cm <- lmer(lg_P ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co_b<-rbind(co,co_b)
#K
cm <- lmer(lg_K ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(lg_Mg ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(lg_Na ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(lg_Si ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Si"
co_b<-rbind(co,co_b)
co_b$herbivore <-rep("bison",6)

###############grass and cattle
cm <- lmer(lg_N ~ month + (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#P
cm <- lmer(lg_P ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#K
cm <- lmer(lg_K ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(lg_Na ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(lg_Mg ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(lg_Si ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Si"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)

#################grass and ungrazed
cm <- lmer(lg_N ~ month + (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#P
cm <- lmer(lg_P ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#K
cm <- lmer(lg_K ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(lg_Na ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(lg_Mg ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(lg_Si ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Si"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)

##################grass and untrtpd
cm <- lmer(lg_N ~ month + (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#P
cm <- lmer(lg_P ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#K
cm <- lmer(lg_K ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(lg_Na ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(lg_Mg ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(lg_Si ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Si"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)

##################grass and trtpd
##############not enough data here

####write grass results
write.csv(co_b,"outputs/GrassNutrients_ResponsesToMonth_WithinSites.csv")

########################################################forb

#############forb and bison
#N
cm <- lmer(lf_N ~ month + (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co_b<-co
#P
cm <- lmer(lf_P ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co_b<-rbind(co,co_b)
#K
cm <- lmer(lf_K ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(lf_Mg ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(llf_Na ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(lf_Si ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Si"
co_b<-rbind(co,co_b)
co_b$herbivore <-rep("bison",6)

###############forb and cattle
cm <- lmer(lf_N ~ month + (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#P
cm <- lmer(lf_P ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#K
cm <- lmer(lf_K ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(llf_Na ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(lf_Mg ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(lf_Si ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Si"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)

#################forb and ungrazed
cm <- lmer(lf_N ~ month + (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#P
cm <- lmer(lf_P ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#K
cm <- lmer(lf_K ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(llf_Na ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(lf_Mg ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(lf_Si ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Si"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)

##################forb and untrtpd
cm <- lmer(lf_N ~ month + (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#P
cm <- lmer(lf_P ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#K
cm <- lmer(lf_K ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(llf_Na ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(lf_Mg ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(lf_Si ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Si"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)

##################forb and trtpd
cm <- lmer(lf_N ~ month + (1|site), data=tp)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co$herbivore <- "trtpd"
co_b<-rbind(co,co_b)
#P
cm <- lmer(lf_P ~ month+ (1|site), data=tp)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co$herbivore <- "trtpd"
co_b<-rbind(co,co_b)
#K
cm <- lmer(lf_K ~ month+ (1|site), data=tp)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co$herbivore <- "trtpd"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(llf_Na ~ month+ (1|site), data=tp)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co$herbivore <- "trtpd"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(lf_Mg ~ month+ (1|site), data=tp)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co$herbivore <- "trtpd"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(lf_Si ~ month+ (1|site), data=tp)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Si"
co$herbivore <- "trtpd"
co_b<-rbind(co,co_b)

####write forb results
write.csv(co_b,"outputs/ForbNutrients_ResponsesToMonth_WithinSites.csv")

########################################################soil

#############soil and bison
#N
cm <- lmer(ls_N ~ month + (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co_b<-co
#P
cm <- lmer(ls_P ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co_b<-rbind(co,co_b)
#K
cm <- lmer(ls_K ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(ls_Mg ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(lls_Na ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(ls_C ~ month+ (1|site), data=b)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "C"
co_b<-rbind(co,co_b)
co_b$herbivore <-rep("bison",6)

###############soil and cattle
cm <- lmer(ls_N ~ month + (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#P
cm <- lmer(ls_P ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#K
cm <- lmer(ls_K ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(lls_Na ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(ls_Mg ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(ls_C ~ month+ (1|site), data=ca)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "C"
co$herbivore <- "cattle"
co_b<-rbind(co,co_b)

#################soil and ungrazed
cm <- lmer(ls_N ~ month + (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#P
cm <- lmer(ls_P ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#K
cm <- lmer(ls_K ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(lls_Na ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(ls_Mg ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(ls_C ~ month+ (1|site), data=un)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "C"
co$herbivore <- "ungrazed"
co_b<-rbind(co,co_b)

##################soil and untrtpd
cm <- lmer(ls_N ~ month + (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#P
cm <- lmer(ls_P ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#K
cm <- lmer(ls_K ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(lls_Na ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(ls_Mg ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(ls_C ~ month+ (1|site), data=pd)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "C"
co$herbivore <- "untrtpd"
co_b<-rbind(co,co_b)

##################soil and trtpd
cm <- lmer(ls_N ~ month + (1|site), data=tp)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "N"
co$herbivore <- "trtpd"
co_b<-rbind(co,co_b)
#P
cm <- lmer(ls_P ~ month+ (1|site), data=tp)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "P"
co$herbivore <- "trtpd"
co_b<-rbind(co,co_b)
#K
cm <- lmer(ls_K ~ month+ (1|site), data=tp)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "K"
co$herbivore <- "trtpd"
co_b<-rbind(co,co_b)
#Na
cm <- lmer(lls_Na ~ month+ (1|site), data=tp)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Na"
co$herbivore <- "trtpd"
co_b<-rbind(co,co_b)
#Mg
cm <- lmer(ls_Mg ~ month+ (1|site), data=tp)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "Mg"
co$herbivore <- "trtpd"
co_b<-rbind(co,co_b)
#Si
cm <- lmer(ls_C ~ month+ (1|site), data=tp)
summary(cm)
coefs <- data.frame(coef(summary(cm)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
co <- coefs[2,]
co <-cbind(co,r.squaredGLMM(cm))
co$element <- "C"
co$herbivore <- "trtpd"
co_b<-rbind(co,co_b)

####write soil results
write.csv(co_b,"outputs/SoilNutrients_ResponsesToMonth_WithinSites.csv")
##