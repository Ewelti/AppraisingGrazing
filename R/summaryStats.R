##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")

# attach data
sc <- read.csv("rawdata/SoilChem.csv")
head(sc)

sc[c('trt', 'rep')] <- str_split_fixed(sc$site, '_', 2)

#calculate trt estimates soil N
tests <- NULL
for(i in unique(sc$trt)){
  sub <- sc[sc$trt == i, ]
  tests.i <- coef(summary(lmer(percN ~ 1 + (1|site), data = sub, )))[1,1:2]
  tests.i <- data.frame(trt = i, t(tests.i))
  tests <- rbind(tests, tests.i) ; rm(tests.i, sub)
} ; rm(i)
tests

#calculate trt estimates soil C
tests <- NULL
for(i in unique(sc$trt)){
  sub <- sc[sc$trt == i, ]
  tests.i <- coef(summary(lmer(percC ~ 1 + (1|site), data = sub, )))[1,1:2]
  tests.i <- data.frame(trt = i, t(tests.i))
  tests <- rbind(tests, tests.i) ; rm(tests.i, sub)
} ; rm(i)
tests
######################################
####amounts eaten
pm <- read.csv("rawData/PastureMeter.csv")
st <- read.csv("rawData/stocking.csv")
pm <- merge(st,pm,by=c("site"),all.x=T, all.y=T)
head(pm)

#calculate trt estimates soil C
tests <- NULL
for(i in unique(pm$trt)){
  sub <- pm[pm$trt == i, ]
  tests.i <- coef(summary(lmer(g_per_m2 ~ 1 + (1|line:site), data = sub, )))[1,1:2]
  tests.i <- data.frame(trt = i, t(tests.i))
  tests <- rbind(tests, tests.i) ; rm(tests.i, sub)
} ; rm(i)
tests
