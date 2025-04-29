##Set working directory (Ellen's computer)
setwd("C:/Users/elwel/OneDrive/Desktop/AppraisingGrazing")
##Set working directory (Julie's computer)
setwd("/Users/julierebh/Downloads/AppraisingGrazing")

##load libraries
#install.packages("lsr")
library(rcompanion)
library(lsr)

############################H1
##load data
est <- read.csv("outputs/SiteMonthLevel.csv")
head(est)

bi  <- est[ which(est$trt=='bison'), ]
ug  <- est[ which(est$trt=='ungrazed'), ]
ca  <- est[ which(est$trt=='cattle'), ]
trtpd  <- est[ which(est$trt=='trtpd'), ]
untrtpd  <- est[ which(est$trt=='untrtpd'), ]

################################## properties split up
bi_sp  <- est[ which(est$trt=='bison' & est$property=='SunPrairie'), ]
bi_wr  <- est[ which(est$trt=='bison' & est$property=='WhiteRock'), ]
bi_df  <- est[ which(est$trt=='bison' & est$property=='DryFork'), ]

trtpd_sp  <- est[ which(est$trt=='trtpd' & est$property=='SunPrairie'), ]
untrtpd_wr  <- est[ which(est$trt=='untrtpd' & est$property=='WhiteRock'), ]
untrtpd_df  <- est[ which(est$trt=='untrtpd' & est$property=='DryFork'), ]

##########SOIL C
################################### compare trt pd in Sun Prairie to bison in Sun Prairie

#####make comparison df
bt <- data.frame(bi_sp$trt, bi_sp$percC)
colnames(bt) <- c("Trt","percC")
tt <- data.frame(trtpd_sp$trt, trtpd_sp$percC)
colnames(tt) <- c("Trt","percC")
btt <- rbind(bt,bt,bt,tt)

#### test differences
cohensD(percC ~ Trt, data = btt, method = "paired")
t.test(percC ~ Trt, data = btt, paired = TRUE)


################################### compare untrt pd in White Rock & Dry Fork 
################################### to bison in White Rock and Dry Fork

#####make comparison df
bt_wr <- data.frame(bi_wr$trt, bi_wr$percC)
colnames(bt_wr) <- c("Trt","percC")
bt_df <- data.frame(bi_df$trt, bi_df$percC)
colnames(bt_df) <- c("Trt","percC")
ut_wr <- data.frame(untrtpd_wr$trt, untrtpd_wr$percC)
colnames(ut_wr) <- c("Trt","percC")
ut_df <- data.frame(untrtpd_df$trt, untrtpd_df$percC)
colnames(ut_df) <- c("Trt","percC")
but <- rbind(bt_wr,bt_df,bt_df,ut_wr,ut_df)

#### test differences
cohensD(percC ~ Trt, data = but, method = "paired")
t.test(percC ~ Trt, data = but, paired = TRUE)

###############comparison plot C
trtpd_c <- data.frame(bi_sp$property, bi_sp$percC,trtpd_sp$percC) ## make comparison df for SP
colnames(trtpd_c) <- c("Property","bi_percC", "trtpd_percC")  ## rename cols for clarity

untrtpd_c_wr <- data.frame(bi_wr$property, bi_wr$percC,untrtpd_wr$percC) ## make comparison df for WR
colnames(untrtpd_c_wr) <- c("Property","bi_percC", "untrtpd_percC")
untrtpd_c_df <- data.frame(bi_df$property, bi_df$percC,untrtpd_df$percC) ## make comparison df for DF
colnames(untrtpd_c_df) <- c("Property","bi_percC", "untrtpd_percC")
untrtpd_c <- rbind(untrtpd_c_wr,untrtpd_c_df) ## make comparison df for all untrtPD

plot(trtpd_c$bi_percC, trtpd_c$trtpd_percC, pch = 16, xlim=c(1.5,5),
     ylim=c(1,15), xlab="Bison soil %C", ylab="Prairie Dog Town soil %C")
abline(0, 1, col="grey60", lwd=2, )
points(trtpd_c$bi_percC, trtpd_c$trtpd_percC, pch = 16, cex =1.5)
points(untrtpd_c$bi_percC, untrtpd_c$untrtpd_percC, pch = 17, cex =1.5, col=2)
legend("topleft", col=c(2,1), pch=c(17,16), legend=c("Untreated", "Insecticide Treated"), bty="n", cex=1.3)


##########SOIL N
################################### compare trt pd in Sun Prairie to bison in Sun Prairie

#####make comparison df
bt <- data.frame(bi_sp$trt, bi_sp$percN)
colnames(bt) <- c("Trt","percN")
tt <- data.frame(trtpd_sp$trt, trtpd_sp$percN)
colnames(tt) <- c("Trt","percN")
btt <- rbind(bt,bt,bt,tt)

#### test differences
cohensD(percN ~ Trt, data = btt, method = "paired")
t.test(percN ~ Trt, data = btt, paired = TRUE)


################################### compare untrt pd in White Rock & Dry Fork 
################################### to bison in White Rock and Dry Fork

#####make comparison df
bt_wr <- data.frame(bi_wr$trt, bi_wr$percN)
colnames(bt_wr) <- c("Trt","percN")
bt_df <- data.frame(bi_df$trt, bi_df$percN)
colnames(bt_df) <- c("Trt","percN")
ut_wr <- data.frame(untrtpd_wr$trt, untrtpd_wr$percN)
colnames(ut_wr) <- c("Trt","percN")
ut_df <- data.frame(untrtpd_df$trt, untrtpd_df$percN)
colnames(ut_df) <- c("Trt","percN")
but <- rbind(bt_wr,bt_df,bt_df,ut_wr,ut_df)

#### test differences
cohensD(percN ~ Trt, data = but, method = "paired")
t.test(percN ~ Trt, data = but, paired = TRUE)

###############comparison plot N
trtpd_c <- data.frame(bi_sp$property, bi_sp$percN,trtpd_sp$percN) ## make comparison df for SP
colnames(trtpd_c) <- c("Property","bi_percN", "trtpd_percN")  ## rename cols for clarity

untrtpd_c_wr <- data.frame(bi_wr$property, bi_wr$percN,untrtpd_wr$percN) ## make comparison df for WR
colnames(untrtpd_c_wr) <- c("Property","bi_percN", "untrtpd_percN")
untrtpd_c_df <- data.frame(bi_df$property, bi_df$percN,untrtpd_df$percN) ## make comparison df for DF
colnames(untrtpd_c_df) <- c("Property","bi_percN", "untrtpd_percN")
untrtpd_c <- rbind(untrtpd_c_wr,untrtpd_c_df) ## make comparison df for all untrtPD

plot(trtpd_c$bi_percN, trtpd_c$trtpd_percN, pch = 16, xlim=c(0,1.2),
     ylim=c(0,1.2), xlab="Bison soil %N", ylab="PD soil %N")
abline(0, 1, col="grey60", lwd=2, )
points(trtpd_c$bi_percN, trtpd_c$trtpd_percN, pch = 16, cex =1.5)
points(untrtpd_c$bi_percN, untrtpd_c$untrtpd_percN, pch = 17, cex =1.5, col=2)

##


