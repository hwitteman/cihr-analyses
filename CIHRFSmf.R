# Analysis of CIHR Foundation Scheme 2014-2015 funding success rates
# by male/female and early-career/non-early-career applicants.

# Prepared May 21, 2016.

#### CLEAN WORKSPACE ####

rm(list=ls())

#### LOAD LIBRARIES ####

library(car) 

#### FUNCTIONS ####

# not applicable here

#### LOAD DATA ####

# Estimates from CIHR report to university delegates Sep 2015, pages 12 and 13

# ECIs (page 13)
.42*559 #235 ECI F applied S1
.58*559 #324 ECI M applied S1
.35*23 #8 ECI F funded
.65*23 #15 ECI M funded

# Overall (page 12)
.37*1366 #505 F overall applied S1
.63*1366 #861 M overall applied S1
.27*150 #40.5 -> 41 F overall funded
.73*150 #109.5 -> 109 M overall funded
# N.B. using 41 and 109 rather than 40 and 110 is a conservative rounding choice, as it
# decreases the likelihood of observing a significant difference in M/F funding rates.

# ECI
X <- matrix(c(235,324,8,15),nrow=2,ncol=2,byrow=T); X; prop.table(X,1); rm(X)

# non-ECI
X <- matrix(c(505-235,861-324,41-8,109-15),nrow=2,ncol=2,byrow=T); X; prop.table(X,1); rm(X)

# M/F, ECI/established, funded/not funded

mf <- c(rep("F",times=505),rep("M",times=861))
careerstage <- c(rep("ECI",times=235),rep("est",times=505-235),rep("ECI",times=324),rep("est",times=861-324))
funded <- c(rep(1,times=8),rep(0,times=235-8), #ECI F
            rep(1,times=41-8),rep(0,times=505-235-(41-8)), #non-ECI (established) F
            rep(1,times=15),rep(0,times=324-15), #ECI M
            rep(1,times=109-15),rep(0,times=861-324-(109-15))) #non-ECI (established) M
FS <- data.frame(mf,careerstage,funded)
X <- table(FS$funded,FS$mf,FS$careerstage); X; rm(X)

#### RESPONSE RATE ####

# not applicable here

#### CLEAN DATA ####

# not applicable here

#### CLEAR PRELIMINARY DATASETS NOT USING ####

# not applicable here

#### DERIVED VARIABLES ####

# not applicable here

#### ANALYSES ####

# If we want to set M as reference, run line:
# FS$mf <- relevel(FS$mf, ref = "M")
# not using this here but allows to look at this from other direction if desired

FSglmfit <- glm(formula = funded ~ mf+careerstage, family = binomial, data = FS)
summary(FSglmfit)
exp(coef(FSglmfit))[-1] # odds ratio (without intercept)
exp(confint(FSglmfit))[-1,] # 95% CI odds ratio (without intercept)
vif(FSglmfit) # not expecting any problems given these data but let's double check, requires package car

#### GRAPHICS FOR PAPER ####

# not applicable here
