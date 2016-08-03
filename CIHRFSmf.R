# Analysis of CIHR Foundation Scheme 2014-2015 & 2015-2016 funding success rates
# by male/female and early-career/non-early-career applicants.

# Prepared May 21, 2016.
# Updated Aug 3, 2016.

#### CLEAN WORKSPACE ####

rm(list=ls())

#### LOAD LIBRARIES ####

library(car) 

#### FUNCTIONS ####

# not applicable here

#### LOAD DATA ####

# Data for 2014-2015 competition

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
X <- matrix(c(235,324,8,15),nrow=2,ncol=2,byrow=T); X; prop.table(X,2); rm(X)

# non-ECI
X <- matrix(c(505-235,861-324,41-8,109-15),nrow=2,ncol=2,byrow=T); X; prop.table(X,1); rm(X)
X <- matrix(c(505-235,861-324,41-8,109-15),nrow=2,ncol=2,byrow=T); X; prop.table(X,2); rm(X)

# M/F, ECI/established, funded/not funded

mf <- c(rep("F",times=505),rep("M",times=861))
careerstage <- c(rep("ECI",times=235),rep("est",times=505-235),rep("ECI",times=324),rep("est",times=861-324))
funded <- c(rep(1,times=8),rep(0,times=235-8), # ECI F
            rep(1,times=41-8),rep(0,times=505-235-(41-8)), # non-ECI (established) F
            rep(1,times=15),rep(0,times=324-15), # ECI M
            rep(1,times=109-15),rep(0,times=861-324-(109-15))) # non-ECI (established) M
FS <- data.frame(mf,careerstage,funded)
X<-table(FS$funded,FS$mf,FS$careerstage); X; rm(X)

# overall funding 2014-2015

X <- table(FS$mf,FS$funded); X; prop.table(X,1); rm(X)

# overall proportion of women 2014-2015

X <- table(FS$mf,FS$careerstage); X; prop.table(X,2); rm(X)

# Data for 2015-2016 competition from http://www.cihr-irsc.gc.ca/e/49854.html
# ECI F: 97/265 submitted, 15/33 funded
# non-ECI F: 185/645 submitted, 14/87 funded

mf2 <- c(rep("F",times=97+185),rep("M",times=265-97+645-185))
careerstage2 <- c(rep("ECI",times=97),rep("est",times=185),rep("ECI",times=265-97),rep("est",times=645-185))
funded2 <- c(rep(1,times=15),rep(0,times=97-15), #ECI women
            rep(1,times=14),rep(0,times=185-14), #established women
            rep(1,times=33-15),rep(0,times=265-97-(33-15)), #ECI men
            rep(1,times=87-14),rep(0,times=645-185-(87-14))) #established men
FS2 <- data.frame(mf2,careerstage2,funded2)
X<-table(FS2$funded2,FS2$mf2,FS2$careerstage2); X; rm(X)

#### RESPONSE RATE ####

# not applicable here

#### CLEAN DATA ####

# not applicable here

#### CLEAR PRELIMINARY DATASETS NOT USING ####

# not applicable here

#### DERIVED VARIABLES ####

# not applicable here

#### ANALYSES ####

# 2014-2015 analyses

FSglmfit <-glm(formula = funded ~ mf+careerstage, family = binomial, data = FS)
summary(FSglmfit)
exp(coef(FSglmfit))[-1] # odds ratio (without intercept)
exp(confint(FSglmfit))[-1,] # 95% CI odds ratio (without intercept)
vif(FSglmfit) # not expecting any problems given these data but let's double check, requires package car

# success rates 2014-2015

X <- table(FS$mf,FS$funded,FS$careerstage); X; rm(X)
X <- table(FS$mf,FS$careerstage); X; rm(X)

# non-ECI men 18%
94/537*100

# non-ECI women 12%
33/270*100

# ECI men 5%
15/324*100

# ECI women 3%
8/235*100

# pooling by career stage
(94+15)/(537+324) # M 13%
(33+8)/(270+235) # F 8%

# 2015-2016 analysis
# Need to model this differently because of likely interaction
# For comprehensibility, rather than reporting OR & 95% CI for an interaction term,
# going to use approach used by Tamblyn et al.: http://cmajopen.ca/content/4/2/E213.full

# Create categorical variable: female ECI, female established, male ECI, male established

FS2$sc2 <- ifelse (FS2$mf2 == "F" & FS2$careerstage2 == "ECI", "F-ECI",
                   ifelse (FS2$mf2 == "F" & FS2$careerstage2 == "est", "F-est",
                           ifelse (FS2$mf2 == "M" & FS2$careerstage2 == "ECI", "M-ECI",
                                   ifelse (FS2$mf2 == "M" & FS2$careerstage2 == "est", "M-est","ERROR"))))
table(FS2$mf2,FS2$sc2) # checks out
table(FS2$careerstage2,FS2$sc2) # checks out

# GLM model with single predictor

FS2$sc2 <- as.factor(FS2$sc2)
FS2$sc2 <- relevel(FS2$sc2, ref = "M-est") # sets reference as established (mid/senior) men
FS2glmfit2 <-glm(formula = funded2 ~ sc2, family = binomial, data = FS2)
summary(FS2glmfit2)
exp(coef(FS2glmfit2))[-1] # odds ratio (without intercept)
exp(confint(FS2glmfit2))[-1,] # 95% CI odds ratio (without intercept)

# success rates 2015-2016

# non-ECI men 16%
87-14
645-185
73/460*100

# non-ECI women 8%
14/185*100

# ECI men 11%
33-15
265-97
18/168*100

# ECI women 15%
15/97*100

# pooling by career stage
(73+18)/(460+168) # M 14%
(14+15)/(185+97) # F 10%

# for reference: PScheme spring 2016 (data from http://www.cihr-irsc.gc.ca/e/49852.html)
(491-150)/(856+1662+1283-1228) # M 13%
150/1228 # F 12%

#### GRAPHICS FOR PAPER ####

# not applicable here
