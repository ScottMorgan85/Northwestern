rm(list=ls())

#Name: Scott Morgan

#Class: PREDICT 422

#Section: 58

#Project: Final Project

# PREDICT 422 Practical Machine Learning

------------------------------------------------------------------------------------------------------------------------------------------------------------

# Course Project -  R Script File

#============================================================================================================================================================
# PACKAGE LOAD
#============================================================================================================================================================

#Load 
#install.packages("caret")
library(caret)
library(readr)
library(GGally)
library(corrplot)
library(tree)
library(ggplot2)
library(PerformanceAnalytics)
library(e1071)
library(psych)
library(gam)
library(outliers)
library(car)
library(gclus)
library(mgcv)
library(jtools)
library(randomForest)
library(mctest)
library(ggthemes)
library(class)
library(MASS)
library(gbm)
library(RSNNS)
library(leaps)
library(ROCR)
library(pROC)
library(pls)

#============================================================================================================================================================
# DATA LOAD
#============================================================================================================================================================
  
# Set working directory

setwd("C:/Users/SMorgan/Desktop/PREDICT 422 Final/SM/data")

charity <- read.csv(file="C:/Users/SMorgan/Desktop/PREDICT 422 Final/SM/data/charity.csv", header=TRUE, sep=",")

charity_raw=charity

charity_raw[charity_raw == ""] <- NA

charity_clean = charity_raw

charity.t = charity_clean

#PREPRARE FULL EDA DATA SET------------------------------------------------------------------------------------------------------------------------------------------------

data.full.eda=charity_clean

# str(data.full.eda)

#PREPRARE OTHER DATA SETS------------------------------------------------------------------------------------------------------------------------------------------------

#Train EDA
data.train.eda = charity_clean[charity_clean$part=="train",]

#Correlations with DAMT (DONR = 1)
data.train.damt=data.train.eda[data.train.eda$donr=="1",]

#Interactions
data.full.eda.inter=data.full.eda
data.full.eda.inter = na.omit(data.full.eda.inter)
data.full.eda.inter <- data.full.eda.inter[,c(7:8,10:23)]

#VIFs
data.full.eda.VIF <- data.full.eda[,c(2:23)]
data.full.eda.VIF=na.omit(data.full.eda.VIF)


#============================================================================================================================================================
# EXPLORATORY DATA ANALYSIS (EDA): FULL DATA SET
#============================================================================================================================================================

#SUMMARIZE FULL DATA SET ---------------------------------------------------------------------------------------------------------------------------------------------------
describe.by(data.full.eda)

#SETUP CORRELATIONS WITH DONR: FULL DATA SET----------------------------------------------------------------------------------------------------------------------------------
data.full.eda.corr.donr = na.omit(data.full.eda)
data.full.eda.corr.donr <- data.full.eda.corr.donr[,c(2:22)]
M1 <- cor(data.full.eda.corr.donr)

View(data.full.eda.corr.donr)
#SETUP CORRELATIONS WITH DAMT: FULL DATA SET----------------------------------------------------------------------------------------------------------------------------------
data.full.eda.corr.damt = na.omit(data.full.eda)
data.full.eda.corr.damt=data.full.eda.corr.damt[data.full.eda.corr.damt$donr=="1",]
data.full.eda.corr.damt = na.omit(data.full.eda.corr.damt)
data.full.eda.corr.damt <- data.full.eda.corr.damt[,c(2:21,23)]
M2 <- cor(data.full.eda.corr.damt)

#CORRELATION GRAPHS: FULL DATA SET----------------------------------------------------------------------------------------------------------------------------------

#Exhibit 1: Correlations with DONR (Left) and DAMT (Right)
par(mfrow=c(1,2))
corrplot(M1, method="circle", type="lower", order = "original", tl.col = "black", tl.srt = 45)
corrplot(M2, method="circle", type="lower", order = "original", tl.col = "black", tl.srt = 45)



#VARIABLE INTERACTIONS AND MULTICOLLINEARITY: FULL DATA SET----------------------------------------------------------------------------------------------------------------------------------------------

##Exhibit 2: Intercorrelations and Additional Diagnostics of Variables
variCorr<-chart.Correlation(data.full.eda.inter)

z <-data.full.eda.VIF[,1:20]
imcdiag(x = z,data.full.eda.VIF$donr)
imcdiag(x = z,data.full.eda.VIF$damt)

##Exhibit 3: Variance Inflation Factors
mc.plot(z, data.full.eda.VIF$donr, Inter = FALSE, vif = 10, ev = 0.01)

#Logistic Interactions----------------------------------------------------------------------------------------------------------------------------------------------
attach(data.train.eda)
# par(mfrow=c(2,2))
# plot(logit_fit5)

#Logistic Regression: Wealth Status Interactions
# plow
# avhv
# incm
# inca

logit_fitplow1 <- lm(donr ~ plow * avhv, family = binomial)
logit_fitplow2 <- lm(donr ~ plow * incm, family = binomial)
logit_fitplow3 <- lm(donr ~ plow * inca, family = binomial)
logit_fitplow4 <- lm(donr ~ avhv * inca, family = binomial)
logit_fitplow5 <- lm(donr ~ plow * avhv * incm * inca, family = binomial)

summ(logit_fitplow1)
summ(logit_fitplow2)
summ(logit_fitplow3)
summ(logit_fitplow4)
summ(logit_fitplow5)

#Logistic Regression: Historical Gifting Interaction
# npro
# agif
# rgif
# lgif
# tgif

logit_fitnpro1 <- lm(donr ~ npro * agif, family = binomial)
logit_fitnpro2 <- lm(donr ~ npro * rgif, family = binomial)
logit_fitnpro3 <- lm(donr ~ npro * lgif, family = binomial)
logit_fitnpro4 <- lm(donr ~ npro * tgif, family = binomial)
logit_fitnpro5 <- lm(donr ~ npro * agif * rgif * tgif, family = binomial)
logit_fit_agif1 <- lm(donr ~ agif * rgif, family = binomial)
logit_fit_agif2 <- lm(donr ~ agif * lgif, family = binomial)
logit_fit_agif3 <- lm(donr ~ agif * tgif, family = binomial)

summ(logit_fitnpro1)
summ(logit_fitnpro2)
summ(logit_fitnpro3)
summ(logit_fitnpro4)
summ(logit_fitnpro5)
summ(logit_fit_agif1)
summ(logit_fit_agif2)
summ(logit_fit_agif3)

#Multiple Regression: Wealth Status Interactions
lm_fitplow1 <- lm(damt ~ plow * avhv, data = data.train.eda)
lm_fitplow2 <- lm(damt ~ plow * incm, data = data.train.eda)
lm_fitplow3 <- lm(damt ~ plow * inca, data = data.train.eda)
lm_fitplow4 <- lm(damt ~ avhv * inca, data = data.train.eda)
lm_fitplow5 <- lm(damt ~ plow * avhv * incm * inca, data = data.train.eda)

summ(lm_fitplow1)
summ(lm_fitplow2)
summ(lm_fitplow3)
summ(lm_fitplow4)
summ(lm_fitplow5)

#Multiple Regression:Historical Gifting Interaction
lm_fitnpro1 <- lm(donr ~ npro * agif, data = data.train.eda)
lm_fitnpro2 <- lm(donr ~ npro * rgif, data = data.train.eda)
lm_fitnpro3 <- lm(donr ~ npro * lgif, data = data.train.eda)
lm_fitnpro4 <- lm(donr ~ npro * tgif, data = data.train.eda)
lm_fitnpro5 <- lm(donr ~ npro * agif * rgif * tgif, data = data.train.eda)
lm_fit_agif1 <- lm(donr ~ agif * rgif, data = data.train.eda)
lm_fit_agif2 <- lm(donr ~ agif * lgif, data = data.train.eda)
lm_fit_agif3 <- lm(donr ~ agif * tgif, data = data.train.eda)

summ(lm_fitnpro1)
summ(lm_fitnpro2)
summ(lm_fitnpro3)
summ(lm_fitnpro4)
summ(lm_fitnpro5)
summ(lm_fit_agif1)
summ(lm_fit_agif2)
summ(lm_fit_agif3)

#Logistic Regression: Child Interactions
logit_fitchld1 <- glm(donr ~ chld * avhv, family = binomial)
logit_fitchld2 <- glm(donr ~ chld * incm, family = binomial)
logit_fitchld3 <- glm(donr ~ chld * inca, family = binomial)
logit_fitchld4 <- glm(donr ~ chld * inca, family = binomial)
logit_fitchld5 <- glm(donr ~ chld * npro, family = binomial)
logit_fitchld6 <- glm(donr ~ chld * plow * avhv * incm * inca, family = binomial)
logit_fitchld7 <- glm(donr ~ chld * wrat, family = binomial)
logit_fitchld8 <- glm(donr ~ chld * hinc, family = binomial)
logit_fitchld9 <- glm(donr ~ chld * reg2, family = binomial)
logit_fitchld10 <- glm(donr ~ chld * home, family = binomial)
logit_fitchld11 <- glm(donr ~ chld * genf, family = binomial)
logit_fitchld12 <- glm(donr ~ chld * wrat * home * reg2, family = binomial)
logit_fitchld13 <- glm(donr ~ chld * wrat * home, family = binomial)

summ(logit_fitchld1)
summ(logit_fitchld2)
summ(logit_fitchld3)
summ(logit_fitchld4)
summ(logit_fitchld5)
summ(logit_fitchld6)
summ(logit_fitchld7)
summ(logit_fitchld8)
summ(logit_fitchld9)
summ(logit_fitchld10)
summ(logit_fitchld11)
summ(logit_fitchld12)
summ(logit_fitchld13)


#Multiple Regression: Child Interactions
lm_fitchld1 <- lm(damt ~ chld * avhv, data = data.train.eda)
lm_fitchld2 <- lm(damt ~ chld * incm, data = data.train.eda)
lm_fitchld3 <- lm(damt ~ chld * inca, data = data.train.eda)
lm_fitchld4 <- lm(damt ~ chld * inca, data = data.train.eda)
lm_fitchld5 <- lm(damt ~ chld * npro, data = data.train.eda)
lm_fitchld6 <- lm(damt ~ chld * plow * avhv * incm * inca, data = data.train.eda)
lm_fitchld7 <- lm(damt ~ chld * wrat, data = data.train.eda)
lm_fitchld8 <- lm(damt ~ chld * hinc, data = data.train.eda)
lm_fitchld9 <- lm(damt ~ chld * reg2, data = data.train.eda)
lm_fitchld10 <- lm(damt ~ chld * home, data = data.train.eda)
lm_fitchld11 <- lm(damt ~ chld * genf, data = data.train.eda)
lm_fitchld12 <- lm(damt ~ chld * wrat * home * reg2, data = data.train.eda)
lm_fitchld13 <- lm(damt ~ chld * wrat * home, data = data.train.eda)

summ(lm_fitchld1)
summ(lm_fitchld2)
summ(lm_fitchld3)
summ(lm_fitchld4)
summ(lm_fitchld5)
summ(lm_fitchld6)
summ(lm_fitchld7)
summ(lm_fitchld8)
summ(lm_fitchld9)
summ(lm_fitchld10)
summ(lm_fitchld11)
summ(lm_fitchld12)
summ(lm_fitchld13)

#COUNT VARIABLES TRANSFORMATION ANALYSIS: TRAIN DATA SET--------------------------------------------------------------------------------------------------------------------------------------

##Exhibit 4 Numerical Variable Outlier and Normality Analysis
dev.off()

par(mfrow=c(2,3))

boxplot(data.train.eda$chld, main = "chld")
boxplot(data.train.eda$hinc, main = "hinc")
boxplot(data.train.eda$wrat, main = "wrat")

qqnorm(data.train.eda$chld, main = "chld QQ"); qqline(data.train.eda$chld, col = 2,lwd=2,lty=2)
qqnorm(data.train.eda$hinc, main  ="hinc QQ"); qqline(data.train.eda$hinc, col = 2,lwd=2,lty=2)
qqnorm(data.train.eda$wrat, main  ="wrat QQ"); qqline(data.train.eda$wrat, col = 2,lwd=2,lty=2)


#CONTINUOUS VARIABLES TRANSFORMATION ANALYSIS: TRAIN DATA SET----------------------------------------------------------------------------------------------------------------------------------

##Exhibit 5 Categorical Variable Outlier and Normality Analysis

par(mfrow=c(2,6))
boxplot(data.train.eda$avhv, main= "avhv")
boxplot(data.train.eda$incm, main= "incm")
boxplot(data.train.eda$inca, main= "inca")
boxplot(data.train.eda$plow, main= "plow")
boxplot(data.train.eda$npro, main= "npro")
boxplot(data.train.eda$tgif, main= "tgif")

qqnorm(data.train.eda$avhv,main= "avhv"); qqline(data.train.eda$avhv, col = 2,lwd=2,lty=2)
qqnorm(data.train.eda$incm,main= "incm"); qqline(data.train.eda$incm, col = 2,lwd=2,lty=2)
qqnorm(data.train.eda$inca,main= "inca"); qqline(data.train.eda$inca, col = 2,lwd=2,lty=2)
qqnorm(data.train.eda$plow,main= "plow"); qqline(data.train.eda$plow, col = 2,lwd=2,lty=2)
qqnorm(data.train.eda$npro,main= "npro"); qqline(data.train.eda$npro, col = 2,lwd=2,lty=2)
qqnorm(data.train.eda$tgif,main= "tgif"); qqline(data.train.eda$tgif, col = 2,lwd=2,lty=2)

par(mfrow=c(2,5))
boxplot(data.train.eda$lgif, main= "lgif")
boxplot(data.train.eda$rgif, main= "rgif")
boxplot(data.train.eda$tdon, main= "tdon")
boxplot(data.train.eda$tlag, main= "tlag")
boxplot(data.train.eda$agif, main= "agif" )

qqnorm(data.train.eda$lgif,main= "lgif"); qqline(data.train.eda$lgif, col = 2,lwd=2,lty=2)
qqnorm(data.train.eda$rgif,main= "rgif"); qqline(data.train.eda$rgif, col = 2,lwd=2,lty=2)
qqnorm(data.train.eda$tdon,main= "tdon"); qqline(data.train.eda$tdon, col = 2,lwd=2,lty=2)
qqnorm(data.train.eda$tlag,main= "tlag"); qqline(data.train.eda$tlag, col = 2,lwd=2,lty=2)
qqnorm(data.train.eda$agif,main= "agif"); qqline(data.train.eda$agif, col = 2,lwd=2,lty=2)

par(mfrow=c(1,1))


#============================================================================================================================================================
# EXPLORATORY DATA ANALYSIS (EDA): Classification Model DONR: Identifying Variables of Interest (Train). 
#============================================================================================================================================================

#DONR:Binary Variables----------------------------------------------------------------------------------------------------------------------------------------------

##Table 4: Binary Variables - Frequency of Donation
xtabs(~ reg1 + donr, data.train.eda)
xtabs(~ reg2 + donr, data.train.eda) 
xtabs(~ reg3 + donr, data.train.eda) 
xtabs(~ reg4 + donr, data.train.eda) 
xtabs(~ home + donr, data.train.eda) 
xtabs(~ genf + donr, data.train.eda)

#DONR: COUNT AND CONTINUOUS VARIABLES----------------------------------------------------------------------------------------------------------------------------------------------

##Table 5: Mean Analysis of Numeric Variables by DONR
tapply(data.train.eda$chld,data.train.eda$donr, mean)
tapply(data.train.eda$hinc,data.train.eda$donr, mean)
tapply(data.train.eda$wrat,data.train.eda$donr, mean)

tapply(data.train.eda$avhv,data.train.eda$donr, mean)
tapply(data.train.eda$incm,data.train.eda$donr, mean)
tapply(data.train.eda$inca,data.train.eda$donr, mean)
tapply(data.train.eda$plow,data.train.eda$donr, mean)
tapply(data.train.eda$npro,data.train.eda$donr, mean)
tapply(data.train.eda$tgif,data.train.eda$donr, mean)
tapply(data.train.eda$lgif,data.train.eda$donr, mean)
tapply(data.train.eda$rgif,data.train.eda$donr, mean)
tapply(data.train.eda$tdon,data.train.eda$donr, mean)
tapply(data.train.eda$tlag,data.train.eda$donr, mean)
tapply(data.train.eda$agif,data.train.eda$donr, mean)


#============================================================================================================================================================
# EXPLORATORY DATA ANALYSIS (EDA): Prediction Model DAMT: Identifying Variables of Interest (Train).  
#============================================================================================================================================================

#DAMT:ANALYZE HIGH CORRELATION CATEGORICAL AND COUNT----------------------------------------------------------------------------------------------------------------------------------------------


##Exhibit 5: Scatterplots for Variables with Highest Correlation to DAMT
ggplot(data.train.damt, aes(home, damt)) +  
  facet_grid(.~wrat) +
  geom_jitter(aes(color = chld),alpha = 0.4) +  
  ggtitle("x= Homeowner, y= Prediction Response Variable (Donation Amount in $), z = Wealth Rating , t = Number of children") +  
  theme_light()

ggplot(data.train.damt, aes(chld, damt)) +  
  facet_grid(.~wrat) +
  geom_jitter(aes(color = home ),alpha = 0.9) +  
  ggtitle("x= Number of Children, y= Prediction Response Variable (Donation Amount in $), z = Wealth Rating , t = Home") + 
  theme_light()

ggplot(data.train.damt, aes(reg2, damt)) +  
  facet_grid(.~chld) +
  geom_jitter(aes(color = agif ),alpha = 0.9) +  
  ggtitle("x= Region 2, y= Prediction Response Variable (Donation Amount in $), z = Number of children , t = Average dollar amount of gifts to date") + 
  theme_light()

ggplot(data.train.damt, aes(rgif, damt)) +  
  facet_grid(.~reg2) +
  geom_jitter(aes(color =genf),alpha = 0.9) +  
  ggtitle("x= Dollar amount of most recent gift, y= Prediction Response Variable (Donation Amount in $), z = Region 2 , t = Gender") +  
  theme_light()


#============================================================================================================================================================
# EXPLORATORY DATA ANALYSIS (EDA):VARIABLE SELECTION
#============================================================================================================================================================

#VARIABLE IMPORTANCE: Classification Analysis (donr)---------------------------------------------------------------------------------------------

##Exhibit 6: Classification Model Variable Importance (DONR)
set.seed(123)
fit_donr=randomForest(factor(donr)~.-damt-ID-part, data=data.train.eda
                      ,ntree=100)

(VI_F=importance(fit_donr))


#VARIABLE IMPORTANCE: Prediction Analysis (damt)---------------------------------------------------------------------------------------------

##Exhibit 7: Predictive Model Variable Importance (DAMT)
set.seed(123)
fit_damt=randomForest(factor(damt)~.-donr-ID-part, data=data.train.eda
                      ,ntree=100)

(VI_F=importance(fit_damt))


#============================================================================================================================================================
# DATA PREPARATION: #OUTLIER DETECTION
#============================================================================================================================================================

#OUTLIER DETECTECTION----------------------------------------------------------------------------------------------------------------

charity.t["avhv_IMP"]=charity.t$avhv
Q1avhv <- quantile(charity.t$avhv, 0.25)
Q3avhv <- quantile(charity.t$avhv, 0.75)
IQRavhv <- Q3avhv - Q1avhv
IQR.1.5avhv <- IQRavhv*1.5
upperThresholdavhv <- Q3avhv + IQR.1.5avhv
upperThresholdavhv
lowerThresholdavhv <- Q1avhv - IQR.1.5avhv
lowerThresholdavhv
sum(charity.t$avhv > upperThresholdavhv)
sum(charity.t$avhv < lowerThresholdavhv)

charity.t["incm_IMP"]=charity.t$incm
Q1incm <- quantile(charity.t$incm, 0.25)
Q3incm <- quantile(charity.t$incm, 0.75)
IQRincm <- Q3incm - Q1incm
IQR.1.5incm <- IQRincm*1.5
upperThresholdincm <- Q3incm + IQR.1.5incm 
upperThresholdincm
lowerThresholdincm <- Q1incm - IQR.1.5incm
lowerThresholdincm
sum(charity.t$incm > upperThresholdincm)
sum(charity.t$incm < lowerThresholdincm)

charity.t["inca_IMP"]=charity.t$inca
Q1inca <- quantile(charity.t$inca, 0.25)
Q3inca <- quantile(charity.t$inca, 0.75)
IQRinca <- Q3inca - Q1inca
IQR.1.5inca <- IQRinca*1.5
upperThresholdinca <- Q3inca + IQR.1.5inca
upperThresholdinca 
lowerThresholdinca <- Q1inca - IQR.1.5inca
lowerThresholdinca
sum(charity.t$inca > upperThresholdinca)
sum(charity.t$inca < lowerThresholdinca)

charity.t["plow_IMP"]=charity.t$plow
Q1plow <- quantile(charity.t$plow, 0.25)
Q3plow <- quantile(charity.t$plow, 0.75)
IQRplow <- Q3plow - Q1plow
IQR.1.5plow <- IQRplow*1.5
upperThresholdplow <- Q3plow + IQR.1.5plow
upperThresholdplow 
lowerThresholdplow <- Q1plow - IQR.1.5plow
lowerThresholdplow
sum(charity.t$plow > upperThresholdplow)
sum(charity.t$plow < lowerThresholdplow)

charity.t["npro_IMP"]=charity.t$npro
Q1npro <- quantile(charity.t$npro, 0.25)
Q3npro <- quantile(charity.t$npro, 0.75)
IQRnpro <- Q3npro - Q1npro
IQR.1.5npro <- IQRnpro*1.5
upperThresholdnpro <- Q3npro + IQR.1.5npro
upperThresholdnpro 
lowerThresholdnpro <- Q1npro - IQR.1.5npro
lowerThresholdnpro
sum(charity.t$npro > upperThresholdnpro)
sum(charity.t$npro < lowerThresholdnpro)

charity.t["tgif_IMP"]=charity.t$tgif
Q1tgif <- quantile(charity.t$tgif, 0.25)
Q3tgif <- quantile(charity.t$tgif, 0.75)
IQRtgif <- Q3tgif - Q1tgif
IQR.1.5tgif <- IQRtgif*1.5
upperThresholdtgif <- Q3tgif + IQR.1.5tgif
upperThresholdtgif 
lowerThresholdtgif <- Q1tgif - IQR.1.5tgif
lowerThresholdtgif
sum(charity.t$tgif > upperThresholdtgif)
sum(charity.t$tgif < lowerThresholdtgif)

charity.t["lgif_IMP"]=charity.t$lgif
Q1lgif <- quantile(charity.t$lgif, 0.25)
Q3lgif <- quantile(charity.t$lgif, 0.75)
IQRlgif <- Q3lgif - Q1lgif
IQR.1.5lgif <- IQRlgif*1.5
upperThresholdlgif <- Q3lgif + IQR.1.5lgif
upperThresholdlgif 
lowerThresholdlgif <- Q1lgif - IQR.1.5lgif
lowerThresholdlgif
sum(charity.t$lgif > upperThresholdlgif)
sum(charity.t$lgif < lowerThresholdlgif)

charity.t["tdon_IMP"]=charity.t$tdon
Q1tdon <- quantile(charity.t$tdon, 0.25)
Q3tdon <- quantile(charity.t$tdon, 0.75)
IQRtdon <- Q3tdon - Q1tdon
IQR.1.5tdon <- IQRtdon*1.5
upperThresholdtdon <- Q3tdon + IQR.1.5tdon
upperThresholdtdon 
lowerThresholdtdon <- Q1tdon - IQR.1.5tdon
lowerThresholdtdon
sum(charity.t$tdon > upperThresholdtdon)
sum(charity.t$tdon < lowerThresholdtdon)

charity.t["tlag_IMP"]=charity.t$tlag
Q1tlag <- quantile(charity.t$tlag, 0.25)
Q3tlag <- quantile(charity.t$tlag, 0.75)
IQRtlag <- Q3tlag - Q1tlag
IQR.1.5tlag <- IQRtlag*1.5
upperThresholdtlag <- Q3tlag + IQR.1.5tlag
upperThresholdtlag 
lowerThresholdtlag <- Q1tlag - IQR.1.5tlag
lowerThresholdtlag
sum(charity.t$tlag > upperThresholdtlag)
sum(charity.t$tlag < lowerThresholdtlag)

charity.t["agif_IMP"]=charity.t$agif
Q1agif <- quantile(charity.t$agif, 0.25)
Q3agif <- quantile(charity.t$agif, 0.75)
IQRagif <- Q3agif - Q1agif
IQR.1.5agif <- IQRagif*1.5
upperThresholdagif <- Q3agif + IQR.1.5agif
upperThresholdagif 
lowerThresholdagif <- Q1agif - IQR.1.5agif
lowerThresholdagif
sum(charity.t$agif > upperThresholdagif)
sum(charity.t$agif < lowerThresholdagif)

#============================================================================================================================================================
# DATA PREPARATION: #VARIABLE TRANSFORMATION DETECTION
#============================================================================================================================================================

#COUNT VARIABLE TRANSFORMATION ANALYSIS: TRAIN DATA SET---------------------------------------------------------------------------------------------
skewness(data.train.eda$chld)
skewness(log(data.train.eda$chld)) 
skewness(log10(data.train.eda$chld))
skewness(exp(data.train.eda$chld))
skewness(abs(data.train.eda$chld))
skewness(sin(data.train.eda$chld))
skewness(asin(data.train.eda$chld))
skewness(sqrt(data.train.eda$chld))
skewness((data.train.eda$chld)^2)
skewness(logit(data.train.eda$chld,percents=TRUE))

skewness(data.train.eda$hinc) 
skewness(log(data.train.eda$hinc)) 
skewness(log10(data.train.eda$hinc))
skewness(exp(data.train.eda$hinc))
skewness(abs(data.train.eda$hinc))
skewness(sin(data.train.eda$hinc))
skewness(asin(data.train.eda$hinc))
skewness(sqrt(data.train.eda$hinc))
skewness((data.train.eda$hinc)^2)
skewness(logit(data.train.eda$hinc,percents=TRUE))

skewness(data.train.eda$wrat) 
skewness(log(data.train.eda$wrat)) 
skewness(log10(data.train.eda$wrat))
skewness(exp(data.train.eda$wrat))
skewness(abs(data.train.eda$wrat))
skewness(sin(data.train.eda$wrat))
skewness(asin(data.train.eda$wrat))
skewness(sqrt(data.train.eda$wrat))
skewness((data.train.eda$wrat)^2) 
skewness(logit(data.train.eda$wrat,percents=TRUE))

#CONTINUOUS VARIABLE TRANSFORMATION ANALYSIS: TRAIN DATA SET ----------------------------------------------------------------------------------------------------------------------------------------------------
skewness(data.train.eda$avhv)
skewness(log(data.train.eda$avhv)) 
skewness(log10(data.train.eda$avhv))
skewness(exp(data.train.eda$avhv))
skewness(abs(data.train.eda$avhv))
skewness(sin(data.train.eda$avhv))
skewness(asin(data.train.eda$avhv))
skewness(sqrt(data.train.eda$avhv))
skewness((data.train.eda$avhv)^2)
skewness(logit(data.train.eda$avhv,percents=TRUE))

skewness(data.train.eda$incm)
skewness(log(data.train.eda$incm)) 
skewness(log10(data.train.eda$incm))
skewness(exp(data.train.eda$incm))
skewness(abs(data.train.eda$incm))
skewness(sin(data.train.eda$incm))
skewness(asin(data.train.eda$incm))
skewness(sqrt(data.train.eda$incm))
skewness((data.train.eda$incm)^2)
skewness(logit(data.train.eda$incm,percents=TRUE))

skewness(data.train.eda$inca)
skewness(log(data.train.eda$inca)) 
skewness(log10(data.train.eda$inca))
skewness(exp(data.train.eda$inca))
skewness(abs(data.train.eda$inca))
skewness(sin(data.train.eda$inca))
skewness(asin(data.train.eda$inca))
skewness(sqrt(data.train.eda$inca))
skewness((data.train.eda$inca)^2)
skewness(logit(data.train.eda$inca,percents=TRUE))

skewness(data.train.eda$plow)
skewness(log(data.train.eda$plow)) 
skewness(log10(data.train.eda$plow))
skewness(exp(data.train.eda$plow))
skewness(abs(data.train.eda$plow))
skewness(sin(data.train.eda$plow))
skewness(asin(data.train.eda$plow))
skewness(sqrt(data.train.eda$plow))
skewness((data.train.eda$plow)^2)
skewness(logit(data.train.eda$plow,percents=TRUE))

skewness(data.train.eda$npro)
skewness(log(data.train.eda$npro)) 
skewness(log10(data.train.eda$npro))
skewness(exp(data.train.eda$npro))
skewness(abs(data.train.eda$npro))
skewness(sin(data.train.eda$npro))
skewness(asin(data.train.eda$npro))
skewness(sqrt(data.train.eda$npro))
skewness((data.train.eda$npro)^2)
skewness(logit(data.train.eda$npro,percents=TRUE))

skewness(data.train.eda$tgif)
skewness(log(data.train.eda$tgif)) 
skewness(log10(data.train.eda$tgif))
skewness(exp(data.train.eda$tgif))
skewness(abs(data.train.eda$tgif))
skewness(sin(data.train.eda$tgif))
skewness(asin(data.train.eda$tgif))
skewness(sqrt(data.train.eda$tgif))
skewness((data.train.eda$tgif)^2)
skewness(logit(data.train.eda$tgif,percents=TRUE))

skewness(data.train.eda$lgif)
skewness(log(data.train.eda$lgif)) #Log Transform
skewness(log10(data.train.eda$lgif))
skewness(exp(data.train.eda$lgif))
skewness(abs(data.train.eda$lgif))
skewness(sin(data.train.eda$lgif))
skewness(asin(data.train.eda$lgif))
skewness(sqrt(data.train.eda$lgif))
skewness((data.train.eda$lgif)^2)
skewness(logit(data.train.eda$lgif,percents=TRUE))

skewness(data.train.eda$rgif)
skewness(log(data.train.eda$rgif)) 
skewness(log10(data.train.eda$rgif))
skewness(exp(data.train.eda$rgif))
skewness(abs(data.train.eda$rgif))
skewness(sin(data.train.eda$rgif)) 
skewness(asin(data.train.eda$rgif))
skewness(sqrt(data.train.eda$rgif))
skewness((data.train.eda$rgif)^2)
skewness(logit(data.train.eda$rgif,percents=TRUE))

skewness(data.train.eda$tdon)
skewness(log(data.train.eda$tdon)) 
skewness(log10(data.train.eda$tdon))
skewness(exp(data.train.eda$tdon))
skewness(abs(data.train.eda$tdon))
skewness(sin(data.train.eda$tdon))
skewness(asin(data.train.eda$tdon))
skewness(sqrt(data.train.eda$tdon))
skewness((data.train.eda$tdon)^2)
skewness(logit(data.train.eda$tdon,percents=TRUE)) 

skewness(data.train.eda$tlag)
skewness(log(data.train.eda$tlag)) 
skewness(log10(data.train.eda$tlag))
skewness(exp(data.train.eda$tlag))
skewness(abs(data.train.eda$tlag))
skewness(sin(data.train.eda$tlag))
skewness(asin(data.train.eda$tlag))
skewness(sqrt(data.train.eda$tlag))
skewness((data.train.eda$tlag)^2)
skewness(logit(data.train.eda$tlag,percents=TRUE)) 

skewness(data.train.eda$agif)
skewness(log(data.train.eda$agif))   
skewness(log10(data.train.eda$agif))
skewness(exp(data.train.eda$agif))
skewness(abs(data.train.eda$agif))
skewness(sin(data.train.eda$agif))
skewness(asin(data.train.eda$agif))
skewness(sqrt(data.train.eda$agif))
skewness((data.train.eda$agif)^2)
skewness(logit(data.train.eda$agif,percents=TRUE)) 


#============================================================================================================================================================
# DATA PREPARATION: #OUTLIER HANDLING
#============================================================================================================================================================

#OUTLIER HANDLING : WHOLE DATA SET------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

charity.t$avhv_IMP [charity.t$avhv_IMP > upperThresholdavhv] <- upperThresholdavhv
#data.train.t$avhv_IMP [charity.t$avhv < lowerThresholdavhv] <- lowerThresholdavhv
charity.t$incm_IMP [charity.t$incm > upperThresholdincm]<- upperThresholdavhv
#charity.t$incm_IMP [charity.t$incm < lowerThresholdincm] <- lowerThresholdincm
charity.t$inca_IMP [charity.t$inca > upperThresholdinca] <- upperThresholdinca
#charity.t$inca_IMP [charity.t$inca < lowerThresholdinca] <- lowerThresholdinca
charity.t$plow_IMP [charity.t$plow > upperThresholdplow] <- upperThresholdplow
#charity.t$plow_IMP [charity.t$plow < lowerThresholdplow] <- lowerThresholdplow
charity.t$npro_IMP [charity.t$npro > upperThresholdnpro] <- upperThresholdnpro
#charity.t$npro_IMP [charity.t$npro < lowerThresholdnpro] <- lowerThresholdnpro
charity.t$tgif_IMP [charity.t$tgif > upperThresholdtgif] <- upperThresholdtgif
#charity.t$tgif_IMP[charity.t$tgif < lowerThresholdtgif] <- lowerThresholdtgif
charity.t$lgif_IMP [charity.t$lgif > upperThresholdlgif] <- upperThresholdlgif
#charity.t$lgif_IMP[charity.t$lgif < lowerThresholdlgif] <- lowerThresholdlgif
charity.t$tdon_IMP [charity.t$tdon > upperThresholdtdon] <- upperThresholdtdon
#charity.t$tdon_IMP [charity.t$tdon < lowerThresholdtdon] <- lowerThresholdtdon
charity.t$tlag_IMP [charity.t$tlag > upperThresholdtlag] <- upperThresholdtlag
#charity.t$tlag_IMP [charity.t$tlag < lowerThresholdtlag] <- lowerThresholdtlag
charity.t$agif_IMP [charity.t$agif > upperThresholdagif] <- upperThresholdagif
#charity.t$agif_IMP [charity.t$agif < lowerThresholdagif] <- lowerThresholdagif

#============================================================================================================================================================
# DATA PREPARATION: #VARIABLE TRANSFORMATION HANDLING
#============================================================================================================================================================

#VARIABLE TRANSFORMATION HANDLING : WHOLE DATA SET------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Counts
charity.t["chld_IMP"]=sqrt(charity.t$chld)
charity.t["wrat_IMP"]=(charity.t$wrat)^2

#Continuous
charity.t$avhv_IMP=log(charity.t$avhv_IMP)
charity.t$incm_IMP=log(charity.t$incm_IMP)
charity.t$inca_IMP=log(charity.t$inca_IMP)
charity.t$plow_IMP=sin(charity.t$plow_IMP)
charity.t$npro_IMP=sin(charity.t$npro_IMP)
charity.t$tgif_IMP=sqrt(charity.t$tgif_IMP) 
charity.t$lgif_IMP =log(charity.t$lgif_IMP) 
charity.t["rgif_IMP"]=log(charity.t$rgif)
charity.t$tdon_IMP=log(charity.t$tdon_IMP)
charity.t$tlag_IMP=log(charity.t$tlag_IMP)
charity.t$agif_IMP=log(charity.t$agif_IMP)

##charity.test.t = charity.t

## Write CSV in R
## write.csv(charity, file = "charity.csv",row.names=FALSE)
# write.csv(charity.t, file = "test.csv",row.names=FALSE)
## View(charity.t)

#Outliers and Transformations Done
charity.t=charity.t[c(1:6,8:9,22:37)]
charity.t <- charity.t[c( "ID",	 "reg1",	 "reg2",	 "reg3",	 "reg4",	 "home",	 "chld_IMP",	 "hinc",	 "genf",	 "wrat_IMP",	 "avhv_IMP",	"incm_IMP",	 "inca_IMP",	 "plow_IMP",	 "npro_IMP",	 "tgif_IMP",	 "lgif_IMP",	 "rgif_IMP",	 "tdon_IMP",	 "tlag_IMP",	 "agif_IMP",	 "donr",	 "damt",	 "part")]

#View(charity.t)

# describe.by(charity.t)

#=======================================================================================================
# DATA PREPARATION: STANDARDIZATION OF CLEAN DATA SET
#============================================================================================================================================================

data.train <- charity.t[charity.t$part=="train",]
x.train <- data.train[,2:21]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity.t$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity.t$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)

#============================================================================================================================================================
# MODEL PRODUCTION: Classification Models: LOGISTISTIC REGRESSION
#============================================================================================================================================================

#CLASSIFICATION MODEL: LOGISTIC REGRESSION: BASE --------------------------------------------------------------------------------------------- ---------------------------------------------------------------------------------------------
model.logb <- glm(donr ~ . , 
                  data = as.data.frame(data.train.std.c), family=binomial("logit"))

summary(model.logb)

post.valid.logb <- predict(model.logb, data.valid.std.c, type="response") # n.valid post probs
profit.logb <- cumsum(14.5*c.valid[order(post.valid.logb, decreasing=T)]-2)
n.mail.valid <- which.max(profit.logb) # Number of mailings that maximizes profits
c(n.mail.valid, max(profit.logb)) #Number of mailings and maximum profit
cutoff.logb <- sort(post.valid.logb, decreasing=T)[n.mail.valid+1] # Set cutoff based on n.mail.valid
chat.valid.logb <- ifelse(post.valid.logb>cutoff.logb, 1, 0) # Mail to everyone above the cutoff
table(chat.valid.logb, c.valid)


#CLASSIFICATION MODEL: LOGISTIC REGRESSION: REDUCED --------------------------------------------------------------------------------------------- ---------------------------------------------------------------------------------------------

model.log1 <- glm(donr ~ chld_IMP*home + hinc + tgif_IMP + tdon_IMP + npro_IMP ,
                  data.train.std.c, family=binomial("logit"))

post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") 
profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
n.mail.valid <- which.max(profit.log1) 
c(n.mail.valid, max(profit.log1)) 
cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] 
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) 
table(chat.valid.log1, c.valid)



#============================================================================================================================================================
# MODEL PRODUCTION: Classification Models: LOGISTIC REGRESSION GAM
#============================================================================================================================================================


#CLASSIFICATION MODEL: LOGISTIC REGRESSION GAM: BASE ---------------------------------------------------------------------------------------------
library(splines)

model.gamb <- gam(I(donr == 1) ~ reg1 + reg2 + reg3 + reg4 + home
                  + ns(chld_IMP,df=10) + hinc + genf + wrat_IMP + avhv_IMP + incm_IMP + inca_IMP + plow_IMP + npro_IMP
                  + tgif_IMP + lgif_IMP + rgif_IMP + tdon_IMP + tlag_IMP + agif_IMP, family = binomial,data = data.train.std.c)

post.valid.gamb <- predict(model.gamb, data.valid.std.c, type="response") 
profit.gamb <- cumsum(14.5*c.valid[order(post.valid.gamb, decreasing=T)]-2)
n.mail.valid <- which.max(profit.gamb) 
c(n.mail.valid, max(profit.gamb))
cutoff.gamb <- sort(post.valid.gamb, decreasing=T)[n.mail.valid+1] 
chat.valid.gamb <- ifelse(post.valid.gamb>cutoff.gamb, 1, 0) 
table(chat.valid.gamb, c.valid) 

#CLASSIFICATION: LOGISTIC REGRESSION GAM: REDUCED ---------------------------------------------------------------------------------------------

model.gam1 <- gam(I(donr == 1) ~ chld_IMP*home + hinc + tgif_IMP + tdon_IMP + npro_IMP,
                  family = binomial,data = data.train.std.c)

post.valid.gam1<- predict(model.gam1, data.valid.std.c, type="response") 
profit.gam1 <- cumsum(14.5*c.valid[order(post.valid.gam1, decreasing=T)]-2)
n.mail.valid <- which.max(profit.gam1) 
c(n.mail.valid, max(profit.gam1)) 
cutoff.gam1 <- sort(post.valid.gam1, decreasing=T)[n.mail.valid+1] 
chat.valid.gam1 <- ifelse(post.valid.gam1>cutoff.gam1, 1, 0) 
table(chat.valid.gam1, c.valid)

#============================================================================================================================================================
# MODEL PRODUCTION: Classification Models: Linear Discriminant Analysis
#============================================================================================================================================================

#CLASSIFICATION MODEL LINEAR DISCRIMINANT ANALYSIS: BASE ---------------------------------------------------------------------------------------------

model.ldab <- lda(donr ~ ., data.train.std.c)
post.valid.ldab <- predict(model.ldab, data.valid.std.c)$posterior[,2]
profit.ldab <- cumsum(14.5*c.valid[order(post.valid.ldab, decreasing=T)]-2)
n.mail.valid <- which.max(profit.ldab) 
c(n.mail.valid, max(profit.ldab))
cutoff.ldab <- sort(post.valid.ldab, decreasing=T)[n.mail.valid+1] 
chat.valid.ldab <- ifelse(post.valid.ldab>cutoff.ldab, 1, 0) 
table(chat.valid.ldab, c.valid)

#CLASSIFICATION MODEL LINEAR DISCRIMINANT ANALYSIS: REDUCED ---------------------------------------------------------------------------------------------

model.lda1 <- lda(donr ~ chld_IMP*home + hinc + tgif_IMP + tdon_IMP + npro_IMP, 
                  data.train.std.c)

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] 
profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
n.mail.valid <- which.max(profit.lda1) 
c(n.mail.valid, max(profit.lda1)) 
cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] 
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) 
table(chat.valid.lda1, c.valid) 

#============================================================================================================================================================
# MODEL PRODUCTION: Classification Models: Quadratic Discriminant Analysis
#============================================================================================================================================================

#CLASSIFICATION MODEL: QUADRATIC DISCRIMINANT ANALYSIS: BASE ---------------------------------------------------------------------------------------------

model.qdab <- qda(donr ~ .,data.train.std.c)

post.valid.qdab <- predict(model.qdab, data.valid.std.c)$posterior[,2] 
profit.qdab <- cumsum(14.5*c.valid[order(post.valid.qdab, decreasing=T)]-2)
n.mail.valid <- which.max(profit.qdab) 
c(n.mail.valid, max(profit.qdab))
cutoff.qdab <- sort(post.valid.qdab, decreasing=T)[n.mail.valid+1] 
chat.valid.qdab <- ifelse(post.valid.qdab>cutoff.qdab, 1, 0) 
table(chat.valid.qdab, c.valid) 


#CLASSIFICATION MODEL: QUADRATIC DISCRIMINANT ANALYSIS: REDUCED ---------------------------------------------------------------------------------------------

model.qda1 <- qda(donr ~ chld_IMP*home + hinc + tgif_IMP + tdon_IMP + npro_IMP, 
                  data.train.std.c)

post.valid.qda1 <- predict(model.qda1, data.valid.std.c)$posterior[,2] 
profit.qda1 <- cumsum(14.5*c.valid[order(post.valid.qda1, decreasing=T)]-2)
n.mail.valid <- which.max(profit.qda1) 
c(n.mail.valid, max(profit.qda1)) 
cutoff.qda1 <- sort(post.valid.qda1, decreasing=T)[n.mail.valid+1] 
chat.valid.qda1 <- ifelse(post.valid.qda1>cutoff.qda1, 1, 0) 
table(chat.valid.qda1, c.valid) 

qda1Err <- mean(chat.valid.qda1!= c.valid)
qda1Err

#============================================================================================================================================================
# MODEL PRODUCTION: Classification Models: K-NEAREST NEIGHBORS
#============================================================================================================================================================


#CLASSIFICATION MODEL: K-NEAREST NEIGHBORS: BASE K = 1,5,10 ---------------------------------------------------------------------------------------------

train.bcharity.X <- cbind(data.train.std.c$reg1, data.train.std.c$reg2,
                         data.train.std.c$reg3, data.train.std.c$reg4,
                         data.train.std.c$home, data.train.std.c$chld,
                         data.train.std.c$hinc, data.train.std.c$genf, 
                         data.train.std.c$wrat, data.train.std.c$avhv, 
                         data.train.std.c$incm, data.train.std.c$inca, 
                         data.train.std.c$plow, data.train.std.c$npro, 
                         data.train.std.c$tgif, data.train.std.c$lgif, 
                         data.train.std.c$rgif, data.train.std.c$tdon, 
                         data.train.std.c$tlag, data.train.std.c$agif)
test.bcharity.X <- cbind(data.valid.std.c$reg1, data.valid.std.c$reg2, 
                        data.valid.std.c$reg3, data.valid.std.c$reg4, 
                        data.valid.std.c$home, data.valid.std.c$chld, 
                        data.valid.std.c$hinc, data.valid.std.c$genf, 
                        data.valid.std.c$wrat, data.valid.std.c$avhv, 
                        data.valid.std.c$incm, data.valid.std.c$inca, 
                        data.valid.std.c$plow, data.valid.std.c$npro, 
                        data.valid.std.c$tgif, data.valid.std.c$lgif, 
                        data.valid.std.c$rgif, data.valid.std.c$tdon,
                        data.valid.std.c$tlag, data.valid.std.c$agif)

train.donrb = data.train.std.c$donr

set.seed(1)
knn.pred.b1=knn(train.bcharity.X,test.bcharity.X,train.donrb,k=1)
knn.pred.b5=knn(train.bcharity.X,test.bcharity.X,train.donrb,k=5)
knn.pred.b10=knn(train.bcharity.X,test.bcharity.X,train.donrb,k=10)

table(knn.pred.b1, c.valid)
table(knn.pred.b5, c.valid)
table(knn.pred.b10, c.valid)

knnErrb1 <- mean(knn.pred.b1 != c.valid)
knnErrb5 <- mean(knn.pred.b5 != c.valid)
knnErrb10 <- mean(knn.pred.b10 != c.valid)

knnErrb1
knnErrb5
knnErrb10

#============================================================================================================================================================
# MODEL PRODUCTION: Classification Models: DECISION TREE
#============================================================================================================================================================


#CLASSIFICATION MODEL: DECISION TREE: BASE ------------------------------------------------------------------------------------------------------------

tree.data.train = data.train.std.c
tree.data.train$donr = as.factor(tree.data.train$donr)
tree.data.valid = data.valid.std.c
tree.data.valid$donr = as.factor(tree.data.valid$donr)

par(mfrow=c(1,1))
tree.donr = tree(donr ~., tree.data.train)
plot(tree.donr)
text(tree.donr, pretty=0)

tree.pred= predict(tree.donr, tree.data.valid, type = "class")
table(tree.pred, c.valid)

treeErrorb <- mean(tree.pred!= c.valid)


#CLASSIFICATION MODEL: DECISION TREE: CV TO BASE PRUNE ------------------------------------------------------------------------------------------------------------

cv.tree.donr <- cv.tree(tree.donr, FUN=prune.misclass)
plot(cv.tree.donr$size, cv.tree.donr$dev, type = "b")
plot(cv.tree.donr$k, cv.tree.donr$dev, type = "b")

#Best at 5

prune.tree = prune.misclass(tree.donr, best=5)
tree.pred.cv = predict(prune.tree, tree.data.valid, type="class")
table(tree.pred.cv, c.valid)

treeErrorcv <- mean(tree.pred.cv != c.valid)
treeErrorcv

#============================================================================================================================================================
# MODEL PRODUCTION: Classification Models: BAGGING
#============================================================================================================================================================


#CLASSIFICATION MODEL: BAGGING BASE ------------------------------------------------------------------------------------------------------------

bag.donrb = randomForest(donr ~ ., data=tree.data.train, mtry=20, 
                        importance=TRUE, type="classification")

importance(bag.donrb)
set.seed(123)
bag.predb = predict(bag.donrb, newdata=tree.data.valid)
table(bag.predb, c.valid)
bagErrorb = mean(bag.predb != c.valid)


#CLASSIFICATION MODEL: BAGGING REDUCED ------------------------------------------------------------------------------------------------------------

bag.donr1 = randomForest(donr ~ chld_IMP*home + hinc + tgif_IMP + tdon_IMP + npro_IMP, data=tree.data.train, mtry=20, 
                        importance=TRUE, type="classification")

importance(bag.donr1)
set.seed(123)
bag.pred1 = predict(bag.donr1, newdata=tree.data.valid)
table(bag.pred1, c.valid)
bagError1 = mean(bag.pred1 != c.valid)


#============================================================================================================================================================
# MODEL PRODUCTION: Classification Models: BOOSTING
#============================================================================================================================================================


#CLASSIFICATION MODEL: BOOSTING BASE  ------------------------------------------------------------------------------------------------------------

set.seed(123)
boost.donrb = gbm(donr ~ ., data = data.train.std.c,distribution = "bernoulli",
                 n.trees = 1000, interaction.depth = 3)
set.seed(123)
boost.probsb = predict(boost.donrb, newdata=data.valid.std.c,
                n.trees = 1000, type = "response")

boost.predb = rep("0", 2018)
boost.predb[boost.probsb > .5] = "1"
table(boost.predb , c.valid)
boostErrorb <- mean(boost.predb != c.valid)
boostErrorb

#CLASSIFICATION: BOOSTING WITH SHRINKAGE + REDUCED VARIABLES + MORE TREES  ------------------------------------------------------------------------------------------------------------

set.seed(123)
boost.donr1 = gbm(donr ~ chld_IMP*home + hinc + tgif_IMP + tdon_IMP + npro_IMP,
                  data = data.train.std.c,distribution = "bernoulli",
                  n.trees = 5000, interaction.depth = 3, shrinkage =0.2)
set.seed(123)
boost.probs1 = predict(boost.donr1, newdata=data.valid.std.c,
                       n.trees = 5000, type = "response")

boost.pred1 = rep("0", 2018)
boost.pred1[boost.probs1 > .5] = "1"
table(boost.pred1 , c.valid)
boostError1 <- mean(boost.pred1 != c.valid)
boostError1


#============================================================================================================================================================
# MODEL PRODUCTION: Classification Models: SUPPORT VECTOR CLASSIFIER
#============================================================================================================================================================

#CLASSIFICATION MODEL: SUPPORT VECTOR CLASSIFIER:BASE  ------------------------------------------------------------------------------------------------------------
#Cross-validation to select the best parameters
set.seed(1)
tune.outb = tune(svm, donr ~., data = tree.data.train, kernel = "linear",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10)))

set.seed(1)
svc.fitb = svm(donr ~., data = tree.data.train, kernel = "linear",
               cost = 1, scale = FALSE)

bestmodb = tune.outb$best.model



summary(bestmodb)
set.seed(1)
svc.predb = predict(bestmodb, tree.data.valid)
table(svc.predb, c.valid)

svcErrorb <- mean(svc.predb != c.valid)
svcErrorb

#============================================================================================================================================================
# MODEL PRODUCTION: Classification Models: CONFUSION MATRIX AND STATS
#============================================================================================================================================================

install.packages("caret")

library(caret)
confusion_logb=confusionMatrix(chat.valid.logb, c.valid,positive = "1")
confusion_gamb=confusionMatrix(chat.valid.gamb, c.valid,positive = "1")
confusion_ldab=confusionMatrix(chat.valid.ldab, c.valid,positive = "1")
confusion_qdab=confusionMatrix(chat.valid.qdab, c.valid,positive = "1")
confusion_knn=confusionMatrix(knn.pred.b10, c.valid,positive = "1")
confusion_tree=confusionMatrix(tree.pred, c.valid,positive = "1")
confusion_bag=confusionMatrix(bag.predb, c.valid,positive = "1")
confusion_boost=confusionMatrix(boost.predb, c.valid,positive = "1")
confusion_svc=confusionMatrix(svc.predb, c.valid,positive = "1")

confusion_logb
confusion_gamb
confusion_ldab
confusion_qdab
confusion_knn
confusion_tree
confusion_bag
confusion_boost
confusion_svc


#============================================================================================================================================================
# MODEL PRODUCTION: Classification Models: AREA UNDER CURVE SUMMARY
#============================================================================================================================================================

auc(chat.valid.logb, c.valid) 
auc(chat.valid.log1, c.valid)

auc(chat.valid.gamb, c.valid) 
auc(chat.valid.gam1, c.valid)

auc(chat.valid.ldab, c.valid)
auc(chat.valid.lda1, c.valid) 

auc(chat.valid.qdab, c.valid)
auc(chat.valid.qda1, c.valid) 

auc(knn.pred.b1, c.valid)
auc(knn.pred.b5, c.valid)
auc(knn.pred.b10, c.valid)

auc(tree.pred,c.valid)
auc(tree.pred.cv,c.valid)

auc(bag.predb, c.valid)
auc(bag.pred1, c.valid)

auc(boost.predb, c.valid)
auc(boost.pred1, c.valid)

auc(svc.predb, c.valid)

#============================================================================================================================================================
# MODEL CONSTRUCTION - CLASSIFICATION OVERSAMPLING ADJUSTMENT
#============================================================================================================================================================
#Post probs for test data
post.test <- predict(model.gamb, data.test.std, type="response") 

n.mail.valid <- which.max(profit.gamb)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set



#============================================================================================================================================================
# MODEL CONSTRUCTION - CLASSIFICATION TEST PREDICTIONS
#============================================================================================================================================================

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)

#============================================================================================================================================================
# # MODEL PRODUCTION: Prediction Models: ORDINARY LEAST SQUARES
#============================================================================================================================================================

#PREDICTIVE MODEL: ORDINARY LEAST SQUARES BASE------------------------------------------------------------------------------------------------------------
 
model.lsb <- lm(damt ~ ., data.train.std.y)

pred.valid.lsb <- predict(model.lsb, newdata = data.valid.std.y) # validation predictions
MPElsb=mean((y.valid - pred.valid.lsb)^2) # mean prediction error
SElsb=sd((y.valid - pred.valid.lsb)^2)/sqrt(n.valid.y) # std error

MPElsb
SElsb

# par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
# plot(model.lsb)

#PREDICTIVE MODEL: ORDINARY LEAST SQUARES REDUCED------------------------------------------------------------------------------------------------------------

model.ls1 <- lm(damt ~ chld_IMP*home + agif_IMP + lgif_IMP + rgif_IMP + npro_IMP + plow_IMP, data.train.std.y)

pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
MPEls1=mean((y.valid - pred.valid.ls1)^2) 
SEls1=sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error

MPEls1
SEls1

#============================================================================================================================================================
# MODEL PRODUCTION: Prediction Models: BEST SUBSET REGRESSION
#============================================================================================================================================================

#PREDICTIVE MODEL: BEST SUBSET WITH CV ------------------------------------------------------------------------------------------------------------
set.seed(1)
regfit.best <- regsubsets(damt ~reg1 + reg2 + reg3 + reg4 + home
                       + chld_IMP + hinc + genf + wrat_IMP + avhv_IMP + incm_IMP + inca_IMP + plow_IMP + npro_IMP
                       + tgif_IMP + lgif_IMP + rgif_IMP + tdon_IMP + tlag_IMP + agif_IMP,data.train.std.y, nvmax = 20)

set.seed(1)
test.mat = model.matrix(damt ~reg1 + reg2 + reg3 + reg4 + home
          + chld_IMP + hinc + genf + wrat_IMP + avhv_IMP + incm_IMP + inca_IMP + plow_IMP + npro_IMP
          + tgif_IMP + lgif_IMP + rgif_IMP + tdon_IMP + tlag_IMP + agif_IMP,data.train.std.y)

val.errors <- rep(NA,20)

for (i in 1:20) {
  coefi = coef(regfit.best, id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((y.valid - pred)^2)}

plot(val.errors)
which.min(val.errors)
coef(regfit.best,14)

model.bestsub.train <- lm(damt ~ reg2 + reg3 + reg4 +home + chld_IMP
                    + hinc + genf + avhv_IMP + incm_IMP + tgif_IMP
                    +lgif_IMP + rgif_IMP + tdon_IMP + agif_IMP, data.train.std.y)

model.bestsub.valid <- lm(damt ~ reg2 + reg3 + reg4 +home + chld_IMP
                    + hinc + genf + avhv_IMP + incm_IMP + tgif_IMP
                    +lgif_IMP + rgif_IMP + tdon_IMP + agif_IMP, data.valid.std.y)
# Validation Predictions
pred.valid.bestsub <- predict(model.bestsub.train, data.valid.std.y)

MPEbestsub <- mean((y.valid - pred.valid.bestsub)^2) # mean prediction error
SEbestsub <- sd((y.valid - pred.valid.bestsub)^2)/sqrt(n.valid.y)  #std error

#============================================================================================================================================================
# MODEL PRODUCTION: Prediction Models: PRINCIPAL COMPONENT REGRESSION
#============================================================================================================================================================

#PREDICTIVE MODEL: PRINCIPAL COMPONENT REGRESSION 5 COMPONENTS ------------------------------------------------------------------------------------------------------------

set.seed(1)
model.pcr.fit = pcr(damt ~ ., data = data.train.std.y,
              scale = TRUE, validation = "CV")

#summary(model.pcr)

validationplot(model.pcr.fit, val.type = "MSEP", type = "b")

#Shows elbow at 5

set.seed(123)
pred.valid.pcr5 = predict(model.pcr.fit, data.valid.std.y, ncomp=5)
MPEpcr5 <- mean((y.valid - pred.valid.pcr5)^2)
SEpcr5 <- sd((y.valid - pred.valid.pcr5)^2)/sqrt(n.valid.y)

#PREDICTIVE MODEL: PRINCIPAL COMPONENT REGRESSION 20 COMPONENTS ------------------------------------------------------------------------------------------------------------
set.seed(1)
pred.valid.pcr20 = predict(model.pcr.fit, data.valid.std.y, ncomp=20)
MPEpcr20 <- mean((y.valid - pred.valid.pcr20)^2)
SEpcr20 <- sd((y.valid - pred.valid.pcr20)^2)/sqrt(n.valid.y)

#============================================================================================================================================================
# MODEL PRODUCTION: Prediction Models: PARTIAL LEAST SQUARES
#============================================================================================================================================================

#PREDICTIVE MODEL: PARTIAL LEAST SQUARES 2 COMPONENTS------------------------------------------------------------------------------------------------------------
set.seed(1)
model.plsr.fit = plsr(damt ~ . , data = data.train.std.y,
               scale = TRUE, validation = "CV") 

# summary(model.plsr.fit)

validationplot(model.plsr.fit, val.type="MSEP", type = "b")

# There is an 'elbow' in the graph at 2, with minimal reduction after.
set.seed(1)
pred.valid.plsr2 = predict(model.plsr.fit, data.valid.std.y, ncomp=2)
MPEplsr2 <- mean((y.valid - pred.valid.plsr2)^2)
SEplsr2<- sd((y.valid - pred.valid.plsr2)^2)/sqrt(n.valid.y)


#PREDICTIVE MODEL: PARTIAL LEAST SQUARES 15 COMPONENTS------------------------------------------------------------------------------------------------------------

# Just to see 15
set.seed(1)
pred.valid.plsr15 = predict(model.plsr.fit, data.valid.std.y, ncomp=15)
MPEplsr15 <- mean((y.valid - pred.valid.plsr15)^2)
SEplsr15<- sd((y.valid - pred.valid.plsr15)^2)/sqrt(n.valid.y)

#============================================================================================================================================================
# MODEL PRODUCTION: Prediction Models: RIDGE REGRESSION
#============================================================================================================================================================

#PREDICTIVE MODEL: RIDGE REGRESSION WITH CROSS VALIDATIIN------------------------------------------------------------------------------------------------------------

library(glmnet)

set.seed(1)
grid = 10^seq(10, -2, length=100)
ridge.train <- data.matrix(data.train.std.y)
ridge.train <- ridge.train[,-21]

ridge.mod = glmnet(ridge.train, y.train, alpha=0, lambda=grid,
                   thresh=1e-12)

# Lambda selection via cross-validation.

set.seed(1)
cv.out = cv.glmnet(ridge.train, y.train, alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

# Make predictions and view errors.
ridge.valid = as.matrix(data.valid.std.y)
ridge.valid <- ridge.valid[,-21]
set.seed(1)
ridge.pred = predict(ridge.mod, s=bestlam, newx=ridge.valid)

MPEridge <- mean((y.valid - ridge.pred)^2)
SEridge <- sd((y.valid - ridge.pred)^2)/sqrt(n.valid.y)

#============================================================================================================================================================
# MODEL PRODUCTION: Prediction Models: LASSO REGRESSION
#============================================================================================================================================================

#PREDICTIVE MODEL: RIDGE REGRESSION WITH CROSS VALIDATION------------------------------------------------------------------------------------------------------------

model.lasso = glmnet(ridge.train, y.train, alpha=1, lambda=grid)

# Use cross-validation to select lambda.
set.seed(1)
cv.out.lasso = cv.glmnet(ridge.train, y.train, alpha=1)
plot(cv.out.lasso)
bestlamlasso = cv.out.lasso$lambda.min
bestlamlasso
set.seed(1)
lasso.pred = predict(model.lasso, s=bestlamlasso, newx=ridge.valid)

MPElassocv <- mean((y.valid - lasso.pred)^2)
SElassocv <- sd((y.valid - lasso.pred)^2)/sqrt(n.valid.y)

#============================================================================================================================================================
# MODEL PRODUCTION: Prediction Models: DECISION TREE
#============================================================================================================================================================

#PREDICTIVE MODEL: DECISION TREE BASE------------------------------------------------------------------------------------------------------------

set.seed(123)
tree.damt = tree(damt ~., data.train.std.y)

# plot(tree.Damt)
# text(tree.Damt, pretty=0)
# summary(tree.damt)

set.seed(123)
tree.pred = predict(tree.damt, data.valid.std.y)
MPEtreedmtb <- mean((y.valid - tree.pred)^2)
SEtreedmtb <- sd((y.valid - tree.pred)^2)/sqrt(n.valid.y)

MPEtreedmtb
SEtreedmtb

#PREDICTIVE MODEL: DECISION TREE WITH CV TO PRUNE------------------------------------------------------------------------------------------------------------

cv.damt = cv.tree(tree.damt)
plot(cv.damt$size, cv.damt$dev, type = "b")
# Elbow at 7, lowest at 11.

set.seed(123)
prune.damt = prune.tree(tree.damt, best = 7)
# plot(prune.Damt)
# text(prune.Damt, pretty = 0)

set.seed(123)
tree.pred2 <- predict(prune.damt, data.valid.std.y)
MPEtreedmt1 <- mean((y.valid - tree.pred2)^2)
SEtreedmt1 <- sd((y.valid - tree.pred2)^2)/sqrt(n.valid.y)
MPEtreedmt1
SEtreedmt1

#============================================================================================================================================================
# MODEL PRODUCTION: Prediction Models: BAGGING
#============================================================================================================================================================

#PREDICTIVE MODEL: BAGGING BASE------------------------------------------------------------------------------------------------------------

set.seed(123)
model.bag.damtb = randomForest(damt ~., data = data.train.std.y, mtry=20,
                        importance=TRUE)
set.seed(123)
pred.bag.damtb <- predict(model.bag.damtb, newdata=data.valid.std.y)
MPEbagb <- mean((y.valid - pred.bag.damtb)^2)
SEbagb <- sd((y.valid - pred.bag.damtb)^2/sqrt(n.valid.y))
MPEbagb
SEbagb

#PREDICTIVE MODEL: BAGGING REDUCED------------------------------------------------------------------------------------------------------------

set.seed(123)
model.bag.damt1 = randomForest(damt ~chld_IMP*home + agif_IMP + lgif_IMP + rgif_IMP + npro_IMP + plow_IMP, data = data.train.std.y,
                               importance=FALSE)
set.seed(123)
pred.bag.damt1 <- predict(model.bag.damt1, newdata=data.valid.std.y)
MPEbag1 <- mean((y.valid - pred.bag.damt1)^2)
SEbag1 <- sd((y.valid - pred.bag.damt1)^2/sqrt(n.valid.y))
MPEbag1
SEbag1

#=====================================================================================================================================
# MODEL PRODUCTION: Prediction Models: BOOSTING
#============================================================================================================================================================

#PREDICTIVE MODEL: BOOSTING BASE------------------------------------------------------------------------------------------------------------

set.seed(123)
boost.damtb = gbm(damt ~., data = data.train.std.y, distribution = "gaussian",
                 n.trees=1000, interaction.depth=4)

set.seed(123)
pred.boost.damtb <- predict(boost.damtb, newdata = data.valid.std.y, n.trees = 1000)
MPEboostb <- mean((y.valid - pred.boost.damtb)^2)
SEboostb <- sd((y.valid - pred.boost.damtb)^2/sqrt(n.valid.y))

MPEboostb
SEboostb

#PREDICTIVE MODEL: BOOSTING REDUCED AND MORE TREES------------------------------------------------------------------------------------------------------------

set.seed(123)
boost.damt1 = gbm(damt ~chld_IMP*home + agif_IMP + lgif_IMP + rgif_IMP + npro_IMP + plow_IMP, data = data.train.std.y, distribution = "gaussian",
                  n.trees=5000, interaction.depth=4)

set.seed(123)
pred.boost.damt1 <- predict(boost.damt1, newdata = data.valid.std.y, n.trees = 5000)
MPEboost1 <- mean((y.valid - pred.boost.damt1)^2)
SEboost1 <- sd((y.valid - pred.boost.damt1)^2/sqrt(n.valid.y))



#============================================================================================================================================================
# MODEL PRODUCTION: Prediction Models: RANDOM FOREST
#============================================================================================================================================================

set.seed(123)
model.rfdamtb = randomForest(damt ~., data=data.train.std.y, importance=TRUE)
model.rfdamtb
set.seed(123)
pred.rfdamtb <- predict(model.rfdamtb, newdata = data.valid.std.y)
MPErfb <- mean((y.valid - pred.rfdamtb)^2)
SErfb <- sd((y.valid - pred.rfdamtb)^2/sqrt(n.valid.y))

#============================================================================================================================================================
# MODEL PRODUCTION: Prediction Model Comparison
#============================================================================================================================================================
par(mfrow = c(2,2))
plot(model.lsb)

par(mfrow = c(2,3))
plot(regfit.best, main = "Best Subset")
validationplot(model.pcr.fit, val.type = "MSEP", type = "b", main = "PCR")
plot(model.plsr.fit, main = "Partial Least Squares")
plot(ridge.mod,main = "Ridge")

par(mfrow = c(1,1))
coefficients(model.lasso)
plot(model.lasso,main = "Lasso", label = TRUE)

par(mfrow = c(1,2))
plot(tree.damt, main = "Tree")
text(tree.damt, pretty = 0)

plot(prune.damt, main = "Tree with CV to Prune")
text(prune.damt, pretty = 0)

par(mfrow = c(1,3))
plot(model.bag.damtb, main = "Bagging Base")
plot(model.bag.damt1, main = "Bagging Reduced")
plot(model.rfdamtb, main = "Random Forest")


par(mfrow = c(1,3))

varImpPlot(model.bag.damtb)
varImpPlot(model.bag.damt1)
varImpPlot(model.rfdamtb)


PredictionModel <- c("OLS Base", "OLS Reduced", "Best Subset w/ CV", "PCR 5 Comp.", "PCR 20 Comp.", "PLS 2 Comp.", "PLS 15 Comp.",
            "Ridge w/ CV", "Lasso w/ CV", "Decision Tree","Decision Tree Pruned","Bagging Base","Bagging Reduced w/Tree",
            "Boosting Base","Boosting Reduced w/Tree","Random Forest")


MPE <-c(MPElsb,MPEls1,MPEbestsub,MPEpcr5,MPEpcr20,MPEplsr2,MPEplsr15,MPEridge,MPElassocv,MPEtreedmtb,MPEtreedmt1,MPEbagb,MPEbag1,MPEboostb,MPEboost1,MPErfb)
StdError<-c(SElsb,	SEls1,	SEbestsub,	SEpcr5,	SEpcr20,	SEplsr2,	SEplsr15,	SEridge,	SElassocv,	SEtreedmtb,	SEtreedmt1,	SEbagb,	SEbag1,	SEboostb,	SEboost1,	SErfb)

damterrorcomps <- as.data.frame(cbind(PredictionModel, MPE, StdError))

View(damterrorcomps)

#Criteria is minimum prediction error

which.min(MPE)


#============================================================================================================================================================
# MODEL CONSTRUCTION - CLASSIFICATION TEST PREDICTIONS
#============================================================================================================================================================

yhat.test <- predict(model.plsr.fit, data.test.std, ncomp=2)

#============================================================================================================================================================
# FINAL RESULTS
#============================================================================================================================================================
# Save final results for both classification and prediction models

length(chat.test) # length = 2007
length(yhat.test) # length = 2007
chat.test[1:10] # DONR Check
yhat.test[1:10] # DAMT Check

# View(pred.data.frame)

pred.data.frame <- data.frame(chat = chat.test, yhat = yhat.test)
colnames(pred.data.frame)[2] <- "yhat"
predictedDAMT <- subset(pred.data.frame, chat==1)
finalProfit <- sum(predictedDAMT$yhat)
finalProfit

table(chat.test)

ip <- pred.data.frame
write.csv(ip, file="Scott_Morgan_Scored_Records.csv", row.names=FALSE)