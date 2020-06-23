rm(list=ls())

#Bonus Assignmenmt 1#


#1.Write R code or code in any language of your choice to perform cluster wise regression
#(pg 46 of Sync Session Slides for Session #2). 

#OR 

#UJse flexmix package in R to perform cluster-wise regression (referred to as latent class in flexmix) regressions

#2.Apply the code to perform cluster-wise regressionm for the car.test.frame data in package rpart.

#3. Submit results and code from 1

#https://www.r-bloggers.com/mixture-distributions-and-models-a-clarification/

library(rpart)
library(flexmix)
library(car)
library(gclus)
library(corrplot)
library(fpc)
library(cluster)
library(ggplot2)

#View(car.test.frame)

attach(car.test.frame)

summary(car.test.frame)

str(car.test.frame)

#Price will be the response variable for this exercise

#################################
#          Quick EDA            #
#################################

#Exhibit 1: Continuous variables
car.test.frame.EDA <- car.test.frame[,c(1,3:4,6:8)]
str(car.test.frame.EDA)

M <- cor(car.test.frame.EDA)
corrplot(M, method="circle")

#Exhibit 2: Categorical variables
boxplot(Price ~ Country, data = car.test.frame)

boxplot(Price ~ Type, data = car.test.frame)


#################################
#          Data PREP            #
#################################


#Drop N/A

car.test.frame.train = na.omit(car.test.frame)

#Drop Categorical Variables

drops  <- c("Country", "Type")

car.test.frame.train=car.test.frame.train[ , !(names(car.test.frame.train) %in% drops)]

#car.test.frame.train = car.test.frame[,c(1,3:4,6:8)]



####View(car.test.frame.train)

str(car.test.frame.train)

summary(car.test.frame.train)

#################################
#        Fit Model 1            #
#################################

clustW_1<-flexmix(Price ~ Mileage + Reliability + Weight + Disp. + HP, data = car.test.frame.train, 
                  control = list(verb = 5, iter=1000), k =1)
summary(clustW_1)

plot(clustW_1)

parameters(clustW_1, component=1)

#AIC: 916.476   BIC: 929.7187


#################################
#        Fit Model 2            #
#################################

?flexmix

clustW_2<-flexmix(Price ~ Mileage + Reliability + Weight + Disp. + HP, data = car.test.frame.train, 
                  control = list(verb = 5, iter=1000), k =2)
summary(clustW_2)

plot(clustW_2)

parameters(clustW_2, component=1)
parameters(clustW_2, component=2)

#AIC: 891.8166   BIC: 920.1939 
#################################
#        Fit Model 3            #
#################################

clustW_3<-flexmix(Price ~ Mileage + Reliability + Weight + Disp. + HP, data = car.test.frame.train, 
                       control = list(verb = 5, iter=1000), k =5)
summary(clustW_3)

plot(clustW_3)

parameters(clustW_3, component=1)
parameters(clustW_3, component=2)

#AIC: 852.9268   BIC: 896.4387

#################################
#        Fit Model 4            #
#################################

#Poisson
Model4 <- FLXMRziglm(family = "poisson")
clustW_4<-flexmix(Price ~ Mileage + Reliability + Weight + Disp. + HP,
         model = Model4, k = 10 , data = car.test.frame.train, control = list(minprior = 0.01,verb = 5, iter=1000))

summary(clustW_4)

#AIC: 850.7957   BIC: 968.0886



#PCA
clus <- kmeans(car.test.frame.EDA, centers=8)

plotcluster(car.test.frame.EDA, clus$cluster)

clusplot(car.test.frame.EDA, clus$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

