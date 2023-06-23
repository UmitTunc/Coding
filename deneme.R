

rm(list=ls())
library(lubridate) #FOR DATE
library(dplyr)  # for Data Manipulation
library(SciViews) ## for ln function
source("C:/Users/umits/OneDrive/Masa�st�/Bitirme/Functions.R")

#### For minimize A4 value 2 ~ Gradient Vector ####

dataset<- churn
Tm <- 15
learning_rate<-0.00001
tolerance <- 0.1
kmax<-5
start<- 2
end<-9
n<-8


heuristic_min_lse.frame<-random_ivalue(dataset,Tm,learning_rate,tolerance,kmax,start,end,n)
heuristic_min_lse.frame
index<-which(heuristic_min_lse.frame$lse_value==min(heuristic_min_lse.frame$lse_value))
heuristic_min_lse.frame[index,]



#### A5 equation ####
dataset<- churn
r_a<- heuristic_min_lse.frame[index,1]
s <- heuristic_min_lse.frame[index,2]
B <- heuristic_min_lse.frame[index,3]
Tm <- 15

alpha_estimator(dataset,r_a,s,B,Tm)




##Churn Prob Calculation with Heuristic Parameters



