rm(list=ls())

library(lubridate) #FOR DATE
library(dplyr)  # for Data Manipulation
library(base)
library(SciViews) ## for ln function
library(pROC)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(partykit)
library(pROC)
library(ggplot2)
source("C:/Users/umits/OneDrive - Kadir Has University/Masa�st�/Bitirme/Functions.R")

# Set the values of a, b, c, and z
a <- 1
b <- 2
c <- 3
z <- 0.5
integral_hypergeometric(a,b,c,z)
fundamental_hyper(a,b,c,z)

## Set the values r, alpha, s, B, x, t, T, T_future
r <- 0.415
alpha <- 0.415
r_a<-1
s <- 2
B <- 4
x <-1
t <-1
T <- 2
T_future <- 2

## Probability of alive(churn) given that information
Alive.prob(r,alpha,s,B,x,t,T)

#### Equation 17  
# The expected number of purchases
Expected_X(r_a,s,B,T) # for T=2 expected purchase

Expected_X_updated1 <- (((r+1) * (B+T)) / ((alpha+T) * (s - 1))) * (1 - ((B+T) / ((B+T) + 2))^(s-1))
Expected_X_future1 <- Expected_X_updated1 * Alive.prob(r,alpha,s,B,1,1,2)
Expected_X_future1

#### Validation Table_1   ####
r <- 0.415
alpha <- 0.415
r_a<-1
s <- 0.3
B <- 0.6
x <- c(0,1,2,3,4,5)
t <- c(0.25,1,3)
T <- c(0.5,1.25,3.25,2,4,6)

valid_0.25.vec <- NULL
valid_1.vec <- NULL
valid_3.vec <- NULL
Valid_table1.data_frame <- data.frame()
Valid_table1.update <- data.frame()

for (i in c(1:length(x))){
  value_x <- x[i]
  for (j in c(1:length(t))){
    value_t <- t[j]
    for (k in c(1:length(T))){
      value_T <- T[k]
      if(value_T>value_t){
        if(value_x !=0){
          Time_dif <- value_T-value_t
          ## Case 2 !Dikkat B > alpha olmal�
          a2 <- r+value_x+s
          b2 <- r+value_x
          c2 <- r+value_x+s+1
          F_dist1 <- fundamental_hyper(a2, b2, c2, z2(value_t))
          F_dist2 <- fundamental_hyper(a2, b2, c2, z2(value_T))
          P_alive = 1/((1+((s/(r+value_x+s)) * (((((alpha+value_T)/(B+value_t))^(r+value_x)) * (((B+value_T)/(B+value_t))^s) * F_dist1) - ((((alpha+value_T)/(B+value_T))^(r+value_x)) * F_dist2)))))
          P_alive
          new_row <- cbind(value_t,Time_dif,value_x,P_alive) 
          Valid_table1.data_frame<- rbind(Valid_table1.data_frame, new_row)
        }
        if(value_x == 0){
          value_dif_t = 0
          Time_dif <- value_T-value_t
          ## Case 2 !Dikkat B > alpha olmal�
          a2 <- r+value_x+s
          b2 <- r+value_x
          c2 <- r+value_x+s+1
          F_dist1 <- fundamental_hyper(a2, b2, c2, z2(value_dif_t))
          F_dist2 <- fundamental_hyper(a2, b2, c2, z2(value_T))
          P_alive = 1/((1+((s/(r+value_x+s)) * (((((alpha+value_T)/(B+value_dif_t))^(r+value_x)) * (((B+value_T)/(B+value_dif_t))^s) * F_dist1) - ((((alpha+value_T)/(B+value_T))^(r+value_x)) * F_dist2)))))
          P_alive
          new_row <- cbind(value_t,Time_dif,value_x,P_alive) 
          Valid_table1.data_frame<- rbind(Valid_table1.data_frame, new_row)
        }
      }
      if(value_T<=value_t){
        new_row <- NULL
        Valid_table1.data_frame<- rbind(Valid_table1.data_frame, new_row)
      }
    }
  }
}

Valid_table1.data_frame
updated_value_t <- Valid_table1.data_frame$value_t[Valid_table1.data_frame$Time_dif==0.25]
updated_Time_dif <- Valid_table1.data_frame$Time_dif[Valid_table1.data_frame$Time_dif==0.25]
updated_value_x <- Valid_table1.data_frame$value_x[Valid_table1.data_frame$Time_dif==0.25]
updated_P_alive <- Valid_table1.data_frame$P_alive[Valid_table1.data_frame$Time_dif==0.25]
Valid_table1.update <- cbind(updated_value_t, updated_Time_dif, updated_value_x, updated_P_alive)

updated_value_t <- Valid_table1.data_frame$value_t[Valid_table1.data_frame$Time_dif==1]
updated_Time_dif <- Valid_table1.data_frame$Time_dif[Valid_table1.data_frame$Time_dif==1]
updated_value_x <- Valid_table1.data_frame$value_x[Valid_table1.data_frame$Time_dif==1]
updated_P_alive <- Valid_table1.data_frame$P_alive[Valid_table1.data_frame$Time_dif==1]
new_row <- cbind(updated_value_t, updated_Time_dif, updated_value_x, updated_P_alive) 
Valid_table1.update <- rbind(Valid_table1.update, new_row)

updated_value_t <- Valid_table1.data_frame$value_t[Valid_table1.data_frame$Time_dif==3]
updated_Time_dif <- Valid_table1.data_frame$Time_dif[Valid_table1.data_frame$Time_dif==3]
updated_value_x <- Valid_table1.data_frame$value_x[Valid_table1.data_frame$Time_dif==3]
updated_P_alive <- Valid_table1.data_frame$P_alive[Valid_table1.data_frame$Time_dif==3]
new_row <- cbind(updated_value_t, updated_Time_dif, updated_value_x, updated_P_alive)
Valid_table1.update <- rbind(Valid_table1.update, new_row)
Valid_table1.update


#### Validation Table_2  #####
r <- 0.415
alpha <- 0.415
s <- 2
B <- 4
t <- 1
T <- 2
T_future <- 2
x <- c(0,1,2,3,4,5)

valid_prob <- NULL
valid_E <- NULL
Valid_table2.data_frame <- data.frame()

for (i in c(1:length(x))){
  ## Case 2
  value_x <- x[i]
  if(value_x != 0){
    if (alpha < B) {
      a2 <- r+value_x+s
      b2 <- r+value_x
      c2 <- r+value_x+s+1
      F_dist1 <- hypergeometric(a2, b2, c2, z2(t))
      F_dist2 <- hypergeometric(a2, b2, c2, z2(T))
      
      P_alive = 1/((1+((s/(r+value_x+s)) * (((((alpha+T)/(B+t))^(r+value_x)) * (((B+T)/(B+t))^s) * F_dist1) - ((((alpha+T)/(B+T))^(r+value_x)) * F_dist2)))))
      Expected_X_updated <- (((r+value_x) * (B+T)) / ((alpha+T) * (s - 1))) * (1 - ((B+T) / ((B+T) + T_future))^(s-1))
      Expected_X_future <- Expected_X_updated * P_alive
      
      new_row <- cbind(value_x,P_alive,Expected_X_future) 
      Valid_table2.data_frame<- rbind(Valid_table2.data_frame,new_row)
    } 
  }
  if(value_x == 0){
    value_t<-0
    a2 <- r+ value_x+s
    b2 <- r+ value_x
    c2 <- r+value_x+s+1
    F_dist1 <- hypergeometric(a2, b2, c2, z2(value_t))
    F_dist2 <- hypergeometric(a2, b2, c2, z2(T))
    
    P_alive = 1/((1+((s/(r+value_x+s)) * (((((alpha+T)/(B+value_t))^(r+value_x)) * (((B+T)/(B+value_t))^s) * F_dist1) - ((((alpha+T)/(B+T))^(r+value_x)) * F_dist2)))))
    Expected_X_updated <- (((r+value_x) * (B+T)) / ((alpha+T) * (s - 1))) * (1 - ((B+T) / ((B+T) + T_future))^(s-1))
    Expected_X_future <- Expected_X_updated * P_alive
    new_row <- cbind(value_x,P_alive,Expected_X_future) 
    Valid_table2.data_frame<- rbind(Valid_table2.data_frame,new_row)
  }
}

Valid_table2.data_frame<- as.data.frame(Valid_table2.data_frame)
Valid_table1.data_frame <-  as.data.frame(Valid_table1.update)
Valid_table1.data_frame
Valid_table2.data_frame


### Pareto/NBD Model validation table result
table1_value_prob <- c(0.851,0.662,0.451,0.908,0.952,0.979,0.892,0.948,0.978,0.873,0.944,0.977,0.850,0.939,0.977,0.822,0.933,0.977,0.662,0.558,0.408,0.648,
                       0.809,0.916,0.504,0.756,0.904,0.347,0.688,0.891,0.211,0.607,0.875,0.114,0.514,0.857,0.451,0.408,0.331,0.283,0.512,0.754,0.100,0.325,
                       0.676,0.027,0.171,0.580,0.006,0.077,0.471,0.001,0.031,0.359)
table2_value_prob <- c(0.36,0.60,0.53,0.44, 0.36,0.28)
table2_value_E <- c(0.09, 0.53,0.79,0.93,0.99,0.93)

### Article and our own results difference vector
table1_diff<- round((Valid_table1.data_frame$updated_P_alive - table1_value_prob),3)
table2_diff<-round((Valid_table2.data_frame$P_alive - table2_value_prob),3)
table1_diff
table2_diff


#### A4 Equation ####

#### A4,1 ~ Xt(Tm) is the average number of transactions per customer during the customers' first month.
T <- 30 # cutt off time 15 days
dataset<- churn
Tm<-600
average_transactions_pcustomer(dataset,T,Tm)

### Validation ####
# spesific customer id = H00767,H01964,H00493,H00931,H01262
dataset<- churn
dim(dataset)
dataset<-dataset[dataset$Hasta.Takma.Ad� == c('H00493') | dataset$Hasta.Takma.Ad� == c('H00767')| dataset$Hasta.Takma.Ad� == c('H01964') 
                 | dataset$Hasta.Takma.Ad� == c('H00931') | dataset$Hasta.Takma.Ad� == c('H01262'),]
dim(dataset)
min_date<- min(churn$Tarih)
customers_first_purchase <- aggregate(Tarih ~ Hasta.Takma.Ad�, data = dataset, min)
customers_last_purchase <- aggregate(Tarih ~ Hasta.Takma.Ad�, dataset, max)
customers_first_purchase$diff <- customers_first_purchase$Tarih - min_date
df <- dataset %>% left_join( customers_first_purchase, 
                             by=c('Hasta.Takma.Ad�'='Hasta.Takma.Ad�'))
df1 <- df %>% left_join( customers_last_purchase, 
                         by=c('Hasta.Takma.Ad�'='Hasta.Takma.Ad�'))
df1$transfered_date<-df1$Tarih.x-df1$diff
colnames(df1) <- c('data_record.Tarih','Hasta.Takma.Ad�','Sat��.Miktar�','cf_purchase.Tarih','diff',' customers_last_purchase','transfered_Date')
date_range<- max(df1$transfered_Date) - min(df1$transfered_Date)
date_range<-as.numeric(date_range)

date_interval<-0
cutoff<-0
date_interval.frame<-data.frame()
while(max(df1$transfered_Date)>date_interval){
  date_interval=min_date+cutoff
  as.Date(date_interval)
  cutoff<- T + cutoff
  new_row <- cbind(format(as.Date(date_interval,'%Y-%m-%d')))
  colnames(new_row) <- c('date_split')
  date_interval.frame<- rbind(date_interval.frame,new_row)
}
date_interval.frame$date_split<-as.Date(date_interval.frame$date_split)
date_interval.frame

gen_average.frame <-data.frame()

for(i in c(1:(length(date_interval.frame$date_split)-1))){
  start_date <-date_interval.frame$date_split[i]
  end_date<-date_interval.frame$date_split[i+1]-1
  filtered_data <- subset(df1, transfered_Date >= start_date & transfered_Date <= end_date)
  customer_average <- aggregate(Sat��.Miktar� ~ Hasta.Takma.Ad�, filtered_data, sum)
  gen_average <- sum(customer_average$Sat��.Miktar�) /length(customer_average$Sat��.Miktar�)
  gen_average
  new_row <- cbind(i,format(as.Date(start_date,'%Y-%m-%d')), format(as.Date(end_date,'%Y-%m-%d')), gen_average)
  gen_average.frame<- rbind(gen_average.frame,new_row)
}
colnames(gen_average.frame) <- c('Interval_num','start_date','end_date','gen_average')
gen_average.frame

write.csv(gen_average.frame, file = "average_transactions_pcustomer.csv")
write.table(gen_average.frame, file = "average_transactions_pcustomer.txt", sep = '\t')


#### A4 General Equation 
dataset<- churn
r_a <- 4.427
s <- 2.772
B <- 5.050
T <- 30
Tm<-600
lse.valid<-lse(dataset,T,Tm,r_a,s,B)
lse.valid
write.table(lse.valid, file = "lse_X_R.txt", sep = '\t')

dataset<- churn
Tm<-600
T <-30
average_transactions_pcustomer(dataset,T,Tm)
variance_transactions_pcustomer(dataset,T,Tm)

#### For minimize A4 value 1 ~ Finite interval ####
r_a <- c(0.415,0.65)
s <- c(2,3,5)
B <- c(1,5)
T <- 30
dataset <- churn
A4_minimize.finite_interval(r_a,s,B,T,dataset) 

dataset<- churn
r_a <- 6.4
s <- 4.4
B <- 7.5
T <- 30
Tm<-600
learning_rate<-0.00001
tolerance <- 1
kmax<-100
gradient_search.frame1<-gradient_search(dataset,r_a,s,B,T,Tm,learning_rate,tolerance,kmax)     
gradient_search.frame1 
View(gradient_search.frame1) 

### Herustic Optimal Using Gradient Descent with n sample ###
dataset<- churn
T <- 30
Tm<-600
learning_rate<-0.00001
tolerance <- 1
kmax<-10000
start<- 2
end<-9
n<-8
heuristic_min_lse.frame<-random_ivalue(dataset,T,learning_rate,tolerance,kmax,start,end,n)
heuristic_min_lse.frame
index<-which(heuristic_min_lse.frame$lse_value==min(heuristic_min_lse.frame$lse_value))
round(heuristic_min_lse.frame[index,],3)

### Herustic Optimal Using Gradient Descent with n sample  vol2###
heuristic_min_lse.frame2<-random_ivalue.v2(dataset,T,learning_rate,tolerance,kmax)
heuristic_min_lse.frame2
index<-which(heuristic_min_lse.frame2$lse_value==min(heuristic_min_lse.frame2$lse_value))
round(heuristic_min_lse.frame2[index,],3)

######### Optimal Heuristic Value Tm=600 #########
estimator_r_a<- 4.427
estimator_s<- 2.772
estimator_B<- 5.050
# lse_value == 451
# f' value == -0.820


################################# Part 2 ######################################

rm(list=ls())
library(lubridate) #FOR DATE
library(dplyr)  # for Data Manipulation
library(base)
library(SciViews) ## for ln function
library(pROC)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(partykit)
library(pROC)
library(ggplot2)
source("C:/Users/umits/OneDrive - Kadir Has University/Masa�st�/Bitirme/Functions.R")

## A5 equation
dataset<- churn
T <- 30
Tm<-600 #among customers with a purchase history of approximately 20 months
r_a<- 4.427
s<- 2.772
B<- 5.050
variable_set<-alpha_estimator(dataset,r_a,s,B,T,Tm)
variable_set

## Probability of alive(churn) given that information
dataset<- churn
r <- variable_set[1,1]
alpha <- variable_set[1,2]
s <- variable_set[1,3]
B <- variable_set[1,4]
Tm <- 600
T_future1<-90
T_future2<-180
T_future3<-270
T_future4<-360

Churn_Last<-Alive.prob.v2(dataset,r,alpha,s,B,Tm)
dim(Churn_Last)
head(Churn_Last)
View(Churn_Last)

Churn_Last$Last_Purchase<- min(churn$Tarih)+days(Churn_Last$Last_Purchase)
Churn_Last
dim(Churn_Last)
NaN.customer_�d<-Churn_Last$Customer_�d[Churn_Last$Churn_prob == "NaN"]
NaN.customer_�d
write.csv(NaN.customer_�d,"NaN.customer_�d.csv")
Churn_Last<-Churn_Last[Churn_Last$Churn_prob != "NaN",]
write.csv(Churn_Last,"Churn_Last_Final.csv")
head(Churn_Last)

Expected_final<-Expected_X_future(dataset,r,alpha,s,B,T,T_future1,T_future2,T_future3,T_future4)
head(Expected_final)
write.csv(Expected_final,"Expected_final_son.csv")

Expected_final_plot<-NULL
Expected_final_plot$Month_3_Total<- sum(Expected_final$Expected_purchase_3_Month)
Expected_final_plot$Month_6_Total<- sum(Expected_final$Expected_purchase_6_Month)
Expected_final_plot$Month_9_Total<- sum(Expected_final$Expected_purchase_9_Month)
Expected_final_plot$Month_12_Total<- sum(Expected_final$Expected_purchase_12_Month)

head(Expected_final_plot)

write.csv(Expected_final_plot,"Expected_final_plot_son.csv")

# Data frame creation
Expected_final_plot <- data.frame(
  Month = factor(c("Month_3_Total",  "Month_6_Total",  "Month_9_Total", "Month_12_Total"), 
                 levels = c("Month_3_Total", "Month_6_Total", "Month_9_Total", "Month_12_Total")),
  Total = c(
    sum(Expected_final$Expected_purchase_3_Month),
    sum(Expected_final$Expected_purchase_6_Month),
    sum(Expected_final$Expected_purchase_9_Month),
    sum(Expected_final$Expected_purchase_12_Month)
  )
)

# �izgi grafi�i olu�turma
ggplot(Expected_final_plot) +
  geom_line(aes(x = Month, y = Total, group = 1)) +
  geom_point(aes(x = Month, y = Total)) +
  labs(x = "Month", y = "Total") +
  ggtitle("Total Purchases by Month")


################################# Part 3 ######################################

rm(list=ls())
library(lubridate) #FOR DATE
library(dplyr)  # for Data Manipulation
library(base)
library(SciViews) ## for ln function
library(pROC)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(partykit)
library(pROC)
library(ggplot2)
source("C:/Users/umits/OneDrive - Kadir Has University/Masa�st�/Bitirme/Functions.R")
head(churn)

dataset<- churn
Tm<-600
T <-30
average_transactions_pcustomer(dataset,T,Tm)
variance_transactions_pcustomer(dataset,T,Tm)

## A5 equation
dataset<- churn
T <- 30
Tm<-600 #among customers with a purchase history of approximately 20 months
r_a <- 4.427
s <- 2.772
B <- 5.050
variable_set<-alpha_estimator(dataset,r_a,s,B,T,Tm)
variable_set

##We find the customer's last purchases and specify their last purchase.
dataset<- churn
Tm <- 600
customer_info<-customers_information(dataset,Tm)
customer_info
t.vect<-customer_info$t
x.vect<-customer_info$pc_total_purchaseTm

## Probability of alive(churn) given that information
dataset<- churn
r <- variable_set[1,1]
alpha <- variable_set[1,2]
s <- variable_set[1,3]
B <- variable_set[1,4]
T <- 600

alive_probability<-Alive.prob.v2(dataset,r,alpha,s,B,T)
alive_probability[alive_probability$t==250,]

#### split the data
NaN.customer_�d<-alive_probability$Customer_�d[alive_probability$Churn_prob == "NaN"]
NaN.customer_�d
write.csv(NaN.customer_�d,"NaN.customer_�d.csv")
alive_probability<-alive_probability[alive_probability$Churn_prob != "NaN",]
for(i in c(1:length(NaN.customer_�d))){
  churn<-churn[churn$Hasta.Takma.Ad�!=NaN.customer_�d[i],]
  churn
}
test<-churn[churn$Tarih>T+min(churn$Tarih),]
test$churn<- 0
test<-test[,-c(1,3)]
head(test)
test<- test[!duplicated(test), ]
test[test$Hasta.Takma.Ad�=="H01303",]
churn <- churn %>% left_join(test, 
                       by=c('Hasta.Takma.Ad�'='Hasta.Takma.Ad�'))
churn$churn[is.na(churn$churn)] <- 1
Churn <- alive_probability %>% left_join(churn, 
                             by=c('Customer_�d'='Hasta.Takma.Ad�'))
Churn<- Churn[!duplicated(Churn), ]
pharmacy<-Churn
pharmacy<-pharmacy[,-c(5,6)]
pharmacy<- pharmacy[!duplicated(pharmacy),]
str(pharmacy)
pharmacy$Churn_prob<- as.numeric(pharmacy$Churn_prob)
pharmacy$Purchase_amount<- as.numeric(pharmacy$Purchase_amount)
pharmacy$Last_Purchase<- as.numeric(pharmacy$Last_Purchase)
View(pharmacy)
View(churn)
View(data)
pharmacy[pharmacy$Customer_�d=="H00025",]
View(pharmacy)
write.csv(pharmacy,"pharmacy.csv")


####################  Binary Classification   ##################

set.seed(123)
Index <- createDataPartition(pharmacy$churn, p = 0.2, list = FALSE)
pharmacy.test <- pharmacy[Index,]
View(pharmacy.test)

tau = 0.8
pharmacy.test$Predict.Class<- (pharmacy.test$Churn_prob>tau)*1

TP= sum((pharmacy.test$Predict.Class==1)&(pharmacy.test$churn==1))
TN= sum((pharmacy.test$Predict.Class==0)&(pharmacy.test$churn==0))
FP= sum((pharmacy.test$Predict.Class==1)&(pharmacy.test$churn==0))
FN= sum((pharmacy.test$Predict.Class==0)&(pharmacy.test$churn==1))

confusion.mat.02<- matrix(c(TP,FP,FN,TN),2,2)
confusion.mat.02

#accuracy     ## It measures how many observations, both positive and negative, were correctly classified. 
## Shouldn�t use accuracy on imbalanced problems.
accuracy.02<-(TP+TN) / (TP+TN+FP+FN)
accuracy.02

#precision   ## It shows how many of those predicted as positive are actually positive. 
## If the precision is low, it indicates a large number of false positives.
precision.02 = TP / (TP + FP)
precision.02

#recall == sensitivity   ## It shows how many of the values we need to predict as positive are positive. 
##If the recall is low, it means that there are many false negatives.
recall.02<- TP / (TP + FN)
recall.02

#F1 score = 2* (precision *recall) / (precision +recal) (*beta=1 for this function)
## It combines precision and recall into one metric by calculating the harmonic mean between those two.
F1_score.02 <- 2 * (precision.02*recall.02)/(precision.02+recall.02)
F1_score.02


tau = 0.8
pharmacy.test$Predict.Class<- (pharmacy.test$Churn_prob>tau)*1

TP= sum((pharmacy.test$Predict.Class==1)&(pharmacy.test$churn==1))
TN= sum((pharmacy.test$Predict.Class==0)&(pharmacy.test$churn==0))
FP= sum((pharmacy.test$Predict.Class==1)&(pharmacy.test$churn==0))
FN= sum((pharmacy.test$Predict.Class==0)&(pharmacy.test$churn==1))

confusion.mat.08<- matrix(c(TP,FP,FN,TN),2,2)
confusion.mat.08

#accuracy     ## It measures how many observations, both positive and negative, were correctly classified. 
## Shouldn�t use accuracy on imbalanced problems.
accuracy.03<-(TP+TN) / (TP+TN+FP+FN)
accuracy.03

#precision   ## It shows how many of those predicted as positive are actually positive. 
## If the precision is low, it indicates a large number of false positives.
precision.03 = TP / (TP + FP)
precision.03

#recall == sensitivity   ## It shows how many of the values we need to predict as positive are positive. 
##If the recall is low, it means that there are many false negatives.
recall.03<- TP / (TP + FN)
recall.03

#F1 score = 2* (precision *recall) / (precision +recal) (*beta=1 for this function)
## It combines precision and recall into one metric by calculating the harmonic mean between those two.
F1_score.03 <- 2 * (precision.03*recall.03)/(precision.03+recall.03)
F1_score.03



##################  FOR VAL�DAT�ON  ##################
accuracy2.05.vect<-0
recall2.05.vect<- 0
precision2.05.vect<- 0
F1_score.05<-0
N05.vect<- 0
P05.vect<- 0
FPR05.vect<- 0
TPR05.vect<- 0
TP05<-0
TN05<-0
FP05<-0
FN05<-0
accuracy2.03.vect<-0
recall2.03.vect<- 0
precision2.03.vect<- 0
F1_score.03<-0
N03.vect<- 0
P03.vect<- 0
FPR03.vect<- 0
TPR03.vect<- 0
TP03<-0
TN03<-0
FP03<-0
FN03<-0


for(r in 1:100)
{
  
  Index <- createDataPartition(pharmacy$churn, p = 0.2, list = FALSE)
  pharmacy.test <- pharmacy[Index,]
  
  tau = 0.5
  pharmacy.test$Predict.Class<- (pharmacy.test$Churn_prob>tau)*1
  
  TP05[r]= sum((pharmacy.test$Predict.Class==1)&(pharmacy.test$churn==1))
  TN05[r]= sum((pharmacy.test$Predict.Class==0)&(pharmacy.test$churn==0))
  FP05[r]= sum((pharmacy.test$Predict.Class==1)&(pharmacy.test$churn==0))
  FN05[r]= sum((pharmacy.test$Predict.Class==0)&(pharmacy.test$churn==1))
  
  confusion.mat2.05<- matrix(c(TN05,FP05,FN05,TP05),2,2)
  confusion.mat2.05
  
  accuracy2.05.vect[r]<-(TP05[r]+TN05[r]) / (TP05[r]+TN05[r]+FP05[r]+FN05[r])
  precision2.05.vect[r] = TP05[r] / (TP05[r] + FP05[r])
  recall2.05.vect[r] <-  TP05[r] / (TP05[r] + FN05[r])
  F1_score.05[r] <- 2 * ( precision2.05.vect[r]* recall2.05.vect[r])/( precision2.05.vect[r]+ recall2.05.vect[r])

  N05.vect[r]<- TN05[r] + FP05[r]
  P05.vect[r]<- FN05[r] + TP05[r]
  FPR05.vect[r]<- FP05[r] / N05.vect[r]
  TPR05.vect[r]<- TP05[r] / P05.vect[r]
  
  
  tau = 0.3
  pharmacy.test$Predict.Class<- (pharmacy.test$Churn_prob>tau)*1
  
  TP03[r]= sum((pharmacy.test$Predict.Class==1)&(pharmacy.test$churn==1))
  TN03[r]= sum((pharmacy.test$Predict.Class==0)&(pharmacy.test$churn==0))
  FP03[r]= sum((pharmacy.test$Predict.Class==1)&(pharmacy.test$churn==0))
  FN03[r]= sum((pharmacy.test$Predict.Class==0)&(pharmacy.test$churn==1))
  
  confusion.mat2.03<- matrix(c(TN03,FP03,FN03,TP03),2,2)
  confusion.mat2.03
  
  accuracy2.03.vect[r]<-(TP03[r]+TN03[r]) / (TP03[r]+TN03[r]+FP03[r]+FN03[r])
  precision2.03.vect[r] = TP03[r] / (TP03[r] + FP03[r])
  recall2.03.vect[r] <-  TP03[r] / (TP03[r] + FN03[r])
  F1_score.03[r] <- 2 * ( precision2.03.vect[r]* recall2.03.vect[r])/( precision2.03.vect[r]+ recall2.03.vect[r])
  
  N03.vect[r]<- TN03[r] + FP03[r]
  P03.vect[r]<- FN03[r] + TP03[r]
  FPR03.vect[r]<- FP03[r] / N03.vect[r]
  TPR03.vect[r]<- TP03[r] / P03.vect[r]
}


accuracy2.05.vect
precision2.05<- mean(precision2.05.vect)
accuracy2.05<- mean(accuracy2.05.vect)
recall2.05<- mean(recall2.05.vect)
F1_score.05<-mean(F1_score.05)

precision2.03<- mean(precision2.03.vect)
accuracy2.03<- mean(accuracy2.03.vect)
recall2.03<- mean(recall2.03.vect)
F1_score.03<-mean(F1_score.03)

M.FPR05<-mean(FPR05.vect)
M.TPR05<-mean(TPR05.vect)
M.TP05<-mean(TP05)
M.TN05<-mean(TN05)
M.FP05<-mean(FP05)
M.FN05<-mean(FN05)
M.FPR03<-mean(FPR03.vect)
M.TPR03<-mean(TPR03.vect)
M.TP03<-mean(TP03)
M.TN03<-mean(TN03)
M.FP03<-mean(FP03)
M.FN03<-mean(FN03)


####  ROC CURVE 
accuracy.vect<-0
recall.vect<- 0
precision.vect<- 0
N.vect<- 0
P.vect<- 0
FPR.vect<- 0
TPR.vect<- 0
F1_score.vect<-0
TP<-0
TN<-0
FP<-0
FN<-0

tau.vect<-c(seq(0,1,0.00001))
tau.vect
length(tau.vect)
Index <- createDataPartition(pharmacy$churn, p = 0.2, list = FALSE)
pharmacy.test <- pharmacy[Index,]
dim(pharmacy.test)

for(r in c(1:length(tau.vect))){
  tau = tau.vect[r]
  pharmacy.test$Predict.Class<- (pharmacy.test$Churn_prob>tau)*1
  
  TP[r]= sum((pharmacy.test$Predict.Class==1)&(pharmacy.test$churn==1))
  TN[r]= sum((pharmacy.test$Predict.Class==0)&(pharmacy.test$churn==0))
  FP[r]= sum((pharmacy.test$Predict.Class==1)&(pharmacy.test$churn==0))
  FN[r]= sum((pharmacy.test$Predict.Class==0)&(pharmacy.test$churn==1))
  
  confusion.mat<- matrix(c(TN[r],FP[r],FN[r],TP[r]),2,2)
  confusion.mat
  
  accuracy.vect[r]<-(TP[r]+TN[r]) / (TP[r]+TN[r]+FP[r]+FN[r])
  precision.vect[r] = TP[r] / (TP[r] + FP[r])
  recall.vect[r] <-  TP[r] / (TP[r] + FN[r])
  F1_score.vect[r] <- 2 * (precision.vect[r]*recall.vect[r])/(precision.vect[r]+recall.vect[r])
  
  ## TPR = TP / (TP + FN)
  ## FPR = FP / (FP + TN)
  N.vect[r]<- TN[r] + FP[r]
  P.vect[r]<- FN[r] + TP[r]
  FPR.vect[r]<- FP[r] / N.vect[r]
  TPR.vect[r]<- TP[r] / P.vect[r]

}

TPR.vect
FPR.vect

# Plot the ROC curve
plot(FPR.vect, TPR.vect, type = "l", lwd = 2, col = "blue",
     xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)",
     xlim = c(0, 1), ylim = c(0, 1), main = "ROC Curve for Pareto/NBD Model")
abline(a = 0, b = 1, lwd = 1, col = "black")

### AUC VALUE
auc_result <- roc(pharmacy.test$churn, pharmacy.test$Churn_prob)
Pareto_NBD.auc_value <- auc(auc_result)
Pareto_NBD.auc_value

# Calculate the AUC (Area Under the Curve)
legend("bottomright", legend = paste("AUC =", round(Pareto_NBD.auc_value, 3)), bty = "n", cex = 0.8)


tau.vect<-c(seq(0,1,0.00001))
tau.vect
#### Best Tau ####
best_tau<- data.frame(
  tau<-tau.vect,
  accuracy<- c(accuracy.vect),
  precision<- precision.vect,
  recall<-recall.vect,
  F1_score<- F1_score.vect
)
colnames(best_tau) <- c('tau','accuracy','precision','recall','F1_score')
best_tau<-best_tau[-101,]

unique(best_tau$accuracy)
which(max(best_tau$accuracy)==best_tau$accuracy)
best_F1.score<-best_tau[which(max(best_tau$accuracy)==best_tau$accuracy)[1],]
best_F1.score

##################################### THE END ###########################


################################## Classic Models ########################

###############  Feature extraction and preprocessing #################
## feature extraction and preprocessing prepared in excel
classic_data <- read.csv('Filtrelenmi�-SON-6.csv',sep =';',header=TRUE, encoding="UTF-8")
View(classic_data)
str(classic_data)
colnames(classic_data)[1] <- ("Hasta.Takma.Ad�")

for(i in c(1:length(NaN.customer_�d))){
  classic_data<-classic_data[classic_data$Hasta.Takma.Ad�!=NaN.customer_�d[i],]
  classic_data
}

classic_data <- classic_data %>% left_join(pharmacy, 
                             by=c('Hasta.Takma.Ad�'='Customer_�d'))
colnames(classic_data)
dim(classic_data)
sapply(classic_data, function(x) sum(is.na(x)))
which(is.na(classic_data$Churn_prob))  
classic_data<-classic_data[-c(1211,1297,1717),]
dim(classic_data)
dim(pharmacy)
sum(is.na(classic_data))
sum(is.na(pharmacy))


#split the data
set.seed(123)
Index <- createDataPartition(classic_data$churn, p = 0.8, list = FALSE)
classic_data.train <- classic_data[Index,]
dim(classic_data.train)
summary(classic_data.train)
str(classic_data.train)
classic_data.test <- classic_data[-Index,]
summary(classic_data.test)
dim(classic_data.test)
str(classic_data.test)
str(classic_data.train)

##################  Decision Tree ###################### 
classic_data1<-classic_data[,-c(1,8,10,12)]
classic_data1.train<- classic_data.train[,-c(1,8,10,12)]
classic_data1.test<- classic_data.test[,-c(1,8,10,12)]
colnames(classic_data1.train)
str(classic_data)
colnames(classic_data1)
colnames(classic_data1.train)
str(classic_data.test)
str(classic_data.train)
dim(classic_data1)
dim(classic_data1.train)
dim(classic_data1.test)
classic_data1$churn<-as.factor(classic_data1$churn)
classic_data1.train$churn<-as.factor(classic_data1.train$churn)
classic_data1.test$churn<-as.factor(classic_data1.test$churn)
str(classic_data1)

# model<- ctree(churn ~ . , data = classic_data1.train)
# summary(model)
# plot(model)
# prediction_tree<- predict(model, newdata = classic_data1.test, type = "response")
# prediction_tree
  
dec_tree<- rpart(churn ~ . , data = classic_data1.train,parms = list(prior = c(.60,.40),split = "information"))
summary(dec_tree)
rpart.plot(dec_tree, type=5,cex=0.6)

prediction_tree<- predict(dec_tree,newdata = classic_data1.test,type = "class",)
prediction_tree
ctable <- table(classic_data1.test$churn,prediction_tree)
ctable
confusionMatrix(ctable)

precision1 <- precision(ctable)
recall1 <- recall(ctable)
F1_score1<- F_meas(ctable)

prediction_tree_prob<- predict(dec_tree, newdata = classic_data1.test, type = "prob")
head(prediction_tree_prob)
prediction_tree_prob<-prediction_tree_prob[,2]
prediction_tree_prob

## ROC CURVE FOR 
accuracy.vect<-0
recall.vect<- 0
precision.vect<- 0
N.vect<- 0
P.vect<- 0
FPR.vect1<- 0
TPR.vect1<- 0
F1_score.vect<-0
TP<-0
TN<-0
FP<-0
FN<-0

tau.vect<-c(seq(0,0.99999,0.00001))
for(r in c(1:length(tau.vect))){
  tau = tau.vect[r]
  classic_data1.test$Predict.Class<- (prediction_tree_prob>tau)*1
  
  TP[r]= sum((classic_data1.test$Predict.Class==1)&(classic_data1.test$churn==1))
  TN[r]= sum((classic_data1.test$Predict.Class==0)&(classic_data1.test$churn==0))
  FP[r]= sum((classic_data1.test$Predict.Class==1)&(classic_data1.test$churn==0))
  FN[r]= sum((classic_data1.test$Predict.Class==0)&(classic_data1.test$churn==1))
  
  confusion.mat<- matrix(c(TN[r],FP[r],FN[r],TP[r]),2,2)
  confusion.mat
  
  accuracy.vect[r]<-(TP[r]+TN[r]) / (TP[r]+TN[r]+FP[r]+FN[r])
  precision.vect[r] = TP[r] / (TP[r] + FP[r])
  recall.vect[r] <-  TP[r] / (TP[r] + FN[r])
  F1_score.vect[r] <- 2 * (precision.vect[r]*recall.vect[r])/(precision.vect[r]+recall.vect[r])
  
  ## TPR = TP / (TP + FN)
  ## FPR = FP / (FP + TN)
  N.vect[r]<- TN[r] + FP[r]
  P.vect[r]<- FN[r] + TP[r]
  FPR.vect1[r]<- FP[r] / N.vect[r]
  TPR.vect1[r]<- TP[r] / P.vect[r]
}

TPR.vect1
FPR.vect1

# Plot the ROC curve
plot(FPR.vect1, TPR.vect1, type = "l", lwd = 2, col = "red",
     xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)",
     xlim = c(0, 1), ylim = c(0, 1), main = "ROC Curve of Decision Tree")
abline(a = 0, b = 1, lwd = 1, col = "black")

# AUC VALUE
roc_obj <- roc(classic_data1.test$churn, prediction_tree_prob)
decision_tree.auc_value<- auc(roc_obj)
decision_tree.auc_value

# Calculate the AUC (Area Under the Curve)
legend("bottomright", legend = paste("AUC =", round(decision_tree.auc_value, 3)), bty = "n", cex = 0.8)


############## KNN ##################
knn_fit <- train(churn ~ ., data = classic_data1.train, method = "knn",preProcess = c("scale"))
knn_fit$bestTune
knn_fit
plot(knn_fit)

predict3 <- predict(knn_fit, classic_data1.test)
ctable3 <- table(classic_data1.test$churn, predict3)
ctable3
confusionMatrix(ctable3)
precision3 <- precision(ctable3)
recall3 <- recall(ctable3)
F1_score3<- F_meas(ctable3)

predict_knn_prob <- predict(knn_fit, newdata = classic_data1.test, type = "prob")
predict_knn_prob<-predict_knn_prob[,2]
predict_knn_prob


library(ggplot2)

# Train the K-NN model
knn_fit <- train(churn ~ ., data = classic_data1.train, method = "knn", preProcess = c("scale"))

# Make predictions on the test set
predictions <- predict(knn_fit, newdata = classic_data1.test)

# Create a scatter plot of two features with color-coded classes
ggplot(classic_data1.test, aes(x = classic_data1.test$Sum_Time_Between, y = classic_data1.test$Arrivals_num_person, color = churn)) +
  geom_point() +
  labs(x = "Feature 1", y = "Feature 2", color = "Churn") +
  theme_minimal()



## ROC CURVE FOR 
accuracy.vect<-0
recall.vect<- 0
precision.vect<- 0
N.vect<- 0
P.vect<- 0
FPR.vect3<- 0
TPR.vect3<- 0
F1_score.vect<-0
TP<-0
TN<-0
FP<-0
FN<-0

tau.vect<-c(seq(0,1,0.0001))
for(r in c(1:length(tau.vect))){
  tau = tau.vect[r]
  classic_data1.test$Predict.Class<- (predict_knn_prob>tau)*1
  
  TP[r]= sum((classic_data1.test$Predict.Class==1)&(classic_data1.test$churn==1))
  TN[r]= sum((classic_data1.test$Predict.Class==0)&(classic_data1.test$churn==0))
  FP[r]= sum((classic_data1.test$Predict.Class==1)&(classic_data1.test$churn==0))
  FN[r]= sum((classic_data1.test$Predict.Class==0)&(classic_data1.test$churn==1))

  confusion.mat<- matrix(c(TN[r],FP[r],FN[r],TP[r]),2,2)
  confusion.mat
  accuracy.vect[r]<-(TP[r]+TN[r]) / (TP[r]+TN[r]+FP[r]+FN[r])
  precision.vect[r] = TP[r] / (TP[r] + FP[r])
  recall.vect[r] <-  TP[r] / (TP[r] + FN[r])
  F1_score.vect[r] <- 2 * (precision.vect[r]*recall.vect[r])/(precision.vect[r]+recall.vect[r])
  
  ## TPR = TP / (TP + FN)
  ## FPR = FP / (FP + TN)
  N.vect[r]<- TN[r] + FP[r]
  P.vect[r]<- FN[r] + TP[r]
  FPR.vect3[r]<- FP[r] / N.vect[r]
  TPR.vect3[r]<- TP[r] / P.vect[r]
}

TPR.vect3
FPR.vect3

# Plot the ROC curve
plot(FPR.vect3, TPR.vect3, type = "l", lwd = 2, col = "green",
     xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)",
     xlim = c(0, 1), ylim = c(0, 1), main = "ROC Curve of KNN")
abline(a = 0, b = 1, lwd = 1, col  = "black")

# AUC VALUE
roc_obj <- roc(classic_data1.test$churn, predict_knn_prob)
KNN.auc_value<- auc(roc_obj)
KNN.auc_value

# Calculate the AUC (Area Under the Curve)
legend("bottomright", legend = paste("AUC =", round(KNN.auc_value, 3)), bty = "n", cex = 0.8)


############## Random Forest ##################
rf_model <- randomForest(churn~., data = classic_data1.train, ntree = 100)
rf_model
plot(rf_model)

# Make predictions on the test set
predict4 <- predict(rf_model, newdata = classic_data1.test)
ctable4 <- table(classic_data1.test$churn, predict4)
ctable4
confusionMatrix(ctable4)
precision4 <- precision(ctable4)
recall4 <- recall(ctable4)
F1_score4<- F_meas(ctable4)

predict_rf_prob <- predict(rf_model, newdata = classic_data1.test, type = "prob")
predict_rf_prob<-predict_rf_prob[,2]
predict_rf_prob


# Calculate feature importance
importance <- importance(rf_model)

# Plot feature importance
varImpPlot(rf_model)


# Install required packages if not already installed
install.packages(c("randomForest", "partykit"))
library(randomForest)
library(partykit)

# Train the Random Forest model
rf_model <- randomForest(churn ~ ., data = classic_data1.train, ntree = 100)

# Convert Random Forest model to party object
party_model <- as.party(rf_model)

# Plot the party object
plot(party_model)
##############333
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Yas', 'TahminiMaas')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Random Forest S�n�fland�rma (Test seti)',
     xlab = 'Ya�', ylab = 'Maa�',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))








## ROC CURVE FOR
accuracy.vect<-0
recall.vect<- 0
precision.vect<- 0
N.vect<- 0
P.vect<- 0
FPR.vect4<- 0
TPR.vect4<- 0
F1_score.vect<-0
TP<-0
TN<-0
FP<-0
FN<-0

tau.vect<-c(seq(0,1,0.0001))
for(r in c(1:length(tau.vect))){
  tau = tau.vect[r]
  classic_data1.test$Predict.Class<- (predict_rf_prob>tau)*1

  TP[r]= sum((classic_data1.test$Predict.Class==1)&(classic_data1.test$churn==1))
  TN[r]= sum((classic_data1.test$Predict.Class==0)&(classic_data1.test$churn==0))
  FP[r]= sum((classic_data1.test$Predict.Class==1)&(classic_data1.test$churn==0))
  FN[r]= sum((classic_data1.test$Predict.Class==0)&(classic_data1.test$churn==1))
  confusion.mat<- matrix(c(TN[r],FP[r],FN[r],TP[r]),2,2)
  confusion.mat
  accuracy.vect[r]<-(TP[r]+TN[r]) / (TP[r]+TN[r]+FP[r]+FN[r])
  precision.vect[r] = TP[r] / (TP[r] + FP[r])
  recall.vect[r] <-  TP[r] / (TP[r] + FN[r])
  F1_score.vect[r] <- 2 * (precision.vect[r]*recall.vect[r])/(precision.vect[r]+recall.vect[r])

  ## TPR = TP / (TP + FN)
  ## FPR = FP / (FP + TN)
  N.vect[r]<- TN[r] + FP[r]
  P.vect[r]<- FN[r] + TP[r]
  FPR.vect4[r]<- FP[r] / N.vect[r]
  TPR.vect4[r]<- TP[r] / P.vect[r]

}

TPR.vect4
FPR.vect4

# Plot the ROC curve
plot(FPR.vect4, TPR.vect4, type = "l", lwd = 2, col = "orange",
     xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)",
     xlim = c(0, 1), ylim = c(0, 1), main = "ROC Curve of Random Forest")
abline(a = 0, b = 1, lwd = 1, col = "black")

# AUC VALUE
roc_obj <- roc(classic_data1.test$churn, predict_rf_prob)
RF.auc_value<- auc(roc_obj)
RF.auc_value

# Calculate the AUC (Area Under the Curve)
legend("bottomright", legend = paste("AUC =", round(RF.auc_value, 3)), bty = "n", cex = 0.8)


# Evaluate the model  #############

## TAU 0.2
comparison <- data.frame(
  Model = c("Pareto/NBD","Decision Tree","Naive Bayes","kNN"),
  #accuracy = c(best_F1.score$accuracy,accuracy1, accuracy2,accuracy3),
  precision = c(precision1, precision3,precision4),
  recall = c(recall1, recall3,recall4),
  F1_score = c(F1_score1,F1_score3,F1_score4),
  AUC_score = c(decision_tree.auc_value,KNN.auc_value,RF.auc_value),
  stringsAsFactors = FALSE
)

comparison



TPR.vect
FPR.vect

# Plot the ROC curve
plot(FPR.vect, TPR.vect, type = "l", lwd = 2, col = "blue",
     xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)",
     xlim = c(0, 1), ylim = c(0, 1), main = "ROC Curve for Pareto/NBD Model")
abline(a = 0, b = 1, lwd = 1, col = "black")

### AUC VALUE
auc_result <- roc(pharmacy.test$churn, pharmacy.test$Churn_prob)
Pareto_NBD.auc_value <- auc(auc_result)
Pareto_NBD.auc_value

# Calculate the AUC (Area Under the Curve)
legend("bottomright", legend = paste("AUC =", round(Pareto_NBD.auc_value, 3)), bty = "n", cex = 0.8)





TPR.vect1
FPR.vect1

# Plot the ROC curve
plot(FPR.vect1, TPR.vect1, type = "l", lwd = 2, col = "red",
     xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)",
     xlim = c(0, 1), ylim = c(0, 1), main = "ROC Curve of Decision Tree")
abline(a = 0, b = 1, lwd = 1, col = "black")

# AUC VALUE
roc_obj <- roc(classic_data1.test$churn, prediction_tree_prob)
decision_tree.auc_value<- auc(roc_obj)
decision_tree.auc_value

# Calculate the AUC (Area Under the Curve)
legend("bottomright", legend = paste("AUC =", round(decision_tree.auc_value, 3)), bty = "n", cex = 0.8)



TPR.vect3
FPR.vect3

# Plot the ROC curve
plot(FPR.vect3, TPR.vect3, type = "l", lwd = 2, col = "green",
     xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)",
     xlim = c(0, 1), ylim = c(0, 1), main = "ROC Curve of KNN")
abline(a = 0, b = 1, lwd = 1, col  = "black")

# AUC VALUE
roc_obj <- roc(classic_data1.test$churn, predict_knn_prob)
KNN.auc_value<- auc(roc_obj)
KNN.auc_value

# Calculate the AUC (Area Under the Curve)
legend("bottomright", legend = paste("AUC =", round(KNN.auc_value, 3)), bty = "n", cex = 0.8)



TPR.vect4
FPR.vect4

# Plot the ROC curve
plot(FPR.vect4, TPR.vect4, type = "l", lwd = 2, col = "orange",
     xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)",
     xlim = c(0, 1), ylim = c(0, 1), main = "ROC Curve of Random Forest")
abline(a = 0, b = 1, lwd = 1, col = "black")

# AUC VALUE
roc_obj <- roc(classic_data1.test$churn, predict_rf_prob)
RF.auc_value<- auc(roc_obj)
RF.auc_value

# Calculate the AUC (Area Under the Curve)
legend("bottomright", legend = paste("AUC =", round(RF.auc_value, 3)), bty = "n", cex = 0.8)


###############

# Create a new plot with empty axes
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
     xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)",
     main = "ROC Curves of Different Models")

# Plot ROC curve for Pareto/NBD Model
lines(FPR.vect, TPR.vect, type = "l", lwd = 2, col = "blue")

# Plot ROC curve for Decision Tree
lines(FPR.vect1, TPR.vect1, type = "l", lwd = 2, col = "red")

# Plot ROC curve for KNN
lines(FPR.vect3, TPR.vect3, type = "l", lwd = 2, col = "green")

# Plot ROC curve for Random Forest
lines(FPR.vect4, TPR.vect4, type = "l", lwd = 2, col = "orange")

# Add legend for AUC values
legend("bottomright", legend = c(paste("Pareto/NBD AUC =", round(Pareto_NBD.auc_value, 3)),
                                 paste("Decision Tree AUC =", round(decision_tree.auc_value, 3)),
                                 paste("KNN AUC =", round(KNN.auc_value, 3)),
                                 paste("Random Forest AUC =", round(RF.auc_value, 3))),
       bty = "n", col = c("blue", "red", "green", "orange"), lwd = 2, cex = 0.8)

# Add a diagonal reference line
abline(a = 0, b = 1, lwd = 1, col = "black")
