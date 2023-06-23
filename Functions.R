############################################### DATA PREPROCESSING  ############################################### 
## Na, date and unnecessary column checking 
data<- read.csv('2017-2022VerilerMaskelenmis_Stoksuz.csv',sep =';',header=TRUE, encoding="UTF-8")
head(data)
data<- data[,-c(1,4,5)]
head(data)
summary(data)
data$Tarih<- strptime(data$Tarih ,'%d.%m.%Y')
data$Tarih <- as.Date(data$Tarih)
index<- (data$Hasta.Takma.Ad� != '#YOK')
data <- data[index,]

## Outlier analysis
LB<- mean(data$Sat��.Miktar�) - (3*sd(data$Sat��.Miktar�))
UB<-round(mean(data$Sat��.Miktar�) + (3*sd(data$Sat��.Miktar�)))
index<-(data$Sat��.Miktar�<=UB)
churn <- data.frame()
churn<- data[index,]
dim(churn)
head(churn)

############################################### FUNCTIONS ############################################### 

## Calculation of z parameter over alpha B,r and s, T,t values in calculation for Churn Probability( Case 1,2,3)
z1 <- function(y) {
  z <- (alpha-B)/(alpha+y)
}
z2 <- function(y) {
  z <- (B-alpha)/(B+y)
}

## integral type  for hypergeometric 
integral_hypergeometric <-function(a,b,c,z){
  integral <- function(t) {
    t^(b-1) * (1-t)^(c-b-1) * (1-z*t)^(-a)
  }
  result <- integrate(integral, lower=0, upper=1)
  if (is.finite(result$value)) {
    hypergeo_val <- (gamma(c) / (gamma(b) * gamma(c - b))) * result$value
  } 
  else {
    hypergeo_val <- NaN
  }
  hypergeo_val
}

## fundamental hypergeometric equation 15.1.1 (abrahowitz and Stegun)
fundamental_hyper<- function(a,b,c,z){
  nvect= c(0:500)
  xvect= gamma(a+nvect)*gamma(b+nvect)/gamma(c+nvect) * (z^nvect)/factorial(nvect) 
  sum(xvect[is.finite(xvect)]) * gamma(c) / gamma(b) / gamma(a)
}

#### Equation 11,12,13 (Pareto/NBD Model)
## Probability of alive given that information
Alive.prob  <-function(r,alpha,s,B,x,t,T){

    if (alpha > B) {
      a1 <- r+x+s
      b1 <- s+1
      c1 <- r+x[k]+s+1
      F_dist1 <- fundamental_hyper(a1, b1, c1, z1(t))
      F_dist2 <- fundamental_hyper(a1, b1, c1, z1(T))
      P_alive = 1/((1+((s/(r+x+s)) * (((((alpha+T)/(alpha+t))^(r+x[k])) * (((B+T)/(alpha+t))^s) * F_dist1) - ((((B+T)/(alpha+T))^s) * F_dist2)))))
    }
    ## Case 2
    if (alpha < B) {
      a2 <- r+x+s
      b2 <- r+x
      c2 <- r+x+s+1
      F_dist1 <- fundamental_hyper(a2, b2, c2, z2(t))
      F_dist2 <- fundamental_hyper(a2, b2, c2, z2(T))
      P_alive = 1/((1+((s/(r+x+s)) * (((((alpha+T)/(B+t))^(r+x)) * (((B+T)/(B+t))^s) * F_dist1) - ((((alpha+T)/(B+T))^(r+x)) * F_dist2)))))
    } 
    ## Case 3
    if (alpha == B) {
      P_alive =  1/((1+((s/(r+x+s)) * ((((alpha+T)/(alpha+t))^(r+x+s))-1))))  
    } 
    P_alive
  
  }
 

#### Equation 11,12,13 (Pareto/NBD Model)
## Probability of alive given that information
Alive.prob.v2 <-function(dataset,r,alpha,s,B,T){
  ## Case 1
  P_alive<-NULL
  alive_probability<-data.frame()
  train<-dataset[dataset$Tarih<=T+min(dataset$Tarih),]
  min_date<-min(train$Tarih)
  customers_last_purchase <- aggregate(Tarih ~ Hasta.Takma.Ad�, train, max)
  customers_last_purchase$t<-as.integer(customers_last_purchase$Tarih-min_date)
  customers_total_purchase <- aggregate(Sat��.Miktar� ~ Hasta.Takma.Ad�, train, sum)
  customer_info <- customers_last_purchase %>% left_join( customers_total_purchase, 
                                                          by=c('Hasta.Takma.Ad�'='Hasta.Takma.Ad�'))
  colnames(customer_info) <- c('Hasta.Takma.Ad�','cl_purchase.Tarih','t','pc_total_purchaseTm')
  customer_info
  t.vect<-customer_info$t
  x.vect<-customer_info$pc_total_purchaseTm
  
  for(k in c(1:length(t.vect))){
    if (alpha > B) {
      a1 <- r+x.vect[k]+s
      b1 <- s+1
      c1 <- r+x.vect[k]+s+1
      F_dist1 <- fundamental_hyper(a1, b1, c1, z1(t.vect[k]))
      F_dist2 <- fundamental_hyper(a1, b1, c1, z1(T))
      P_alive = 1/((1+((s/(r+x.vect[k]+s)) * (((((alpha+T)/(alpha+t.vect[k]))^(r+x.vect[k])) * (((B+T)/(alpha+t.vect[k]))^s) * F_dist1) - ((((B+T)/(alpha+T))^s) * F_dist2)))))
    }
    ## Case 2
    if (alpha < B) {
      a2 <- r+x.vect[k]+s
      b2 <- r+x.vect[k]
      c2 <- r+x.vect[k]+s+1
      F_dist1 <- fundamental_hyper(a2, b2, c2, z2(t.vect[k]))
      F_dist2 <- fundamental_hyper(a2, b2, c2, z2(T))
      P_alive = 1/((1+((s/(r+x.vect[k]+s)) * (((((alpha+T)/(B+t.vect[k]))^(r+x.vect[k])) * (((B+T)/(B+t.vect[k]))^s) * F_dist1) - ((((alpha+T)/(B+T))^(r+x.vect[k])) * F_dist2)))))
    } 
    ## Case 3
    if (alpha == B) {
      P_alive =  1/((1+((s/(r+x.vect[k]+s)) * ((((alpha+T)/(alpha+t.vect[k]))^(r+x.vect[k]+s))-1))))  
    } 
    P_churn<-1-P_alive
    new_row <- cbind(customer_info$Hasta.Takma.Ad�[k], x.vect[k], t.vect[k], P_churn)
    alive_probability<- rbind(alive_probability,new_row)
  }
  colnames(alive_probability) <- c('Customer_�d','Purchase_amount','Last_Purchase','Churn_prob')
  alive_probability
}


#### Equation 17
# The expected number of purchases
Expected_X<- function(r_a,s,B,T){
  ((r_a * B) /(s - 1)) *  (1 - ((B / (B + T))^(s-1)))
}


#### Equation 22
# The expected number of purchases in the period (T, T + T*] 
Expected_X_future<- function(dataset,r,alpha,s,B,T,T_future1,T_future2,T_future3,T_future4){
  
  E_future_purchase<-data.frame()
  train<-dataset[dataset$Tarih<=T+min(dataset$Tarih),]
  min_date<-min(train$Tarih)
  customers_last_purchase <- aggregate(Tarih ~ Hasta.Takma.Ad�, train, max)
  customers_last_purchase$t<-as.integer(customers_last_purchase$Tarih-min_date)
  customers_total_purchase <- aggregate(Sat��.Miktar� ~ Hasta.Takma.Ad�, train, sum)
  customer_info <- customers_last_purchase %>% left_join( customers_total_purchase, 
                                                          by=c('Hasta.Takma.Ad�'='Hasta.Takma.Ad�'))
  colnames(customer_info) <- c('Hasta.Takma.Ad�','cl_purchase.Tarih','t','pc_total_purchaseTm')
  customer_info
  t.vect<-customer_info$t
  x.vect<-customer_info$pc_total_purchaseTm
  
  Prob=Alive.prob.v2(dataset,r,alpha,s,B,T)
  # The expected number of purchases with updated parameters for future expected purchase
  Expected_X_updated1 <- (((r+x.vect) * (B+T)) / ((alpha+T) * (s - 1))) * (1 - ((B+T) / ((B+T) + T_future1))^(s-1))
  Expected_X_future1 <- Expected_X_updated1 * as.numeric(Prob$Churn_prob)
  Expected_X_future1
  # T2
  Expected_X_updated2 <- (((r+x.vect) * (B+T)) / ((alpha+T) * (s - 1))) * (1 - ((B+T) / ((B+T) + T_future2))^(s-1))
  Expected_X_future2 <- Expected_X_updated2 * as.numeric(Prob$Churn_prob)
  # T3
  Expected_X_updated3 <- (((r+x.vect) * (B+T)) / ((alpha+T) * (s - 1))) * (1 - ((B+T) / ((B+T) + T_future3))^(s-1))
  Expected_X_future3 <- Expected_X_updated3 * as.numeric(Prob$Churn_prob)
  # T4
  Expected_X_updated4 <- (((r+x.vect) * (B+T)) / ((alpha+T) * (s - 1))) * (1 - ((B+T) / ((B+T) + T_future4))^(s-1))
  Expected_X_future4 <- Expected_X_updated4 * as.numeric(Prob$Churn_prob)

  E_future_purchase<- data.frame(
    Customer_id <-  customer_info$Hasta.Takma.Ad�,
    Expected_purchase_3_Month<- Expected_X_future1,
    Expected_purchase_6_Month<- Expected_X_future2,
    Expected_purchase_9_Month<- Expected_X_future3,
    Expected_purchase_12_Month<- Expected_X_future4
  )
  colnames(E_future_purchase) <- c('Customer_id','Expected_purchase_3_Month','Expected_purchase_6_Month','Expected_purchase_9_Month','Expected_purchase_12_Month')
  E_future_purchase
}


###############################################   PARAMETER ESTIMATION   ############################################### 

#### A4,1 ~ Xt(Tm) is the average number of transactions per customer during the customers' first month.
average_transactions_pcustomer <- function(dataset,T,Tm){
  min_date<- min(dataset$Tarih)
  customers_first_purchase <- aggregate(Tarih ~ Hasta.Takma.Ad�, dataset, min)
  customers_first_purchase$min_date<-min(dataset$Tarih)
  customers_first_purchase$diff <- customers_first_purchase$Tarih - min_date

  df <- dataset %>% left_join( customers_first_purchase, 
                               by=c('Hasta.Takma.Ad�'='Hasta.Takma.Ad�'))
  df$transfered_date<-df$Tarih.x-df$diff
  colnames(df) <- c('data_record.Tarih','Hasta.Takma.Ad�','Real_Sat��.Miktar�','cf_purchase.Tarih_bt','min_date','diff','transfered_Date')
  
  train<-df[df$transfered_Date <=Tm+min(df$transfered_Date),]
  date_interval<-0
  cutoff<-0
  date_interval.frame<-data.frame()
  Tm_time<- min_date+Tm
  while(Tm_time>date_interval){
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
    start_date <-date_interval.frame$date_split[1]
    end_date<-date_interval.frame$date_split[i+1]-1
    filtered_data <- subset(df, transfered_Date >= start_date & transfered_Date <= end_date)
    customer_average <- aggregate(Real_Sat��.Miktar� ~ Hasta.Takma.Ad�, filtered_data, sum)
    gen_average <- sum(customer_average$Real_Sat��.Miktar�) / length(customer_average$Real_Sat��.Miktar�)
    gen_average
    new_row <- cbind(i,format(as.Date(start_date,'%Y-%m-%d')), format(as.Date(end_date,'%Y-%m-%d')), gen_average)
    gen_average.frame<- rbind(gen_average.frame,new_row)
    
    
  }
  colnames(gen_average.frame) <- c('Interval_num','start_date','end_date','gen_average')
  gen_average.frame
}

## A5 equation
## Var E[X|...] 
### PARETO/NBD MODEL
## var_X <-((2*(B/(s-1))) * ((B/(s-2)) - ((B/(s-2)) * ((B/(B+T))^(s-2))) - (T*((B/(B+T))^(s-1))))) 
### Content_server 
## var_X <- Expected_X(r,alpha,s,B,T) - (Expected_X(r,alpha,s,B,T)^2) + ((r_a*(r+1)/a)*  ((2*(B/(s-1))) * ((B/(s-2)) - ((B/(s-2)) * ((B/(B+T))^(s-2))) - (T*((B/(B+T))^(s-1)))))) 
variance_transactions_pcustomer <- function(dataset,T,Tm){
  min_date<- min(dataset$Tarih)
  customers_first_purchase <- aggregate(Tarih ~ Hasta.Takma.Ad�, dataset, min)
  customers_first_purchase$min_date<-min(dataset$Tarih)
  customers_first_purchase$diff <- customers_first_purchase$Tarih - min_date

  df <- dataset %>% left_join( customers_first_purchase, 
                             by=c('Hasta.Takma.Ad�'='Hasta.Takma.Ad�'))
  df$transfered_date<-df$Tarih.x-df$diff
  colnames(df) <- c('data_record.Tarih','Hasta.Takma.Ad�','Real_Sat��.Miktar�','cf_purchase.Tarih_bt','min_date','diff','transfered_Date')
  
  train<-df[df$transfered_Date <=Tm+min(df$transfered_Date),]
  date_interval<-0
  cutoff<-0
  date_interval.frame<-data.frame()
  Tm_time<- min_date+Tm
  while(Tm_time>date_interval){
    date_interval=min_date+cutoff
    as.Date(date_interval)
    cutoff<- T + cutoff
    new_row <- cbind(format(as.Date(date_interval,'%Y-%m-%d')))
    colnames(new_row) <- c('date_split')
    date_interval.frame<- rbind(date_interval.frame,new_row)
  }
  date_interval.frame$date_split<-as.Date(date_interval.frame$date_split)
  date_interval.frame
  
  gen_var.frame <-data.frame()
  for(i in c(1:(length(date_interval.frame$date_split)-1))){
    start_date <-date_interval.frame$date_split[1]
    end_date<-date_interval.frame$date_split[i+1]-1
    filtered_data <- subset(df, transfered_Date >= start_date & transfered_Date <= end_date)
    customer_average <- aggregate(Real_Sat��.Miktar� ~ Hasta.Takma.Ad�, filtered_data, sum)
    gen_var <- var(customer_average$Real_Sat��.Miktar�) 
    gen_var
    new_row <- cbind(i,format(as.Date(start_date,'%Y-%m-%d')), format(as.Date(end_date,'%Y-%m-%d')), gen_var)
    gen_var.frame<- rbind(gen_var.frame,new_row)
  }
  colnames(gen_var.frame) <- c('Interval_num','start_date','end_date','gen_var')
  gen_var.frame
}

Expected_X<- function(r_a,s,B,T){
  ((r_a * B) /(s - 1)) *  (1 - ((B / (B + T))^(s-1)))
}


## A4 General Equation
lse<- function(dataset,T,Tm,r_a,s,B){
  transactions_num <- average_transactions_pcustomer(dataset,T,Tm)
  x_purchase<- as.numeric(transactions_num$gen_average)
  T.vect<-seq(T,dim(transactions_num)[1]*T,by=T)
  diff.vec<- NULL
  for(i in c(1:(dim(transactions_num)[1]))){
    diff.vec[i]<- (Expected_X(r_a,s,B,T.vect[i]) - x_purchase[i])^2
  }
  diff.vec
  minimize_value<- sum(diff.vec)
  minimize_value
}


#### For minimize A4 value 1 ~ Finite interval ####
A4_minimize.finite_interval <- function(r_a,s,B,T,dataset){
  comb.data_frame <- data.frame()
  lse.vec <- NULL
  value_T <- NULL
  for (i in c(1:length(r_a))){
    ind_r_a <- r_a[i]
    for (j in c(1:length(s))){
      ind_s <- s[j]
      for (m in c(1:length(B))){
        ind_B <- B[m]
        new_row <- cbind(ind_r_a,ind_s,ind_B) 
        comb.data_frame<- rbind(comb.data_frame, new_row)
      }
    }
  }
  comb.data_frame
  for (l in c(1:(length(r_a)*length(s)*length(B)))){
    value_r_a <- comb.data_frame[l,1]
    value_s <- comb.data_frame[l,2]
    value_B <- comb.data_frame[l,3]
    lse.vec[l]<- lse(dataset,T,Tm,value_r_a,value_s,value_B)
  }
  comb.data_frame<- cbind(comb.data_frame, lse.vec)
  comb.data_frame
  comb.data_frame[which(comb.data_frame$lse.vec == min(comb.data_frame$lse.vec)),]
}


##### for minimize A4 value 2 ~~ Gradient Descent Method ####
gradient_search<-function(dataset,r_a,s,B,T,Tm,learning_rate,tolerance,kmax){
  gradient.frame<- data.frame()
  transactions_num <- average_transactions_pcustomer(dataset,T,Tm)
  x_purchase<- as.numeric(transactions_num$gen_average)
  T.vect<-seq(T,dim(transactions_num)[1]*T,by=T)
  # Define the function (lse)
  gradient_lse <- function(dataset,r_a,s,B){
    #gradient paramemeters r_a,s,B
    grad_r_a<-0
    grad_s<-0
    grad_B<-0
    for (q in c(1:(dim(transactions_num)[1]))){
      purchase_diff <- (r_a * ((B /(s - 1)) *  (1 - ((B / (B + T.vect[q]))^(s-1))))) - x_purchase[q]
      grad_r_a[q]<- 2 * purchase_diff * ((B/(s - 1)) * (1 - (B/(B + T.vect[q]))^(s - 1)))
      grad_s[q]<- 2 * purchase_diff * ((B*r_a) * ((-ln(B/(B+T.vect[q])) * ((B/(B+T.vect[q]))^(s-1)) * (s-1)) -1 + (B/(B+T.vect[q])^(s-1)))/((s-1)^2))
      grad_B[q]<- 2 * purchase_diff * ((r_a / (s-1)) * (1-((B/(B+T.vect[q]))^(s-1))-(B * (s-1) * (B/(B+T.vect[q])^(s-2)) * ((T.vect[q]/((B+T.vect[q])^(2)))))))
    }
    grad_r_a<-sum(grad_r_a)
    grad_s<-sum(grad_s)
    grad_B<-sum(grad_B)
    grad_variable<-c(grad_r_a,grad_s,grad_B)
    grad_variable
  }
  k<-0
  grad_variable_sum<- sum(gradient_lse(dataset,r_a,s,B))
  # Run gradient search
  while (abs(grad_variable_sum) > tolerance & k<kmax) {
    
    # Calculate the gradient refer to lse function
    grad_value <- gradient_lse(dataset,r_a,s,B)
    # Update the variable
    r_a<- r_a - (learning_rate * grad_value[1])
    s<- s - (learning_rate * grad_value[2])
    B<- B - (learning_rate * grad_value[3])
    lse_value<-lse(dataset,T,Tm,r_a,s,B)
    grad_variable_sum<- sum(grad_value)
    new_row <- cbind(r_a,s,B,lse_value,grad_variable_sum) 
    gradient.frame<- rbind(gradient.frame, new_row)
    gradient.frame 
    k<-k+1
  }
  gradient.frame
}


### Herustic Optimal Using Gradient Descent with n sample ###
random_ivalue<- function(dataset,T,learning_rate,tolerance,kmax,start,end,n){
  r_a.vect <- sample(start:end, n)#   r_a.vect<-round(runif(n, min, max),3)
  s.vect <- sample(start:end, n)#   s.vect<-round(runif(n, min, max),3)
  B.vect <- sample(start:end, n)  #   B.vect<-round(runif(n, min, max),3)
  random_value.frame<- data.frame()
  random_value.frame <- cbind(r_a.vect,s.vect,B.vect)
  local_min_lse <- data.frame()
  for (i in c(1:length(B.vect))) {
    r_a <- r_a.vect[i]
    s <- s.vect[i]
    B <- B.vect[i]
    gradient_search.frame <- gradient_search(dataset, r_a, s, B, T, learning_rate,tolerance,kmax) 
    gradient_search.frame<- as.data.frame(gradient_search.frame)
    last_row <- gradient_search.frame %>% slice(nrow(.) -0)
    last_row 
    local_min_lse <- rbind(local_min_lse, last_row)
  }
  local_min_lse
}


### Herustic Optimal Using Gradient Descent with n sample ###
random_ivalue.v2<- function(dataset,T,learning_rate,tolerance,kmax){
  r_a.vect<-c(8,3,6,5,4,5,25,14)#   r_a.vect<-round(runif(n, min, max),3)
  s.vect <- c(6,3,4,6,3,3,56,24)#   s.vect<-round(runif(n, min, max),3)
  B.vect <- c(6,4,8,9,5,3,34,21)  #   B.vect<-round(runif(n, min, max),3)
  random_value.frame<- data.frame()
  random_value.frame <- cbind(r_a.vect,s.vect,B.vect)
  local_min_lse <- data.frame()
  
  
  for (i in c(1:length(B.vect))) {
    r_a <- r_a.vect[i]
    s <- s.vect[i]
    B <- B.vect[i]
    gradient_search.frame <- gradient_search(dataset, r_a, s, B, T,Tm, learning_rate,tolerance,kmax) 
    gradient_search.frame<- as.data.frame(gradient_search.frame)
    last_row <-gradient_search.frame[nrow(gradient_search.frame), ]
    last_row 
    local_min_lse <- rbind(local_min_lse, last_row)
  }
  local_min_lse
}

### Finding the values of 4 parameters using the estimated values of 3 parameters
alpha_estimator<- function(dataset,r_a,s,B,T,Tm){
  var_X.vec<- NULL
  alpha_estimate<-NULL
  transactions_var <- variance_transactions_pcustomer(dataset,T,Tm)
  var_purchase<- as.numeric(transactions_var$gen_var)
  T.vect<-seq(T,dim(transactions_var)[1]*T,by=T)
  
  for (i in c(1:(length(T.vect)))){
    alpha_estimate[i]<-(2*r_a*B)/((((Expected_X(r_a,s,B,T.vect[i])^2 - Expected_X(r_a,s,B,T.vect[i]) + var_purchase[i])/((B/(s-2))-((B/(s-2))*(B/(B+T.vect[i]))^(s-1))-(T.vect[i]*((B/(B+T.vect[i]))^(s-1)))))*(s-1))-(2*(r_a^2)*B))
    
  }
  alpha_est <-(sum(alpha_estimate))/length(T.vect)
  r_estimate <- r_a *alpha_est
  r_a_frame <- data.frame(
    r_estimator= c(r_estimate),
    alpha_estimator= c(alpha_est),
    s_estimator= c(s),
    B_estimator= c(B)
  )
  r_a_frame
}


### Find customers' last purchases and their total range within the specified range
customers_information<- function(dataset,Tm){
  train<-dataset[dataset$Tarih<=Tm+min(dataset$Tarih),]
  min_date<-min(train$Tarih)
  customers_last_purchase <- aggregate(Tarih ~ Hasta.Takma.Ad�, train, max)
  customers_last_purchase$t<-as.integer(customers_last_purchase$Tarih-min_date)
  customers_total_purchase <- aggregate(Sat��.Miktar� ~ Hasta.Takma.Ad�, train, sum)
  customer_info <- customers_last_purchase %>% left_join( customers_total_purchase, 
                               by=c('Hasta.Takma.Ad�'='Hasta.Takma.Ad�'))
  customer_info
  colnames(customer_info) <- c('Hasta.Takma.Ad�','cl_purchase.Tarih','t','pc_total_purchaseTm')
  customer_info
}

