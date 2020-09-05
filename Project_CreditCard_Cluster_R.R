rm(list=ls())

#Setting Wowking directory
setwd('E:/Data Science(Edwisor)/Project/Project #2/Dataset')

#Libraries
#install.packages(c('data.table','mltools'))
#install.packages('fpc')
library('DMwR')
library(ggplot2)
library(corrgram)
library(e1071)
library(caret)
library(cluster)
library(data.table)
library(mltools)
library(fpc)


#Reading Data
dataset = read.csv('credit-card-data.csv')
dataset = subset(dataset,select = -CUST_ID)
str(dataset)

#Checking for missing values
colnames(dataset)[colSums(is.na(dataset))>0]

#Using KNN Imputation for the Missing values
dataset = knnImputation(dataset, k = 5)
sum(is.na(dataset))

#Feature engineering to create KPI'S

#1 Monthly Average Purchase
dataset$MONTHLY_AVG_PURCHASE =  dataset$PURCHASES/dataset$TENURE

#2 Average Cash Advance Amount
dataset$CASH_ADVANCE_AVG = dataset$CASH_ADVANCE/dataset$TENURE

#3 Purchases by Type

for(i in 1:nrow(dataset)){
  if(dataset$ONEOFF_PURCHASES[i] == 0 && dataset$INSTALLMENTS_PURCHASES[i] ==0){
    dataset$PURCHASE_TYPE[i] = 'NONE'
  }
  else if(dataset$ONEOFF_PURCHASES[i] > 0 && dataset$INSTALLMENTS_PURCHASES[i] ==0){
    dataset$PURCHASE_TYPE[i] = 'ONEOFF'
  }
  else if(dataset$ONEOFF_PURCHASES[i] == 0 && dataset$INSTALLMENTS_PURCHASES[i] >0){
    dataset$PURCHASE_TYPE[i] = 'INSTALLEMENT'
  }
  else{
    dataset$PURCHASE_TYPE[i] = 'BOTH'
  }
}
#Encoding categorical variables
dataset$PURCHASE_TYPE = as.factor(dataset$PURCHASE_TYPE)
dataset = one_hot(as.data.table(dataset))
#4  limit usage (balance to credit limit ratio)
dataset$LIMIT_RATIO =dataset$BALANCE/dataset$CREDIT_LIMIT


#5 payments to minimum payments ratio 
dataset$PAYMENT_MIN_PAYEMENT_RATIO = dataset$PAYMENTS/dataset$MINIMUM_PAYMENTS


#Gaining Insights on KPI

ggplot(dataset, aes(dataset$PURCHASE_TYPE,dataset$MONTHLY_AVG_PURCHASE)) +
  geom_bar(stat = 'identity') + 
  labs(y = 'Monthly Avg Purchase', x = 'Purchase Type')

ggplot(dataset, aes(dataset$PURCHASE_TYPE,dataset$CASH_ADVANCE_AVG)) +
  geom_bar(stat = 'identity') + 
  labs(y = 'Cash Advance Average', x = 'Purchase Type')

ggplot(dataset, aes(dataset$PURCHASE_TYPE,dataset$LIMIT_RATIO)) +
  geom_bar(stat = 'identity') + 
  labs(y = 'Limit Ratio', x = 'Purchase Type')

ggplot(dataset, aes(dataset$PURCHASE_TYPE,dataset$PAYMENT_MIN_PAYEMENT_RATIO)) +
  geom_bar(stat = 'identity') + 
  labs(y = 'Payment ratio', x = 'Purchase Type')


#Feature Selection
numeric_index = sapply(dataset,is.numeric)
corrgram(dataset[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


#removing variables with high corelation

dataset_new = subset(dataset,select = -c(BALANCE, PURCHASES ,PAYMENTS ,MINIMUM_PAYMENTS, PRC_FULL_PAYMENT, TENURE, CASH_ADVANCE ,CREDIT_LIMIT))


numeric_index = sapply(dataset_new,is.numeric)
corrgram(dataset_new[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")



#Feature scaling

cnames = colnames(dataset_new)

# #Standardisation
 for(i in cnames){
   print(i)
   if(i == 'PURCHASE_TYPE'){
     
   }
   else{
     dataset_new[,i] = (dataset_new[,i] - mean(dataset_new[,i]))/
       sd(dataset_new[,i])
   }
 }


str(dataset_new)


#Dimensionality Reduction Using PCA

pca = preProcess(x = dataset_new, method = 'pca', pcaComp = 4)
dataset_new = predict(pca, dataset_new)

str(dataset_new)


#Clustering data set
# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = dataset_new, centers = 4)
y_kmeans = kmeans$cluster

# Visualising the clusters

plotcluster(dataset_new, y_kmeans)

