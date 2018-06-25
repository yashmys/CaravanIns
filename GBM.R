library(tidyverse)
library(sqldf)
library(reshape2)
library(scales)
library(plyr)
library(dplyr)
library(randomForest)
#install.packages("ROSE")
library(ROSE)
install.packages("caret")
install.packages('e1071', dependencies=TRUE)
library(caret)
install.packages("CRAN")
#install.packages("gbm")
library(gbm)

##### applying GBM for caravan insurance  the dataset is unbalanced 

###  
#load the caravan insurance data 
Cins <-  read.csv.sql("C1.csv","select * from file where ORIGIN = 'train' ", header=TRUE, sep=",")
# Exclude the  first column 
Caravan_train <- Cins[-1]



set.seed(1)

gbm_unb_model <-  gbm(Caravan_train$Number_of_mobile_home_policies ~ ., distribution = "bernoulli", 
                   data = Caravan_train ,  n.trees = 10000)

print(gbm_unb_model)
summary(gbm_unb_model)


############# predict on the test 

###  
#load the caravan insurance data 
Cins2 <-  read.csv.sql("C2.csv","select * from file where ORIGIN = 'test' ", header=TRUE, sep=",")
# Exclude the  first column 
Caravan_test <- Cins2[-1]


# Generate predictions on the test set
preds1 <- predict(object = gbm_unb_model, 
                  newdata = Caravan_test,
                  n.trees = 10000)
range(preds1)

# Generate predictions on the test set (scale to response)
preds2 <- predict(object = gbm_unb_model, 
                  newdata = Caravan_test,
                  n.trees = 10000,
                  type = "response")

range(preds2)

# Generate the test set AUCs using the two sets of preditions & compare
install.packages("AUC")
library(AUC)
install.packages('ROCR')
library('ROCR')

# 
# auc(as.factor(Caravan_test$Number_of_mobile_home_policies), preds1)  #default
# auc(Caravan_test$Number_of_mobile_home_policies, preds2)  #rescaled


caravan_under_data <-  ovun.sample( Number_of_mobile_home_policies ~ . , data = Caravan_train , method = "under",
                                    N = 700 , seed = 1 )$data
table(caravan_under_data$Number_of_mobile_home_policies)


set.seed(1)

gbm_under_model <-  gbm(caravan_under_data$Number_of_mobile_home_policies ~ ., distribution = "bernoulli", 
                      data = caravan_under_data,  n.trees = 10000)

print(gbm_under_model )
summary(gbm_under_model )


####################################################################################################
####  SELECT ONLY THE 56 VARIABLES THAT HAVE INFLUENCE 
#################################################################################################

Caravan_train_SUB56  <-  subset(Caravan_train, select = c( 
  
  car_policies                          
  ,Lower_level_education                 
  ,fire_policies                         
  ,Average_income                        
  ,private_third_party_insurance         
  ,X1_car                                
  ,Social_class_C                        
  ,Other_religion                        
  ,boat_policies                         
  ,No_car                                
  ,Customer_Subtype                      
  ,Middle_management                     
  ,Income_75.122.000                     
  ,tractor_policies                      
  ,Social_class_B1                       
  ,Social_class_D                        
  ,High_level_education                  
  ,Protestant                            
  ,Medium_level_education                
  ,bicycle_policies                      
  ,Income_45.75.000                      
  ,Unskilled_labourers                   
  ,Farmer                                
  ,Purchasing_power                      
  ,Number_of_car_policies                
  ,Income_._30                           
  ,Customer_main_type                    
  ,Household_with_children               
  ,Income_30.45.000                      
  ,Rented_house                          
  ,moped_policies                        
  ,Married                               
  ,Social_class_B2                       
  ,X2_cars                               
  ,Skilled_labourers                     
  ,Social_class_A                        
  ,Household_without_children            
  ,High_status                           
  ,No_religion                           
  ,Entrepreneur                          
  ,social_security_insurance_policies    
  ,Home_owners                           
  ,third_party_insurane_agriculture      
  ,Living_together                       
  ,Avg_size_household                    
  ,Avg_age                               
  ,Number_of_houses                      
  ,Singles                               
  ,Roman_catholic                        
  ,Other_relation                        
  ,Income_.123.000                       
  ,life_insurances                       
  ,National_Health_Service               
  ,Contribution_motorcycle_scooter       
  ,Private_health_insurance              
  ,Number_of_life_insurances  , Number_of_mobile_home_policies   )  )  

caravan_under_data56 <-  ovun.sample( Number_of_mobile_home_policies ~ . , data = Caravan_train_SUB56 , method = "under",
                                    N = 700 , seed = 1 )$data
table(caravan_under_data56$Number_of_mobile_home_policies)


set.seed(1)

gbm_under_model56 <-  gbm(caravan_under_data56$Number_of_mobile_home_policies ~ ., distribution = "bernoulli", 
                        data = caravan_under_data56 ,  n.trees = 10000)

print(gbm_under_model56 )
summary(gbm_under_model56 )

# Generate predictions on the test set
preds3 <- predict(object = gbm_under_model56, 
                  newdata = Caravan_test,
                  n.trees = 10000,   type = "response")

summary(preds3)
preds3


# Optimal ntree estimate based on OOB
ntree_opt_oob <- gbm.perf(object = gbm_under_model56, 
                          method = "OOB", 
                          oobag.curve = TRUE)

# Train a CV GBM model
set.seed(1)
credit_model_cv <- gbm(formula = Number_of_mobile_home_policies ~ ., 
                       distribution = "bernoulli", 
                       data = caravan_under_data56 ,
                       n.trees = 10000,
                       cv.folds = 2)

# Optimal ntree estimate based on CV
ntree_opt_cv <- gbm.perf(object = credit_model_cv, 
                         method = "cv")

# Compare the estimates                         
print(paste0("Optimal n.trees (OOB Estimate): ", ntree_opt_oob))                         
print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv))
