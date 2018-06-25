
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



#load the caravan insurance data 

Cins <-  read.csv.sql("C1.csv","select * from file where ORIGIN = 'train' ", header=TRUE, sep=",")

# Exclude the  first column 
Caravan_train <- Cins[-1]


#head(caravan_ins_data)
dim(Caravan_train )
names(Caravan_train )


# Inspect the data 
str(Caravan_train )
head(Caravan_train )


## check for missing  values 

Missing_Count <-  sum(is.na.data.frame(Caravan_train))
print(paste ("Missing count -> " , Missing_Count))


table(Caravan_train$Number_of_mobile_home_policies)


########   Random Forest using Under sampling


attach(Caravan_train)
caravan_under_data <-  ovun.sample( Number_of_mobile_home_policies ~ . , data = Caravan_train , method = "under",
                                    N = 700 , seed = 1 )$data
table(caravan_under_data$Number_of_mobile_home_policies)



attach(caravan_under_data)
rf.Under.Samp.Train <-  randomForest( as.factor(Number_of_mobile_home_policies)  ~ . , data = caravan_under_data,importance = TRUE )
print(rf.Under.Samp.Train)

# grab the error matrix 

err <- rf.Under.Samp.Train$err.rate
head(err)
dim(rf.Under.Samp.Train$err.rate)

# Look at final OOB error rate (last row in err matrix)
oob_err <- err[nrow(err),"OOB"]
print(oob_err)

# Plot the model trained in the previous exercise
plot(rf.Under.Samp.Train)

# Add a legend since it doesn't have one by default
legend(x = "right", 
       legend = colnames(err),
       fill = 1:ncol(err))

#important(rf.Under.Samp.Train)
varImpPlot(rf.Under.Samp.Train)



# Generate predicted classes using the model object
rf.Under.Samp.Prediction <-  predict(rf.Under.Samp.Train,newdata = Caravan_test,type = "class")

# Calculate the confusion matrix for the test set

# cm <- confusionMatrix(as.factor(rf.Under.Samp.Prediction),         
#                       reference = as.factor(Caravan_test$Number_of_mobile_home_policies) )
# print(cm)

# Compare test set accuracy to OOB accuracy
paste0("Test Accuracy: ", cm$overall[1])
paste0("OOB Accuracy: ", 1 - oob_err)

# Compute the AUC (`actual` must be a binary 1/0 numeric vector)
#library(ROCR)
#install.packages("pROC")
#library(pROC)
#auc(Caravan_test$Number_of_mobile_home_policies,rf.Unb.Prediction)

###### performance tuning using tuneRF
# Execute the tuning process
set.seed(1) 

res <- tuneRF(x = subset(caravan_under_data, select = -Number_of_mobile_home_policies ),
              y = as.factor(caravan_under_data$Number_of_mobile_home_policies),
              ntreeTry = 500)
print(res)
# Find the mtry value that minimizes OOB Error
mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
print(mtry_opt)



############ performance tuning using hyper grid 
# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(10, ncol(caravan_under_data) * 0.8, 2)
nodesize <- seq(7, 8, 2)
sampsize <- nrow(caravan_under_data) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)


# Create an empty vector to store OOB error values
oob_err_vector <- c()


# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  print(i)
  print(hyper_grid$mtry[i])
  print( hyper_grid$nodesize[i])
  print(hyper_grid$sampsize[i])
  
  # Train a Random Forest model
  model <- randomForest(as.factor(Number_of_mobile_home_policies)  ~ . , data = caravan_under_data ,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])



