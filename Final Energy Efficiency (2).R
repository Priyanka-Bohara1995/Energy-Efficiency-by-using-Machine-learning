#install all required packages
install.packages("ggplot2") # Install and load ggplot2
install.packages ("dplyr")
install.packages ("tidyr")

#load the Dataset
setwd(dirname(file.choose()))
getwd()

# read in data from csv file
energyEfficiency<- read.csv("energyEfficiency.csv", stringsAsFactors = FALSE)
head(energyEfficiency)    # Inspect top rows of the data
View(energyEfficiency)

#summary of Dataset
summary(energyEfficiency)

#see the structure of the dataset
str(energyEfficiency)   

# general checking for missing values
apply(energyEfficiency, MARGIN = 2, FUN = function(x) sum(is.na(x)))

# Check for missing values from the data and suppose the dataset is loaded as data
missing_values <- sapply(energyEfficiency, function(x) sum(is.na(x)))
print(missing_values)

# check for the missing data
apply(energyEfficiency, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
missmap(energyEfficiency, col = c("black", "grey"), legend = FALSE)
#energyefficiency <- na.omit(energyefficiency)


#Boxplot 

# boxplot all the variables 

boxplot(energyEfficiency, main = "Boxplot for all variables")

# individual boxplot
boxplot(energyEfficiency$Y1, main = "Heating Load", xlab = "Heating Load", ylab = "Variable")
boxplot(energyEfficiency$Y2,main = "Cooling Load", xlab = "Cooling Load", ylab = "Variable")

#-----Normalization-------------
# min-max scaling
energyEfficiency.mm <- apply(energyEfficiency, MARGIN = 2, FUN = function(x) (x - min(x))/diff(range(x)))

boxplot(energyEfficiency.mm,main = "min-max scaling")

# z-score

energyEfficiency.z1 <- apply(energyEfficiency, MARGIN = 2, FUN = function(x) (x - mean(x))/sd(x))
energyEfficiency.z2 <- apply(energyEfficiency, MARGIN = 2, FUN = function(x) (x - mean(x))/(2*sd(x)))

boxplot (energyEfficiency.z1, main = "Z-score, 1 sd")
boxplot (energyEfficiency.z2, main = "Z-score, 2 sd")
#-------------------Exploration-----------------------------

# exploring and preparing the data
install.packages("forecast")
library(forecast)
## Correlation 
# calculate spearman and Pearson correlations 
cor(energyEfficiency$Y1, energyEfficiency$X1, method = "spearman")
cor(energyEfficiency$Y1,energyEfficiency$X2,method = "spearman")
cor(energyEfficiency$Y1, energyEfficiency$X3,method = "spearman")
cor(energyEfficiency$Y1, energyEfficiency$X4,method = "spearman")
cor(energyEfficiency$Y1, energyEfficiency$X5,method = "spearman")
cor(energyEfficiency$Y1, energyEfficiency$X6,method = "spearman")
cor(energyEfficiency$Y1, energyEfficiency$X7,method = "spearman")
cor(energyEfficiency$Y1, energyEfficiency$X8,method = "spearman")
cor(energyEfficiency$Y1, energyEfficiency$Y2, method = "spearman")
cor(energyEfficiency$Y1, energyEfficiency$X1,method = "pearson")
cor(energyEfficiency$Y1, energyEfficiency$X2,method = "pearson")
cor(energyEfficiency$Y1, energyEfficiency$X3,method = "pearson")
cor(energyEfficiency$Y1, energyEfficiency$X4,method = "pearson")
cor(energyEfficiency$Y1, energyEfficiency$X5,method = "pearson")
cor(energyEfficiency$Y1, energyEfficiency$X6,method = "pearson")
cor(energyEfficiency$Y1, energyEfficiency$X7,method = "pearson")
cor(energyEfficiency$Y1, energyEfficiency$X8,method = "pearson")
cor(energyEfficiency$Y1, energyEfficiency$Y2, method = "pearson")

#Correlation Matrix of Y1
library(polycor)
energyEfficiency.cor <- hetcor(energyEfficiency[-11,-12], use = "pairwise.complete.obs")
round(energyEfficiency.cor$correlations, 2)

# Plot correlagram using correlation matrix
library(corrplot)

corrplot(energyEfficiency.cor$correlations, is.corr = TRUE, type = "upper", tl.col = "black", tl.srt = 45)

# Remove the high correlated  variables
energyEfficiency_cor <- energyEfficiency[, -which(names(energyEfficiency) %in% c("X3", "X2","X6"))]
print(energyEfficiency_cor)

#-----------------Split the dataset----------------------------
set.seed(12345)
energyEfficiency.rand <- energyEfficiency[order(runif(768)), ]
#Split the data for training and testing 
# create training (80%) and test data (20%) (data already in random order)

energyEfficiency_train <- energyEfficiency.rand[1:614, ] #80%
energyEfficiency_test <- energyEfficiency.rand[615:768, ] #20%

# check that the proportion of spam is similar
prop.table(table(energyEfficiency_train$Y1))
prop.table(table(energyEfficiency_test$Y1))

#----------------------Regression Tree-------------------

# regression tree using rpart

library(rpart)
set.seed(12345)
e.rpart <- rpart(Y1 ~ ., data = energyEfficiency_train)

# get basic information about the tree
e.rpart

# get more detailed information about the tree
summary(e.rpart)

# use the rpart.plot package to create a visualization
library(rpart.plot)

# a basic decision tree diagram
rpart.plot(e.rpart, digits = 3)

# a few adjustments to the diagram
rpart.plot(e.rpart, digits = 3, fallen.leaves = TRUE, type = 3, extra = 101)

# alternative
install.packages("rattle")
library(rattle)
?fancyRpartPlot()

fancyRpartPlot(e.rpart)
#------------  model performance-------------------------------
## evaluate model performance
o.rpart <- rpart(Y1 ~ ., data = energyEfficiency_test)

# # generate predictions for the testing dataset
e1.rpart <- predict(o.rpart, energyEfficiency_test)

# compare the correlation between actual and predicted
cor.test(energyEfficiency_test$Y1, e1.rpart, method = "spearman", exact = FALSE)

# calculate the mean absolute error of function
MAE <- function(actual, predicted) {mean(abs(actual - predicted))}

# mean absolute error between actual and predicted
MAE(energyEfficiency_test$Y1, e1.rpart)

# mean absolute error between actual and mean of actual
# MAE value above should be smaller if model is better than average
MAE(mean(energyEfficiency_train$Y1), energyEfficiency_test$Y1)

# calculate the root mean square error (RMSE) of  function
RMSE <- function(actual, predicted) {sqrt(mean((actual - predicted)^2))}

# RMSE between actual and predicted
RMSE(energyEfficiency_test$Y1, e1.rpart)

# RMSE between actual and mean of actual
# RMSE value above should be smaller if model is better than average
RMSE(mean(energyEfficiency_train$Y1), energyEfficiency_test$Y1)

#--------------------End of the regression tree----------------------------------

#SVR

#?ksvm()

# run initial model
set.seed(12345)
library(kernlab)
#svm0 <- ksvm(Y1 ~ ., data = energyEfficiency_train, kernel = "vanilladot", type = "C-svc")
# vanilladot is a Linear kernel; -- WARNING -- some kernels take a long time

# look at basic information about the model
#svm0

svm_model <- ksvm(Y1 ~ ., 
                  data = energyEfficiency_train, 
                  type = "eps-svr",  # Specifies the type of SVM model (epsilon-SVR for regression)
                  kernel = "rbfdot", # Radial basis function (RBF) kernel, commonly used for SVM regression
                  kpar = list(sigma = 0.1),  # Kernel parameters (e.g., sigma for RBF kernel)
                  C = 10)  # Cost parameter for the soft-margin SVM
summary(svm_model)

# evaluate
energyEfficiency.pred1 <- predict(svm_model, energyEfficiency_test)
#class(energyEfficiencytest_$Y1)
table(energyEfficiency.pred1, energyEfficiency_test$Y1)
round(prop.table(table(energyEfficiency.pred1, energyEfficiency_test$Y1))*100,1)
# sum diagonal for accuracy
sum(diag(round(prop.table(table(energyEfficiency.pred1, energyEfficiency_test$Y1))*100,1)))

# evaluate model performance
# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}
# RMSE
error <- energyEfficiency_test$Y1 - energyEfficiency.pred1
rmse(error)
# MAE
mae(error)


#-----------------ENd of SVR-----------------------------------------------------------
#Random forest 
# random forest with default settings
library(randomForest)

?randomForest()

# Convert Y1 to a factor for classification
energyEfficiency$Y1 <- as.factor(energyEfficiency$Y1)


# Fit the random forest model
rf <- randomForest(Y1 ~ ., data = energyEfficiency_train)

# Print the summary of the model
print(rf)

# Plot variable importance
varImpPlot(rf, main = "rf - variable importance")

#Convert Y1 into factor 
energyEfficiency_test$Y1 <- factor(energyEfficiency_test$Y1)

# Apply the model to make predictions
p <- predict(rf, energyEfficiency_test)

install.packages("gmodels")
library(gmodels)
#Evaluate 
CrossTable(energyEfficiency_test$Y1, p,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
          dnn = c('actual default', 'predicted default'))
# evaluate model performance
# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}
# Calculate RMSE and MAE
rmse_value <- rmse(error)
mae_value <- mae(error)
# Print the results
print(paste("RMSE:", rmse_value))
print(paste("MAE:", mae_value))

#____End of Random forest------------------------
