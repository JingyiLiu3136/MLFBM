# data importation
wd = getwd()
setwd(wd)
bank_additional_full = read.csv("bank-additional-full.csv", sep = ";")

# data partition
set.seed(123)  # start with a random seed so that we can make this analysis repeatable 
ind = sample(2, nrow(bank_additional_full), replace = TRUE, prob = c(0.8, 0.2))  # divide the dataset into two parts
train = bank_additional_full[ind == 1, ]  # train dataset
test = bank_additional_full[ind == 2, ]  # test dataset

# default random forest model
library(randomForest)
set.seed(222)
rf_default = randomForest(y ~ ., data = train)  # By default mtry=floor(sqrt(ncol(x)))  ntree = 500
print(rf_default)

# prediction and confusion matrix
library(lattice)
library(caret)
p1 = predict(rf_default, train)
confusionMatrix(p1, train$y)

# prediciton with test data
p2 = predict(rf_default, test)
confusionMatrix(p2, test$y)

# calculate AUC
library(pROC)
PredictionwithProb_default = predict(rf_default, test, type = "prob")
auc = auc(test$y, PredictionwithProb_default[, 2])
plot(roc(test$y, PredictionwithProb_default[, 2]))  # plot ROC curve
auc

plot(rf_default)  # From the plot, we can choose the reasonable value of ntree.

# tune mtry
t = tuneRF(train[, -16], train[, 16], stepFactor = 0.5, plot = TRUE, ntreeTry = 100, trace = TRUE, improve = 0.05)  # From the result, we can choose the reasonable value of mtry.

# tuned random forest model
rf_tune = randomForest(y ~ ., data = train, ntree = 100, mtry = 3, importance = TRUE, proximity = TRUE)
print(rf_tune)

# prediction with test data for tuned random forest model
p2_tune = predict(rf_tune, test)
confusionMatrix(p2_tune, test$y)


# calculate AUC for tuned random forest model
library(pROC)
PredictionwithProb_tune = predict(rf_tune, test, type = "prob")
auc = auc(test$y, PredictionwithProb_tune[, 2])
plot(roc(test$y, PredictionwithProb_tune[, 2]))
auc

# number of nodes for the trees in tuned random forest model
hist(treesize(rf_tune), main = "No. of Nodes for the Trees", col = "green")

# variable importance in tuned random forest model
varImpPlot(rf_tune, sort = T, n.var = 10, main = "Top 10 - Variable Importance")  # showing the importance of the variables in plot

importance(rf_tune)  # showing the importance of the variables in values
varUsed(rf_tune)  # how many times for each variables be uesd in the tuned random forest model


# Partial Dependence Plot
partialPlot(rf_tune, train, "duration", "no")

# extract single tree from the forest
getTree(rf_tune, 1, labelVar = TRUE)  # When the results of status is -1, it means that its the terminal node

# Finnishing the simple tuned random forest model for prediction Three Methods for tuning parameter 'mtry' RF
# default
library(caret)
x = bank_additional_full[, 1:15]
y = bank_additional_full[, 16]
control = trainControl(method = "repeatedcv", number = 3)
seed = 10
set.seed(seed)
mtry = sqrt(ncol(x))
tunegrid = expand.grid(.mtry = mtry)
rf_default = train(y ~ ., data = bank_additional_full, method = "rf", metric = "Accuracy", tuneGrid = tunegrid, trControl = control)
print(rf_default)

# Method 1: Random Search
control = trainControl(method = "repeatedcv", number = 3, search = "random")
set.seed(seed)
mtry = sqrt(ncol(x))
rf_random = train(y ~ ., data = bank_additional_full, method = "rf", metric = "Accuracy", tuneLength = 15, trControl = control)
print(rf_random)
plot(rf_random)

# Method 2: Grid Search
control = trainControl(method = "repeatedcv", number = 3, search = "grid")
set.seed(seed)
tunegrid = expand.grid(.mtry = c(1:10))
rf_gridsearch = train(y ~ ., data = bank_additional_full, method = "rf", metric = "Accuracy", tuneGrid = tunegrid, 
                      trControl = control)
print(rf_gridsearch)
plot(rf_gridsearch)

# Method 3: with 'randomForest' package by using Algorithm Tune (tuneRF)
library(randomForest)
set.seed(seed)
bestmtry = tuneRF(x, y, stepFactor = 0.5, improve = 1e-05, ntree = 500)
print(bestmtry)

# OOB estimate of  error rate: 10.31%
# Confusion matrix:
#        no yes class.error
# no  29213 111 0.003785295
# yes  3295 419 0.887183630     the error is very high for prediciton the "yes" result

# prediction and confusion matrix
library(lattice)
library(caret)
p1<- predict(rf_default,train)
confusionMatrix(p1, train$y)

#prediciton with test data
p2<- predict(rf_default,test)
confusionMatrix(p2,test$y)
## The Results:
# Confusion Matrix and Statistics

#              Reference
#Prediction    no   yes
#       no    7195  808
#       yes     29  118

# Accuracy : 0.8973 

#  calculate AUC 
library(pROC)
PredictionwithProb_default <-predict(rf_default, test, type="prob")
auc<- auc(test$y, PredictionwithProb_default[,2])
plot(roc(test$y, PredictionwithProb_default[,2]))


plot(rf_default)
# from the plot, we have found the OOB almost doesn't change when the number of trees is around 50. As a result, I choose the "ntree=100"

# tune mtry
t <- tuneRF(train[,-16],train[,16],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 100,
            trace = TRUE,
            improve = 0.05
)
## The Result and graph show that when mtry=3, the OOB error is smallest. So we choose mtry=3

# tune random forest
rf_tune <- randomForest(y~., data=train,
                        ntree = 100,
                        mtry = 3,
                        importance = TRUE,
                        proximity = TRUE)
print(rf_tune)   
# The result
# Call:
#randomForest(formula = y ~ ., data = train, ntree = 100, mtry = 3,importance = TRUE, proximity = TRUE) 
#Type of random forest: classification
#Number of trees: 100
#No. of variables tried at each split: 3

#OOB estimate of  error rate: 10.25%  which is little better than the OOB error of rf_default 10.31%
# Confusion matrix:
#       no yes class.error
#no  29149 175 0.005967808
#yes  3212 502 0.864835757

# Compare the prediction results
p2_tune <- predict(rf_tune,test)
confusionMatrix(p2_tune,test$y)
# The Results:
# Confusion Matrix and Statistics

#                 Reference
#    Prediction   no  yes
#         no     7178  782
#         yes     46  144

#   Accuracy : 0.8984  

#  calculate AUC 
library(pROC)
PredictionwithProb_tune <-predict(rf_tune, test, type="prob")
auc<- auc(test$y, PredictionwithProb_tune[,2])
plot(roc(test$y, PredictionwithProb_tune[,2]))
auc

# number of nodes for the trees
hist(treesize(rf_tune),
     main = "No. of Nodes for the Trees",
     col= "green")

# variable importance 
varImpPlot(rf_tune,
           sort=T,
           n.var = 10,
           main = "Top 10 - Variable Importance") 
# graph 4
importance(rf_tune)
varUsed(rf_tune)
# The Result shows how many times each variable is chosen for construction the random forest
# [1] 20235 26119 15550 23321  5300 15034 11465  4236 24805 25828 10732 23515  3837
# [14]  7219  5071

# Partial Dependence Plot
partialPlot(rf_tune, train, "duration", "no")
# graph 5

# extract single tree from the forest
getTree(rf_tune, 1, labelVar = TRUE)  # When the results of status is -1, it means its the terminal node


### tune parameter "mtry"
### RF default
library(caret)
x<- bank_additional_full[,1:15]
y<- bank_additional_full[,16]
control<- trainControl(method = "repeatedcv", number=3)
seed<-10
set.seed(seed)
mtry<-sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default<- train(y~., data= bank_additional_full, method="rf", metric="Accuracy", tuneGrid = tunegrid, trControl= control)
print(rf_default)

## The Results:
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.8995096  0.2559038
# Tuning parameter 'mtry' was held constant at a value of 3.872983

# Method 1: Random Search
control <- trainControl(method="repeatedcv", number=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(y~., data= bank_additional_full, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

# Method 2: Grid Search
control <- trainControl(method="repeatedcv", number=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:10))
rf_gridsearch <- train(y~., data= bank_additional_full, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

# Method 3: with "randomForest" package by using Algorithm Tune (tuneRF)
library(randomForest)
set.seed(seed)
bestmtry <- tuneRF(x, y, stepFactor=0.5, improve=1e-5, ntree=500)
print(bestmtry)