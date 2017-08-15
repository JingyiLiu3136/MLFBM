# data importation
wd = getwd()
setwd(wd)
bank_additional_full = read.csv("bank-additional-full.csv", sep = ";")

# data partition
set.seed(123)  # start with a random seed so that we can make this analysis repeatable 
ind   = sample(2, nrow(bank_additional_full), replace = TRUE, prob = c(0.8, 0.2))  # divide the dataset into two parts
train = bank_additional_full[ind == 1, ]  # train dataset
test  = bank_additional_full[ind == 2, ]  # test dataset

# rf_default1
library(randomForest)
set.seed(222)
rf_default1 = randomForest(y ~ ., data = train)  # By default mtry=floor(sqrt(ncol(x)))  ntree = 500
print(rf_default1)
plot(rf_default1)
varImpPlot(rf_default1, sort = T, n.var = 10, main = "Top 10 - Variable Importance")  # showing the importance of the variables in plot
varUsed(rf_default1)  # how many times for each variable is be used in the tuned random forest model


# prediciton and confusion matrix with test data
library(lattice)
library(caret)
p1 = predict(rf_default1, test)
confusionMatrix(p1, test$y)

# calculate AUC for rf_default1
library(pROC)
PredictionwithProb_default1 = predict(rf_default1, test, type = "prob")
auc1                        = auc(test$y, PredictionwithProb_default1[, 2])
plot(roc(test$y, PredictionwithProb_default1[, 2]))  # plot ROC curve
auc1

# rf_default2
# data importation
bank_additional_full = read.csv("bank-additional-full.csv", sep = ";")
# data adjustment
bank_additional_full$nr.employed    = NULL#as.factor(bank_additional_full$nr.employed)
bank_additional_full$euribor3m      = NULL#as.factor(bank_additional_full$euribor3m)
bank_additional_full$cons.conf.idx  = NULL#as.factor(bank_additional_full$cons.conf.idx)
bank_additional_full$cons.price.idx = NULL#as.factor(bank_additional_full$cons.price.idx)
bank_additional_full$emp.var.rate   = NULL#as.factor(bank_additional_full$emp.var.rate)
# data partition
set.seed(123)  # start with a random seed so that we can make this analysis repeatable 
ind   = sample(2, nrow(bank_additional_full), replace = TRUE, prob = c(0.8, 0.2))  # divide the dataset into two parts
train = bank_additional_full[ind == 1, ]  # train dataset
test  = bank_additional_full[ind == 2, ]  # test dataset
# rf_default2
library(randomForest)
set.seed(222)
rf_default2 = randomForest(y ~ ., data = train)  # By default mtry=floor(sqrt(ncol(x)))  ntree = 500
print(rf_default2)
plot(rf_default2)
varImpPlot(rf_default2, sort = T, n.var = 10, main = "Top 10 - Variable Importance")  # showing the importance of the variables in plot
varUsed(rf_default2)  # how many times for each variable is be used in the tuned random forest model

# prediciton with test data
library(lattice)
library(caret)
p2 = predict(rf_default2, test)
confusionMatrix(p2, test$y)

# calculate AUC2
library(pROC)
PredictionwithProb_default2 = predict(rf_default2, test, type = "prob")
auc2                        = auc(test$y, PredictionwithProb_default2[, 2])
plot(roc(test$y, PredictionwithProb_default2[, 2]))  # plot ROC curve
auc2

# rf_default3
# data importation
bank_additional_full = read.csv("bank-additional-full.csv", sep = ";")
# data adjustment
bank_additional_full$duration =
  ifelse(
    bank_additional_full$duration < 200,
    as.character("200-"),
    ifelse(
      bank_additional_full$duration > 200 &
        bank_additional_full$duration < 400,
      as.character("200-400"),
      ifelse(
        bank_additional_full$duration > 400 &
          bank_additional_full$duration < 600,
        as.character("400-600"),
        as.character("600+")
      )
    )
  )
bank_additional_full$duration = as.factor(bank_additional_full$duration)
bank_additional_full$age      =
  ifelse(
    bank_additional_full$age < 20,
    as.character("20-"),
    ifelse(
      bank_additional_full$age >= 20 &
        bank_additional_full$age < 30,
      as.character("20-30"),
      ifelse(
        bank_additional_full$age >= 30 &
          bank_additional_full$age < 40,
        as.character("30-40"),
        ifelse(
          bank_additional_full$age >= 40 &
            bank_additional_full$age < 50,
          as.character("40-50"),
          ifelse(
            bank_additional_full$age >= 50 &
              bank_additional_full$age < 60,
            as.character("50-60"),
            as.character("60+")
          )
        )
      )
    )
  )
bank_additional_full$age = as.factor(bank_additional_full$age)
# data partition
set.seed(123)  # start with a random seed so that we can make this analysis repeatable 
ind   = sample(2, nrow(bank_additional_full), replace = TRUE, prob = c(0.8, 0.2))  # divide the dataset into two parts
train = bank_additional_full[ind == 1, ]  # train dataset
test  = bank_additional_full[ind == 2, ]  # test dataset
# rf_default3
library(randomForest)
set.seed(222)
rf_default3 = randomForest(y ~ ., data = train)  # By default mtry=floor(sqrt(ncol(x)))  ntree = 500
print(rf_default3)
plot(rf_default3)
varImpPlot(rf_default3, sort = T, n.var = 10, main = "Top 10 - Variable Importance")  # showing the importance of the variables in plot
varUsed(rf_default3)  # how many times for each variable is be used in the tuned random forest model

# prediciton with test data
library(lattice)
library(caret)
p3 = predict(rf_default3, test)
confusionMatrix(p3, test$y)

# calculate AUC3
library(pROC)
PredictionwithProb_default3 = predict(rf_default3, test, type = "prob")
auc3                        = auc(test$y, PredictionwithProb_default2[, 2])
plot(roc(test$y, PredictionwithProb_default2[, 2]))  # plot ROC curve
auc3

# rf_default4
# data importation
bank_additional_full = read.csv("bank-additional-full.csv", sep = ";")
# data adjustment
bank_additional_full$marital        = NULL
bank_additional_full$default        = NULL
bank_additional_full$housing        = NULL
bank_additional_full$loan           = NULL
bank_additional_full$contact        = NULL
bank_additional_full$month          = NULL
bank_additional_full$previous       = NULL
bank_additional_full$cons.conf.idx  = NULL
bank_additional_full$cons.price.idx = NULL
bank_additional_full$emp.var.rate   = NULL
# data partition
set.seed(123)  # start with a random seed so that we can make this analysis repeatable 
ind   = sample(2, nrow(bank_additional_full), replace = TRUE, prob = c(0.8, 0.2))  # divide the dataset into two parts
train = bank_additional_full[ind == 1, ]  # train dataset
test  = bank_additional_full[ind == 2, ]  # test dataset
# rf_default4
library(randomForest)
set.seed(222)
rf_default4 = randomForest(y ~ ., data = train)  # By default mtry=floor(sqrt(ncol(x)))  ntree = 500
print(rf_default4)
plot(rf_default4)
varImpPlot(rf_default4, sort = T, n.var = 10, main = "Top 10 - Variable Importance")  # showing the importance of the variables in plot
varUsed(rf_default4)  # how many times for each variable is be used in the tuned random forest model

# prediciton with test data
library(lattice)
library(caret)
p4= predict(rf_default4, test)
confusionMatrix(p4, test$y)

# calculate AUC4
library(pROC)
PredictionwithProb_default4 = predict(rf_default4, test, type = "prob")
auc4                        = auc(test$y, PredictionwithProb_default4[, 2])
plot(roc(test$y, PredictionwithProb_default4[, 2]))  # plot ROC curve
auc4
# compare the AUC testing results of 4 default models
print(auc1,auc2, auc3, auc4)

## Use the rf_default1 to tune the parameters
# data impartation
bank_additional_full = read.csv("bank-additional-full.csv", sep = ";")

# data partition
set.seed(123)  # start with a random seed so that we can make this analysis repeatable 
ind   = sample(2, nrow(bank_additional_full), replace = TRUE, prob = c(0.8, 0.2))  # divide the dataset into two parts
train = bank_additional_full[ind == 1, ]  # train dataset
test  = bank_additional_full[ind == 2, ]  # test dataset

# rf_default1
library(randomForest)
set.seed(222)
rf_default1 = randomForest(y ~ ., data = train)  # By default mtry=floor(sqrt(ncol(x)))  ntree = 500
print(rf_default1)
plot(rf_default1)
varImpPlot(rf_default1, sort = T, n.var = 10, main = "Top 10 - Variable Importance")  # showing the importance of the variables in plot
varUsed(rf_default1)  # how many times for each variables be uesd in the tuned random forest model


# prediciton and confusion matrix with test data
p1 = predict(rf_default1, test)
confusionMatrix(p1, test$y)

# calculate AUC for rf_default1
library(pROC)
PredictionwithProb_default1 = predict(rf_default1, test, type = "prob")
auc1                        = auc(test$y, PredictionwithProb_default1[, 2])
plot(roc(test$y, PredictionwithProb_default1[, 2]))  # plot ROC curve
auc1

# tune mtry
t = tuneRF(train[, -21], train[, 21], stepFactor = 0.5, plot = TRUE, ntreeTry = 500, trace = TRUE, improve = 0.05)  # From the result, we can choose the reasonable value of mtry.

# tuned random forest model
rf_tune = randomForest(y ~ ., data = train, ntree = 500, mtry = 8, importance = TRUE, proximity = TRUE)
print(rf_tune)

# prediction with test data for tuned random forest model
p_tune = predict(rf_tune, test)
confusionMatrix(p_tune, test$y)

# calculate AUC for tuned random forest model
library(pROC)
PredictionwithProb_tune = predict(rf_tune, test, type = "prob")
auc_tune                = auc(test$y, PredictionwithProb_tune[, 2])
plot(roc(test$y, PredictionwithProb_tune[, 2]))
auc_tune

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
x       = bank_additional_full[, 1:20]
y       = bank_additional_full[, 21]
control = trainControl(method = "repeatedcv", number = 3)
seed    = 10
set.seed(seed)
mtry       = floor(sqrt(ncol(x)))
tunegrid   = expand.grid(.mtry = mtry)
rf_default = train(y ~ ., data = bank_additional_full, method = "rf", metric = "Accuracy", tuneGrid = tunegrid, trControl = control)
print(rf_default)

# Method 1: Random Search
control = trainControl(method = "repeatedcv", number = 3, search = "random")
set.seed(seed)
mtry      = floor(sqrt(ncol(x)))
rf_random = train(y ~ ., data = bank_additional_full, method = "rf", metric = "Accuracy", tuneLength = 15, trControl = control)
print(rf_random)
plot(rf_random)

# Method 2: Grid Search
control = trainControl(method = "repeatedcv", number = 3, search = "grid")
set.seed(seed)
tunegrid      = expand.grid(.mtry = c(1:10))
rf_gridsearch = train(y ~ ., data = bank_additional_full, method = "rf", metric = "Accuracy", tuneGrid = tunegrid, 
                      trControl = control)
print(rf_gridsearch)
plot(rf_gridsearch)

# Method 3: with 'randomForest' package by using Algorithm Tune (tuneRF)
library(randomForest)
set.seed(seed)
bestmtry = tuneRF(x, y, stepFactor = 0.5, improve = 1e-05, ntree = 500)
print(bestmtry)

print(bestmtry)