# data importation
wd = getwd()
setwd(wd)
bank_additional_full = read.csv("bank-additional-full.csv", sep = ";")

# data partition
set.seed(123)  # start with a random seed so that we can make this analysis repeatable 
ind   = sample(2, nrow(bank_additional_full), replace = TRUE, prob = c(0.8, 0.2))
train = bank_additional_full[ind == 1, ]
test  = bank_additional_full[ind == 2, ]

# SVM_default
library(e1071)
set.seed(222)
svm_default = svm(y ~ ., data = train, type = "C-classification", kernel = "radial", probability = TRUE)
summary(svm_default)

# prediction
svm_default.pred = predict(svm_default, test)
# confusion matric and calculate the Accuracy
CF_default  = table(Predicted = svm_default.pred, Actual = test$y)
CF_default
ACC_default = sum(diag(CF))/sum(CF)
ACC_default

# try to calculate the AUC
#predict the test data
svm_default.predict = predict(svm_default,subset(test,select=-y),decision.values=TRUE)
svm_default.probs   = attr(svm_default.predict,"decision.values")
svm_default.class   = predict(svm_default,test,type="class")
svm_default.labels  = test$y

#calculate the AUC
library(ROCR)
svm_default.prediction  = prediction(svm_default.probs,svm_default.labels)
svm_default.performance = performance(svm_default.prediction,"tpr","fpr")
plot(svm_default.performance)
svm_default.auc         = performance(svm_default.prediction,"auc")@y.values[[1]]
svm_default.auc


# tuning parameter gamma and cost with Radial Basis Function(RBF) 
set.seed(222) 
para.tuned = tune.svm(y~., data=train, gamma=10^(-2:1), cost= 10^(-1:1)) # It would take lots of time to run this code since it contains the 10-fold cv.
summary(para.tuned) 

#tuned SVM 
svm_tuned                 = svm(y~., data= train, gamma=0.01, cost=10) 
#prediction
svm_tuned.pred = predict(svm_tuned,test) 
CF_tune        = table(Predicted = svm_default.pred, Actual = test$y) 
CF_tune
ACC_tune       = sum(diag(CF))/ sum(CF) 
ACC_tune
