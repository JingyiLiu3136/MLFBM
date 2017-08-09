# data importation
wd = getwd()
setwd(wd)
bank_additional_full = read.csv("bank-additional-full.csv", sep = ";")

# data partition
set.seed(123)  # start with a random seed so that we can make this analysis repeatable 
ind = sample(2, nrow(bank_additional_full), replace = TRUE, prob = c(0.8, 0.2))
train = bank_additional_full[ind == 1, ]
test = bank_additional_full[ind == 2, ]

# SVM_default
library(e1071)
set.seed(222)
svm_default = svm(y ~ ., data = train, type = "C-classification", kernel = "radial", probability = TRUE)
summary(svm_default)

# prediction
svm_default.pred = predict(svm_default, test)
# confusion matric and calculate the Accuracy
CF = table(Predicted = svm_default.pred, Actual = test$y)
CF
ACC = sum(diag(CF))/sum(CF)
ACC

# tuning parameter gamma and cost with Radial Basis Function(RBF) set.seed(222) para.tuned =
# tune.svm(y~., data=train, gamma=10^(-2:1), cost= 10^(-1:1), cv = 3) summary(para.tuned) It took
# lots of time to tune the parameter gamma and cost. As a result, we can't get the best parameter
# of them. Based on we have already got good accuracy, so we don't need to waste lots of time to
# tune the parameters here. I also attatch the code for tuning as following.  tuned SVM svm_tuned
# = svm(y~., data= train, gamma= , cost= , cv=) prediciton svm_tuned.pred = predict(svm_tuned,
# test) confusion matric and calculate the Accuracy CF = table(Predicted = svm_default.pred,
# Actual = test$y) CF ACC= sum(diag(CF))/ sum(CF) ACC
