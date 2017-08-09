# Neural network model Load the data frame bank_addtional_full.
bank_additional_full = read.csv("bank-additional-full.csv", sep = ";")
set.seed(123)

# When I use the neural network model,there are four variables I think don't have enough
# meaning of result,so I remove them from the data frame.
summary(bank_additional_full$poutcome)
summary(bank_additional_full$contact)
useddata = bank_additional_full[, -c(8, 9, 10, 15)]

# in this model,data must be numeric,so I present the variable 'education' as the length
# of educational years.
summary(useddata$education)
useddata$education = as.character(useddata$education)
useddata$education[which(useddata$education == "basic.4y")] = "4"
useddata$education[which(useddata$education == "basic.6y")] = "6"
useddata$education[which(useddata$education == "basic.9y")] = "9"
useddata$education[which(useddata$education == "high.school")] = "12"
useddata$education[which(useddata$education == "professional.course")] = "12"
useddata$education[which(useddata$education == "university.degree")] = "16"
useddata$education[which(useddata$education == "illiterate")] = "0"
useddata$education[which(useddata$education == "unknown")] = "0"
useddata$education = as.numeric(useddata$education)
summary(useddata$education)

# In the model,all variable must be normalized,so I normalize the quantitative variable
useddata$age            = (useddata$age - min(useddata$age))/(max(useddata$age) - min(useddata$age))
useddata$education      = (useddata$education - min(useddata$education))/(max(useddata$education) - 
    min(useddata$education))
useddata$duration       = (useddata$duration - min(useddata$duration))/(max(useddata$duration) - 
    min(useddata$duration))
useddata$campaign       = (useddata$campaign - min(useddata$campaign))/(max(useddata$campaign) - 
    min(useddata$campaign))
useddata$pdays          = (useddata$pdays - min(useddata$pdays))/(max(useddata$pdays) - min(useddata$pdays))
useddata$previous       = (useddata$previous - min(useddata$previous))/(max(useddata$previous) - 
    min(useddata$previous))
useddata$emp.var.rate   = (useddata$emp.var.rate - min(useddata$emp.var.rate))/(max(useddata$emp.var.rate) - 
    min(useddata$emp.var.rate))
useddata$cons.price.idx = (useddata$cons.price.idx - min(useddata$cons.price.idx))/(max(useddata$cons.price.idx) - 
    min(useddata$cons.price.idx))
useddata$cons.conf.idx  = (useddata$cons.conf.idx - min(useddata$cons.conf.idx))/(max(useddata$cons.conf.idx) - 
    min(useddata$cons.conf.idx))
useddata$euribor3m      = (useddata$euribor3m - min(useddata$euribor3m))/(max(useddata$euribor3m) - 
    min(useddata$euribor3m))
useddata$nr.employed    = (useddata$nr.employed - min(useddata$nr.employed))/(max(useddata$nr.employed) - 
    min(useddata$nr.employed))
summary(useddata)

# Use package 'dummy' to transfer the qualitative variable as dummy variable, and use
# 'data.marix()' transfer all variable as numeric,but this function will change all dummy
# variable as 1 and 2,so we need take all dummy variable minus 1.
library("dummy")
dummydata    = dummy(useddata, p = "all", object = NULL, int = FALSE, verbose = FALSE)
dummydata2   = data.matrix(dummydata)
for (i in 1:27) {
    dummydata2[, i] = dummydata2[, i] - 1
}
numericdata  = useddata[, -c(2, 3, 5, 6, 7, 17)]
numericdata2 = data.matrix(numericdata)
yes          = dummydata2[, 27]
no           = dummydata2[, 26]
dummydata3   = dummydata2[, -c(26, 27)]
alldata      = data.frame(dummydata3, numericdata2, no, yes)  #rebuild the data frame as numerical data frame.

# devide the whole data frame as two data frame,one for train model,one for test model.
library("caret")
resulttrain = createDataPartition(y = alldata$yes, p = 0.5, list = FALSE)
train       = alldata[resulttrain, ]
test        = alldata[-resulttrain, ]

# use 'neuralnet' package to train model and plot it
library("neuralnet")
formula = yes + no ~ job_admin. + job_blue.collar + job_entrepreneur + job_housemaid + job_management + 
    job_retired + job_self.employed + job_services + job_student + job_technician + job_unemployed + 
    job_unknown + marital_divorced + marital_married + marital_single + marital_unknown + 
    default_no + default_unknown + default_yes + housing_no + housing_unknown + housing_yes + 
    loan_no + loan_unknown + loan_yes + age + education + duration + campaign + pdays + previous + 
    emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed
mynn = neuralnet(formula, data = train, hidden = 5, threshold = 0.01, act.fct = "logistic", 
    linear.output = FALSE)
plot(mynn)
mynn$result.matrix

# predict the data frame 'test',get the result
mynn.prediction = compute(mynn, test[, 1:36])
mynn.prediction = mynn.prediction$net.result
head(mynn.prediction)
View(mynn.prediction)

# analysis and compare the result of prediction by ROC and AUC plot the ROC picture and
# AUC result
predictyes = mynn.prediction[, 1]
library("ROCR")
pr  = ROCR::prediction(predictyes, test$yes)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]
auc
