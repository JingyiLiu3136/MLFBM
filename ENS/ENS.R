#First load the packages we need, 
#the caret and caretEnsemble package will help us build the model
#the pROC package will help to calculate roc and plot it
library("caret") 
library("caretEnsemble")
library("pROC") 
library("doParallel")
set.seed(1234)
#use the function in caret pakage to help split the data,
#data train is for the model building
#test is the for the model testing
#here we set the split to 80:20,means 80% of data will be random select as train data, 20% will be test data
idx.train = createDataPartition(y = bank_additional_full$y, p = 0.8, list = FALSE)
train     = bank_additional_full[idx.train, ]
test      =  bank_additional_full[-idx.train, ]


#here we define which method to use to control how models will be build
#we will use cross validation method, the number of folds are 5
#It also enable the the ablity from the package to do the parallel computing
ctrl  = trainControl(method = "cv", number = 5, classProbs = TRUE,  savePredictions = "final", summaryFunction = twoClassSummary, allowParallel = TRUE)

#setting up parallel cores
nrOfCores = detectCores()
cl        = makeCluster( max(1,detectCores()-1))
registerDoParallel(cl)
#define the modellist, which contains the models and the parameters to use,here we jsut set all the parameter to default
modelList = list(caretModelSpec(method = "nnet"),
                  caretModelSpec(method = "rf"),
                  caretModelSpec(method = "rpart"),
                  caretModelSpec(method = "glm", family = binomial(link = "logit"))
                  ) 

#train the models
models = caretList(y~., data = train, trControl = ctrl, tuneList = modelList, continue_on_fail = F)
stopCluster(cl)
lb     = matrix(c(test$y,test$y,test$y,test$y),nrow = 8237, byrow = T)
lb[,1] = ifelse(lb[,1] == 1, "no", "yes")
lb[,2] = ifelse(lb[,2] == 1, "no", "yes")
lb[,3] = ifelse(lb[,3] == 1, "no", "yes")
lb[,4] = ifelse(lb[,4] == 1, "no", "yes")


models.pred     = sapply(models, function(x) predict(x, newdata = test, type = "prob")[,2])
models.pred.raw = sapply(models, function(x) predict(x, newdata = test, type = "raw"))
models.auc      = apply(models.pred, 2, function(x) auc(response = test$y, predictor = x))
predictions     = ROCR::prediction(predictions = models.pred, labels = lb)

rocr.pred.rf    = ROCR::prediction(predictions = predict(models$rf, newdata = test, type = "prob")[,2], labels = test$y)
rocr.pred.nn    = ROCR::prediction(predictions = predict(models$nn, newdata = test, type = "prob")[,2], labels = test$y)
rocr.pred.rpart = ROCR::prediction(predictions = predict(models$rpart, newdata = test, type = "prob")[,2], labels = test$y)
rocr.pred.glm   = ROCR::prediction(predictions = predict(models$glm, newdata = test, type = "prob")[,2], labels = test$y)

rocr.rf.roc     = ROCR::performance(rocr.pred.rf,"tpr", "fpr")
rocr.nn.roc     = ROCR::performance(rocr.pred.nn,"tpr", "fpr")
rocr.rpart.roc  = ROCR::performance(rocr.pred.rpart,"tpr", "fpr")
rocr.glm.roc    = ROCR::performance(rocr.pred.glm,"tpr", "fpr")
library(ggplot2)
ROCR::plot(rocr.rf.roc,main     = "ROC Curve - Random Forest", colorize = T,  lwd=10)
ROCR::plot(rocr.nn.roc,main     = "ROC Curve - Neural Network", colorize = T,  lwd=10)
ROCR::plot(rocr.rpart.roc,main  = "ROC Curve - Decision Tree", colorize = T,  lwd=10)
ROCR::plot(rocr.glm.roc,main    = "ROC Curve - Logistic Regression", colorize = T,  lwd=10)

models.pred.acc  =  ROCR::performance(prediction.obj = predictions, "acc")
models.pred.roc  =  ROCR::performance(prediction.obj = predictions, "tpr", "fpr")
models.pred.lift =  ROCR::performance(prediction.obj = predictions, "lift")

best_current_auc = max(models.auc)

greedy_ensemble  = caretEnsemble(
  models, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))
summary(greedy_ensemble)
library("caTools")
model_preds = lapply(models, predict, newdata=test, type="prob")
model_preds = lapply(model_preds, function(x) x[,"yes"])
model_preds = data.frame(model_preds)
ense_preds  = predict(greedy_ensemble, newdata=test, type="prob")
model_preds$ensemble = ense_preds
caTools::colAUC(model_preds, test$y)
varImp(greedy_ensemble)
