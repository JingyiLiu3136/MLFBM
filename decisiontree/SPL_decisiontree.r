# Decision Tree
bank_additional_full = read.csv("bank-additional-full.csv",sep = ";")
#Install and load package rpart.
if (!require("rpart")) install.packages("rpart")
library("rpart") 

# build the model
# create decision tree classifier
dt = rpart(y ~ ., data = bank_additional_full, method = "class")  
summary(dt)
plot(dt)
text(dt)

# Producing a nice chart to visualize the decision trees.
if (!require("rpart.plot")) install.packages("rpart.plot")
library("rpart.plot")

# Visualize the results from 'dtree' using the rpart.plot() function.
rpart.plot(dt, 
           extra = 104, 
           box.palette = "GnBu", 
           branch.lty = 3, 
           shadow.col = "gray", 
           nn = TRUE)
prp(dt, 
    type = 2, 
    extra = 106, 
    nn = TRUE, 
    fallen.leaves = TRUE, 
    shadow.col = "grey")

# Party Tree
install.packages("partykit")
library(partykit)
class(dt)
plot(as.party(dt))

# Prediction
pred.dt.class = predict(dt, newdata = bank_additional_full, type = "class")
head(pred.dt.class)
mean(pred.dt.class == bank_additional_full$y)

pred.dt.prob = predict(dt, newdata = bank_additional_full, type = "prob")[, 2]

# Next we see how good the model is by seeing how it fares against the test data. Evaluate
# Decision Tree—Error Matrix
predicted = pred.dt.class
actual    = bank_additional_full$y
table(predicted, actual)

# Assessing the predictive ability of the model.The accuracy of a model describes how often the
# predicted class matches the observed outcome.  Infer the predicted class from the predicted
# class probabilities We chose the default threshold of 0.5
class.dt.prob = ifelse(pred.dt.prob > 0.5, "yes", "no")
accuracy.dt.prob = sum(class.dt.prob == bank_additional_full$y)/length(class.dt.prob)
print(accuracy.dt.prob)

# We can print a table of optimal prunings based on a complexity parameter using printcp(). The
# plotcp() plots the cross-validation results.  Here we see a set of possible cost-complexity
# prunings of the tree.
printcp(dt)
plotcp(dt)

# To see if a shallower subtree can give us comparable results.  If so, we’d be better of
# choosing the shallower tree because it reduces the likelihood of overfitting.  get index of CP
# with lowest xerror
opt = which.min(dt$cptable[, "xerror"])
# get its value
cp  = dt$cptable[opt, "CP"]
# prune the tree based on this value of CP prune tree
pruned_dt = prune(dt, cp)
# plot tree
plot(pruned_dt)
text(pruned_dt)
# find proportion of correct predictions using test set
pred.dt.class.pruned = predict(pruned_dt, 
                               newdata = bank_additional_full,
                               type = "class")
mean(pred.dt.class.pruned == bank_additional_full$y)
# This means, as far as the cost complexity pruning is concerned, the optimal subtree is the same
# as the original tree

# Calculate performance according to ROC
library(ROCR)
pr.dt  = prediction(pred.dt.prob, bank_additional_full$y)
roc.dt = performance(pr.dt, measure = "tpr", x.measure = "fpr")  
## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
plot(roc.dt,
     main = "ROC curve Decision Tree", 
     col = "orange", lwd = 2)
lines(x = c(0, 1),
      y = c(0, 1), 
      col = "red", 
      lwd = 2)

auc.dt = performance(pr.dt, measure = "auc")
slot(auc.dt, "y.values")






