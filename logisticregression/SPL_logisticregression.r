bank_additional_full = read.csv("bank-additional-full.csv",sep = ";")
# Logistic regression 
# Use the generalized model function to create the model and save the trained model as 'lr'.
lr = glm(formula = y ~ ., data = bank_additional_full, family = binomial(link = "logit"))
summary(lr)

#extract the coefficients and p-values with function coef
coef(summary(lr))

# we can see that 'housing' and 'loan' are not statistically significant. That means that they
# both can be 100% predicted by some combination of other charactoristics.  However, the
# selection of formula-based model by AIC should be further processed.

# Analysis of Deviance Table Analyzing the table we can see the drop in deviance when adding each
# variable one at a time.
anoval = anova(lr, test = "Chisq")
print(anoval)

# Select a formula-based model by AIC.
lrstep = step(lr, 
              scale = 0, 
              direction = c("both", "backward", "forward"), 
              trace = 1, 
              keep = NULL, 
              steps = 1000,
              k = 2)
summary(lrstep)
lrstep$anova
lrstep

# Compute model predictions. Run a logistic regression to predict the dependent variable y based
# on all other variables in the data set.  Above we examined the coefficients and significance of
# the variables in our logistic regression model. Several variables turned out to be
# insignificant.  So we use a step model as an improvement.
pred.lr.step = predict(lrstep, newdata = bank_additional_full, type = "response")
# predict function
head(pred.lr.step)

# Assessing the predictive ability of the model the accuracy of a model describes how often the
# predicted class matches the observed outcome.  Infer the predicted class from the predicted
# class probabilities We chose the default threshold of 0.5
class.lr.step = ifelse(pred.lr.step > 0.5, "yes", "no")
print(class.lr.step)
accuracy.lr.step = sum(class.lr.step == bank_additional_full$y)/length(class.lr.step)
print(accuracy.lr.step)
sum(pred.lr.step > 0.5)


# Assess by ROC method. plot the ROC curve and calculate the AUC (area under the curve) which are
# typical performance measurements for a binary classifier.
if (!require("ROCR")) install.packages("ROCR")
library(ROCR)
pr.step  = prediction(pred.lr.step, bank_additional_full$y)
# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
prf.step = performance(pr.step, measure = "tpr", x.measure = "fpr")  
plot(prf.step)  

auc.step = performance(pr.step, measure = "auc")
slot(auc.step, "y.values")




