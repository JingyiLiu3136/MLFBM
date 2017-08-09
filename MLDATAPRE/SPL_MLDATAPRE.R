# clear
rm(list = ls(all = TRUE))
graphics.off()

# load data
wd = getwd()
setwd(wd)
bank_additional_full = read.csv("bank-additional-full.csv", sep = ";")
summary(bank_additional_full)

# Plot 1
library(ggplot2)
bank_additional_full = read.csv("bank-additional-full.csv", sep = ";")
summary(bank_additional_full$age)
age     = bank_additional_full$age
marital = bank_additional_full$marital
ggplot(bank_additional_full) + geom_histogram(aes(x = age))  #make histogram of age distribution
ggplot(bank_additional_full) + geom_density(aes(x = age))
ggplot(bank_additional_full) + geom_density(aes(x = age, color = marital))
p = ggplot(data = bank_additional_full, mapping = aes(x = age, y = marital))
p + geom_point()  #point the age distribution by marital

# plot to analyse variable job and education,use density plot and bar plot
job       = bank_additional_full$job
education = bank_additional_full$education
ggplot(bank_additional_full) + geom_bar(aes(x = job))
ggplot(bank_additional_full) + geom_bar(aes(x = job, fill = education))

# plot to analyse the correlation between different variables
loan = bank_additional_full$loan
buy  = bank_additional_full$y
ggplot(bank_additional_full) + geom_boxplot(aes(x = job, y = age, fill = buy))
ggplot(bank_additional_full) + geom_histogram(aes(x = age, fill = loan))

# plot the linear correlation between consumer price index and consumer confidence
consumerprice     = bank_additional_full$cons.price.idx
consumercofidence = bank_additional_full$cons.conf.idx
ggplot(bank_additional_full, aes(x = consumerprice, y = consumercofidence)) + geom_point()
ggplot(bank_additional_full, aes(x = consumerprice, y = consumercofidence)) + geom_point() + 
    stat_smooth()

# try to find the relationship between number of employement and variation rate of
# employement
emp.var.rate = bank_additional_full$emp.var.rate
nr.emp       = bank_additional_full$nr.employed
ggplot(bank_additional_full, aes(x = nr.emp, y = emp.var.rate)) + geom_point() + geom_smooth() + 
    labs(x = "number of employement") + labs(y = "variation rate of employement")

# creat a function to search outliers
outlier_detection = function(data) {
    boundary = mean(data, na.rm = T) + 3 * sd(data, na.rm = T)
    message("Boundary is ", boundary)
    message("Count over Boundary: ", sum(data > boundary, na.rm = T), " (", (sum(data > boundary, 
        na.rm = T)/length(data)) * 100, "%)")
}
outlier_detection(bank_additional_full$duration)
# Boundary is 1036.12275670654 Count over Boundary: 861 (2.09041468388851%)
outlier_detection(bank_additional_full$campaign)
# Boundary is 12.0579033080844 Count over Boundary: 840 (1.85795492247462%)
outlier_detection(bank_additional_full$pdays)
# Boundary is 1523.20817604996 Count over Boundary: 0 (0%)
outlier_detection(bank_additional_full$previous)
# Boundary is 1.6576662384496 Count over Boundary: 1064 (2.58327668252889%)

# next part are two kinds of data-processing methods,for some models,like random forest
# and decision tree,we need to use these methods to prepare data for running the model
# more efficently 
# group the data of one variable
bank_additional_full$duration = ifelse(bank_additional_full$duration < 200, as.character("200-"), 
    ifelse(bank_additional_full$duration > 200 & bank_additional_full$duration < 400, as.character("200-400"), 
        ifelse(bank_additional_full$duration > 400 & bank_additional_full$duration < 600, 
            as.character("400-600"), as.character("600+"))))
bank_additional_full$duration = as.factor(bank_additional_full$duration)
bank_additional_full$age = ifelse(bank_additional_full$age < 20, as.character("20-"), ifelse(bank_additional_full$age >= 
    20 & bank_additional_full$age < 30, as.character("20-30"), ifelse(bank_additional_full$age >= 
    300 & bank_additional_full$age < 400, as.character("30-40"), ifelse(bank_additional_full$age >= 
    400 & bank_additional_full$age < 500, as.character("40-50"), ifelse(bank_additional_full$age >= 
    50 & bank_additional_full$age < 60, as.character("50-60"), as.character("60+"))))))

# variables selecting
bank_additional_full$age            = as.factor(bank_additional_full$age)
bank_additional_full$nr.employed    = NULL  #as.factor(bank_additional_full$nr.employed)
bank_additional_full$euribor3m      = NULL  #as.factor(bank_additional_full$euribor3m)
bank_additional_full$cons.conf.idx  = NULL  #as.factor(bank_additional_full$cons.conf.idx)
bank_additional_full$cons.price.idx = NULL  #as.factor(bank_additional_full$cons.price.idx)
bank_additional_full$emp.var.rate   = NULL  #as.factor(bank_additional_full$emp.var.rate)
bank_additional_full$campaign       = as.factor(bank_additional_full$campaign)
bank_additional_full$pdays          = as.factor(bank_additional_full$pdays)
bank_additional_full$previous       = as.factor(bank_additional_full$previous)

