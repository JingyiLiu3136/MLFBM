#date completion
#create a column that gives the complete date of the data, so that we could add other economical data
library("lubridate")#load required package to deal with date entries
bank_additional_full = read.csv("bank-additional-full.csv",sep = ";")

idx.year.bank_add_full.2008 = 27691 #set the index of the row which the year changed from 2008 to 2009
idx.year.bank_add_full.2009 = 39131 #set the index of the row which the year changed from 2009 to 2010
#the index comes from manually observation using view()functions


idx.day.bank_add_full_automate =
  function(data = bank_additional_full) {
    idx = vector()
    for (i in 2:length(data$day_of_week)) {
      if (data$day_of_week[i - 1] != data$day_of_week[i]) {
        idx = c(idx, i)
      }
    }
    return(idx)
  }#This function detects the change of the day and return the vector of all the row numbers which the day changes

idx.weekday.bank_add_full =
  idx.day.bank_add_full_automate(bank_additional_full)


View(bank_additional_full[idx.weekday.bank_add_full, ]) #using this view together with a calendar we are able to identify the exact date through simple deduction
idx.day.bank_add_full = c( #Through the calandar and the weekdays we will be able to identify the exact day of the month of each row
  5:9,12:16,19:21,23,26:30,#May,2008
  2:6,9,11,12,16:20,23:27,30,#June
  1:4,7:11,14:18,21:25,28:31,#July
  4:8,11:14,18:22,25:29,#AUG
  13:17,20:24,#Oct
  3:7,10:14,17:21,26:28,#Nov
  1,3:5,8,9,11,12,15,#Dec
  3:6,9:13,16:20,23:27,30,31,#Mar,2009
  1:3,6:9,14:17,20:24,27:30,#Apr
  4:8,11:15,18,22,25:28,#May
  1:5, 8, 9,19,22:26,29,30,#June
  1:3,6:10,13,15,16,20:24,27:30,#July
  4:7,10:14,17:21,24:28,31,#Aug
  1:4,7:11,14:18,22:25,28:30,#Sep
  1,2,6:9,12:16,19:23,26:30,#Oct
  2:6,9:13,16:18,20,30,#Nov
  2:4,7,9:11,14:18,21:24,28:31,#Dec
  1:5,8:12,15:18,22,23,25,26,29:31,#Mar,2010
  1,6:8,12:16,19:23,26:30,#Apr
  4:7,10:14,17:21,24:28,31,#May
  1,2,4,7:9,11,14:18,21:25,28:30,#June
  1,2,5:9,12:16,19:23,26:30,#July
  2:6,9:13,16:20,23:25,27,30,31,#Aug
  1,3,6:10,13:17,20:24,27:29,#Sep
  1,4:8,11:15,18:22,25:28,#Oct
  2,8:12,15:19,22:26)#Nov

month_conversion = function(data = bank_full) {
  data$month = as.character(data$month)
  data$month[which(data$month == "jan")] = "1"
  data$month[which(data$month == "feb")] = "2"
  data$month[which(data$month == "mar")] = "3"
  data$month[which(data$month == "apr")] = "4"
  data$month[which(data$month == "may")] = "5"
  data$month[which(data$month == "jun")] = "6"
  data$month[which(data$month == "jul")] = "7"
  data$month[which(data$month == "aug")] = "8"
  data$month[which(data$month == "sep")] = "9"
  data$month[which(data$month == "oct")] = "10"
  data$month[which(data$month == "nov")] = "11"
  data$month[which(data$month == "dec")] = "12"
  data$month = as.integer(data$month)
  return(data)
}#convert the month to numbers so that we can easily generate exact date

bank_additional_full = month_conversion(bank_additional_full)

bank_additional_full$day = c(1:length(bank_additional_full$age))

weekday_day_conversion =
  function(data = bank_additional_full,
           idx.weekday.bank_add_full,
           idx.day.bank_add_full) {
    for (i in 1:length(idx.weekday.bank_add_full)) {
      if (i == 1) {
        data$day[1:idx.weekday.bank_add_full[1]] =
          idx.day.bank_add_full[1]
      }
      else{
        data$day[idx.weekday.bank_add_full[i - 1]:idx.weekday.bank_add_full[i]] =
          idx.day.bank_add_full[i]
      }
    }
    return(data)
  }
bank_additional_full =
  weekday_day_conversion(bank_additional_full,
                         idx.weekday.bank_add_full,
                         idx.day.bank_add_full)


date_completion = function(data = bank_full) {# the function to combine all create day, month and year together
    month(data$date) = data$month
    day(data$date) = data$day
    year(data$date[1:27690])      = 2008
    year(data$date[27691:39130])  = 2009
    year(data$date[39131:41188])  = 2010
  return(data)
}
bank_additional_full$date = as.Date("01-01-2008", format = "%d-%m-%Y")
bank_additional_full = date_completion(bank_additional_full)

bank_additional_full$year = year(bank_additional_full$date)
addtional.data = read.csv("aditional_data.CSV")
addtional.data$Time.series.name = as.character(addtional.data$Time.series.name)
addtional.data$Time.series.name = as.Date(addtional.data$Time.series.name,format = "%d-%m-%Y")

bank_additional_full[,25:56] = 1
for(y in 2008:2010) {
  for (m in 1:12) {
    for (l in 1:32) {
      bank_additional_full[year(bank_additional_full$date) == y &
                             month(bank_additional_full$date) == m, l + 24] =
        addtional.data[month(addtional.data$Time.series.name) == m &
                    year(addtional.data$Time.series.name) == y, l+1]
    }
  } 
}
names(bank_additional_full)[25:56] = names(addtional.data)[2:33]
