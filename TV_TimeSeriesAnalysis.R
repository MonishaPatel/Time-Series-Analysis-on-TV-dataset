---
title: "Time Series Analysis on TV viewers Dataset"
author: "Monisha Patel"
date: "October 11, 2018"
---

The data set consists of list ot tables with column names:
  date: consists of year and month starting Jan 2014 till Dec 2017
  Network: consists network, A and B
  DayPart: this columns has filters for days of week and timings
  viewers: consists of number of viewers for a particular network and daypart every month starting Jan 2014
  
To start with first read the csv.

```{r}
tv_data <- read.csv("tv_data.csv")
head(tv_data)
summary(tv_data)

```

Next step is to make different tables based on the filters in network and daypart. Which will give us 22 tables.
And store each table in a different variable.

```{r}
library(reshape2)
all_tables <- split(x = tv_data, f = list(tv_data$network, tv_data$daypart))

for (i in 1:length(all_tables)){
  assign(paste("sub_table",i, sep =""),as.data.frame(all_tables[i]))
}

```

Now we will make a function to analyse each time series and make prediction using HoltWinters

```{r }
library(forecast)

timeseries_analysis <- function(dftable){
  
  tablename <- colnames(dftable)[4]
  print(tablename)
  
  colnames(dftable)[4] <- "viewers"
  # convert data frame into time series
  ts_tvdata <- ts(dftable$viewers, start = c(2014, 01), frequency = 12) 
  
  #decompose time series and plot
  decomp <- stl(ts_tvdata, s.window = 12) 
  plot(decomp, main = tablename) 
  
  #split data into test(Jan 2017 till Dec 2017) and train(Jan 2014 till Dec 2016)
  training_set<- window(ts_tvdata, start = c(2014, 01), end = c(2016, 12), frequency = 12) 
  test_set <- window(ts_tvdata, start = c(2017, 01), end = c(2017, 12), frequency = 12)
  
  #plots Auto-correlation
  acf(training_set)
  
  #plots partial auto-correlation
  pacf(training_set)
  
  #fit HolyWinters on training set
  fit = HoltWinters(training_set, beta = FALSE)
  
  #forecast values for next 12 months
  fcast = forecast(fit, h = 12)
  
  #plot time series with the forecasted values
  plot(fcast, main=tablename)
  lines(test_set, col = "red")
  
  # Find all errors in both the time series including MAE
  acc <- accuracy(fcast, test_set)
  print(acc)
  
  #Finds mean absolute error
  error <- fcast$mean - test_set
  print(paste("The MAE is", round(mean(abs(error)), 3)))
}


```

```{r}

timeseries_analysis(sub_table1)
timeseries_analysis(sub_table2)
timeseries_analysis(sub_table3)
timeseries_analysis(sub_table4)
timeseries_analysis(sub_table5)
timeseries_analysis(sub_table6)
timeseries_analysis(sub_table7)
timeseries_analysis(sub_table8)
timeseries_analysis(sub_table9)
timeseries_analysis(sub_table10)
timeseries_analysis(sub_table11)
timeseries_analysis(sub_table12)
timeseries_analysis(sub_table13)
timeseries_analysis(sub_table14)
timeseries_analysis(sub_table15)
timeseries_analysis(sub_table16)
timeseries_analysis(sub_table17)
timeseries_analysis(sub_table18)
timeseries_analysis(sub_table19)
#
#
#As all the observation in training set is Zero for Sub_table20, acf and pacf of training set cannot be found hence will throw an error
#Moreover, when forecasted on the bases of training set, we will get predicted values as zero
#timeseries_analysis(sub_table20) 
#
#

timeseries_analysis(sub_table21)
timeseries_analysis(sub_table22)

```

