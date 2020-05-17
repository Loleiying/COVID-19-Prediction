# importing library
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(timeSeries))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(tseries))
suppressPackageStartupMessages(library(lubridate))

# importing dataset
train=clean_names(read.csv("E:/Education/George Mason University/S20/202010.21270 CDS-403-001/Final Project/COVID-19 Week 3/train.csv", header=TRUE, stringsAsFactors=FALSE))
test=clean_names(read.csv("E:/Education/George Mason University/S20/202010.21270 CDS-403-001/Final Project/COVID-19 Week 3/test.csv", header=TRUE, stringsAsFactors=FALSE))
submission=clean_names(read.csv("E:/Education/George Mason University/S20/202010.21270 CDS-403-001/Final Project/COVID-19 Week 3/submission.csv", header=TRUE, stringsAsFactors=FALSE))

# correcting data type
train$date=as.Date(train$date, format="%Y-%m-%d")
test$date=as.Date(test$date, format="%Y-%m-%d")

#####################################################################
# Try to Model for One Country
#####################################################################

# fetching dataset for the country
train.one.cases=train %>%
  dplyr::filter(country_region=="Afghanistan") %>%
  select(date,confirmed_cases)

train.one.fatalities=train %>%
  dplyr::filter(country_region=="Afghanistan")%>%
  select(date,fatalities)

# STEP 1. VISUALIZING THE TIME SERIES
# visualizing the time series
plot(train.one.cases)

# i'm not sure about that plot. i think it's showing me
# a general trend. now i'm replotting the data using
# another method. i think this is more reliable.
ts.data1=ts(train.one.cases$confirmed_cases, start=c(2020,1,22), frequency = 365.25)
plot(ts.data1, xlab="time", ylab="predicted confirmed_cases", main="Time Series Plot on Original Data")

# the plot shows i'm dealing with exponential time-series.
# i may plot single exponential smoothing (SES) over the dataset.
# however, i must first check the three assumption.

# i may need further research on this but
# i'm just going to test the stationary assumption,
# since it's the most obvious from the plot.
# if it's stationary, it shouldn't show an exponential trend.

# Dickey Fuller Test of Stationarity
adf.test(train.one.cases$confirmed_cases)

# the augmented dickey-fuller (ADF) test shows that
# p-value is extremely large. i fail to reject the
# null hypothesis that the data isn't stationary (explosive).

# STEP 2. STATIONARIZE THE DATASET

# i'm going to do log transformation and do differencing
# to take care of the problem. first, i need to get rid of
# values that are zero because that's going to cause
# problem when taking the log.

train.one.cases=train.one.cases %>%
  dplyr::filter(confirmed_cases > 0)

# this is the log transformation.
train.one.cases$confirmed_cases=log(train.one.cases$confirmed_cases)

# let's test the dataset with ADF.
# i'd like to proof that the transformation
# improves the p-value from ADF test.
adf.test(train.one.cases$confirmed_cases)

# the ADF test comes up with p-value of 0.1333,
# which is a great improvement.
# that means, the transformation is making the data
# closer to being stationary.
# i'm going to run the ADF again on differenced dataset.
adf.test(diff(train.one.cases$confirmed_cases))

# the p-value from the test has dropped to 0.09498.
# admittedly, i wouldn't call this ideal as it's
# greater than default alpha of 0.05.
# i think that if i were to have more data on this
# the p-value will get better.

# i'm going to try to plot the data after knowing the transformation.
ts.data2=ts(diff(train.one.cases$confirmed_cases), start=c(2020,1,22), frequency = 365.25)
plot(ts.data2, xlab="time", ylab="predicted confirmed_cases", main="Time Series Plot on Transformed Data")

# that looks more like a proper timeseries plot.


# STEP 3. PLOTTING THE ACF/PACF CHARTS TO FIND OPTIMAL PARAMETER
acf(diff(train.one.cases$confirmed_cases))
pacf(diff(train.one.cases$confirmed_cases))

# the plot is too inconsistent to be deciding the parameters.
# from the ACF plot, the first order differencing make
# the ts stationary. I=2.
# AR model might be investigated first with lag length selected from 
# the PACF or via empirical investigation. In my case, it's clearly 
# that within 3 lags the AR is significant. Which means, we 
# can use AR = 3.
# To avoid the potential for incorrectly specifying the MA order 
# (in the case where the MA is first tried then the MA order is being set to 0), 
# it may often make sense to extend the lag 
# observed from the last significant term in the PACF.

ts.data2=ts(diff(train.one.cases$confirmed_cases), start=c(2020,1), end=c(2020,12), frequency=365.25)
plot(ts.data2)

# the plot shows a flat line.
# made a mistake somewhere but i don't know.

# trying second method to make the model.
# the model is using Holt-Winter because it was
# originally exponential.
# i decided to try to predict the first 100 values.
fit.holt=HoltWinters(train.one.cases$confirmed_cases, beta=FALSE, gamma=FALSE)
pred=forecast(fit.holt,20)
plot(pred$x)