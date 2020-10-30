# Dawei Wang
# February 27, 2019
# Workshop 5: Forecasting
# Objectives: Create a forecast. Determine whether the forecast
# should include trend and seasonality parameters. Determine 
# whether to segment the forecast into two or more subgroups.
# Several challenges have questions. You do not need to answer these
# questions in your scripts. They are there to help you understand 
# what your analyses are telling you.
# Challenge 0: Create a new script and add a multi-line comment
# at the top with the name of the workshop, your name, and the date.
# Copy the remaining portion of this script and paste it into your
# script. Save the script in your R script folder.
# Six Sigma has two main methodologies: DMAIC and DMADV.
# DMAIC = Define, Measure, Analyze, Improve, Control.
# Typically used for improving existing processes.
# DMADV = Define, Measure, Analyze, Design, Verify.
# Typically used for creating new things, but may also be 
# used to improve existing processes if making major changes.
# Forecasting may be used in DMAIC's and DMADV's Analyze phase 
# to understand which model is best to predict future outcomes
# from past data, and to understand whether to account for trend
# and seasonality. 
# 
# Forecasting's most important uses are in 
# in DMAIC's Improve and DMADV's Verify phases. Since our
# improvements/designs are targeted at future performance,
# their future impacts should be assessed. Forecasts are 
# necessary to establish the values of future-state inputs
# (such as queue arrivals).
# Challenge 1: Install the forecast package and reference it as
# a library in this script.
# Documentation: https://cran.r-project.org/web/packages/forecast/forecast.pdf.
library(forecast)
library(ggplot2)
library(openxlsx)

# Paper: https://www.jstatsoft.org/article/view/v027i03. Download and open
# the pdf. The R example are in section 4, starting on page 12.
# MEASURE - Read in data and look for trend, seasonality and
# segmentation.
# Challenge 2: Read "OR Cases.csv" into a data.frame.
orcasescsvdata<-read.csv("oRCases.csv", header=TRUE, fileEncoding = "UTF-8-BOM")

# Challenge 3: Keep only the first 2 columns of the
# data.frame. Eliminate all rows of the data.frame
# where the Date is blank (""). You should now have
# a table with no blank rows or columns. 
csvdata<-orcasescsvdata[,c(1,2)]

# Challenge 4: Convert the Date column to R dates.
csvdata$Date = as.Date(csvdata$Date, "%d-%b-%y") #print as R dates

# Challenge 5: (1) Plot a trend chart of OR cases over time.
# (b) Make sure the x and y axes nave nice titles.
# (c) Make sure the y axis has nice, round number endpoints.
# (d) Add a title.
# Q: Do you see a trend? Seasonality? Do you see stratification,
# where the samples seem to be taken from two or more different 
# populations?
plot(csvdata$Number.of.cases,
     type="l",
     main="OR Cases by Day",
     xlab="Day",
     ylab="Number of OR Cases",
     col="navy",
     xlim=c(0,400),
     ylim=c(0,40))

# Challenge 6: Supplement your data.frame with four 
# new columns for day of week, week of year, month, 
# weekday vs. weekend.
csvdata$Dayofweek=format(as.Date(csvdata$Date),"%a")
csvdata$Weekofyear=format(as.Date(csvdata$Date),"%V")
csvdata$Month=format(as.Date(csvdata$Date),"%b")
csvdata$Weekday="Weekday"

# Challenge 7: Extract only weekdays. Plot a trendchart of the data.
# Q: Do you see a trend? Seasonality? Do you see stratification,
# where the samples seem to be taken from two or more different 
# populations?
csvdata[is.element(csvdata$Dayofweek, c("Sat", "Sun")),"Weekday"] = "Weekend"
weekdayORCases = csvdata[csvdata["Weekday"] == "Weekday",]
plot(weekdayORCases$Number.of.cases,
     type="l",
     main="OR Cases by Day",
     xlab="Day",
     ylab="Number of OR Cases",
     col="navy",
     xlim=c(0,400),
     ylim=c(0,40))

# Challenge 8: Extract only weekends Plot a trendchart of the data.
# Q: Do you see a trend? Seasonality? Do you see stratification,
# where the samples seem to be taken from two or more different 
# populations?

weekendORCases = csvdata[csvdata["Weekday"] == "Weekend",]
plot(weekendORCases$Number.of.cases,
     type="l",
     main="OR Cases by Day in 2014",
     xlab="Day",
     ylab="Number of OR Cases",
     col="navy",
     xlim=c(0,400),
     ylim=c(0,40))

# ANALYZE -- Test for trend, seasonality and segmentation. 
# Find the best forecasting approach.
# Challenge 9: Perform an ANOVA of the number of OR cases to see if 
# the variation in the number of cases is driven by year, month, week,
# weekend, and day of week. Print the summary table.
# Tutorials: https://www.statmethods.net/stats/anova.html,
# https://www.rdocumentation.org/packages/stats/versions/3.5.2/topics/formula
# Q: Do the results make the case for daily, weekly, monthly, 
# or yearly seasonality? Do they make the case for weekday vs. weekend
# stratification?
analysis<-aov(Number.of.cases~Dayofweek+Weekofyear+Month+Weekday,data = csvdata)
summary(analysis)

# Challenge 10: Perform a multiple regression of the number of OR cases
# as a function of Date, Week, Weekday and Day. Print the summary.
# Q: Is Date's coefficient consistent with an increasing trend?
# Tutorial: https://www.statmethods.net/stats/regression.html
analysis<-lm(Number.of.cases~Dayofweek+Weekofyear+Month+Weekday,data = csvdata)
summary(analysis)

# Challenge 11: Forecast number of cases per day using exponential
# smoothing (exponential time series). Use the entire dataset. 
# Let the algorithm decide whether to use an additive vs. multiplicative 
# model and whether to include trend and seasonality factors.
# Print a summary of the model's results.
# Documentation: https://www.rdocumentation.org/packages/forecast/versions/8.5/topics/ets
# Q: What model does the algorithm select? What are the model's 
# parameters? What is its root mean square error?
etsforecast<-ets(csvdata$Number.of.cases)
summary(etsforecast)

# Let's try to segment the forecasts by weekday vs. weekend to see
# if we get better results.
# Challenge 12: Repeat your forecast above, but only for weekdays. 
# Q: What model does the algorithm select? What are the model's 
# parameters? What is its root mean square error? Is this RMSE better
# than the RMSE using the entire data set?
etsforecast<-ets(weekdayORCases$Number.of.cases)
summary(etsforecast)

# Challenge 13: Repeat your forecast above, but only for weekends. 
# Q: What model does the algorithm select? What are the model's 
# parameters? What is its root mean square error? Is this RMSE better
# than the RMSE using the entire data set?
etsforecast<-ets(weekendORCases$Number.of.cases)
summary(etsforecast)

# Q: Based on your ANOVA, what other forecast segmentations might
# you want to try?

# IMPROVE/VERIFY
# Challenge 14: Forecast the next 30 days. Use a 95% confidence
# level for the prediction intervals. Plot the forecast.
# Q: Does the forecast look reasonable? Is the prediction interval 
# wide or narrow?
etsForecasthirtyDay<-ets(csvdata$Number.of.cases,y=30, confint(csvdata$Number.of.cases,level = 0.95))
plot(etsforecast)

# Challenge 15: Forecast the next 30 weekdays. Use a 95% confidence
# level for the prediction intervals. Plot the forecast.
# Q: Does the forecast look reasonable? How does the prediction
# interval compare to the prior forecast of all days?
etsForecasthirtyDay<-ets(weekdayORCases$Number.of.cases,y=30, confint(weekdayORCases$Number.of.cases,level = 0.95))
plot(etsforecast)

# Challenge 16: Forecast the next 30 weekend days. Use a 95% confidence
# level for the prediction intervals. Plot the forecast.
# Q: Does the forecast look reasonable? How does the prediction
# interval compare to the prior forecast of all days?
etsForecasthirtyDay<-ets(weekendORCases$Number.of.cases,y=30, confint(weekendORCases$Number.of.cases,level = 0.95))
plot(etsforecast)