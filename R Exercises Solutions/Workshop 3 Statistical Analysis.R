# Dawei Wang
# February 13, 2019
# Workshop 3: Statistical Analysis

library(ggplot2)
library(openxlsx)

# Objectives: Calculate descriptive statistics, perform graphical analyses
# and conduct statistical hypothesis tests.

# Several challenges have questions. You do not need to answer these
# questions in your scripts. They are there to help you understand 
# what your analyses are telling you.

# Budget your time:
# 30 minutes, read data & descriptive statistics (Challenges 1-6)
# 45 minutes, graphical analysis (Challenges 7-19)
# 45 minutes, ANOVA (Challenges 20-21)
# 45 minutes, Multiple Regression (Challenges 22-25)
#
# If you're running behind, skip ahead. If you don't finish all
# the challenges, that's OK. Just make a good effort and get as 
# far as you can.

# Challenge 0: Create a new script and add a multi-line comment
# at the top with the name of the workshop, your name, and the date. 
# Save the script in your R script folder.

# MEASURE. In the Measure phase, we generate descriptive statistics 
# and perform graphical analyses to understand our data better.

# Challenge 1: Read "OR Cases.csv" into a data.frame.
orcasescsvdata<-read.csv("oRCases.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
# "UTF-8-BOM"removes i... when referencing data frame column

# Challenge 2: Keep only the first 2 columns of the
# data.frame. The remaining columns have only NA values.

csvdata<-orcasescsvdata[,c(1,2)]
# assign first 2 columns to new data frame 

# Challenge 3: Eliminate all rows of the data.frame
# where the Date is blank (""). You should now have
# a table with no blank rows or columns.

csvdata = orcasescsvdata[orcasescsvdata["Date"] != "",]

# Challenge 4: Convert the Date column to R dates.
print(csvdata["Date"] != "")
print(csvdata$Date) #print the dates as list
csvdata$Date = as.Date(csvdata$Date, "%d-%b-%y") #print as R dates


# Challenge 5: For number of cases, calculate the summary statistics 
# of location: mean, median, min, max, 1st quartile, 3rd quartile.
# Q: Is the distribution symmetric around the mean?
# Tutorial: https://www.statmethods.net/stats/descriptives.html.
summary(csvdata$Number.of.cases)


# Challenge 6: For number of cases, calculate the summary statistics 
# of spread: standard deviation, variance, range.
# Q: Is the spread wide or narrow?
# WARNING: Make sure the range = max - min.

sapply(csvdata$Number.of.cases, range, na.rm=TRUE)
sapply(csvdata$Number.of.cases, sd, na.rm=TRUE)
sapply(csvdata$Number.of.cases, var, na.rm=TRUE)
#sapply(data.frame$column, statistic, na.rm=TRUE omits NA values)

# Challenge 7: (1) Plot a trend chart of OR cases over time.
# (b) Make sure the x and y axes nave nice titles.
# (c) Make sure the y axis has nice, round number endpoints.
# (d) Add a title.
# Q: Do you see a trend? Seasonality? Do you see stratification,
# where the samples seem to be taken from two or more different populations?
plot(csvdata$Number.of.cases,
     type="l",
     main="OR Cases by Day",
     xlab="Day",
     ylab="Number of OR Cases",
     col="navy",
     xlim=c(0,400),
     ylim=c(0,40))


# Challenge 8: (a) Plot a histogram of OR cases by day.
# (b) Make sure the x- and y-axis scales have nice, round numbers.
# (c) Make sure the bins have nice, round number breakpoints.
# Q: Is the distribution symmetric around the mean? Is the spread
# wide or narrow? Do you see stratification, where the samples seem 
# to be taken from two or more different populations?

orcasesbyday<-csvdata$Number.of.cases
hist<-hist(orcasesbyday) # hist(VARIABLENAME) plot histogram

# Challenge 9: Plot a histogram of OR cases by day, 
# this time as a probability density. Make sure
# the x- and y-axis scales have nice, round numbers,
# and make sure the bins have nice, round number
# breakpoints.

hist1<-hist(orcasesbyday, freq = FALSE)

# Challenge 10: Supplement your data.frame with three 
# new columns: day of week, week of year, month.
csvdata$Dayofweek=format(as.Date(csvdata$Date),"%a")
csvdata$Weekofyear=format(as.Date(csvdata$Date),"%V")
csvdata$Month=format(as.Date(csvdata$Date),"%b")



# Challenge 11: Add a new column, "Weekday", to the data.frame.
# Initialize its value to "Weekday", even it the day falls on
# a weekend. Print the first 10 lines.
csvdata$Weekday="Weekday"
head(csvdata$Weekday,10)


# Challenge 12: Create a test of whether a day falls on a weekend.
# Hint: https://stackoverflow.com/questions/11612235/select-rows-from-a-data-frame-based-on-values-in-a-vector
# Hint: Use the is.element function.
# Tutorial: Learn how to use R's built-in help: https://www.r-project.org/help.html.
# In RStudio, if you type "?.is.element" (without the quotes) into the Console,
# you will see the help in the Help pane. (If you don't see the Help pane,
# go to the View menu and select Panes/Show All Panes.)

is.element(csvdata$Dayofweek,c("Sat","Sun"))


# Challenge 13: Use the test from the prior challenge to set the values in
# the Weekday column to Weekend if the day falls on Sat or Sun.
# Print the first 10 rows to make sure you're getting the correct results.
csvdata[is.element(csvdata$Dayofweek,c("Sat","Sun")),"Weekday"]="Weekend"

# NOTE: If you are running short of time, skip to Challenge 20.

# Challenge 14: Get the subset of data that falls on a week day.
# Print the first 10 rows.
# Hint: https://stackoverflow.com/questions/7381455/filtering-a-data-frame-by-values-in-a-column


# Challenge 15: Create a histogram of the number of week day OR cases.
# Q: What distribution does the data seem to follow?


# Challenge 16: Create a histogram of the natural log of the number 
# of week day OR cases.
# Q: Does the tranformed data look more "normal" than in the prior challenge?


# Challenge 17: Get the subset of data that falls on a weekend.
# Hint: https://stackoverflow.com/questions/7381455/filtering-a-data-frame-by-values-in-a-column


# Challenge 18: Create a histogram of the number of weekend OR cases.
# Q: What distribution does the data seem to follow? Is the data
# stratified?


# Challenge 19: Create a histogram of the natural log of the number 
# of weekend OR cases.
# Q: What distribution does the data seem to follow? Is the data
# stratified?


# ANALYZE: Perform analyses to understanding what is causing 
# the behavior of the system.

# Challenge 20: Perform an ANOVA of the number of OR cases to see if 
# the variation in the number of cases is driven by year, month, week,
# weekend, and day of week. Print the summary table.
# Tutorials: https://www.statmethods.net/stats/anova.html,
# https://www.rdocumentation.org/packages/stats/versions/3.5.2/topics/formula
# Q: Which factors are statistically significant? Why do the factors
# have the degrees of freedom that they have? Do the results make the case
# for daily, weekly, monthly, or yearly seasonality?
head(csvdata,10)

# One Way Anova (Completely Randomized Design)
analysis<-aov(Number.of.cases~Dayofweek+Weekofyear+Month,data = csvdata)
anova(analysis)
summary(analysis)



# Challenge 21: Using the ANOVA results, perform TukeyHSD tests
# to see which Months, Weeks, Weekday vs. Weekend, and Days are
# different from the others.
# Plot the results. (This will result in one plot per factor.)

plot(TukeyHSD(aov(analysis)))
anovaplot<-hist(analysis)

# Challenge 22: Perform a multiple regression of the number of OR cases
# as a function of Date, Week, Weekday and Day. Print the
# summary table.
# Q: Is the model statistically different from zero (p-value < 0.05)?
# Is the model is predictive (Adjusted R-squared > 0.95).
# Is Date's coefficient consistent with an increasing trend?
# Note: Even if the model is not predictive, if it is statistically
# different from zero, then it is good for hypothesis testing of
# whether the independent variables (factors) affect the dependent
# variable (response).
# Tutorial: https://www.statmethods.net/stats/regression.html
regression<-lm(Number.of.cases~Dayofweek+Weekofyear+Month ,data = csvdata)
summary(regression)
#lm is for regression

# Challenge 23: Print the regression's ANOVA table. 
# Q: Which, if any, of the independent variables are statistically 
# significant? Is there evidence for a trend? For annual or
# weekly seasonality?
anova(regression)


# Challenge 24: Print the 95% confidence interval table.
confint(regression, level = 0.95)

# Challenge 25: Plot the diagnostics. Regression assumes
# normally distributed residuals, independent data points, 
# and constant variance (homoscedasticity). Do regression's
# assumptions seem to be satisfied? 

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(regression)

# If you have time, go back and complete any challenges you
# skipped. Also answer the questions to ensure you understand
# how to derive insights from the analyses.

# When you're done, copy this script, paste it into Canvas's
# text box, and submit it for credit. You must submit by
# the end of class; no late submissions will be accepted.