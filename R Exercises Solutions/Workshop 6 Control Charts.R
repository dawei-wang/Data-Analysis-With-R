#Dawei Wang
# March 6, 2019
# Workshop 6: Control Charts

# Objectives: Create an xMR (I-MR) control chart. Distinguish common 
# from assignable (special) causes.

# Several challenges have questions. You do not need to answer these
# questions in your scripts. They are there to help you understand 
# what your analyses are telling you.

# Challenge 0: Create a new script and add a multi-line comment
# at the top with the name of the workshop, your name, and the date.
# Copy the remaining portion of this script and paste it into your
# script. Save the script in your R script folder.

# Control charts may be used in DMAIC's and DMADV's Analyze phase 
# to determine whether a process is in control. They may be used
# in DMAIC's Control phase to determine whether a process remains
# in control.

# Challenge 1: Install the qcc, car and nortest packages and reference 
# them as libraries in this script.
# Documentation: https://cran.r-project.org/web/packages/qcc/qcc.pdf,
# https://cran.r-project.org/web/packages/car/car.pdf,
# https://cran.r-project.org/web/packages/nortest/nortest.pdf.
library(car)
library(qcc)
library(nortest)

# MEASURE: The x control chart has three key assumptions: 
# 1. Independence of data points. Test by looking for non-random
# patterns in a trend chart.
# 2. Constant variance (homoscedasticity). The MR chart will 
# catch this.
# 3. Normality of the control chart parameter, x. Test by looking
# at a historgram and q-q chart, and by using the Andersen-Darling
# test of non-normality. (Note, however, that even if normality
# is violated, control charts are typically robust to non-normality.
# However, we would prefer normality and should try some
# some transformations if the data is non-normal.)

# Challenge 2: Read "OR Cases.csv" into a data.frame.
orcasescsvdata<-read.csv("oRCases.csv", header=TRUE, fileEncoding = "UTF-8-BOM")

# Challenge 3: (a) Keep only the first 2 columns of the
# data.frame. (b) Eliminate all rows of the data.frame
# where the Date is blank (""). (c) Convert the Date column to R dates.
orCases=orcasescsvdata[1:2]
orCases = orCases[orCases["Date"] != "",]
orCases$Date = as.Date(orCases$Date, "%d-%b-%y")

# In prior workshops, we learned that weekdays and weekend days
# were different. Let's only look at the weekdays for now.

# Challenge 4: Create a subset of the data.frame with only
# the weekdays.
orCases$Day = format(as.Date(orCases$Date), "%a")
orCases$Week = format(as.Date(orCases$Date), "%V")
orCases$Month = format(as.Date(orCases$Date), "%b")
orCases$Weekday = "Weekday"
orCases[is.element(orCases$Day, c("Sat", "Sun")),"Weekday"] = "Weekend"
weekdayORCases = orCases[orCases["Weekday"] == "Weekday",]

# Challenge 5: Plot the number of or cases by weekday.
plot(weekdayORCases$Number.of.cases,
     type="l",
     main="OR Cases by Day: Weekdays",
     xlab="Day",
     ylab="Number of OR Cases",
     col="navy",
     xlim=c(0,400),
     ylim=c(0,40))

# Challenge 6: Check to see if the weekday data is normally distributed
# using a histogram.
# Q: Does the data look normal?
hist(weekdayORCases$Number.of.cases,
     main="Distribution of OR Cases:  Weekdays",
     xlab="Number of OR Cases",
     ylab="Frequency")

# Challenge 7: Check to see if the weekday data is normally distributed
# using a q-q plot.
# Hint: Look in the car package's documentation.
# Suggestion: Use the envelope parameter.
# Q: Does the data look normal?
normanalysis<-qqPlot(weekdayORCases$Number.of.cases, envelope = 0.95)

# Challenge 8: Test the weekday data for NON-normality using 
# the Anderson-Darling Test. H0: Normal, H1: Not normal.
# If p-value < alpha then reject H0 and conclude x is non-normal.
# Hint: Look at the nortest package's documentation.
# Q: Do you reject or fail to reject H0? What does that say about
# the normality assumption?
ad.test(weekdayORCases$Number.of.cases)


# ANALYZE: Determine if the process is in control.
# Identify any special cause variations and their causes.

# Challenge 9: Plot the x chart of weekday OR cases.
# Example: https://gist.github.com/tomhopper/9000495
# Note: We might be tempted to use a c chart since we are dealing
# with counts. However, number of OR patients is not measuring
# attribute data per se, and the number of patients is not count of
# "pass" vs. "fail" of inspections for defects. An x chart is more
# appropriate.
# Q: Do you see any special cause variations? Is the process in control?

#' Create the individuals chart and qcc object
my.xmr.x <- qcc(weekdayORCases$Number.of.cases, type = "xbar.one",xlim=c(0,400),
                ylim=c(0,40), plot = TRUE)



# Challenge 10: Create a data.frame with the two columns of weekday data
# needed for the moving range chart.
# Example: https://gist.github.com/tomhopper/9000495

#' Create the moving range chart and qcc object. qcc takes a two-column matrix
#' that is used to calculate the moving range.
my.xmr.raw.r <- matrix(cbind(weekdayORCases$Number.of.cases[1:length(weekdayORCases$Number.of.cases)-1], weekdayORCases$Number.of.cases[2:length(weekdayORCases$Number.of.cases)]), ncol=2)


# Challenge 11: Plot the MR chart of the weekday data.
# Hint: Look at the qcc package's documentation. 
# Example: https://gist.github.com/tomhopper/9000495.
# Q: Do you see any special cause variations? Is the process in control?
# Is the x-chart's assumption of homoscedasticity satisfied?
my.xmr.mr <- qcc(my.xmr.raw.r, type="R",xlim=c(0,400),
                 ylim=c(0,40), plot = TRUE)

# CONTROL: See if an in-control process remains in control. 
# If it goes out-of-control, investigate and take corrective action.

# Assume you created the control chart after collecting the first 200
# days of data in the data set. You then kept the control limits fixed
# and, each day thereafter, added that day's point to the control chart.
# You are now on day 260 (the last day in the data set).

# Challenge 12: Plot the x chart of weekday OR cases using days 1-200
# as the control chart's data and days 201-260 as the control chart's
# new data.
# Q: Do you see any special cause variations? Is the process in control?
my.xmr.x <- qcc(weekdayORCases$Number.of.cases[1:200], type = "xbar.one",xlim=c(0,400),
                ylim=c(0,40), plot = TRUE)
my.xmr.x <- qcc(weekdayORCases$Number.of.cases[200:259], type = "xbar.one",xlim=c(0,400),
                ylim=c(0,40), plot = TRUE)
# Challenge 13: Plot the MR cha'rt of weekday OR cases using days 1-200
# as the control chart's data and days 201-260 as the control chart's
# new data.''''''''''''''''''''
# Note: Since you created a new data.frame with the moving range's data,
# you'll need to use rows 1-199 for the data and rows 200-259 for 
# the new data.
# Q: Do you see any special cause variations? Is the process in control?
# Is the x-chart's assumption of homoscedasticity satisfied?
my.xmr.raw.r <- matrix(cbind(weekdayORCases$Number.of.cases[1:length(weekdayORCases$Number.of.cases[1:200])-1], weekdayORCases$Number.of.cases[2:length(weekdayORCases$Number.of.cases[1:200])]), ncol=2)
my.xmr.mr <- qcc(my.xmr.raw.r, type="R",xlim=c(0,400),
                 ylim=c(0,40), plot = TRUE)
my.xmr.raw.r <- matrix(cbind(weekdayORCases$Number.of.cases[1:length(weekdayORCases$Number.of.cases[200:259])-1], weekdayORCases$Number.of.cases[2:length(weekdayORCases$Number.of.cases[200:259])]), ncol=2)
my.xmr.mr <- qcc(my.xmr.raw.r, type="R",xlim=c(0,400),
                 ylim=c(0,40), plot = TRUE)

# When you're done, copy this script, paste it into Canvas's
# text box, and submit it for credit. You must submit by
# the end of class; no late submissions will be accepted.