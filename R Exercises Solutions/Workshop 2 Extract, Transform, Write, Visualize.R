# Dawei Wang
# Workshop 2: R data extraction, transformation and visualization
# February 6, 2019

# Challenge 2
library(ggplot2)
library(openxlsx)


# library(NAME) loads packages

# Challenge 3
orcasestxtdata<-read.delim("oRCases.txt")
#read.delim implies delimiter
# read table needs to specify
head(orcasestxtdata,10)
# head prints first N rows
# tail printvs last N rows
tail(orcasestxtdata,10)
header=TRUE
fill=TRUE

# Challenge 4
orcasesxlsxdata<-read.xlsx("oRCases.xlsx", sheet=1)
head(orcasesxlsxdata,10)
tail(orcasesxlsxdata,10)

#Challenge 5
orcasescsvdata<-read.csv("oRCases.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
head(orcasescsvdata,10)
tail(orcasescsvdata,10)

#Challenge 6
orcasescsvdata[,c(1,2)]

#Challenge 7
csvdata<-orcasescsvdata[,c(1,2)]
# assign first 2 columns to new data frame 
head(csvdata,10)
tail(csvdata,10)

# Challenge 8
dat<-csvdata$ï..Date
print(dat)
# $ calls headers


# Challenge 9: Determine whether each date in the Date column
# is blank (""). The result should be True if the date is blank
# and False if the date is not blank.
# Hint: https://stackoverflow.com/questions/1686569/filter-data-frame-rows-by-a-logical-condition
print(is.na(dat))
# display blank columns

print(is.finite(dat))
# display filled columns

# Challenge 10: Eliminate all rows of the data.frame
# where the Date is blank (""). You should now have
# a table with no blank rows or columns. Print the
# first and last 10 rows.
# Hint: Use the results of the prior challenge.
# 
na.omit(csvdata)

# Challenge 11: Print the list (not the column) of dates extracted 
# data.frame (using "$Date").
print(csvdata$Date) #print the dates as list
dates2<-csvdata$Date


# Challenge 12: The Date column is composed of strings, not dates
# that R will understand. Convert these to R dates. Hint: You can
# assign the string dates to the R dates in one step using the list
# of dates from the prior challenge. 
# See https://www.statmethods.net/input/dates.html.
# Print the first and last 10 rows.
as.Date(dates2,format="%B %d %Y") # converts to R dates
head(dates2,10)
tail(dates2,10)

# Challenge 13: Write the data.frame as a CSV file named "OR Cases 2.csv".
# Do NOT include row names.
# Suggestion: Open the CSV file and make sure it's what you expect.
write.csv(csvdata, file = "OR Cases 2.csv", row.names = FALSE)

# Challenge 14: Write the data.frame as a TSV file named "OR Cases 2.txt".
# Do NOT include row names.
write.table(csvdata, file='OR Cases 2.txt', quote=FALSE, sep='\t', col.names = FALSE, row.names = FALSE)

# Challenge 15: Write the data.frame as an xlsx file named "OR Cases 2.xlsx".
# Do NOT include row names.

#write.xlsx(csvdata, file ='OR Cases 2.xlsx',sheet=1, row.names(NA)) ?

# Challenge 16: (a) Plot a histogram of OR cases by day.
# (b) Make sure the x- and y-axis scales have nice, round numbers.
# (c) Make sure the bins have nice, round number breakpoints. 
# Tutorials:
# https://www.datamentor.io/r-programming/histogram/
# https://www.r-bloggers.com/basics-of-histograms/
orcasesbyday<-csvdata$Number.of.cases
hist<-hist(orcasesbyday)

# Challenge 17: Plot a histogram of OR cases by day, 
# this time as a probability density. Make sure
# the x- and y-axis scales have nice, round numbers,
# and make sure the bins have nice, round number
# breakpoints.
hist1<-hist(orcasesbyday, freq = FALSE)

# Challenge 18: (a) Plot OR cases by day as a trend chart. 
# (b) Provide a title and label the x and y axes. 
# The y-axis label should include the metric's units. 
# (d) Make sure the x- and y-axis scales end at nice, round numbers.
plot(csvdata$Date,csvdata$Number.of.cases, type="l", 
                        main="OR Case per Day",
                        xlab="Day", ylab="# of cases")

# Challenge 19: Supplement your data.frame with three 
# new columns: day of week, week of year, month.
# Hint: https://stackoverflow.com/questions/26043627/r-extract-day-from-datetime
# Print the first 10 rows.
#format(as.Date(csvdata$date,format="%Y-%m-%d"), "%d") ?

# Challenge 20: Sum the OR cases by week of year. Print the first 10 rows.
# Hint: https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group.
#aggregate(csvdata$Frequency, by=list(Category=csvdata$date), FUN=sum) ?

# Challenge 21: Plot OR cases by week of year.

# When you're done, copy your script, paste it into Canvas's
# text box, and submit it for credit. You must submit by
# the end of class; no late submissions will be accepted.