# Dawei Wang
# March 13, 2019
# Workshop 7: Queueing
# Objectives: Pick an appropriate queueing model, analyze
# the queue's performance and understand the impacts of queue
# input and configuration changes.
# Budget your time:
# MEASURE Phase (Challenges 1-15): 1.5 hours
# ANALYZE Phase (Challenges 16-18): 0.75 hours
# IMPROVE Phase (Challenges 19-22): 0.5 hours
# 
# EMPHASIZE Challenges 3, 6, 8-16, and 19. Be sure to attempt these. 
# Don't get bogged down with the other challenges.
# Several challenges have questions. You do not need to answer these
# questions in your scripts. They are there to help you understand 
# what your analyses are telling you.
# Challenge 0: Create a new script and add a multi-line comment
# at the top with the name of the workshop, your name, and the date.
# Copy the remaining portion of this script and paste it into your
# script. Save the script in your R script folder.
# Queueing analysis may be used in DMAIC's and DMADV's Analyze phase 
# to understand current process performance. It may be used
# in DMAIC's Improve and DMADV's Verify phases to assess the impacts 
# of changes in queueing inputs on process performance.
# Challenge 1: Install the queueing and vcd (visualizing
# categorical data) packagesand reference them as libraries in this script.
# Documentation: https://cran.r-project.org/web/packages/qcc/qcc.pdf,
# https://cran.r-project.org/web/packages/vcd/vcd.pdf.
library(vcd)
library(queueing)

# MEASURE: Ideally we would like to use an M/M/1 or M/M/c
# model. Queueing has three critical input parameters: 
# arrival rate (lambda), service rate (mu) and number of servers (c).
#
# In our OR Cases example:
# lambda = average patients scheduled per day scheduled
# mu = average patients per day processed by a BUSY operating room
# c = number of operating rooms (not number of doctors since
# multiple doctors may use the same operating room)
#
# Let's look at the data.
# Challenge 2: (a) Read "OR Cases.csv" into a data.frame.
# (b) Keep only the first 2 columns of the data.frame and 
# eliminate all rows of the data.frame where the Date is blank (""). 
# (c) Convert the Date column to R dates.
# (d) Add a week of year (1, 2, 3, ..., 52) column.
orcasescsvdata<-read.csv("oRCases.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
orCases=orcasescsvdata[1:2]
orCases = orCases[orCases["Date"] != "",]
orCases$Date = as.Date(orCases$Date, "%d-%b-%y")
orCases$Week = format(as.Date(orCases$Date), "%V")

# Challenge 3: From prior workshops, we know that weekdays
# and weekend days have different average numbers of patients
# per day. They try to schedule patients during the week, but
# if there is insufficient capacity, they schedule patients
# for the weekend. Thus, it's better to look at cases per
# week than cases per day.
# 
# Sum the OR cases by week of year. Change the column
# names to "Week" and "Number.of.cases". Print the first 10 rows.
# Be sure to assign these to variables for use later.
# Hints: https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group,
# https://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame.
orCasesByWeek = aggregate(orCases$Number.of.cases,
                          by=list(Week=orCases$Week),
                          FUN=sum)
head(orCasesByWeek, n=10)

# Challenge 4: We don't know how many patients per week are
# scheduled for surgery in the future, only how many surgeries
# actually occurred. We will use the number of surgeries that
# actually occurred as a proxy for the the number scheduled
# (i.e., the arrival rate, lambda).
# (This assumes the waiting time for a surgery stays constant.)
#
# Calculate the mean arrival rate, lambda.
# Q: What are the units of lambda?
lambda<-mean(orCasesByWeek$x)
# Challenge 5: We would prefer that the arrivals be Markovian
# so we can use M/M/c queueing models. Arrivals are Markovian
# if the time between arrivals is exponentially distributed or,
# equivalently, the number of arrivals by day is Poisson
# distributed.
#
# Create a histogram of the number of cases by week.
# Q: Does this look Poisson?
hist(orCasesByWeek$x)

# Challenge 6: Determine whether the number of cases per week 
# is Poisson-distributed.
# (a) Perform a goodness of fit test with maximum likelihood as
# the goodness of fit criterion. 
# (b) Create a summary so you can do a hypothesis test:
# H0: Data is Poisson-distributed. H1: Data is not Poisson-distributed.
# (c) Plot the results.
# Documentation: https://www.rdocumentation.org/packages/vcd/versions/1.4-4/topics/goodfit
# Q: Is the data Poisson-distributed? Can we conclude that
# our arrival rates are Markovian?
analysis<-goodfit(orCasesByWeek$x)
summary(orCasesByWeek$x)
plot(analysis)


# Challenge 7: The hospital has 10 operating rooms.
# Set the value of c.
c=10

# Challenge 8: We don't have processing times. We know that
# the service rate, mu, reflects the rate of a "busy" server.
# We can approximate mu by picking the busiest week to determine
# how many surgeries per room per week were performed.
# (a) Determine the maximum number of cases in a week.
# (b) Determine mu = the maximum number of cases per room in a week.
maxcasesperweek<-max(orCasesByWeek$x)
print(maxcasesperweek)
mu<-maxcasesperweek/c
print(mu)
# Challenge 9: Suppose the average wait, Wq, for surgery is 4.5 weeks.
# Calculate the average number of patients in the queue, Lq.
# Hint: https://en.wikipedia.org/wiki/Little%27s_law
Lq<-mu*4.5
print(Lq)
# Challenge 10: Calculate the mean number of patients in service, rho.
# Hint: rho = lambda / mu.
rho<-lambda/mu
print(rho)

# Challenge 11: Calculate the mean number of patients in the system, Ls.
# Hint: Ls = mean number of patients in queue + mean number of patients
# in service.
Ls<-Lq+rho
print(Ls)

# Challenge 12: Calculate the expected time in weeks a patient spends in
# service (i.e., in the operating room).
# Hint: mu is in patients per week, and we want weeks per patient.
# Q: If the operating rooms are open 12 hours per day during weekdays
# and 8 hours, 7 days a week, how many hours does a patient spend 
# in service
expectedTime<-1/mu
print(expectedTime)

# Challenge 13: Operating rooms are open 12 hours per day during
# weekdays and 8 hours per day on weekend days. (a) Calculate
# the number of working hours in a week. (b) Calculate the hours
# a patient spends in service in a week.
hoursPerWeek<-(12*5)+(2*8)
print(hoursPerWeek)
hoursInService<-maxcasesperweek/hoursPerWeek
print(hoursInService)

# Challenge 14: Calculate the expected time in weeks a patient spends in 
# the system, Ws.
# Hint: Ws = time in queue + time in service.
Ws<-hoursInService+4.5
print(Ws)
# Challenge 15: Calculate OR room utilization.
# Hint: Utilization = arrival rate / (service rate * number of servers)
# Q: Does this seem like a reasonable utilization rate?
utilization<-lambda/(expectedTime*c)
print(utilization)
# ANALYZE: None of the above calculations relied made any assumptions
# about the distributions of arrivals and processing times.
# If we have a G/G/c queue, we can use simulation to do a further
# analysis. This is out-of-scope for this workshop.
#
# Suppose in the Measure phase we found that the assumptions of 
# an M/M/c queue were satisfied.
# Challenge 16: Create an M/M/c queueing model and provide
# a summary and a report of the findings.
# Hint: Follow the procedure on page 9 of the queueing package's
# documentation. Set n=10 since we will want to check to see
# the probability all rooms are busy.
NewInput.MMC(lambda, mu,c=10)
OrCasesMMC<-QueueingModel(OrCasesMMC)
# Challenge 17: From the model, retrieve Wq, the mean time (weeks) 
# patients spend in the queue. (Call it WqModel so as not
# to confuse it with the Wq calculated above.)
# Note: The Wq calculated in the Measure phase is much longer.
# WqModel is only the wait due to queueing. The rest of the actual
# time spent is due to non-queueing things like batching (i.e., 
# scheduling by week), doctor availability, patient availabity,
# patient preference, waiting for test results, etc.
# Q: What proportion of total wait time is due to queuing?
WqModel<-Wq(OrCasesMMC)


# Challenge 18: From the model, retrieve Pn, the probabilities 
# the OR has n=0,...,10 patients in it.
# Q: What is the probability the OR is idle? Full?
PnModel<-Pn(OrCasesMMC)


# IMPROVE/VERIFY: Let's estimate the impacts of changes on
# the queueing system. To reduce wait times, we can increase
# c, increase mu and decrease lambda.
# Challenge 19: Management is considering increasing capacity
# by 10% by using the rooms more hours per week. 
# (a) Create a new M/M/c model. (b) Calculating its new
# queueing time, newWqModel. (c) Determine how many weeks of
# waiting is saved vs. WqModel.
# Note: Mathematically, adding another room would have the same
# impact.
# Q: Does this have a big impact on patient wait time?
mu2<-mu+0.10*mu
lambda2<-lambda-0.10*lambda
newQueue<-NewInput.MMC(lambda2, mu2,c)
newQueueingModel<-QueueingModel(newQueue)
newWqModel<-Wq(newQueueingModel)
print(newWqModel)
wqDifference<-newWqModel-WqModel
print(wqDifference) #weeks saved 
# Challenge 20: The service time includes actual room setup 
# (15 minutes), patient entry (15 minutes), operating time, 
# patient exit (15 minutes), and cleaning (45 minutes).
# In other words, 1.5 hours of service time is spent on
# non-operating activities. Management want to know what
# the benefit would be of reducing this time to 1 hour.
# (a) Using the working time in challenge 13, calculate the working
# weeks saved per operation.
# (b) Calculate the new service time per operation.
# (c) Calculate the new service rate, newMu. (See challenge 12.)
# (d) Create a new M/M/c model based on the original M/M/c model
# but with a new service rate. 
# (e) Calculating its new queueing time, newWqModel. 
# (f) Determine how many weeks of waiting is saved vs. WqModel.
# Q: Does this have a big impact on patient wait time?
print(hoursPerWeek) # working time
newServiceTime=hoursInService-1.5
print(newServiceTime)
newMu<-

# Challenge 21: Management has determined that some surgeries
# can be handled by an outside clinic. This will reduce the arrival
# rate by 10%. 
# (a) Create a new M/M/c model based on the original M/M/c model
# but with a new arrival rate. 
# (b) Calculating its new queueing time, newWqModel. 
# (c) Determine how many weeks of waiting is saved vs. WqModel.
# Q: Does this have a big impact on patient wait time?
  newLambda=lambda-0.10*lambda
newQueue<-NewInput.MMC(newLambda, mu,c)
newQueueingModel<-QueueingModel(newQueue)
newWqModel<-Wq(newQueueingModel)
wqDifference<-newWqModel-WqModel

# Challenge 22: Management wants each room to have a specialization.
# Thus, each room will handle certain types of surgeries. Suppose
# the rooms will have no overlaps in specializations, resulting in
# c M/M/1 queues instead of 1 M/M/c queue. Assume arrival rates
# will be evenly split across the rooms. Also, assume specialization
# will have the benefit of increasing service rates by 20%.
# (a) Create a new M/M/1 model based with these assumptions. 
# (b) Calculating its new queueing time, newWqModel. 
# (c) Determine how many weeks of waiting is saved vs. WqModel.
# Q: Does this have a big impact on patient wait time?
newMu=mu+0.20*mu
newQueue<-NewInput.MM1(lambda,newMu,c)
newQueueingModel<-QueueingModel(newQueue)
newWqModel<-Wq(newQueueingModel)
wqDifference<-newWqModel-WqModel

# When you're done, copy this script, paste it into Canvas's
# text box, and submit it for credit. You must submit by
# the end of class; no late submissions will be accepted.