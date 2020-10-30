# Dawei Wang
# March, 20, 2019
# Workshop 8: Linear Programming

# Objectives: Build a linear program, solve it, and interpret 
# the findings.

# Let's create a mixed integer-linear program to schedule nurses.
# Nurses work weekly shift schedules ("tours"). For example,
# a nurse may work the tour of Monday, Tuesday and Wednesday.
# We want to determine how many nurses should work each possible
# tour such that we can cover all patients whil minimizing cost.
#
# Our optimization will take the form:
# 
# min cTx s.t. Ax>=b, x>=0, x int
#
# xj = the number of nurses assigned to possible tour j=1,...,n
# cj = the cost per nurse of tour j=1,...,n
# bi = the number of nurses required for day i=1,...,m
# aij = 1 if a nurse works day i on tour j; 0 otherwise

# Linear Programming may used in DMAIC's Improve and DMADV's Design
# phases. However, a lot of the data needed to build a linear
# program comes from DMAIC's and DMADV's Measure phase.

# Several challenges have questions. You do not need to answer these
# questions in your scripts. They are there to help you understand 
# what your analyses are telling you.

# Challenge 0: Create a new script and add a multi-line comment
# at the top with the name of the workshop, your name, and the date.
# Copy the remaining portion of this script and paste it into your
# script. Save the script in your R script folder.

# Challenge 1: Install the lpSolve and lpSolveAPI packages 
# and reference them as libraries in this script. lpSolveAPI is 
# a wrapper for lpSolve that can be easier to use.
# Documentation: 
# https://cran.r-project.org/web/packages/lpSolve/lpSolve.pdf
# http://lpsolve.sourceforge.net/5.5/index.htm
# https://cran.r-project.org/web/packages/lpSolveAPI/lpSolveAPI.pdf
# http://civil.colorado.edu/~balajir/CVEN5393/R-sessions/sess1/lpSolveAPI-vignettes.pdf
library(lpSolve)
library(lpSolveAPI)

# DEFINE: Supposes weeks 1-48 have already been executed or scheduled. 
# We need to schedule nurses for week 49. 
# 
# - The objective is to minimize cost. 
# - Weeks start on Mondays and end on Sundays.
# - Operating rooms are used up to 12 hours per day.
# - Nurses work 12-hour shifts and may work 3 consecutive days 
# in a week.
# - Nurses receive two 30-minute breaks and two 15-minute breaks 
# per shift, so their effective capacity is 10.5 hours per shift.
# - A sufficient number of nurses must work each day to cover all 
# surgery-related activities, including pre-surgery, surgery 
# and post-surgery.

# MEASURE: We need to estimate cost per nurse per tour and 
# nurses required per tour.

# Challenge 2: (a) Read "OR Cases.csv" into a data.frame.
# (b) Keep only the first 2 columns of the data.frame and 
# eliminate all rows of the data.frame where the Date is blank (""). 
# (c) Convert the Dates to R dates.
# (d) Add a day of week (Mon, Tue, Wed, etc.) column.
# (e) Add a week of year (1, 2, 3, ..., 52) column.
orcasescsvdata<-read.csv("oRCases.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
orCases=orcasescsvdata[1:2]
orCases = orCases[orCases["Date"] != "",]
orCases$Date = as.Date(orCases$Date, "%d-%b-%y")
orCases$Week = format(as.Date(orCases$Date), "%V")
orCases$Day=format(as.Date(orCases$Date), "%a")


# Challenge 3: For the objective function, we need the cost
# per nurse per tour, cj. 
# - Nurses earn, on average, base wages of $120,000/year. 
# - Nurses' benefits are equivalent to an additional 40% of their wages.
# - Nurses have, on average, 3 weeks of vacation per year. (Vacation
# cost is counted in base wages, not benefits.)
# - Nurses are scheduled, on average, for 36 hours (one tour) per work week.
# (a) Calculate a nurse's total wages + benefits per year ($/year).
# (b) Calculate the number of tours worked by a nurse in a year (nurse-tours/year).
# (c) Calculate the cost per nurse per tour, cj ($/nurse-tour).
#a)
wage=120000
wageplusbenefits=wage+(0.40*wage)
toursPerYear=49
cost=wage/toursPerYear


# Challenge 4: From our queueing analysis (workshop 7),
# we determined that an OR room requires about 4.6 hours per surgery.
# This time includes room setup (15 minutes), patient entry (15 minutes), 
# operating time, patient exit (15 minutes), and cleaning (45 minutes).
# A nurse is required in the room for everything except cleaning.
# Estimate the number of OR room hours required per nurse per case.
nursehrpersurgery<-4.6-0.75*(4.6)


# Challenge 5: A patient requires 1 hours of pre-operative preparation 
# and 1 hour of post-operative recovery. A nurse can handle three
# patients at a time in pre-op and post-op. For one case, 
# estimate the number of hours of nursing time required outside of surgery.
hoursOutsideSurgery=2/3

# Challenge 6: Use the results of challenges 4 & 5 to estimate 
# the number of nursing hours required per patient, including
# pre-op, surgery and post-op.
grandTotalHoursRequired=(nursehrpersurgery+hoursOutsideSurgery)


# Challenge 7: Nurses have two 30-minute breaks 
# and two 15-minute breaks per 12 hour shift. Determine
# the number of hours in a 12-hour day a nurse actually has to work 
# with patients.
actualHoursWorked=12-1.5 


# Challenge 8: Use the results of challenges 6 and 7 to determine
# the number of nurse-days required per case.
nurseDaysRequired=grandTotalHoursRequired/actualHoursWorked

# Challenge 9: The number of nurses required per day = number of cases
# per day x nurse days per case. 
# (a) Extract the scheduled number of cases by day for Week 47
# from the OR cases data.frame. The result should be a new data.frame.
# (b) Add a new column to the Week 47 data.frame with the number of nurses 
# required by day using the results from (a) and challenge 8.
# (c) Since we cannot have fractional nurses, round the numbers in 
# the new column up to the nearest integer to arrive at nurses required 
# by day, bi.
# Hint: https://www.rdocumentation.org/packages/base/versions/3.5.2/topics/Round
orCases47 = orCases[orCases$Week==47,]
nurserequiredperday=orCases47$Number.of.cases*nurseDaysRequired
round(nurserequiredperday, 0)


# IMPROVE/DESIGN: Let's create an minimum cost schedule that meets
# the required number of nurses.

# SUGGESTION: Wherever possible, use lpSolveAPI's functions:
# https://www.rdocumentation.org/packages/lpSolveAPI/versions/5.5.2.0-17.

# Challenge 10: Make a new, empty LP with 7 rows (for 7 days) and 5 columns
# (for 5 possible tours).
orLp<-make.lp(7,5)


# Challenge 11: Set a column for each tour.
# (a) Set a column c(1, 1, 1, 0, 0, 0, 0) for a tour of Monday, Tuesday, Wednesday.
# (b) Set a column c(0, 1, 1, 1, 0, 0, 0) for a tour of Tuesday, Wednesday, Thursday.
# (c) Set a column for a tour of Wednesday, Thursday, Friday.
# (d) Set a column for a tour of Thursday, Friday, Saturday.
# (e) Set a column for a tour of Friday, Saturday, Sunday.
set.column(orLp,1,c(1, 1, 1, 0, 0, 0, 0))
set.column(orLp,2,c(0, 1, 1, 1, 0, 0, 0))
set.column(orLp,3,c(0, 0, 1, 1, 1, 0, 0))
set.column(orLp,4,c(0, 0, 0, 1, 1, 1, 0))
set.column(orLp,5,c(0, 0, 0, 0, 1, 1, 1))

# Challensge 12: Set the LP's right-hand side to the number of nurses required by day.
set.rhs(orLp,nurserequiredperday)

# Challenge 13: Set the contraint type to >=.
set.constr.type(orLp,rep(">=",5))

# Challenge 14: Set the LP's objective function coefficients for each tour
# to the nurse cost per tour.
set.objfn(orLp,cost)

# Challenge 15: Set the "sense" of the LP to minimize.
# Hint: https://www.rdocumentation.org/packages/lpSolveAPI/versions/5.5.2.0-17/topics/lp.control.options
set.type(orLp, "minimize")

# Challenge 16: Print the LP and make sure it looks OK. For now we won't worry about
# the variables being integers.
orLp

# Challenge 17: Solve the LP.
solve.lpExtPtr(orLp)

# Challenge 18: Get the solution.
# (a) Get the objective function's value. This is the total nurse cost.
# (b) Get the variables' values. These are the numbers of nurses assigned to each tour.
# (c) Get the constraints' values. These are the numbers of nurses scheduled by day.
# Q: How many nurses are assigned to each tour? How many nurses are assigned to each day?
# Are the numbers of nurses assigned to each tour integer values?
get.objective(orLp)
get.variables(orLp)
get.constr.value(orLp)
# Challenge 19: Get the dual solution and the reduced costs. The dual solution
# gives the cost of increasing the right-hand side by one unit. The reduced cost
# gives the amount you would have to reduce the cost of a tour for it to be
# attractive enough to replace another tour.
# Documentation: https://www.rdocumentation.org/packages/lpSolveAPI/versions/5.5.2.0-17/topics/get.dual.solution
# Hint: The first 7 elements are the dual variables in order of the rows (days).
# The remaining 5 elements are the reduced costs in order of the columns (tours).
get.dual.solution(orLp)

# Challenge 20: Set the variable types to integers.
set.type(orLp, 1,"integer")
set.type(orLp, 2,"integer")
set.type(orLp, 3,"integer")
set.type(orLp, 4,"integer")
set.type(orLp, 5,"integer")

# Challenge 21: Solve the Mixed Integer-Linear Program.
solve.lpExtPtr(orLp)

# Challenge 22: Get the solution.
# (a) Get the objective function's value. This is the total nurse cost.
# (b) Get the variables' values. These are the numbers of nurses assigned to each tour.
# (c) Get the constraints' values. These are the numbers of nurses scheduled by day.
# Q: Are there any differences from the solution in Challenge 18?
get.objective(orLp)
get.variables(orLp)
get.constr.value(orLp)

# When you're done, copy this script, paste it into Canvas's
# text box, and submit it for credit. You must submit by
# the end of class; no late submissions will be accepted.