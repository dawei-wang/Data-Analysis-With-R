# Dawei Wang
# Workshop 4: Design of Experiments (DOE)
# February 20, 2019

# Objectives: Perform factorial analyses, with hypothesis testing 
# and main effect and interaction charts.

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

# DOE may be used in DMAIC's and DMADV's Analyze phase to understand
# how different factors affect outcomes. DOE may also be used
# in DMAIC's Improve and DMADV's Verify phases to understand
# whether different recommendations are worth pursuing from an
# operational perspective.

# SCENARIO: A movie theater chain wants to improve popcorn yield
# (measured in cups) and taste (on a subjective scale from 1=bad
# to 10=great). The chain currently uses ordinary popcorn, a ratio
# of 8 cups of popcorn kernels per cup of oil, and a batch size of 
# 0.33 cups of popcorn kernels.

# IMPROVE/DESIGN. The chain is considering whether to switch to
# gourmet popcorn, a ratio of 4 cups of popcorn kernels per cup of
# oil, and a batch size of 0.67 cups.

# IMPROVE/VERIFY. The chain has hired you to perform a DOE to
# determine whether to make the changes.

# Challenge 1: Install the DoE.base and FrF2 packages and reference 
# them a libraries in this script.
# Documentation: https://cran.r-project.org/web/packages/DoE.base/DoE.base.pdf,
# https://cran.r-project.org/web/packages/FrF2/FrF2.pdf.

library(DoE.base)
library(FrF2)
library(ggplot2)
library(openxlsx)

# Challenge 2: Create an experimental design matrix for
# the following 2^3 full factorial design:
#
# Levels
# Factor - + Notes
# ______ ________ ________ _________________________________
# Brand ordinary gourmet 
# P2O 4 8 Popcorn to oil ratio, in cups
# Batch 0.33 0.67 Batch size in cups
# 
# Print the experimental design matrix.
#
# Documentation: https://www.rdocumentation.org/packages/DoE.base/versions/1.1-1/topics/fac.design
# Hint: Focus on the factor.names parameter.

fac.design(nlevels = 3,nfactors = 2,factor.names = c("P2O","Batch"), replications = 1)

# The trials in your design matrix were performed. 
# The data is in "Popcorn.csv".

# Challenge 3: Read "Popcorn.csv" into a data.frame. 
# Print the data.frame.
PopcornCsv<-read.csv("Popcorn.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
print(PopcornCsv)

# Challenge 4: P2O and Batch are numerical values in the data.frame.
# Convert them to categorical "factors" since that's what 
# a 2^k factorial needs.
# Tutorial: https://www.stat.berkeley.edu/~s133/factors.html.
PopcornCsv$P2O<-factor(PopcornCsv$P2O)
PopcornCsv$Batch<-factor(PopcornCsv$Batch)

# Challenge 5: (a) Create a scatter chart of Taste vs. Yield
# (b) Add an appropriate title. (c) Label the x and y axes with 
# their metrics and units.
# Q: Is there a trial that is best on both dimensions, or is there
# a tradeoff?
plot(PopcornCsv$Taste,PopcornCsv$Yield, main = "Popcorn Taste Inflence on Yield", xlab = "Taste",ylab = "Yield",
     col="navy",
     xlim=c(0,10),
     ylim=c(0,20))


# Challenge 6: Perform an ANOVA of the Yield to see if 
# the variation in Yield is driven by Brand, P2O and Batch.
# (Ignore interactions for now.) Print a summary of
# the results.
# Q: Which main effects are significant?
analysis<-aov(Yield~Brand+P2O+Batch,data = PopcornCsv)
anova(analysis)
summary(analysis)

# Challenge 7: Plot the main effects using the Yield ANOVA's results.
# Q: Which levels of the factors would you choose to maximize yield?

MEPlot(analysis)

# Challenge 8: Perform the Yield ANOVA including the 2-factor interactions.
# Print a summary of the results.
# Q: Which main effects are significant? Which interactions
# are significant?
analysis<-aov(Yield~Brand+P2O+Batch+Brand:P2O+Brand:Batch+Batch:P2O,
              data = PopcornCsv)
summary(analysis)

# Challenge 9: Plot the main effects using the Yield ANOVA's results.
# Q: Ignoring interactions, which levels of the factors would you 
# choose to maximize yield?
MEPlot(analysis) # MEPlot produces Main Effect Plots

# Challenge 10: Plot the interactions of the factors using 
# the Yield ANOVA's results.
# Q: Do the interactions that were significant in the ANOVA look like
# significant interactions? Accounting for main effects and interactions,
# which levels of the factors would you choose to maximize yield?
IAPlot(analysis) # IA PLOT produce interaction plots


# Challenge 11: Perform the Taste ANOVA including the 2-factor interactions.
# Print a summary of the results.
# Q: Which main effects are significant? Which interactions
# are significant?
tasteAnalysis<-aov(Taste~Brand+P2O+Batch+Brand:P2O+Brand:Batch+Batch:P2O,data = PopcornCsv)
summary(tasteAnalysis)
# : between column names denotes interactions

# Challenge 12: Plot the main effects using the Taste ANOVA's results.
# Q: Ignoring interactions, which levels of the factors would you 
# choose to maximize taste?
MEPlot(tasteAnalysis)


# Challenge 13: Plot the interactions of the factors using 
# the Taste ANOVA's results.
# Q: Do the interactions that were significant in the ANOVA look like
# significant interactions? Accounting for main effects and interactions,
# which levels of the factors would you choose to maximize taste?
IAPlot(tasteAnalysis)

# Challenge 14: Perform a multiple regression of Yield
# as a function of Brand, P2O and Batch. Print the summary table.
# Q: Is the model statistically different from zero (p-value < 0.05)?
# Is the model predictive (Adjusted R-squared > 0.95)?
# How would you construct a formula of Yield as a function of
# the factors?

yieldRegression<-lm(Yield~Brand+P2O+Batch+Brand:P2O+Brand:Batch+Batch:P2O,data = PopcornCsv)
summary(yieldRegression)
# Challenge 15: Print the Yield regression's ANOVA table. 
# Q: Which, if any, of the independent variables are statistically 
# significant? Do these agree with your analyses, above?
anova(yieldRegression)

# Q: Suppose you were given the costs of the different brands, 
# the cost of oil, and the cost of making a batch. Suppose you
# were also given a function that related demand for popcorn as
# function of cost and taste. How would you use this information 
# and the information from your analyses above to make a final
# decision about which combination of factors to use?

# When you're done, copy this script, paste it into Canvas's
# text box, and submit it for credit. You must submit by
# the end of class; no late submissions will be accepted.