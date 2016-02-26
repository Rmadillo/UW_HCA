#########
# Code for UW HCA Cert class
# Dwight Barry & Bryan Nice
# 2015-02-25
# https://github.com/Rmadillo/uw_hca

# Central line associated blood stream infection (CLA-BSI) analysis
# Example of a data-to-product process


#########
# Step 1: load data from github

bsi = read.csv("https://raw.githubusercontent.com/Rmadillo/UW_HCA/master/data/clabsi.csv")


#########
# Step 2: create a date field
# paste is basically the same as Excel's CONCATENATE
# appending -01 to the month value (i.e., make it the first of the month)
# makes it easier to work with

bsi$Month = paste(bsi$Month, "01", sep="-")

# convert it to a Date class

bsi$Month = as.Date(bsi$Month)


#########
# Step 3: check the transformation with 'str' (structure)

str(bsi)


#########
# Step 4: calculate a monthly rate of CLA-BSIs per 1000 central line days
# and round to nearest decimal place (digits=1)

bsi$Rate = round( ((bsi$BSI / bsi$central_line_days) * 1000), 1)

# always worthwhile to double check your work

str(bsi)


#########
# Step 5: visualize the data with a simple line plot (type='l')

# base graphics in R aren't pretty but they're quick

plot(x=bsi$Month, y=bsi$Rate, type='l')


#########
# Step 6: create a basic control chart
# Rate data are generally best presented as a 'u' chart
# see https://www.spcforexcel.com/knowledge/attribute-control-charts/u-control-charts
# for a nice overview of u-charts using a healthcare example

# load the 'qcc' library to allow R to access its functions
# if it's not already downloaded and installed, you need to run
# install.packages("qcc")

library(qcc)

# look at the qcc function help page to understand how it works
# and to play with examples, if necessary

?qcc

# the qcc function works with the raw data, not the rate, so we'll include
# the conversion to a per 1000 value in the sizes option of the function call

qcc(bsi$BSI, sizes = bsi$central_line_days/1000, type = "u", nsigmas = 2)

# anything you create in R can be stored as its own object, which 
# you can manipulate with other R functions. we'll store this control chart in
# an object called 'bob'

bob = qcc(bsi$BSI, sizes = bsi$central_line_days/1000, type = "u", nsigmas = 2)

# look at the structure of 'bob':

str(bob)


#########
# Step 7: the qcc control chart is ugly, so we'll use ggplot to make a pretty graph

# load the ggplot2 package

library(ggplot2)

# join the control limits stored in 'bob' back with the original data in 'bsi'

bsi = data.frame(bsi, bob$limits)

# create a basic ggplot of the rate

ggplot(bsi, aes(x=Month, y=Rate)) +
  geom_line()

# ggplot works in layers of aesthetics ('aes'), so graphs are infinitely customizable
# we'll next add the control limits, with the rate last so it will be on top.
# we'll also use color and line types to distinguish the rate from the limits.

ggplot(bsi, aes(x = Month, y = Rate)) +
  geom_line(aes(y=UCL), linetype = "dotted") +
  geom_line(aes(y=LCL), linetype = "dotted") +
  geom_line(color="darkblue")

# we can also save the plot as an object and customize it from there

gg = ggplot(bsi, aes(x = Month, y = Rate)) +
  geom_line(aes(y=UCL), linetype = "dotted") +
  geom_line(aes(y=LCL), linetype = "dotted") +
  geom_line(color="darkblue")

gg = gg + ggtitle("Monthly CLA-BSIs per 1,000 Central Line Days")
  
gg

gg = gg + ylab("Rate (per 1,000 line days)")
gg = gg + theme_bw()

gg 


#########
# Step 7: sometimes an interactive graph is useful
# the dygraphs package is built specifically for time series data

library(dygraphs)

# we need to convert our data into a time series R object
# and we'll remove the excess columns to focus on rates and limits

bsi_base = bsi[,4:6]
  
bsi_ts = ts(bsi_base, start = c(1998, 1), frequency = 12)

# the %>% ('pipe') operator is in many newer R packages, and allows
# you to string together commands or functions; here, it adds a range selector
# to the dygraph plot

dygraph(bsi_ts, main = "Monthly CLA-BSI Rate Control Chart") %>%
  dyRangeSelector()


#########
# Step 8: that time series looks suspicious
# is a control chart appropriate? control charts require
# that the data be free of autocorrelation and/or trending

# examine autocorrelation with acf

acf(bsi$Rate)

# example whether a trend line is different from flat
# using the row number (order) of the data

summary(lm(bsi$Rate ~ index(bsi$Rate)))


#########
# Step 9: distinct shifts in a time series can be detected
# via changepoint analysis (aka intervention analysis)

# load strucchange package

require(strucchange)

# create a time series object with just the rate

clabsi_ts = ts(bsi$Rate, start = c(1998, 1), frequency = 12)

# Use strucchange to find [change] breakpoints

clabsi_bp = breakpoints(clabsi_ts ~ 1)
clabsi_bp_summary = summary(clabsi_bp)
clabsi_bp_confint = confint(clabsi_bp, level=0.95)

# view the changepoint objects

clabsi_bp
clabsi_bp_summary
clabsi_bp_confint

# use base graphics for a simple plot

plot(clabsi_ts)
lines(clabsi_bp)
lines(clabsi_bp_confint)

# create linear model set for 0 to 5 step changes and 1 linear trend

vm0 = lm(clabsi_ts ~ 1)
vm1 = lm(clabsi_ts ~ breakfactor(clabsi_bp, breaks = 1))
vm2 = lm(clabsi_ts ~ breakfactor(clabsi_bp, breaks = 2))
vm3 = lm(clabsi_ts ~ breakfactor(clabsi_bp, breaks = 3))
vm4 = lm(clabsi_ts ~ breakfactor(clabsi_bp, breaks = 4))
vm5 = lm(clabsi_ts ~ breakfactor(clabsi_bp, breaks = 5))
# lm not supported by residuals but doing it anyway for the heck of it
vlm = lm(bsi$Rate ~ index(bsi$Rate))

# Calculate BICs

BICvalues = AIC(clabsi_bp, k = log(clabsi_bp$nobs))

# BIC for linear model

BIClm = AIC(vlm, k=log(nrow(bsi)))
names(BIClm) = "lm"

# Make vector of all BIC values

BICs = append(BICvalues, BIClm)
BICs = BICvalues

# Calculate deltas

deltas = BICs - min(BICs)

# Calculate log likelihoods

logliks = exp(-0.5*deltas)

# Calculate model weights

mweights = logliks/sum(logliks)

# Make IT summary table

BICtable = cbind(BICs, deltas, logliks, mweights)

# Results of model comparisons

BICtable

# hard to read, so use round on whole table

round(BICtable, 3)

# set up some objects for later use in graphing

optimal_bps = clabsi_bp$breakpoints
bp_dates = bsi$Month[optimal_bps]
lci = clabsi_bp_confint$confint[,1]
uci = clabsi_bp_confint$confint[,3]
seg1 = mean(clabsi_ts[1:optimal_bps])
seg2 = mean(clabsi_ts[optimal_bps:length(clabsi_ts)])

# make pretty plot of time series and optimal changepoint model

ggplot(bsi, aes(Month, Rate)) +
  scale_x_date() +
  ylab("CLA-BSI Rate per 1000 Central Line Days\n") +
  ggtitle("All Hospital Monthly CLA-BSI Rates, Jan 1998-Dec 2007") +
  geom_vline(aes(xintercept = as.numeric(Month[optimal_bps])), lty=4, lwd=1, col="gray35") +
  geom_line(lwd=1) +
  geom_line(aes(Month, fitted(vm1, start = 0)), col="blue", lwd=0.8) +
  geom_errorbarh(data=bsi, aes(xmin = bsi$Month[lci], y = 0.25, xmax = bsi$Month[uci]), height=0.2, col="blue") +
  geom_point(aes(x = bsi$Month[optimal_bps], y = 0.25), shape=21, fill="white", col="blue", size=3)


#########
# Step 10: take the time series after the change point for control charting

# subset the original data set, using the dplyr library

bsi_sub = filter(bsi, Month >= '2001-09-01')

# evaluate the subset for autocorrelation and trending

acf(bsi_sub$Rate)
summary(lm(bsi_sub$Rate ~ index(bsi_sub$Rate)))

# run qcc again on the subset

bob2 = qcc(bsi_sub$BSI, sizes = bsi_sub$central_line_days/1000, type = "u", nsigmas = 2)

# add the control limits to the data frame

bsi = data.frame(bsi_sub, bob2$limits)

# create a basic ggplot of the rate

ggplot(bsi_sub, aes(x=Month, y=Rate)) +
  geom_line(aes(y=UCL), linetype='dashed', color='darkred') +
  geom_hline(aes(yintercept=mean(Rate))) +
  geom_line(color='darkblue', lwd=1)
