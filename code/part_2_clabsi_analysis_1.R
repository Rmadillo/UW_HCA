#########
# Code for UW HCA Cert class
# Dwight Barry & Bryan Nice
# 2015-02-25
# https://github.com/Rmadillo/uw_hca

# Central line associated blood stream infection (CLA-BSI) analysis
# Example of a data-to-product process


#########
# Step 1: load data from github

bsi = read.csv("https://raw.githubusercontent.com/Rmadillo/uw_hca/master/clabsi.csv")


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


# sometimes an interactive graph is useful
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

