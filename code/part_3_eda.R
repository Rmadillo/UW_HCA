## R code and data for Ryan and Jeff's class
install.packages(c("ggplot2", "dplyr", "dygraphs", "DT", "googleVis", "qcc", "tidyr", "scales","knitr", "strucchange", "devtools", "lubridate", "htmlTable", "htmlwidgets","VIM","quantreg","GGally","psych"))
devtools::install_github("hrbrmstr/taucharts")

#########
# Code for UW HCA Cert class
# Dwight Barry & Bryan Nice
# 2015-03-03
# https://github.com/Rmadillo/uw_hca


#### Exploratory data analysis (EDA) ####


#### Load data ####

# FROM A NON-UW PCE COMPUTER
patients = read.csv("http://raw.githubusercontent.com/Rmadillo/UW_HCA/master/data/uw_population.csv", header=T)

# FROM A UW PCE COMPUTER
# download.file("http://raw.githubusercontent.com/Rmadillo/UW_HCA/master/data/uw_population.csv", "patients.csv")
# getwd()
# patients = read.csv("patients.csv", header=T)


#### Load packages ####
require(psych)     # for descriptive statistics
require(ggplot2)   # for plotting
require(dplyr)     # for data manipulation
require(GGally)    # for pairs plot
require(quantreg)  # for quantile trends
require(VIM)       # for missing data visualization
require(psych)

# Load a custom function to create a mosaic plot in ggplot
# source: http://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2

ggMMplot <- function(var1, var2){
  require(ggplot2)
  levVar1 <- length(levels(var1))
  levVar2 <- length(levels(var2))
  
  jointTable <- prop.table(table(var1, var2))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(var1))
  plotData$var2Height <- plotData$Freq / plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
    plotData$marginVar1 / 2
  
  ggplot(plotData, aes(var1Center, var2Height)) +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "White") +
    geom_text(aes(label = as.character(var1), x = var1Center, y = 1.05)) +
    xlab("Variable 1 Proportion") +
    ylab("Variable 2 Proportion")
}


#### Look at data structure ####
str(patients)


#### Simple cleaning ####

# fill in blanks in the Education column as 'Unknown'
patients$Education = ifelse(patients$Education == "", 
                             "Unknown",
                             as.character(patients$Education))

# make Education into an ordered factor
patients$Education = ordered(as.factor(patients$Education), levels=
                               c("Unknown", 
                                 "High School Graduate", 
                                 "Some College", 
                                 "College Graduate"))


#### Descriptive plots ####

# Histogram, default
ggplot(patients, aes(x=CHOL)) +
  geom_histogram()

# Histogram, custom binwidth 
ggplot(patients, aes(x=CHOL)) +
  geom_histogram(binwidth=10)

# Histogram / density plot
ggplot(patients, aes(x=CHOL)) + 
  geom_histogram(aes(y =..density..), binwidth = 10) + 
  geom_density()

# Bar plot
ggplot(patients, aes(x=ED_Past_30_Days)) +
  geom_bar()

# Scatter plot
ggplot(patients, aes(x=Age, y=PMPM)) +
  geom_point()

# Boxplot
ggplot(patients, aes(x=Readmits_Past_Year, y=PMPM)) +
  geom_boxplot()

# Mosaic plot, using custom function above
ggMMplot(patients$ED_Past_30_Days, patients$Education)

# Trends (loess)
ggplot(patients, aes(x=Age, y=PMPM)) +
  geom_point() +
  geom_smooth()

# Trends (quantile-median)
ggplot(patients, aes(x=Age, y=PMPM)) +
  geom_point() +
  geom_quantile(quantiles = 0.5)

# Trends (quantile-custom)
ggplot(patients, aes(x=Age, y=PMPM)) +
  geom_point() +
  geom_quantile(quantiles = c(0.05, 0.25, 0.50, 0.75, 0.95))

# Trends (linear, continuous data)
ggplot(patients, aes(x=Age, y=PMPM)) +
  geom_point() +
  geom_smooth(method = 'lm')

# Trends (linear, count data)
ggplot(patients, aes(x=PMPM, y=IP_Past_Year)) +
  geom_point() +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"))

# Faceted
ggplot(patients, aes(x=Age, y=PMPM)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~Education)

# Pairs plot (VERY slow! Be patient! [then zoom])
bigplot = ggpairs(patients[,2:8])
bigplot

# Missing data, using VIM package
matrixplot(patients)


#### Descriptive stats ####

# Generic all-purpose summary
summary(patients)

# Descriptive stats for numeric variables only, using psych package
# all data 
describe(patients[,sapply(patients,is.numeric)], skew=FALSE)

# by group
# Note: errors thrown because not all groups have data, ignore
describeBy(patients[,sapply(patients,is.numeric)], skew=FALSE, group=patients$Education)

# Descriptive stats using dplyr package, useful for making new data frames
grouped = group_by(patients, Education)
sum_data_frame = summarize(grouped, mean=mean(PMPM), sd=sd(PMPM), count=n())
str(sum_data_frame)

# Descriptive stats for factors

# one variable
summary(patients$Education)

# contingency table, counts
table(patients$Education, patients$ED_Past_30_Days)

# contingency table, proportions
prop.table(table(patients$Education, patients$ED_Past_30_Days))

# multiway table
table(patients$Education, patients$ED_Past_30_Days, patients$PC_HW_Visit_Past_Year)


#### End of file ####
