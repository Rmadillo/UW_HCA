#########
# Code for UW HCA Cert class
# Dwight Barry & Bryan Nice
# 2015-02-25
# https://github.com/Rmadillo/uw_hca

# Anscombe's Quartet: visualization story and interactive graph example


# load data from github
aq = read.csv("https://gist.githubusercontent.com/alansmithy/68be725ac0c00c790c6f/raw/6dc54a4af348c4bd11a1f87b0b841c53d9f5c4f0/anscombe.csv")

# split data into the 4 series
aq_sep = split(aq, aq$seriesname)

# get linear models for each series
s1 = lm(aq_sep$`series 1`$y ~ aq_sep$`series 1`$x)
s2 = lm(aq_sep$`series 2`$y ~ aq_sep$`series 2`$x)
s3 = lm(aq_sep$`series 3`$y ~ aq_sep$`series 3`$x)
s4 = lm(aq_sep$`series 4`$y ~ aq_sep$`series 4`$x)

# look at regression coefficients
summary(s1)$coef; summary(s2)$coef; summary(s3)$coef; summary(s4)$coef

# look at regression R^2s 
summary(s1)$r.squared; summary(s2)$r.squared; summary(s3)$r.squared; summary(s4)$r.squared

# load taucharts package
require(taucharts)

# create taucharts 
tauchart(aq, width=700, height = 500) %>%
  tau_point("x", "y", color="seriesname") %>%
  tau_guide_x(min=0, max=22) %>%
  tau_guide_y(min=0, max=14) %>%
  tau_legend() %>%
  tau_tooltip( ) %>%
  tau_trendline(models = 'linear', showTrend = F) 


# End of file
