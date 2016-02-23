#########
# Code for UW HCA Cert class
# Dwight Barry & Bryan Nice
# 2015-02-25
# https://github.com/Rmadillo/uw_hca

# Mimic of Hans Rosling's "Developing" world TED talk
# https://www.ted.com/talks/hans_rosling_shows_the_best_stats_you_ve_ever_seen

# load the googleVis package

library(googleVis)

# load the data

rosling_data = read.csv("https://raw.githubusercontent.com/Rmadillo/UW_HCA/master/data/rosling_data.csv")
  
# create the motion plot
plot(gvisMotionChart(rosling_data, 
                     idvar = "Country", 
                     timevar = "Year", 
                     sizevar = "Population", 
                     options=list(width = 700, height = 600)))


# create a searchable table using DT package

library(DT)
datatable(rosling_data)


# End of file
