### California Legislative COVID-19 Occupational Impacts Project
#     Built as part of a joint UC Berkeley and California Research Bureau internship.
#       Looks at how different neighborhoods and communities within CA legislative
#       districts have been impacted by shelter-in-place orders imposed as part of
#       the response to coronavirus.
#
#     By  Ellen Harper, UC Berkeley
#         Joshua Lewis, UC Berkeley
#         Haley Ragan, UC Berkeley
#         Robson Swift, UC Berkeley
#         Leslie Vasquez, UC Berkeley
#         Patrick Rogers, California Research Bureau
#
#     Occupational Variables.R: Uses Gascon 2020 and ACS? Occupational Data to identify
#       Census tracts in California at high risk of unemployment from lockdown orders.

# Setup
rm(list = ls(all.names = TRUE))
setwd("D:\\User_Data\\Desktop\\Cal-in-Sac\\Cal-in-Sac Github")

# Read in Data
gascon <- read.csv("Data\\FRSL Covid Impacts by Occupation.csv",
                   stringsAsFactors = FALSE)



