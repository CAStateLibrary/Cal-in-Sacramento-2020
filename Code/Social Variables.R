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
#     Social Variables.R: Uses American Community Survey and Housing and Urban Development 

# Setup
rm(list = ls(all.names = TRUE))
setwd("D:\\User_Data\\Desktop\\Cal-in-Sac\\Cal-in-Sac Github")

my.key = "b43318029889fc8e7a04e20f94b448e8b18c481b"


# Packages
library(jsonlite)

# Read in Non-ACS Data
housing_quality <- read.csv("Data\\Housing Quality.csv")[,-c(1,5)]
housing_quality$state <- "06"
housing_quality$county <- formatC(housing_quality$county, width = 3, flag = "0")
housing_quality$tract <- formatC(housing_quality$tract, width = 6, flag = "0")
colnames(housing_quality)[3] <- "Housing_Quality"

########################
### Custom Functions ###
########################

fix.column.names <- function(my.table){
  # Takes the first row of data as column name, then drops the first row
  colnames(my.table) <- my.table[1,]
  my.table <- my.table[-1,]
  
  return(my.table)
}

simple.acs.tables <- function(table.names) {
  # Takes a single table name, and returns the 2014-2018 5-year ACS estimate
  
  # Build the API call based on table type
  #   B is Detail Table
  #   C is Detail Table
  #   S is Subject Table
  
  # Check for detail table names
  detail <-
    unlist(regmatches(
      table.names,
      gregexpr(pattern = "[B-C][0-9]{5}_[0-9]{3}E", table.names)
    ))
  
  # Check for subject table names
  subject <-
    unlist(regmatches(
      table.names,
      gregexpr(pattern = "S[0-9]{4}_C[0-9]{2}_[0-9]{3}E", table.names)
    ))
  
  # If detail tables were requested, pull those tables
  detail.list <- vector('list', ceiling(length(detail) / 50))
  if (length(detail.list) > 0) {
    for (i in 1:length(detail.list)) {
      # Build path
      path <- paste0(
        "https://api.census.gov/data/2018/acs/acs5?get=",
        paste(detail[1:min(50, length(detail))], collapse = ","),
        "&for=tract:*&in=state:06&key=",
        my.key
      )
      # API call
      detail.list[[i]] <- data.frame(read_json(path = path,
                                               simplifyVector = TRUE))
      # Drop table names we just pulled
      detail <- detail[-(1:50)]
    }
    detail <- do.call("cbind", detail.list)
    detail <- fix.column.names(detail)
    detail <-
      cbind(detail[, which(colnames(detail) %in% c("state", "county", "tract"))[1:3]],
            detail[, which(!(colnames(detail) %in% c("state", "county", "tract")))])
  }
  
  # If subject tables were requested, pull those tables
  subject.list <- vector('list', ceiling(length(subject) / 50))
  if (length(subject.list) > 0) {
    for (i in 1:length(subject.list)) {
      # Build path
      path <- paste0(
        "https://api.census.gov/data/2018/acs/acs5/subject?get=",
        paste(subject[1:min(50, length(subject))], collapse = ","),
        "&for=tract:*&in=state:06&key=",
        my.key
      )
      # API call
      subject.list[[i]] <- data.frame(read_json(path = path,
                                                simplifyVector = TRUE))
      # Drop table names we just pulled
      subject <- subject[-(1:50)]
    }
    subject <- do.call("cbind", subject.list)
    subject <- fix.column.names(subject)
    subject <-
      cbind(subject[, which(colnames(subject) %in% c("state", "county", "tract"))[1:3]],
            subject[, which(!(colnames(subject) %in% c("state", "county", "tract")))])
  }
  
  # Check if we got both Detail and Subject Tables
  if (ncol(detail) > 0 & ncol(subject) > 0) {
    val <- merge(detail, subject)
  } else {
    # If not both, which?
    if (ncol(detail) > 0) {
      val <- detail
    } else {
      val <- subject
    }
  }
  
  # Convert estimates to numeric
  val[, which(colnames(val) %in% table.names)] <-
    sapply(val[, which(colnames(val) %in% table.names)], as.numeric)
  
  return(val)
}

########################
### READ IN ACS DATA ###
########################

# Create convenience variables

# Read in variable definitions
acs.variables <- data.frame(read.csv(file = "Code\\ACS Variables.csv",
                                     header = TRUE,
                                     stringsAsFactors = FALSE))

# REGEX Pattern to extract ACS table names for varibale definition file
acs.table.pattern <- "((?:[B-C][0-9]{5}_[0-9]{3}E)|(?:S[0-9]{4}_C[0-9]{2}_[0-9]{3}E))"

# Build data frame of raw data

# First get a list of each detail table name
acs.unlisted <- NULL
for (i in 1:nrow(acs.variables)) {
  acs.unlisted <- c(acs.unlisted,
                    unlist(regmatches(
                      acs.variables$Numerator[i],
                      gregexpr(pattern = acs.table.pattern, acs.variables[i, 4])
                    )),
                    unlist(regmatches(
                      acs.variables$Denominator[i],
                      gregexpr(pattern = acs.table.pattern, acs.variables[i, 5])
                    )))
}

acs.unlisted <- unique(acs.unlisted)

# Then send the variables to the ACS API to get the ACS estimates
acs.tables <- simple.acs.tables(acs.unlisted)

# Then use the expressions in acs.variables to calculate our estimates
social.indicators <- NULL
for(i in 1:nrow(acs.variables)) {
  social.indicators <- cbind(social.indicators,
                             eval(str2expression(paste0(
                               gsub(
                                 pattern = acs.table.pattern,
                                 replacement = "acs.tables\\$\\1",
                                 acs.variables$Numerator[i]
                               ),
                               "/",
                               gsub(
                                 pattern = acs.table.pattern,
                                 replacement = "acs.tables\\$\\1",
                                 acs.variables$Denominator[i]
                               )
                             ))))
  colnames(social.indicators)[i] <- acs.variables$Name[i]
}

social.indicators <- data.frame(cbind(acs.tables[,1:3],
                           social.indicators))

social.indicators <- merge(social.indicators, housing_quality,
      by = c("state", "county", "tract"),
      all.x = TRUE)

# Remove 0 Pop Tracts
social.indicators <- social.indicators[which(social.indicators$Median_Age>0),]

# Save social indicators
write.csv(social.indicators, file = "Data\\Raw_Social.csv",
          row.names = FALSE)


