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
setwd("D:\\User_Data\\Documents\\CRB\\Cal-in-Sac\\Cal-in-Sac Github")

my.key = "b43318029889fc8e7a04e20f94b448e8b18c481b"

my.table = "C24010"
my.tablecount = 73

# Packages
library(jsonlite)

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

# Read in Data

# Get ACS Labels
acs.variables <- read.csv("Code\\ACS C24010 Labels.csv",
                          stringsAsFactors = FALSE)[,1:2]
acs.variables <- acs.variables[grep(pattern = "_[0-9]{3}E$", x = acs.variables$Name),]

length(strsplit(acs.variables$Label, split = "!!"))

# Create list of tables
#acs.variables <- paste0(my.table, "_", sprintf(fmt = "%03d", 1:my.tablecount), "E")

# Use the number of "!!" to determine code hierarchy
acs.variables$Level <- unlist(lapply(acs.variables$Label,
                                     function(x) length(strsplit(x, split="!!")[[1]])))

acs.variables$Label <- unlist(lapply(acs.variables$Label,
                                    function(x) strsplit(x, split="!!")[[1]][length(strsplit(x, split="!!")[[1]])]))
write.csv(acs.variables, file = "Code\\ACS C24010 Labels Cleaned.csv", row.names=FALSE)


# Gascon Data
gascon <- read.csv("Data\\FRSL Covid Impacts by Occupation.csv",
                   stringsAsFactors = FALSE)

#

