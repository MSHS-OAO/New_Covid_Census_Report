
# Code for analyzing and summarizing unit level census and COVID-19 census data
# Last Updated 2021_09_02

rm(list=ls())

# Install packages
#install.packages("tidyverse")




# Import Libraries
library(tidyverse)
library(readxl)


# Set working directory
#Census_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Covid IP Staffing Model/Data/Epic Census Data"
Census_path <- "C:\\Users\\aghaer01\\Downloads\\Covid"
setwd(Census_path)



# Select raw data
today <- Sys.Date()
start_date <- "2020-03-12"


#Import the latest REPO file 
repo<- read_excel(paste0(Census_path, "\\REPO\\Census and Covid Repo ", start_date, " to ", today -1, " Created ", today, " Add MSBI.xlsx"))


# Import the most recent census data
details = file.info(list.files(path= Census_path, full.names=TRUE, pattern="ADT_Bed_Census_Daily_By_Dept_Summary.rpt_"))
details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ]



if (max(repo$CensusDate)== today-1){
  files = rownames(details)[1]
  todays_census_data <-  read_xlsx(files)
} else {
  count <- gsub('.*-([0-9]+).*','\\1', difftime(max(repo$CensusDate), today))
  files = rownames(details)[count]
  todays_census_data <-  do.call(rbind.data.frame, files)
  
}



