
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
Census_path <- "C:/Users/aghaer01/Downloads/Covid"
setwd(Census_path)


#Import the latest REPO file 
repo <- file.info(list.files("../REPO", full.names = T))
repo_file <- rownames(repo)[which.max(repo$mtime)]
repo <- read_excel(repo_file)



# Import the most recent census data
census = file.info(list.files(path= Census_path, full.names=TRUE, pattern="ADT_Bed_Census_Daily_By_Dept_Summary.rpt_"))
census = census[with(census, order(as.POSIXct(mtime), decreasing = TRUE)), ]



if (max(repo$CensusDate)==Sys.Date()-1){
  files = rownames(census)[1]
  todays_census_data <-  read_xlsx(files)
} else {
  dif_time= difftime(max(repo$CensusDate), Sys.Date())
  count = gsub('.*-([0-9]+).*','\\1', dif_time )
  files = rownames(census)[1:count]
  todays_census_raw <- lapply(files, function(filename){
    read_excel(filename, col_names = TRUE, na = c("", "NA"))
  })
  todays_census_data <-  do.call(rbind.data.frame,  todays_census_raw )
}

rm(census, todays_census_raw)


# Import the most recent Covid data
covid = file.info(list.files(path= Census_path, full.names=TRUE, pattern="COVID Census Prior Day"))
covid = covid[with(covid, order(as.POSIXct(mtime), decreasing = TRUE)), ]



if (max(repo$CensusDate)==Sys.Date()-1){
  files = rownames(covid)[1]
  todays_covid_data <-  read_xlsx(files)
} else {
  dif_time= difftime(max(repo$CensusDate), Sys.Date())
  count = gsub('.*-([0-9]+).*','\\1', dif_time )
  files = rownames(covid)[1:count]
  todays_covid_raw <- lapply(files, function(filename){
    read_excel(filename, col_names = TRUE, na = c("", "NA"))
  })
  todays_covid_data <-  do.call(rbind.data.frame,  todays_covid_raw )
}

rm(covid, todays_covid_raw)






