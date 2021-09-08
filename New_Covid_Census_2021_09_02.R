
# Code for analyzing and summarizing unit level census and COVID-19 census data
# Last Updated 2021_09_02

rm(list=ls())

# Install packages
#install.packages("tidyverse")
#install.packages("reshape2")


# Import Libraries
library(tidyverse)
library(readxl)
library(reshape2)


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


# Import reference file -----------
site_mapping <- read_excel("AnalysisReference 2021-07-20.xlsx", sheet = "SiteMapping",
                           col_names = TRUE, na = c("", "NA"))
                           
unit_mapping <- read_excel("AnalysisReference 2021-07-20.xlsx", sheet = "EpicPremierUnitMapping",
                           col_names = TRUE,  na = c("", "NA"))
                          

### Clean Census data 
todays_census_data <- todays_census_data %>% mutate(CensusYear= substr(YEAR_MONTH, 1, 4),
                                                    CensusMonth = substr(YEAR_MONTH, 5,6 ))
 

todays_census_data <- todays_census_data %>% 
  mutate(CensusDate= as.Date(paste0(CensusMonth, "/", DAY_OF_MONTH,"/", CensusYear), format = "%m/%d/%Y"))



colnames(todays_census_data)[colnames(todays_census_data) == "%"] <- "Utilization"

# Map site and Premier unit
todays_census_data <- left_join(todays_census_data, site_mapping,
                                by = c("LOC_NAME" = "LOC_NAME"))

# Check data to see if there is data from an extra date
table(todays_census_data$CensusDate)

todays_census_data <- todays_census_data %>%  filter(CensusDate != Sys.Date())
 

# Subset census data
todays_census_subset <- todays_census_data %>% select("Site", "LOC_NAME", "DEPARTMENT_NAME", "CensusDate", 
                                                      "OCCUPIED_COUNT",  "TOTAL_BED_CENSUS", "Utilization")

### Clean up COVID data 
todays_covid_data <- todays_covid_data %>% rename(ICU = GROUP)

# Remove unnecessary columns
todays_covid_data <- subset(todays_covid_data, select = c(1:9 ))
todays_covid_data <- subset(todays_covid_data, select = -c(6) )

# Format COVID data census date
todays_covid_data <- todays_covid_data %>% mutate(CensusDate = as.Date(`REPORT RUN`,  format = "%m/%d/%Y"))
                                       

# Map site and Premier unit
todays_covid_data <- left_join(todays_covid_data, site_mapping,  by = c("LOC_NAME" = "LOC_NAME"))
rm(site_mapping)


# Check data to see if there is data from an extra date
table(todays_covid_data$CensusDate)
todays_covid_data <- todays_covid_data %>%  filter(CensusDate != Sys.Date())

# Remove any duplicates lines from COVID data
todays_covid_data <- unique(todays_covid_data)

# Convert infection status to factor so it can be ordered properly
todays_covid_data <- todays_covid_data %>% mutate(INFECTION_STATUS = factor(INFECTION_STATUS,
                                      levels = c("COVID-19", "SUSC COVID", "PUI - COVID", "PUM"),  ordered = TRUE))


# Remove duplicate MRNs with both a COVID and PUI flag (keep COVID flag)
todays_covid_data <- todays_covid_data %>% group_by(MRN, CensusDate) %>%  mutate(DuplInfect=n())
todays_covid_data <- todays_covid_data[!(todays_covid_data$DuplInfect==2 &  todays_covid_data$INFECTION_STATUS=="PUI - COVID"),]

todays_covid_data$DuplInfect <- NULL

# Summarize patient level COVID data
todays_covid_summary <- todays_covid_data %>%
  group_by(Site, LOC_NAME, DEPARTMENT_NAME, ICU, CensusDate, INFECTION_STATUS) %>%
  summarize(TotalPatients = n()) %>%   ungroup()


# reshape data
todays_covid_cast <- dcast(todays_covid_summary,
                           Site + LOC_NAME + DEPARTMENT_NAME + ICU + CensusDate ~
                             INFECTION_STATUS, 
                           value.var = "TotalPatients")

# rename columns
colnames(todays_covid_cast)[colnames(todays_covid_cast) == "COVID-19"] <- "COVID19"
colnames(todays_covid_cast)[colnames(todays_covid_cast) == "PUI - COVID"] <- "PUI"
colnames(todays_covid_cast)[colnames(todays_covid_cast) == "PUM - RESP"] <- "PUM"
colnames(todays_covid_cast)[colnames(todays_covid_cast) == "SUSC COVID"] <- "SUSC"


# Merge COVID data with daily census
todays_census_covid_merge <- full_join(todays_census_subset, todays_covid_cast, 
                                       by = c("Site" = "Site",
                                              "LOC_NAME" = "LOC_NAME",
                                              "DEPARTMENT_NAME" = "DEPARTMENT_NAME",
                                              "CensusDate" = "CensusDate"))
# Remove LOC_NAME column now that dataframes have been merged
todays_census_covid_merge$LOC_NAME <- NULL


# replace NAs with 0
todays_census_covid_merge[is.na(todays_census_covid_merge$COVID19), "COVID19"] <- 0
todays_census_covid_merge[is.na(todays_census_covid_merge$PUI), "PUI"] <- 0
todays_census_covid_merge[is.na(todays_census_covid_merge$PUM), "PUM"] <- 0
todays_census_covid_merge[is.na(todays_census_covid_merge$SUSC), "SUSC"] <- 0 

# Estimate Covid Utilization 
todays_census_covid_merge <- todays_census_covid_merge %>% 
                           mutate(COVIDUtilization=round((COVID19+PUI+PUM)/TOTAL_BED_CENSUS, 3))


# Map units to Premier name, Unit Type, Include, etc.
unit_mapping$LOC_NAME <- NULL
todays_census_covid_merge <- left_join(todays_census_covid_merge,
                                       unit_mapping[],
                                       by = c("DEPARTMENT_NAME" =
                                                "DEPARTMENT_NAME"))

todays_census_covid_merge$PremierMapping <-
  ifelse(str_detect(todays_census_covid_merge$PremierUnit,
                    "NO PREMIER MAPPING"), FALSE, TRUE)




                              