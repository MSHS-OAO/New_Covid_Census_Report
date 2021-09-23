
# Code for analyzing and summarizing unit level census and COVID-19 census data
# Last Updated 2021_09_014

rm(list=ls())

# Install packages
#install.packages("tidyverse")
#install.packages("reshape2")


# Import Libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(reshape2)
library(svDialogs)
library(formattable)
library(scales)
library(ggpubr)
library(knitr)
library(kableExtra)
library(rmarkdown)


# Set working directory
Census_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Covid IP Staffing Model/Data/Epic Census Data"
#Census_path <- "C:\\Users\\aghaer01\\Downloads\\Covid"
setwd(Census_path)


#Import the latest REPO file 
repo <- file.info(list.files(path = paste0(Census_path,"/REPO"), full.names = T , pattern =paste0("Census and Covid Repo 2020-03-12 to ",  format(Sys.Date(), "%Y-%m"))))
repo_file <- rownames(repo)[which.max(repo$ctime)]
repo <- read_excel(repo_file)

#repo <- read_excel(paste0(Census_path,"/REPO/Census and Covid Repo 2020-03-12 to ", Sys.Date()-2, " Created ", Sys.Date()-1, " Add MSBI.xlsx"))
max(repo$CensusDate)


# Import the most recent census data
census = file.info(list.files(path= Census_path, full.names=TRUE, pattern=paste0("ADT_Bed_Census_Daily_By_Dept_Summary.rpt_", format(Sys.Date(), "%Y-%m-"))))
census = census[with(census, order(as.POSIXct(ctime), decreasing = TRUE)), ]

dif_time= difftime(max(repo$CensusDate), Sys.Date()-1)
count = gsub('.*-([0-9]+).*','\\1', dif_time )

files = rownames(census)[1:count]
todays_census_raw <- lapply(files, function(filename){
  read_excel(filename, col_names = TRUE, na = c("", "NA"))})

todays_census_data <-  do.call(rbind.data.frame,  todays_census_raw )

rm(census, todays_census_raw)


# Import the most recent Covid data
covid = file.info(list.files(path= Census_path, full.names=TRUE, pattern=paste0("^COVID Census Prior Day.rpt_",  format(Sys.Date(), "%Y-%m-"))))
covid = covid[with(covid, order(as.POSIXct(ctime), decreasing = TRUE)), ]

files = rownames(covid)[1:count]
todays_covid_raw <- lapply(files, function(filename){
  read_excel(filename, col_names = TRUE, na = c("", "NA"))
})
todays_covid_data <-  do.call(rbind.data.frame,  todays_covid_raw )

rm(covid, todays_covid_raw)


# Import reference file -----------
site_mapping <- read_excel("AnalysisReference 2021-07-20.xlsx", sheet = "SiteMapping",
                           col_names = TRUE, na = c("", "NA"))
                           
unit_mapping <- read_excel("AnalysisReference 2021-07-20.xlsx", sheet = "EpicPremierUnitMapping",
                           col_names = TRUE,  na = c("", "NA"))
                          

### Clean Census data 
colnames(todays_census_data)[colnames(todays_census_data) == "%"] <- "Utilization"
todays_census_data <- todays_census_data %>% mutate(CensusYear= substr(YEAR_MONTH, 1, 4),
                                                    CensusMonth = substr(YEAR_MONTH, 5,6 ))
 
todays_census_data <- todays_census_data %>% 
  mutate(CensusDate= as.Date(paste0(CensusMonth, "/", DAY_OF_MONTH,"/", CensusYear), format = "%m/%d/%Y"))


# Check data to see if there is data from an extra date
todays_census_data <- todays_census_data %>% group_by(DEPARTMENT_ID, DEPARTMENT_NAME, CensusDate ) %>% 
                      mutate(ExtraDate=1:n()) %>%  ungroup()
todays_census_data <- todays_census_data[(todays_census_data$ExtraDate == 1),]
todays_census_data <- todays_census_data %>%  filter(CensusDate != Sys.Date())


# Map site and Premier unit
todays_census_data <- left_join(todays_census_data, site_mapping,
                                by = c("LOC_NAME" = "LOC_NAME"))

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
todays_covid_cast <- dcast(todays_covid_summary, Site + LOC_NAME + DEPARTMENT_NAME + ICU + CensusDate ~
                             INFECTION_STATUS, value.var = "TotalPatients")



# Replace NA with 0
todays_covid_cast[is.na(todays_covid_cast$`COVID-19`), 'COVID-19']       <- 0
todays_covid_cast[is.na(todays_covid_cast$`PUI - COVID`), 'PUI - COVID'] <- 0
todays_covid_cast[is.na(todays_covid_cast$`PUM - RESP`), 'PUM - RESP']   <- 0
todays_covid_cast[is.na(todays_covid_cast$`SUSC COVID`), 'SUSC COVID']   <- 0

# rename columns
colnames(todays_covid_cast)[colnames(todays_covid_cast) == "COVID-19"]    <- "COVID19"
colnames(todays_covid_cast)[colnames(todays_covid_cast) == "PUI - COVID"] <- "PUI"
colnames(todays_covid_cast)[colnames(todays_covid_cast) == "PUM - RESP"]  <- "PUM"
colnames(todays_covid_cast)[colnames(todays_covid_cast) == "SUSC COVID"]  <- "SUSC"


# Merge COVID data with daily census
todays_census_covid_merge <- full_join(todays_census_subset, todays_covid_cast, 
                                       by = c("Site" = "Site", "LOC_NAME" = "LOC_NAME",
                                              "DEPARTMENT_NAME" = "DEPARTMENT_NAME",  "CensusDate" = "CensusDate"))
                                             

# Remove LOC_NAME column now that dataframes have been merged
todays_census_covid_merge$LOC_NAME <- NULL


# replace NAs with 0
todays_census_covid_merge[is.na(todays_census_covid_merge$COVID19), "COVID19"] <- 0
todays_census_covid_merge[is.na(todays_census_covid_merge$PUI), "PUI"]         <- 0
todays_census_covid_merge[is.na(todays_census_covid_merge$PUM), "PUM"]         <- 0
todays_census_covid_merge[is.na(todays_census_covid_merge$SUSC), "SUSC"]       <- 0 

# Estimate Covid Utilization 
todays_census_covid_merge <- todays_census_covid_merge %>% 
                               mutate(COVIDUtilization=round((COVID19+PUI+PUM)/TOTAL_BED_CENSUS, 3))


# Map units to Premier name, Unit Type, Include, etc.
unit_mapping$LOC_NAME <- NULL
todays_census_covid_merge <- left_join(todays_census_covid_merge,unit_mapping[],
                                       by = c("DEPARTMENT_NAME" =  "DEPARTMENT_NAME"))
                                               

todays_census_covid_merge$PremierMapping <- ifelse(str_detect(todays_census_covid_merge$PremierUnit,
                                                  "NO PREMIER MAPPING"), FALSE, TRUE)



# Determine if unit is a virtual unit added as part of surge planning
todays_census_covid_merge$VirtualUnit <- str_detect(todays_census_covid_merge$DEPARTMENT_NAME,
                                           paste(c("VIRTUAL", "\\sVU[0-9]+", "\\sVU\\s[A-Z]"),  collapse = "|"))


# Remove any data with today's census data to ensure no duplicates
census_date <- unique(todays_census_covid_merge$CensusDate)
repo <- repo %>%  filter(CensusDate != census_date )

# Remove data sets
rm(todays_covid_summary, todays_covid_data, todays_covid_cast )
rm(todays_census_subset, todays_census_data, unit_mapping, site_mapping)

# Bind today's data with repository
new_repo <- rbind(repo, todays_census_covid_merge)
new_repo <- new_repo %>% mutate(COVIDUtilization= round(COVIDUtilization, digits = 3))
new_repo <- unique(new_repo)


new_repo <- new_repo %>% filter(!is.na(Site))


# Save the new repo
start_date <- min(new_repo$CensusDate)
end_date <- max(new_repo$CensusDate)

write_xlsx(new_repo, path = paste0(Census_path, "\\REPO\\Census and Covid Repo ", start_date, " to ", end_date, " Created ", Sys.Date(), " Add MSBI.xlsx"))



# Rename deactivated units before running markdown
new_repo <- new_repo %>% mutate(DEPARTMENT_NAME = ifelse(is.na(str_extract(DEPARTMENT_NAME,
                                 "(?<=X_).+(?=_DEACTIVATED)")), DEPARTMENT_NAME,
                                    str_extract(DEPARTMENT_NAME,  "(?<=X_).+(?=_DEACTIVATED)")))
                                             

rm(repo, todays_census_covid_merge)

### Pull in MSSN Data
setwd("../MSSN COVID")
mssn_path <- getwd()

# Read the new MSSN data
mssn <- read_excel(paste0(mssn_path, "\\Patient Details - ", Sys.Date(), ".xlsx"))

# read mapping mssn
unit_mapping_mssn_covid  <- read_excel("MSSN Unit Mapping 12-15-2020.xlsx", col_names = TRUE, na = c("", "NA"))

setwd(Census_path)
setwd("../..")

#setwd("C:\\Users\\aghaer01\\Downloads")

save_output <- paste0(getwd(), "\\Daily Reporting Output")
rmarkdown::render("Code\\Epic Unit and COVID Census Analysis REPORT 2020-12-15_Inc_MSSN_New_Report_v2.Rmd", output_file = paste("Epic Census and Covid Daily Reporting", Sys.Date()), output_dir = save_output)







                          

