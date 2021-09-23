# Code for analyzing COVID Surge Med data
# Last Updated 2021_09_014

rm(list=ls())

# Import Libraries

library(readxl)
library(writexl)
library(stringr)
library(tidyverse)
library(dplyr)



# Work Directory
wrk.dir <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/COVID Pharmacy/Data/Epic Latest Reports/Daily Reports"
setwd(wrk.dir)


# Import COVID Surge Med WINV data
new_inv_list <- list.files(path= paste0(wrk.dir,"/Med Inventory - Phase2"), full.names=TRUE, pattern = paste0("COVID Surge Med WINV Balance_", Sys.Date()) )
new_inventory_raw <-  read_excel(new_inv_list, col_names = TRUE, na = c("", "NA"))

#Add Date Column
new_inventory_raw <- new_inventory_raw %>% mutate(UpdateDate = as.Date(LAST_UPDATE_TIME, format = "%d-%b-%y"),
                                                  ReportDate =  as.Date(max(LAST_UPDATE_TIME),origin = "2020-01-01")-1)

                                                                      

# Add Site 
new_inventory_raw <- new_inventory_raw %>% mutate(Site = ifelse(str_detect("MSHS COVID 19 Stockpile", INV_NAME), "MSHS Stockpile",
                                                    str_replace(str_extract(INV_NAME, "[A-Z]+(\\s|\\-)"), "\\s|\\-", "")),
                                                    Site=ifelse(Site== "BI","MSBI" , Site))


# Extract inventory item concentration 
# Pattern 1: x MCG/y ML
conc_pattern_1 <- "[0-9]+(\\.|\\,)*[0-9]*\\s(MCG/)[0-9]*(\\.|\\,)*[0-9]*\\s*(ML)"

# Pattern 2: x MG/y ML
conc_pattern_2 <- "[0-9]+(\\.|\\,)*[0-9]*\\s(MG/)[0-9]*(\\.|\\,)*[0-9]*\\s*(ML)"

# Pattern 3: x MG 
conc_pattern_3 <- "[0-9]+(\\.|\\,)*[0-9]*\\s(MG)\\s"

# Pattern 4: x UNIT/ y ML
conc_pattern_4 <- "[0-9]+(\\.|\\,)*[0-9]*\\s(UNIT/)[0-9]*(\\.|\\,)*[0-9]*\\s*(ML)"

# Extract inventory concentration from PRD_NAME
new_inventory_raw <- new_inventory_raw %>%
  mutate(ConcExtract = ifelse(str_detect(PRD_NAME, conc_pattern_1), str_extract(PRD_NAME, conc_pattern_1),
                              ifelse(str_detect(PRD_NAME, conc_pattern_2), str_extract(PRD_NAME, conc_pattern_2),
                                     ifelse(str_detect(PRD_NAME, conc_pattern_3), str_extract(PRD_NAME, conc_pattern_3),
                                            ifelse(str_detect(PRD_NAME, conc_pattern_4), str_extract(PRD_NAME, conc_pattern_4), NA)))))




