
# MSSN Data Analytics

rm(list=ls())

library(readxl)
library(stringr)
library(tidyverse)
library(dplyr)


# Repo work directory
repo_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Covid IP Staffing Model/Data/Epic Census Data"
setwd(repo_path)

# Import Repo
repo <- list.files(path = paste0(repo_path,"/REPO"), full.names = T , pattern =paste0("Census and Covid Repo 2020-03-12 to ", Sys.Date()-1  ))
repo <-  read_excel(repo, col_names = TRUE, na = c("", "NA"))
max(repo$CensusDate)

# Set MSSN working directory
setwd("../MSSN COVID")
mssn_path <- getwd()

# Import mssn data
mssn <- read_excel(paste0(mssn_path, "\\Patient Details - ", Sys.Date()-1, ".xlsx"))


# Status column
colnames(mssn)[colnames(mssn) == "COVID +  Cond"] <- "status"
mssn <- mssn %>% mutate(status=ifelse(status=="Covid+", "COVID19", status))

# Create Unit Type High
mssn <- mssn %>% mutate(`Unit Type High` = ifelse(`Pat Class` %in% "I", "IP", ifelse(`Pat Class` %in% "E", "ED", "Other")))


# Change date format
mssn <- mssn %>% mutate(AdmitDate = as.Date(`Admit date`, "%m/%d/%Y"),
                    DischargeDate = as.Date(`Discharge Date`, "%m/%d/%Y"))


# Creat LOS
mssn <- mssn %>% mutate(end_date = ifelse(is.na(DischargeDate), Sys.Date(), DischargeDate))
mssn <- mssn %>%  mutate(end_date = as.Date(end_date, "1970-01-01"),
                         los = as.integer(end_date - AdmitDate))  %>% filter(los > 0)


#Create Census Date
mssn <- mssn[rep(seq(nrow(mssn)), times = mssn$los),]
mssn <- mssn %>% group_by(Visitid, Medrec, status, `Unit Type High`, AdmitDate, DischargeDate, end_date) %>%
                                                    mutate(number = 1:n(), CensusDate = AdmitDate + (number - 1)) 

mssn <- subset(mssn, select = -c(`Month of Admit date`, `Admit date`, `Discharge Date`,number))


# Death or discharge
mssn <- mssn %>% mutate(outcome= ifelse(str_detect(`Dispsn Desc`, "TRANS")|`Dispsn Desc` == "DIS/TRNS TO ICF/GRP HOME"| 
                                          `Dispsn Desc` == "HOSPICE- MED FACILITY","Care Facility",
                                        ifelse(str_detect(`Dispsn Desc`, "HOME"), "Home Care",  `Dispsn Desc` )))

mssn<- mssn  %>% mutate(outcome= replace(outcome, outcome=="LEFT AMA","ROUTINE DISCHARGE" ))


# ip_covid_MSSN_census
mssn_trend <- mssn %>% group_by(CensusDate, `Unit Type High`) %>%  summarise(COVID19 = n())


mssn_trend <- mssn_trend %>%  mutate(Site = "MSSN", SUSC = 0, PUI = 0, PUM = 0) %>% 
  select(Site, CensusDate, COVID19, SUSC, PUI, PUM, `Unit Type High` )   


mssn_trend <- mssn_trend %>%  mutate(total = `COVID19` + SUSC + PUI+ PUM, CensusDate= as.Date(CensusDate, format="%Y-%m-%d") )

ggplot(mssn_trend, aes(x=CensusDate, y=COVID19, fill=`Unit Type High`, col = `Unit Type High` ))+ 
  geom_line(size= 0.95)+ 
  labs(x="Date", y= "Covid19 Cases", title = "Covid19 Cases Across Time")+
  theme(plot.title = element_text(size = 16, vjust = 2), 
        legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal',
        legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# Add to repo
#repo <- repo %>% mutate(`Unit Type High` = ifelse(UnitType != "ED", "IP", "ED"))
#msn_trend <- subset(mssn_trend, select = -c(total))
#new_repo_merge <- dplyr::bind_rows(repo, mssn_trend)



# Sum of Covid data cases by CensusDate
covid_status_mssn <- mssn_trend[,c("Site","CensusDate",  "COVID19", "SUSC", "PUI", "total")]
covid_status_mssn <- covid_status_mssn %>% group_by(CensusDate) %>% summarise(total=sum(total))
covid_status_mssn <- covid_status_mssn %>% mutate(CensusDate= as.Date(CensusDate, format="%Y-%m-%d"))

ggplot(covid_status_mssn, aes(x=CensusDate, y=total ))+ 
  geom_line(size= 0.95)+ 
  labs(x="Date", y= "Covid19 Cases", title = "Covid19 Cases Across Time")+
  theme(plot.title = element_text(size = 16, vjust = 2), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# Visualization of Los and Discharge Outcome
ggplot(mssn, aes(x=outcome, y=los, fill=`Unit Type High`)) + geom_boxplot()+
       labs(x="Discharge Outcome", y= "Lenght of Stay", title = "LOS vs Discharge Outcome")+
        theme(plot.title = element_text(size = 16, hjust = 0.5), legend.position = "top", legend.box = "vertical",
              legend.title = element_text(size = 10), legend.text = element_text(size = 10),
              axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
              axis.text.y = element_text(size = 10))



mssn_outcome_trend <- mssn %>% group_by(CensusDate, outcome) %>% summarise(LOS= mean(los))
mssn_outcome_trend <- mssn_outcome_trend %>% mutate(CensusDate= as.Date(CensusDate, format="%Y-%m-%d"))
mssn_outcome_trend <- mssn_outcome_trend %>% mutate(outcome=factor(outcome, levels = c("Care Facility", "EXPIRED", "Home Care", "NOT DISCHARGED", "ROUTINE DISCHARGE" )))


ggplot(mssn_outcome_trend, aes(x=CensusDate, y=LOS, fill=outcome, col = outcome ))+ 
      geom_line(size= 0.95)+ 
       labs(x="Date", y= "Lenght of Stay", title = "Lenght of Stay Across Time")+
       theme(plot.title = element_text(size = 16, vjust = 2), 
        legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal',
        legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))
        

            
                      
# Other info 
mssn <- mssn %>% mutate(diag= str_extract(Primary_Dx, "COVID-19"))
mssn <- mssn %>% mutate(diag= ifelse(is.na(diag), str_extract(Primary_Dx, "SEPSIS"), diag ))
mssn <- mssn %>% mutate(diag= ifelse(is.na(diag), str_extract(Primary_Dx, "PNEUMONIA"), diag ))








                     


