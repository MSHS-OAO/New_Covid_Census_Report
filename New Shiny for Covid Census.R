

# Shiny App for Covid Census

rm(list=ls())

#install.packages("shinythemes")

# Import Libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(reshape2)
library(data.table)
library(plotly)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(patchwork)
library(gridExtra)
library(grid)
library(scales)
library(DT)


# Import Data Sets   =====================================

# define working directory
# Set working directory
Census_path <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Covid IP Staffing Model/Data/Epic Census Data/REPO"
setwd(Census_path)



# Read the census repo file countaining the MSSN data
covid_data <- read_excel(paste0("Census and Covid Repo 2020-03-12 to ", Sys.Date()-1, " Created ", Sys.Date(), " Add MSSN.xlsx"))

max(covid_data$CensusDate)

# Report Update date
repo_start_date <- format(min(covid_data$CensusDate), "%m-%d-%Y")
repo_end_date <- format(max(covid_data$CensusDate), "%m-%d-%Y")

report_run_date <- Sys.time()

start_date <- Sys.Date()-months(3)


## MSHS =====================================

## Data for main_plot11 and 12
capacity_data_mshs <- covid_data %>% filter(CensusDate == max(CensusDate), `Unit Type` != "ED" ) %>%
                                     select(Site, `COVID19`, SUSC, PUI, PUM, `non-COVID19`, `Open Beds`)
capacity_data_mshs[is.na(capacity_data_mshs)] <- 0

covid_status <- covid_data %>%  filter(CensusDate == max(CensusDate)) %>% select(Site, `COVID19`, SUSC, PUI)
max_sum <- covid_status %>%  mutate(sum = `COVID19` + SUSC + PUI) %>% group_by(Site) %>% summarise(sum(sum))

covid_status <- reshape2::melt(covid_status, id.vars = c("Site"))
covid_status <- covid_status %>% group_by(Site, variable) %>% summarise(total = sum(value))

covid_loc <- covid_data %>% filter(CensusDate == max(CensusDate)) %>% select(Site, `COVID19`, SUSC, PUI, `Unit Type High`)
covid_loc <- reshape2::melt(covid_loc, id.vars = c("Site","Unit Type High"))
covid_loc <- covid_loc %>% group_by(Site, `Unit Type High`) %>% summarise(total = sum(value))
covid_loc <- covid_loc %>%  mutate(`Unit Type High`= ifelse(is.na(`Unit Type High`), "Other", `Unit Type High`))


## Adult med surge Unit
selected_site <- unique(covid_data$Site)
Adult_med_surge <- covid_data %>% filter(CensusDate < Sys.Date(), `Unit Type` == "Adult Med Surg", VirtualUnit == "FALSE")
Adult_med_choices <- sort(unique(Adult_med_surge$DEPARTMENT_NAME[Adult_med_surge$Site %in% selected_site ]))



## Virtual Unit
Virtual_Unit <- covid_data %>% filter(CensusDate < Sys.Date(),  VirtualUnit == "TRUE") 
Virtual_unit_choices <- sort(unique(Virtual_Unit$DEPARTMENT_NAME[Virtual_Unit$Site %in% selected_site ]))

# Shiny UI   ==================================================


ui <- dashboardPage( 
      dashboardHeader(title= "Covid Census Analysis", titleWidth = 250),
  
      
      dashboardSidebar(width = 250,
                       sidebarMenu(menuItem("Home", tabName = "home", icon = icon("home")),
                                   menuItem("MSHS", tabName = "mshs", icon = icon("th")),
                                   menuItem("Sites", tabName = "sites", icon = icon("th")))),
  
  
      dashboardBody(
           tabItems(
           # Objective
           tabItem(tabName = "home",
                   column(12, 
                          tags$div("MSHS Census and Utilization Analysis", style = "color:	#221f72; font-weight:bold; font-size:34px; margin-left: 20px" ,
                                h3("Health System Operations"),
                                h4(paste0("Report Run Date: ",report_run_date )),
                                h4(paste0("Data Date Range: ",repo_start_date, " to ",repo_end_date )))),
              
              
                   column(12, 
                          tags$div( id = "Objective", style= "color:	#221f72; margin-left: 20px",
                                h3("Description:"),
                                 p("This dashboard summarizes MSHS COVID-19 census. The data is stratified by site, infection status, and patient setting.",
                                    style= "font-size:16px"))),
              
                  column(12, 
                         tags$div( id= 'data', style= "color:	#221f72; margin-left: 20px",
                               h3("Data Sources:"),
                                p("- Epic midnight census from prior day (ADT_Bed_Census_Daily_By_Dept_Summary.rpt)"),
                                p("- Epic infection flag report from prior day (COVID Census Prior Day.rpt)"),
                                p("- MSSN bed census report from MSSN Tableau Dashboard (Patient Details)"))),
              
                 column(12,
                        tags$div(id= "data description", style= "color:	#221f72; font-size:14px; margin-left: 20px",
                              h3("Data Description"),
                              h5("*Only includes Adult Med Surg, ICU, and ED."),
                              h5("**Inaccurate census data on 5/17/2020 and 1/19/2021 due to an Epic Upgrade and outage, respectively."),
                              h5("***COVID census data not available for 7/11/2021."),
                              h5("****MSBI: data starts from its EPIC go-live date on 8/9/2020."),
                              h5("****MSSN: ED vs. IP breakdown is not availble due to data limitation."),
                              h5("****MSSN: Excludes PUI, PUM, and SUSC data due to data limitations."))),
                  ),
      
      
      # Second tabItem for MSHS
      tabItem(tabName = "mshs",
              div("MSHS Covid Census Report", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
              fluidRow(
                column(11, 
                       box(plotOutput("mshs_plot1"),  width= 6, offset= 1,
                           title = "By Infection Status", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE),
                       box(plotOutput("mshs_plot2"),  width= 6,
                           title = "By Patient Setting", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE)),
             
                column(11,
                       box(plotOutput("mshs_plot3"),  width= 12,
                           title = "By Infection Status", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE )),
                
                column(11,
                       box(plotOutput("mshs_plot4"),  width= 12,
                           title = "By Patient Setting", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE )),
                
                column(11,
                       box(plotOutput("mshs_plot5"),  width= 12,
                           title = "Trend By Patient Setting", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           box(width = 3,
                               pickerInput(inputId = "MSHS_UnitTypeHigh1", label = strong("Select Unit Type"), multiple = T,
                                           choices = c("ED", "IP"),
                                           selected = "IP", width =250)))),
                column(11,
                       box(plotOutput("mshs_plot6"),  width= 6,
                           title = "Yearly Comparison By Infection Status", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           selectInput(inputId = "MSHS_PatientType", label = strong("Patient Type"),
                                       choices = c("COVID19", "SUSC", "PUI" ),
                                       selected = "COVID19", width= 250)),
               
                       box(plotOutput("mshs_plot7"),  width= 6,
                           title = "Yearly Comparison By Patient Setting", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           selectInput(inputId = "MSHS_UnitTypeHigh", label = strong("Select Unit Type"),
                                       choices = c("IP", "ED"),  selected = "IP", width =250))),
                                      
                
                # Filter For MSHS Sites ==================================================================================  
                           
              
                tags$head(tags$style(HTML("#FiltersUpdate; {font-size: 18px; position: absolute; left: 35px; top: 53px; height 85%;}"))),
                tags$head(tags$style(HTML("#dropdownbutton; {position: absolute;left: 25px; top: 53px; }"))),
                tags$style(".fa-filter {color:#7f7f7f}"),
                
                                                
                
                dropdown(style = "material-circle", size = "lg", right = T, status = "default",
                         tooltip = tooltipOptions(title = "click to update graphs."),
                         icon = icon("filter"), width = "300px",
                         inputId = "dropdownbutton",
                         
                         
                         br(),
                         actionButton("FiltersUpdate", "CLICK TO UPDATE", width = "80%", height = "100px"),
                         br(),
                         
                         
                         box(width = 12, height = "100px",
                             title = "Select Date Range:",
                             solidHeader = FALSE, 
                             dateRangeInput("DateRange", label = NULL,
                                            start = start_date, end = Sys.Date()-1,
                                            min = min(covid_data$CensusDate), max = max(covid_data$CensusDate)))
                ) # close dropdown
                
            ) # close fluidrow
                          
      ), # Close MSHS Tabitem
      
      ### tabItem for Sites  =========================================================================================
       
      
      tabItem(tabName = "sites",
              fluidRow(
                column(11,
                       box(plotOutput("site_plot1"),  width= 12,
                           title = "By Infection Status", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE)),
                          
              
                
                column(11,
                       box(plotOutput("site_plot2"),  width= 12,
                           title = "By Patient Setting", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE )),
                
                column(11,
                       box(plotOutput("site_plot3"),  width= 12,
                           title = "Trend By Patient Setting", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           box(width= 3,
                               pickerInput(inputId = "site_UnitTypeHigh", label = strong("Select Unit Type"), multiple = T, 
                                           choices = c("IP", "ED"), selected = "IP", width =250)))),
                                           
                
               
                  column(11,
                         box(plotOutput("site_plot4"),  width= 12,
                             title = "Adult Surg Med: Total COVID-19 Patients Census", status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             box(width=3, 
                                 pickerInput(inputId = "site_Unit", label = strong("Select Unit"), 
                                             choices = Adult_med_choices, selected = Adult_med_choices, width =250, multiple = TRUE)))),
                
                                             
              
                column(11,
                       box(plotOutput("site_plot5"),  width= 12,
                           title = "Virtual Units: Total COVID-19 Patients Census", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           box(width=3,
                               pickerInput(inputId = "site_VUnit", label = strong("Select Unit"), multiple = TRUE,
                                           choices = Virtual_unit_choices, selected =  Virtual_unit_choices, width =250)) )), 
                                           
                           
                column(11,
                       box(plotOutput("site_plot6"),  width= 6,
                           title = "Yearly Comparison by Infection Status", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           selectInput(inputId = "site_PatientType", label = strong("Select Patient Type"),
                                       choices = c("COVID19", "SUSC", "PUI", "PUM"),
                                       selected = "COVID19", width =250)),
                
               
                       box(plotOutput("site_plot7"),  width= 6,
                           title = "Yearly Comparison by  Patient Setting", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           selectInput(inputId = "site_UnitTypeHigh1", label = strong("Select Unit Type"),
                                       choices = c("IP", "ED") ,
                                       selected = "IP", width =250))),
              
              
              
              
              tags$head(tags$style(HTML("#dropdownbuttonSite; {position: absolute;left: 25px;top: 53px; }"))),
              tags$style(".fa-filter {color:#7f7f7f}"),
              
              
              dropdown(style = "material-circle", size = "lg", right = T, status = "default",
                       tooltip = tooltipOptions(title = "click to update graphs."),
                       icon = icon("filter"), width = "300px",
                       inputId = "dropdownbuttonSite",
                       
                       
                       br(),
                       actionButton("FiltersUpdate1", "CLICK TO UPDATE", width = "80%", height = "100px"),
                       br(),
                       
                       box(width = 12, height = "100px", title = "Select Site:", solidHeader = F,
                           pickerInput("selectedSite",label= NULL, multiple= F,
                                       choices = sort( unique(covid_data$Site)),  selected = "MSB")),
                                       
                       
                       box(width = 12, height = "100px",
                           title = "Select Date Range:", solidHeader = FALSE, 
                           dateRangeInput("DateRange1", label = NULL, start = start_date, end = Sys.Date()-1,
                                          min = min(covid_data$CensusDate), max = max(covid_data$CensusDate)))
                       
                       
              ) # close dropdown
           ) # close fluidrow
        )# Close Site Tabitem
      
     )
    
  )
  
) 
      
      
      
server <- function(input, output, session) { 
  
          MSHS_Data  <- eventReactive(input$FiltersUpdate, {
          validate( need(input$DateRange[1] < input$DateRange[2], "Error: Start date should be earlier than end date."))
          covid_data %>% filter( 
                          CensusDate > as.Date(input$DateRange[1]) & CensusDate <= as.Date(input$DateRange[2] ))
          }, ignoreNULL = FALSE)
  
  
        # MSHS Hospitalized COVID-19 by infection status
        output$mshs_plot1 <- renderPlot({
          
          ggplot(covid_status, 
                 aes(x=Site, y=total, fill=factor(variable,levels=c("PUI","SUSC","COVID19"))))+
            geom_bar(position="stack",stat="identity", width=0.7)+
            scale_fill_manual(values=c("#7f7f7f","#E69F00","#212070"))+
            ggtitle(label="\nHospitalized COVID-19 Census (ED and IP) \nby Infection Status")+
            labs(x=NULL, y="Beds Occupied")+
            guides(fill=guide_legend(title="Status"))+
            theme_bw()+
            theme(plot.title = element_text(size = 16, hjust = 0.5), legend.position = "top", legend.box = "vertical",
                  legend.title = element_text(size = 10), legend.text = element_text(size = 10),
                  axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
                  axis.text.y = element_text(size = 10),
                  panel.grid.major = element_line(color = "lightgrey"),
                  panel.grid.minor = element_line(color = "lightgrey"))+
            scale_y_continuous(limits = c(0, max(max_sum$`sum(sum)`)*1.2))+
            geom_text(aes(label=total), color="white", 
                      size=3, position = position_stack(vjust = 0.5))+
            stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Site), 
                         geom="text", color="black", 
                         size=3)

        })
        
        output$mshs_plot2 <- renderPlot({
          ggplot(covid_loc, 
                 aes(x=Site, y=total, fill=factor(`Unit Type High`,levels=c("ED","IP","Other"))))+
            geom_bar(position="stack",stat="identity", width=0.7)+
            scale_fill_manual(values=c("#00aeef","#d80b8c","#863198"))+
            ggtitle(label="\nHospitalized COVID-19 Census \nby ED vs. IP")+
            labs(x=NULL, y="Beds Occupied")+
            guides(fill=guide_legend(title="Unit Type"))+
            theme_bw()+
            theme(plot.title = element_text(size = 16, hjust = 0.5), legend.position = "top", legend.box = "vertical",
                  legend.title = element_text(size = 10), legend.text = element_text(size = 10),
                  axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
                  axis.text.y = element_text(size = 10),
                  panel.grid.major = element_line(color = "lightgrey"),
                  panel.grid.minor = element_line(color = "lightgrey"))+
            scale_y_continuous(limits = c(0, max(max_sum$`sum(sum)`)*1.2))+
            geom_text(aes(label=total), color="white", 
                      size=3, position = position_stack(vjust = 0.5))+
            stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Site), 
                         geom="text", color="black", 
                         size=3)
          
        })
        
        
        
        
        # MSHS Hospitalized Census by Infection Status
        output$mshs_plot3 <- renderPlot({
          
          mshs_inf <- MSHS_Data()
          
          covid_pts_trend <- mshs_inf %>% filter(CensusDate < Sys.Date()) %>% select(CensusDate,`COVID19`, SUSC, PUI, PUM) %>%
            mutate(CensusDate = as.Date(CensusDate))
          
          covid_pts_trend <- reshape2::melt(covid_pts_trend, id.vars = c("CensusDate"))
          covid_pts_trend <- covid_pts_trend %>% mutate(CensusDate = as.Date(CensusDate, format="%Y-%m-%d"))
          covid_pts_trend <- aggregate(covid_pts_trend$value, by=list(covid_pts_trend$CensusDate,covid_pts_trend$variable), FUN=sum)
          colnames(covid_pts_trend) <- c("Date","Patient Type","value")
          
          covid_census_max <- covid_pts_trend %>% group_by(Date) %>% summarise(sum = sum(value))
          
          
          ggplot(covid_pts_trend, 
                 aes(x=Date, y=value, fill=factor(`Patient Type`,levels=c("PUM","PUI","SUSC","COVID19"))))+
            geom_bar(position="stack",stat="identity", width=0.5)+
            scale_fill_manual(values=c("#d80b8c",	"#00aeef","#E69F00","#212070"))+
            ggtitle(label="\nMSHS Hospitalized Census by Infection Status (ED and IP)")+
            labs(x=NULL, y="Beds Occupied")+
            guides(fill=guide_legend(title="Patient Type"))+
            theme_bw()+
            theme(plot.title = element_text(size = 16, vjust = 2), 
                  legend.position='top', 
                  legend.justification='left',
                  legend.direction='horizontal',
                  legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
                  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
                  axis.text.y = element_text(size = 10),
                  panel.grid.major = element_line(color = "lightgrey"),
                  panel.grid.minor = element_line(color = "lightgrey"))+
            scale_y_continuous(limits = c(0, max(covid_census_max$sum)*1.2))+
            scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))+
            geom_text(aes(label=value), color="white", 
                      size=2, position = position_stack(vjust = 0.5))+
            stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Date), 
                         geom="text", color="black", 
                         size=3)
        })
        
  
    
        
        
        output$mshs_plot4 <- renderPlot({
          
          mshs_pts <- MSHS_Data()
          
          covid_pts_census <- mshs_pts %>% filter(CensusDate < Sys.Date()) %>% select(CensusDate,`COVID19`, SUSC, PUI, PUM, `Unit Type High`) %>%
            mutate(total = COVID19 + SUSC + PUI + PUM) %>% group_by(CensusDate, `Unit Type High`) %>%  summarise(total = sum(total))
          
          covid_pts_census$CensusDate <- as.Date(covid_pts_census$CensusDate, format="%Y-%m-%d")
          covid_pts_census <- covid_pts_census %>% group_by(CensusDate, `Unit Type High`) %>% summarise(total = sum(total))
          covid_pts_census$`Unit Type High`[which(is.na(covid_pts_census$`Unit Type High`))] <- "Other"
          
          covid_pts_max <- covid_pts_census %>% group_by(CensusDate) %>% summarise(sum = sum(total))
          
          ggplot(covid_pts_census, 
                 aes(x=CensusDate, y=total, fill=factor(`Unit Type High`,levels=c("ED","IP","Other"))))+
            geom_bar(position="stack",stat="identity", width=0.6)+
            scale_fill_manual(values=c("#d80b8c",	"#00aeef", "#863198"))+
            ggtitle(label="\nMSHS Hospitalized COVID-19 Patients Census by ED vs. IP")+
            labs(x=NULL, y="Beds Occupied")+
            guides(fill=guide_legend(title="Unit Type"))+
            theme_bw()+
            theme(plot.title = element_text(size = 16, vjust = 2), 
                  legend.position='top', 
                  legend.justification='left',
                  legend.direction='horizontal',
                  legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
                  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
                  axis.text.y = element_text(size = 10),
                  panel.grid.major = element_line(color = "lightgrey"),
                  panel.grid.minor = element_line(color = "lightgrey"))+
            scale_y_continuous(limits = c(0, max(covid_pts_max$sum)*1.2))+
            scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))+
            geom_text(aes(label=total), color="white", 
                      size=2, position = position_stack(vjust = 0.5))+
            stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = CensusDate), 
                         geom="text", color="black", 
                         size=3)
        })
        
        
        #Hospitalized covid19 patients census trend by unit type 
        updatePickerInput(session = session, inputId = "MSHS_UnitTypeHigh1", choices = c("ED", "IP"), selected = "IP")
        
        output$mshs_plot5 <- renderPlot({
          
          mshs_pts <- MSHS_Data()
          
          covid_pts_census <- mshs_pts %>% filter(CensusDate < Sys.Date(), `Unit Type High` %in% input$MSHS_UnitTypeHigh1 ) %>%
                                           select(CensusDate,`COVID19`, SUSC, PUI, PUM, `Unit Type High`) %>%  mutate(total = COVID19 + SUSC + PUI + PUM) %>% 
                                           group_by(CensusDate, `Unit Type High`) %>%  summarise(total = sum(total))
          
          covid_pts_census$CensusDate <- as.Date(covid_pts_census$CensusDate, format="%Y-%m-%d")
          covid_pts_census <- covid_pts_census %>% group_by(CensusDate, `Unit Type High`) %>% summarise(total = sum(total))
          covid_pts_census$`Unit Type High`[which(is.na(covid_pts_census$`Unit Type High`))] <- "Other"
          
          covid_pts_max <- covid_pts_census %>% group_by(CensusDate) %>% summarise(sum = sum(total))
        
          
          
          ggplot(data = covid_pts_census) +
            geom_point(aes(x = CensusDate, y = total, shape = `Unit Type High`, color = `Unit Type High`)) +
            geom_line(aes(x = CensusDate, y = total, color = `Unit Type High`)) +
            scale_shape_manual(values = 1:4)+
            scale_color_manual(values=c("#d80b8c",	"#00aeef", "#863198"))+
            labs(title = "\nMSHS Hospitalized COVID-19 Patients Census Trend", 
                 x = NULL, y = "Census", shape = "Unit Type", color = "Unit Type") +
            scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))+
            theme_bw() +
            theme(plot.title = element_text(size = 16, vjust = 2), 
                  legend.position='top', 
                  legend.justification='left',
                  legend.direction='horizontal',
                  legend.title = element_text(size = 10), legend.text = element_text(size = 10),
                  axis.text.x = element_text(size = 10, angle = 45,hjust = 1), 
                  axis.text.y = element_text(size = 10),
                  panel.grid.major = element_line(color = "lightgrey"),
                  panel.grid.minor = element_line(color = "lightgrey"))
        })
        
        
        #Hospitalized covid19 patients census Year over year by infection status 
        updateSelectInput(session = session, inputId = "MSHS_PatientType", choices = c("COVID19", "SUSC", "PUI", "PUM" ),selected = "COVID19")
        
        output$mshs_plot6 <- renderPlot({
          
          mshs_trend <- MSHS_Data()
          
          covid_pts_trend <- mshs_trend %>% filter(CensusDate < Sys.Date() ) %>% 
            select(CensusDate,`COVID19`, SUSC, PUI, PUM) %>% mutate(CensusDate = as.Date(CensusDate))
            
          
          covid_pts_trend <- reshape2::melt(covid_pts_trend, id.vars = c("CensusDate"))
          covid_pts_trend <- covid_pts_trend %>% mutate(CensusDate = as.Date(CensusDate, format="%Y-%m-%d"))
          covid_pts_trend <- aggregate(covid_pts_trend$value, by=list(covid_pts_trend$CensusDate,covid_pts_trend$variable), FUN=sum)
          colnames(covid_pts_trend) <- c("CensusDate","Patient Type","value")
          
          
          
          ##  year over year comparison by infection status
          covid_pts_trend_avg <- covid_pts_trend %>% mutate(year= substr(CensusDate, 1, 4),
                                                            month = substr(CensusDate, 6,7 ))
          
          covid_pts_trend_avg <- covid_pts_trend_avg %>% filter(`Patient Type` %in% input$MSHS_PatientType)%>%
                                  group_by(year, month) %>% summarise(Average= ceiling( mean(value)))
          covid_pts_trend_avg <- covid_pts_trend_avg %>% mutate(month= month.abb[as.numeric(month)])
          covid_census_max <- covid_pts_trend_avg %>% group_by(year, month) %>% summarise(sum = sum(Average))
    
          
          ggplot(covid_pts_trend_avg) +
            geom_line(aes(x =month, y = Average, color = year, group = year)) +
            geom_point(aes(x = month, y = Average,  color = year)) +
            geom_text(aes(x =month, y = Average, label=ceiling( Average)), color="#212070", 
                      size=4, hjust=0.2, vjust=-1)+
            scale_shape_manual(values = 1:4)+
            scale_color_manual(values=c("#d80b8c",	"#00aeef", "#863198"))+
            ggtitle(label=paste0("\n","Monthly Average COVID-19 Patients Census Over Year"))+
            labs(x = NULL, y = "Census", shape = "Unit Type", color = "Unit Type") +
            scale_x_discrete(limits = month.abb)+
            labs(x = NULL, y = "Monthly Average Beds Occupied", color =" year") +
            theme_bw() +
            theme(plot.title = element_text(size = 16, vjust = 2),
                  legend.position='top', 
                  legend.justification='left',
                  legend.direction='horizontal',
                  axis.text.x = element_text(size = 10, angle = 45,hjust = 1), 
                  axis.text.y = element_text(size = 10),
                  panel.grid.major = element_line(color = "lightgrey"),
                  panel.grid.minor = element_line(color = "lightgrey"))+
            scale_y_continuous(limits = c(0, max(covid_census_max$sum)*1.2), breaks= pretty_breaks())
          
        })
        
        
        #### Year over year comparison by Patient setting
        updateSelectInput(session = session, inputId = "MSHS_UnitTypeHigh", choices = c("IP", "ED" ),selected = "IP")
        
        output$mshs_plot7 <- renderPlot({
          
           mshs_pts <- MSHS_Data()
          
          covid_pts_census <-mshs_pts %>% filter(CensusDate < Sys.Date()) %>% select(CensusDate,`COVID19`, SUSC, PUI, PUM, `Unit Type High`) %>%
            mutate(total = COVID19 + SUSC + PUI + PUM) %>% group_by(CensusDate, `Unit Type High`) %>%  summarise(total = sum(total))
          
          covid_pts_census$CensusDate <- as.Date(covid_pts_census$CensusDate, format="%Y-%m-%d")
          covid_pts_census <- covid_pts_census %>% group_by(CensusDate, `Unit Type High`) %>% summarise(total = sum(total))
          covid_pts_census$`Unit Type High`[which(is.na(covid_pts_census$`Unit Type High`))] <- "Other"
          
          
          ## Data for Year over year comparison monthly average
          covid_pts_avg <- covid_pts_census %>% mutate(year= substr(CensusDate, 1, 4), month = substr(CensusDate, 6,7 ))
          covid_pts_avg <- covid_pts_avg %>%  filter(`Unit Type High` %in% input$MSHS_UnitTypeHigh)  %>%
                                              group_by(year, month) %>% summarise(Average= ceiling( mean(total)))
          covid_pts_avg <- covid_pts_avg %>% mutate(month= month.abb[as.numeric(month)])
          covid_pts_max <- covid_pts_avg %>% group_by(year, month) %>% summarise(sum = sum(Average))
          
          
          ggplot(covid_pts_avg) +
            geom_line(aes(x =month, y = Average, color = year, group = year)) +
            geom_point(aes(x = month, y = Average,  color = year)) +
            geom_text(aes(x =month, y = Average, label=ceiling( Average)), color="#212070", 
                      size=4, hjust=0.2, vjust= -1)+
            ggtitle(label=paste0("\n","Monthly Average COVID-19 Patients Census Over Year"))+
            scale_shape_manual(values = 1:4)+
            scale_color_manual(values=c("#d80b8c",	"#00aeef", "#863198"))+
            scale_x_discrete(limits = month.abb)+
            labs(x = NULL, y = "Monthly Average Census", color =" year") +
            theme_bw() +
            theme(plot.title = element_text(size = 16, vjust = 2),
                  legend.position='top', 
                  legend.justification='left',
                  legend.direction='horizontal',
                  axis.text.x = element_text(size = 10, angle = 45,hjust = 1), 
                  axis.text.y = element_text(size = 10),
                  panel.grid.major = element_line(color = "lightgrey"),
                  panel.grid.minor = element_line(color = "lightgrey"))+
            scale_y_continuous(limits = c(0, max(covid_pts_max$sum)*1.2), breaks= pretty_breaks())
          
        })
        
        
        
        #Sites =============================================================================
        
        Site_Data  <- eventReactive(input$FiltersUpdate1, {
          validate( need(input$selectedSite != "" , "Please Select a Site"),
                    need(input$DateRange1[1] < input$DateRange1[2], "Error: Start date should be earlier than end date."))
          covid_data %>% filter(Site %in% input$selectedSite , 
                                CensusDate > as.Date(input$DateRange1[1]) & CensusDate <= as.Date(input$DateRange1[2] ))
        }, ignoreNULL = FALSE)
        
        
        
        output$site_plot1 <- renderPlot({

        site_data <- Site_Data()
        
        covid_pts_trend_site <- site_data  %>% filter(CensusDate < Sys.Date()) %>% select(CensusDate, Site, `COVID19`, SUSC, PUI, PUM)
        
        covid_pts_trend_site <- reshape2::melt(covid_pts_trend_site, id.vars = c("CensusDate", "Site"))
        covid_pts_trend_site$CensusDate <- as.Date(covid_pts_trend_site$CensusDate, format="%Y-%m-%d")
        covid_pts_trend_site <- aggregate(covid_pts_trend_site$value, 
                                          by=list(covid_pts_trend_site$CensusDate,covid_pts_trend_site$variable , covid_pts_trend_site$Site), FUN=sum)
        
        colnames(covid_pts_trend_site) <- c("Date","Patient Type", "Site", "value")
        covid_pts_trend_max_site <- covid_pts_trend_site %>% group_by(Date, Site) %>% summarise(sum = sum(value))
        
        
        ggplot(covid_pts_trend_site, 
               aes(x=Date, y=value, fill=factor(`Patient Type`,levels=c("PUM","PUI","SUSC","COVID19"))))+
          geom_bar(position="stack",stat="identity", width=0.7)+
          scale_fill_manual(values=c("#d80b8c",	"#00aeef","#E69F00","#212070"))+
          ggtitle(label=paste0("Hospitalized Census Infection Status (ED and IP)"))+
          labs(x=NULL, y="Beds Occupied")+
          guides(fill=guide_legend(title="Patient Type"))+
          theme_bw()+
          theme(plot.title = element_text(size = 16, vjust = 2), 
                legend.position='top', 
                legend.justification='left',
                legend.direction='horizontal',
                legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
                axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
                axis.text.y = element_text(size = 10),
                panel.grid.major = element_line(color = "lightgrey"),
                panel.grid.minor = element_line(color = "lightgrey"))+
          scale_y_continuous(limits = c(0, max(covid_pts_trend_max_site$sum)*1.2))+
          scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))+
          geom_text(aes(label=value), color="white", 
                    size=2, position = position_stack(vjust = 0.5))+
          stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Date), 
                       geom="text", color="black", 
                       size=3)
        
        })
        
        
        # Site Hospitalized Census by Infection Status 
        
        output$site_plot2 <- renderPlot({
          
          site_pts_data <- Site_Data()
          
          
          covid_pts_census_site <- site_pts_data  %>% filter(CensusDate < Sys.Date() )%>%
            select(CensusDate, Site, `COVID19`, SUSC, PUI, PUM, `Unit Type High`) %>% mutate(total = COVID19 + SUSC + PUI + PUM) %>%
            group_by(CensusDate, Site, `Unit Type High`) %>% summarise(total = sum(total))
          covid_pts_census_site <- covid_pts_census_site %>% mutate(`Unit Type High`=ifelse(is.na(`Unit Type High`), "Other", `Unit Type High`))
          
          covid_pts_census_site$CensusDate <- as.Date(covid_pts_census_site$CensusDate, format="%Y-%m-%d")
          covid_census_max_site <- covid_pts_census_site%>%  group_by(CensusDate, Site) %>% summarise(sum = sum(total))
          
          ggplot(covid_pts_census_site, aes(x=CensusDate, y=total, fill=`Unit Type High`))+
            geom_bar(position="stack",stat="identity", width=0.7)+
            scale_fill_manual(values=c("#d80b8c",	"#00aeef","#212070","#7f7f7f"))+
            ggtitle(label=paste0("\n","Hospitalized COVID-19 Patients Census by ED vs. IP"))+
            labs(x=NULL, y="Beds Occupied")+
            guides(fill=guide_legend(title="Unit Type"))+
            theme_bw()+
            theme(plot.title = element_text(size = 16, vjust = 2), 
                  legend.position='top', 
                  legend.justification='left',
                  legend.direction='horizontal',
                  legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
                  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
                  axis.text.y = element_text(size = 10),
                  panel.grid.major = element_line(color = "lightgrey"),
                  panel.grid.minor = element_line(color = "lightgrey"))+
            scale_y_continuous(limits = c(0, max(covid_census_max_site$sum)*1.2))+
            scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))+
            geom_text(aes(label=total), color="white", 
                      size=2, position = position_stack(vjust = 0.5))+
            stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = CensusDate), 
                         geom="text", color="black", 
                         size=3)
          
        }) 
        
        
        #Hospitalized covid19 patients census trend by unit type
        updatePickerInput(session = session, inputId = "site_UnitTypeHigh", choices = c("ED", "IP"), selected = "IP")
        
        output$site_plot3 <- renderPlot({
          
          site_pts_trend_data <- Site_Data()
          
          covid_pts_census <- site_pts_trend_data %>% filter(CensusDate < Sys.Date(), `Unit Type High` %in% input$site_UnitTypeHigh )%>%
                   select(CensusDate, Site, `COVID19`, SUSC, PUI, PUM, `Unit Type High`) %>% mutate(total = COVID19 + SUSC + PUI + PUM) %>%
            group_by(CensusDate, Site, `Unit Type High`) %>% summarise(total = sum(total))
          covid_pts_census <- covid_pts_census %>% mutate(`Unit Type High`=ifelse(is.na(`Unit Type High`), "Other", `Unit Type High`))
          
          covid_pts_census$CensusDate <- as.Date(covid_pts_census$CensusDate, format="%Y-%m-%d")
          
        
          
        ggplot(data = covid_pts_census) +
          geom_point(aes(x = CensusDate, y = total, shape = `Unit Type High`, color = `Unit Type High`)) +
          geom_line(aes(x = CensusDate, y = total, color = `Unit Type High`)) +
          scale_shape_manual(values = 1:4)+
          scale_colour_manual(values=c("#d80b8c",	"#00aeef","#212070","#7f7f7f"))+
          labs(title = paste0("\n Hospitalized COVID-19 Patients Census Trend"), 
               x = NULL, y = "Census", shape = "Unit Type", color = "Unit Type") +
          scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))+
          theme_bw() +
          theme(plot.title = element_text(size = 16, vjust = 2), 
                legend.position='top', 
                legend.justification='left',
                legend.direction='horizontal',
                legend.title = element_text(size = 10), legend.text = element_text(size = 10),
                axis.text.x = element_text(size = 10, angle = 45,hjust = 1), 
                axis.text.y = element_text(size = 10),
                panel.grid.major = element_line(color = "lightgrey"),
                panel.grid.minor = element_line(color = "lightgrey"))  
          
       
        })
        
        
        ## Site Adult Med Surg
        
        observeEvent(input$selectedSite,{
        
        unit_type_choices <-  sort(unique(Adult_med_surge$DEPARTMENT_NAME[Adult_med_surge$Site %in% input$selectedSite ]))
                                                           
        updatePickerInput(session, inputId = "site_Unit", choices = unit_type_choices, selected = unit_type_choices)
                          
        })
       
        output$site_plot4 <- renderPlot({
          
          site_ams_data <- Site_Data()
          
          covid_pts <- site_ams_data %>% filter(CensusDate < Sys.Date(), `Unit Type` == "Adult Med Surg", VirtualUnit == "FALSE") %>%
            mutate(total = COVID19 + SUSC + PUI + PUM) %>% filter(total > 0) %>% select(CensusDate, Site, DEPARTMENT_NAME, total) %>%
                                                          group_by(CensusDate, Site, DEPARTMENT_NAME) %>% summarise(total = sum(total))
          
          
          
          covid_pts$CensusDate <- as.Date(covid_pts$CensusDate, format="%Y-%m-%d")
          colnames(covid_pts)[colnames(covid_pts) == "DEPARTMENT_NAME"] <- "Unit"
          
          covid_pts <- covid_pts %>% filter(Unit %in% input$site_Unit)
                                
        
          
          validate(need(nrow(covid_pts)>0, "Please provide a different start date"))
          
          ggplot(covid_pts)+
            geom_point(aes(x=CensusDate, y=total, shape=Unit, color=Unit))+
            geom_line(aes(x=CensusDate, y=total, color=Unit))+
            scale_shape_manual(values = 1:length(unique(covid_pts$Unit)),
                               guide = guide_legend(nrow = ceiling(length(unique(covid_pts$Unit))/6)))+
            ggtitle(label=paste0("\n","Adult Med Surg: Total COVID-19 Patients Census"))+
            labs(x=NULL, y="Beds Occupied")+
            theme_bw()+
            theme(plot.title = element_text(size = 16, vjust = 2), 
                  legend.position='top', 
                  legend.justification='left',
                  legend.direction='horizontal',
                  legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
                  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
                  axis.text.y = element_text(size = 10),
                  panel.grid.major = element_line(color = "lightgrey"),
                  panel.grid.minor = element_line(color = "lightgrey"))+
            scale_y_continuous(limits = c(0, max(covid_pts$total)*1.2), breaks= pretty_breaks())+
            scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))
          
        })
        
        ## Virtual Unit
        observeEvent(input$selectedSite,{
          
          vunit_type_choices <-  sort(unique(Virtual_Unit$DEPARTMENT_NAME[Virtual_Unit$Site %in% input$selectedSite ]))
          
          updatePickerInput(session, inputId = "site_VUnit", choices = vunit_type_choices, selected = vunit_type_choices)
          
        })
        
        
        output$site_plot5 <- renderPlot({
          
          vu_data <- Site_Data()
          
          covid_pts_vu <- vu_data %>% filter(CensusDate < Sys.Date(),  VirtualUnit == "TRUE") %>%
            mutate(total = COVID19 + SUSC + PUI + PUM) %>% filter(total > 0) %>%
            select(CensusDate, Site, DEPARTMENT_NAME, total) %>%
            group_by(CensusDate, Site, DEPARTMENT_NAME) %>% summarise(total = sum(total))
          
          covid_pts_vu$CensusDate <- as.Date(covid_pts_vu$CensusDate, format="%Y-%m-%d")
          colnames(covid_pts_vu) <- c("CensusDate","Site","Unit","total")
          
          covid_pts_vu %>% filter( Unit %in% input$site_Unit)
                    
          
          validate(need(nrow(covid_pts_vu)>0, "Please provide a different start date"))
          
          graph <- ggplot(covid_pts_vu)+
            geom_point(aes(x=CensusDate, y=total, shape=Unit, color=Unit))+
            geom_line(aes(x=CensusDate, y=total, color=Unit))+
            scale_shape_manual(values = 1:length(unique(covid_pts_vu$Unit)),
                               guide = guide_legend(nrow = ceiling(length(unique(covid_pts_vu$Unit))/4)))+
            ggtitle(label=paste0("\n"," Virtual Units: Total COVID-19 Patients Census"))+
            labs(x=NULL, y="Beds Occupied",
                 caption = "\n*Epic virtual units with > 0 beds occupied.")+
            theme_bw()+
            theme(plot.title = element_text(size = 16, vjust = 2), 
                  legend.position='top', 
                  legend.justification='center',
                  legend.direction='horizontal',
                  legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
                  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
                  axis.text.y = element_text(size = 10),
                  panel.grid.major = element_line(color = "lightgrey"),
                  panel.grid.minor = element_line(color = "lightgrey"))+
            scale_y_continuous(limits = c(0, max(covid_pts_vu$total)*1.2), breaks= pretty_breaks())+
            scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))
          
          if(nrow(covid_pts_vu)>0){
            plot(graph)
          }
          
        })
        
        
        #Hospitalized covid19 patients census Year over year by infection status 
        updateSelectInput(session = session, inputId = "site_PatientType", choices = c("COVID19", "SUSC", "PUI", "PUM" ),selected = "COVID19")
        
     
        
        output$site_plot6 <- renderPlot({
          
          site_data <- Site_Data()
          
          covid_pts_trend_site <- site_data  %>% filter(CensusDate < Sys.Date()) %>% select(CensusDate, Site, `COVID19`, SUSC, PUI, PUM)
          covid_pts_trend_site <- reshape2::melt(covid_pts_trend_site, id.vars = c("CensusDate", "Site"))
          covid_pts_trend_site$CensusDate <- as.Date(covid_pts_trend_site$CensusDate, format="%Y-%m-%d")
          covid_pts_trend_site <- aggregate(covid_pts_trend_site$value, 
                                            by=list(covid_pts_trend_site$CensusDate,covid_pts_trend_site$variable , covid_pts_trend_site$Site), FUN=sum)
          colnames(covid_pts_trend_site) <- c("Date","Patient Type", "Site", "value")
          
          covid_trend_site_avg <- covid_pts_trend_site %>% mutate(year= substr(Date, 1, 4),  month = substr(Date, 6,7 ))
          covid_trend_site_avg <- covid_trend_site_avg %>% filter(`Patient Type` %in% input$site_PatientType) %>%
                                          group_by(year, month, Site ) %>% summarise(Average= ceiling(mean(value)))
          covid_trend_site_avg <- covid_trend_site_avg %>% mutate(month= month.abb[as.numeric(month)])
          covid_pts_trend_max_site <- covid_trend_site_avg %>% group_by(year, month, Site) %>% summarise(sum = sum(Average))
          
          ggplot(covid_trend_site_avg) +
            geom_line(aes(x =month, y = Average, color = year, group = year)) +
            geom_point(aes(x = month, y = Average,  color = year)) +
            geom_text(aes(x =month, y = Average, label=ceiling( Average)), color="#212070", 
                      size=4, hjust=0.2, vjust= -1)+
            ggtitle(label=paste0("\n","Monthly Average COVID-19 Patients Census Over Year"))+
            scale_shape_manual(values = 1:4)+
            scale_color_manual(values=c("#d80b8c",	"#00aeef", "#863198"))+
            scale_x_discrete(limits = month.abb)+
            labs(x = NULL, y = "Monthly Average Beds Occupied", color =" year") +
            theme_bw() +
            theme(plot.title = element_text(size = 16, vjust = 2), 
                  legend.position='top', 
                  legend.justification='left',
                  legend.direction='horizontal',
                  axis.text.x = element_text(size = 10, angle = 45,hjust = 1), 
                  axis.text.y = element_text(size = 10),
                  panel.grid.major = element_line(color = "lightgrey"),
                  panel.grid.minor = element_line(color = "lightgrey"))+
            scale_y_continuous(limits = c(0, max(covid_pts_trend_max_site$sum)*1.2), breaks= pretty_breaks())
          
        })
        
        
        #Hospitalized covid19 patients census Year over year by patient setting
        updatePickerInput(session = session, inputId = "site_UnitTypeHigh1", choices = c("ED", "IP"), selected = "IP")
        
        output$site_plot7 <- renderPlot({
          
        avg_trend_data <- Site_Data()
          
        covid_pts_census <- avg_trend_data %>% filter(CensusDate < Sys.Date(), `Unit Type High` %in% input$site_UnitTypeHigh1 )%>%
            select(CensusDate, Site, `COVID19`, SUSC, PUI, PUM, `Unit Type High`) %>% mutate(total = COVID19 + SUSC + PUI + PUM) %>%
            group_by(CensusDate, Site, `Unit Type High`) %>% summarise(total = sum(total))
        covid_pts_census <- covid_pts_census %>% mutate(`Unit Type High`=ifelse(is.na(`Unit Type High`), "Other", `Unit Type High`))
          
        covid_pts_census$CensusDate <- as.Date(covid_pts_census$CensusDate, format="%Y-%m-%d")
      
          
          ## Data for year over year comparison
          covid_pts_site_avg <- covid_pts_census %>% mutate(year= substr(CensusDate, 1, 4), month = substr(CensusDate, 6,7 ))
                                                                
          
          covid_pts_site_avg <- covid_pts_site_avg %>% group_by(year, month, Site, `Unit Type High`) %>% summarise(Average= ceiling(mean(total)))
          covid_pts_site_avg <- covid_pts_site_avg %>% mutate(month= month.abb[as.numeric(month)])
          
          covid_census_max_site <- covid_pts_site_avg %>%  group_by(month, year, Site) %>% summarise(sum = sum(Average))
          
          
          ggplot(covid_pts_site_avg) +
            geom_line(aes(x =month, y = Average, color = year, group = year)) +
            geom_point(aes(x = month, y = Average,  color = year)) +
            geom_text(aes(x =month, y = Average, label=ceiling( Average)), color="#212070", 
                      size=4, hjust=0.2, vjust= -1)+
            ggtitle(label=paste0("\n","Monthly Average COVID-19 Patients Census Over Year"))+
            scale_shape_manual(values = 1:4)+
            scale_color_manual(values=c("#d80b8c",	"#00aeef", "#863198"))+
            scale_x_discrete(limits = month.abb)+
            labs(x = NULL, y = "Monthly Average Census", color =" year") +
            theme_bw() +
            theme(plot.title = element_text(size = 16, vjust = 2),
                  legend.position='top', 
                  legend.justification='left',
                  legend.direction='horizontal',
                  axis.text.x = element_text(size = 10, angle = 45,hjust = 1), 
                  axis.text.y = element_text(size = 10),
                  panel.grid.major = element_line(color = "lightgrey"),
                  panel.grid.minor = element_line(color = "lightgrey"))+
          scale_y_continuous(limits = c(0, max(covid_census_max_site$sum)*1.2), breaks= pretty_breaks())
          
        })
        
        
} 

shinyApp(ui, server)


        
