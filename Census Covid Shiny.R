
# Shiny App for Covid Census

rm(list=ls())

install.packages("shinythemes")

# Import Libraries

library(tidyverse)
library(dplyr)
library(reshape2)
library(data.table)
library(plotly)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(patchwork)
library(gridExtra)
library(grid)
library(scales)


# Import Data Sets   =====================================

# define working directory
path <- "C:\\Users\\aghaer01\\Downloads\\Shiny"
setwd(path)


# Read the census repo file countaining the MSSN data
covid_data <- read_excel(paste0("Census and Covid Repo Created ", Sys.Date(),".xlsx"))



# Report Update 
repo_start_date <- format(min(covid_data$CensusDate), "%m-%d-%Y")
repo_end_date <- format(max(covid_data$CensusDate), "%m-%d-%Y")

report_run_date <- Sys.time()



# Color Functions for Graphs =====================================
theme_set(theme_minimal())

# Mount Sinai corporate colors 
MountSinai_colors <- c(
  `dark purple`  = "#212070",
  `dark pink`    = "#d80b8c",
  `dark blue`    = "#00aeef",
  `dark grey`    = "#7f7f7f",
  `yellow`       = "#ffc000",
  `purple`       = "#7030a0",
  `med purple`   = "#5753d0",
  `med pink`     = "#f75dbe",
  `med blue`     = "#5cd3ff",
  `med grey`     = "#a5a7a5",
  `light purple` = "#c7c6ef",
  `light pink`   = "#fcc9e9",
  `light blue`   = "#c9f0ff",
  `light grey`   = "#dddedd"
)

# Function to extract Mount Sinai colors as hex codes
# Use Character names of MountSinai_colors

MountSinai_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (MountSinai_colors)
  
  MountSinai_colors[cols]
}


types_pallete <- c("#00aeef", "#7f7f7f","#3a5fcd", "#5753d0", "#d80b8c", "#e69f00", "#8b814c", "#212070")

# Color Function that can be used to call all colors is "MountSinai_cols()"
#MountSinai_cols()       # will provide all colors and their hex codes in a table 
#MountSinai_cols("pink") # will provide color name and the hex code for the pink color

all_pallete <- c("#212070","#d80b8c","#00aeef","#7f7f7f","#5753d0","#f75dbe","#5cd3ff","#a5a7a5","#c7c6ef", "#fcc9e9","#c9f0ff","#dddedd")



# Create palettes 
MountSinai_palettes <- list(
  `all`   = MountSinai_cols("dark purple","dark pink","dark blue","dark grey",
                            "med purple","med pink","med blue","med grey", 
                            "light purple","light pink","light blue","light grey"),
  
  `dark`  = MountSinai_cols("dark purple","dark grey",
                            "yellow","med pink","dark pink","dark blue",
                            "med purple","med grey","med blue"),
  
  `main`  = MountSinai_cols("dark purple","dark grey","dark pink","dark blue","med purple","med pink","med blue","med grey"),
  
  `purple`  = MountSinai_cols("dark purple","med purple","light purple"),
  
  `pink`  = MountSinai_cols("dark pink","med pink","light pink"),
  
  `blue`  = MountSinai_cols("dark blue", "med blue", "light blue"),
  
  `grey`  = MountSinai_cols("dark grey", "med grey", "light grey"),
  
  `purpleGrey` = MountSinai_cols("dark purple", "dark grey"),
  
  `pinkBlue` = MountSinai_cols("dark pink", "dark blue")
  
)

# MountSinai_palettes
# Return function to interpolate a Mount Sinai color palette
# default value is the main palette, reverse = True will change the order

MountSinai_pal <- function(palette = "all", reverse = FALSE, ...) {
  pal <- MountSinai_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


# Scale Function for ggplot can be used instead of scale_color_manual
scale_color_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Scale Fill for ggplot insetead of scale_fill_manual 
scale_fill_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}






## MSHS =====================================

## Data for main_plot1
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
covid_loc$`Unit Type High`[which(is.na(covid_loc$`Unit Type High`))] <- "Other"



## Data for main_plot2
covid_pts_trend <- covid_data %>% filter(CensusDate < Sys.Date()) %>% select(CensusDate,`COVID19`, SUSC, PUI, PUM) %>%
  mutate(CensusDate = as.Date(CensusDate))

covid_pts_trend <- reshape2::melt(covid_pts_trend, id.vars = c("CensusDate"))
covid_pts_trend$CensusDate <- as.Date(covid_pts_trend$CensusDate, format="%Y-%m-%d")
covid_pts_trend <- aggregate(covid_pts_trend$value, by=list(covid_pts_trend$CensusDate,covid_pts_trend$variable), FUN=sum)
colnames(covid_pts_trend) <- c("Date","Patient Type","value")

covid_census_max <- covid_pts_trend %>% group_by(Date) %>% summarise(sum = sum(value))


## Data for main_plot3

#Hospitalized covid19 patients census by unit type
covid_pts_census <- covid_data %>% filter(CensusDate < Sys.Date()) %>% select(CensusDate,`COVID19`, SUSC, PUI, PUM, `Unit Type High`) %>%
  mutate(total = COVID19 + SUSC + PUI + PUM) %>% group_by(CensusDate, `Unit Type High`) %>%  summarise(total = sum(total))

covid_pts_census$CensusDate <- as.Date(covid_pts_census$CensusDate, format="%Y-%m-%d")
covid_pts_census <- covid_pts_census %>% group_by(CensusDate, `Unit Type High`) %>% summarise(total = sum(total))
covid_pts_census$`Unit Type High`[which(is.na(covid_pts_census$`Unit Type High`))] <- "Other"

## Data for Sites   =====================================

## Data for site_plot1
covid_pts_trend_site <- covid_data %>% filter(CensusDate < Sys.Date()) %>% select(CensusDate, Site, `COVID19`, SUSC, PUI, PUM)

covid_pts_trend_site <- reshape2::melt(covid_pts_trend_site, id.vars = c("CensusDate", "Site"))
covid_pts_trend_site$CensusDate <- as.Date(covid_pts_trend_site$CensusDate, format="%Y-%m-%d")
covid_pts_trend_site <- aggregate(covid_pts_trend_site$value, 
                                  by=list(covid_pts_trend_site$CensusDate,covid_pts_trend_site$variable , covid_pts_trend_site$Site), FUN=sum)

colnames(covid_pts_trend_site) <- c("Date","Patient Type", "Site", "value")
covid_pts_trend_max_site <- covid_pts_trend_site %>% group_by(Date, Site) %>% summarise(sum = sum(value))



## Data for site_plot2 and siteb_plot3
covid_pts_census_site <- covid_data %>% filter(CensusDate < Sys.Date() )%>%
  select(CensusDate, Site, `COVID19`, SUSC, PUI, PUM, `Unit Type High`) %>% mutate(total = COVID19 + SUSC + PUI + PUM) %>%
  group_by(CensusDate, Site, `Unit Type High`) %>% summarise(total = sum(total))
covid_pts_census_site <- covid_pts_census_site %>% mutate(`Unit Type High`=ifelse(is.na(`Unit Type High`), "Other", `Unit Type High`))

covid_pts_census_site$CensusDate <- as.Date(covid_pts_census_site$CensusDate, format="%Y-%m-%d")
covid_census_max_site <- covid_pts_census_site%>%  group_by(CensusDate, Site) %>% summarise(sum = sum(total))



## Data for Adult Med Surge
covid_pts <- covid_data %>% filter(CensusDate < Sys.Date(), `Unit Type` == "Adult Med Surg", VirtualUnit == "FALSE") %>%
  mutate(total = COVID19 + SUSC + PUI + PUM) %>%
  filter(total > 0) %>% select(CensusDate, Site, DEPARTMENT_NAME, total) %>%
  group_by(CensusDate, Site, DEPARTMENT_NAME) %>% summarise(total = sum(total))



covid_pts$CensusDate <- as.Date(covid_pts$CensusDate, format="%Y-%m-%d")
colnames(covid_pts)[colnames(covid_pts) == "DEPARTMENT_NAME"] <- "Unit"

## Data for Virtual Unit
covid_pts_vu <- covid_data %>%
  filter(CensusDate < Sys.Date(),  VirtualUnit == "TRUE") %>%
  mutate(total = COVID19 + SUSC + PUI + PUM) %>%
  filter(total > 0) %>%
  select(CensusDate, Site, DEPARTMENT_NAME, total) %>%
  group_by(CensusDate, Site, DEPARTMENT_NAME) %>%
  summarise(total = sum(total))

covid_pts_vu$CensusDate <- as.Date(covid_pts_vu$CensusDate, format="%Y-%m-%d")
colnames(covid_pts_vu) <- c("CensusDate","Site","Unit","total")


# Shiny UI   ==================================================

ui <- dashboardPage( 
  dashboardHeader(title= "Covid Census Analysis", titleWidth = 250),
  
 
  
  dashboardSidebar(width = 250,
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("MSHS", tabName = "mshs", icon = icon("th")),
      menuItem("MSB",  tabName = "msb",  icon = icon("th"))
      #menuItem("MSBI", tabName = "msbi",  icon = icon("th")),
      #menuItem("MSH",  tabName = "msh",  icon = icon("th")),
      #menuItem("MSM",  tabName = "msm",  icon = icon("th")),
      #menuItem("MSQ",  tabName = "msq",  icon = icon("th")),
      #menuItem("MSSN", tabName = "mssn",  icon = icon("th")),
      #menuItem("MSW",  tabName = "msw",  icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # first tabItem for objective
      tabItem(tabName = "home",
              column(12, 
              div("MSHS Census and Utilization Analysis", style = "color:	#221f72; font-weight:bold; font-size:34px; margin-left: 20px" ),
              tags$h3("Health System Operations", style= "margin-left: 20px")
              ),
              
              column(12, 
               div(
               tags$h3("Objective", style= "margin-left: 20px")),
               
               
              ),
              column(12, 
              div(
              tags$h3("Data Description", style= "margin-left: 20px"))
                     ),
             
                     
                     ),
             
        
      
      
      
      
      
      # Second tabItem for MSHS
      tabItem(tabName = "mshs",
              fluidRow(
                column(11,
                box(plotOutput("main_plot1"),  width= 250,
                    title = "MSHS Hospitalized COVID-19 Census", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,))),
                
              
              
             fluidRow(
               column(11,
                box(plotOutput("main_plot2"),  width= 250,
                    title = "MSHS Hospitalized Census by Infection Status (ED and IP)", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           dateRangeInput("Date", strong("Select Date range"), start = "2021-08-01", #min(covid_pts_trend$Date), 
                                          end = max(covid_pts_trend$Date),  min = "2020-03-12", max = Sys.Date()-1, width =250)))),
              
              
             fluidRow(
               column(11,
               box(plotOutput("main_plot3"),  width= 250,
                   title = "MSHS Hospitalized COVID-19 Patients Census by ED vs. IP", status = "primary",
                   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                   dateRangeInput("CensusDate", strong("Select Date range"), start = "2021-08-01", 
                                  end = max(covid_pts_census$CensusDate),  min = "2020-03-12", max = Sys.Date()-1, width =250)))),
             
             fluidRow(
               column(11,
               box(plotOutput("main_plot4"),  width= 250,
                   title = "MSHS Hospitalized COVID-19 Patients Census Trend", status = "primary",
                   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                   selectInput(inputId = "UnitTypeHigh", label = strong("Select Unit Type"),
                               choices = unique(covid_pts_census$`Unit Type High`),
                               selected = "COVID19", width =250),
                   dateRangeInput("CensusDate0", strong("Select Date range"), start = "2021-08-01", 
                                  end = max(covid_pts_census$CensusDate),  min = "2020-03-12", max = Sys.Date()-1, width =250)))),
            ),
      
             
      # tabItem for MSB
      tabItem(tabName = "msb",
              fluidRow(
                column(11,
                box(plotOutput("msb_plot1"),  width= 250,
                    title = "MSB: Hospitalized Census by Infection Status (ED and IP)", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                    dateRangeInput("Date1", strong("Select Date range"), start = "2021-08-01",
                                   end = max(covid_pts_trend_site$Date),  min = "2020-03-12", max = Sys.Date()-1, width =250)))),
              
      
             fluidRow(
               column(11,
                box(plotOutput("msb_plot2"),  width= 250,
                    title = "MSB Hospitalized COVID-19 Patients Census by ED vs. IP", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                    dateRangeInput("CensusDate1", strong("Select Date range"), start = "2021-08-01", 
                                   end = max(covid_pts_census_site$CensusDate),  min = "2020-03-12", max = Sys.Date()-1, width =250)))),
              
              fluidRow(
                column(11,
                box(plotOutput("msb_plot3"),  width= 250,
                    title = "MSHS Hospitalized COVID-19 Patients Census Trend", status = "primary",
                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                    selectInput(inputId = "UnitTypeHigh1", label = strong("Select Unit Type"),
                                choices = unique(covid_pts_census_site$`Unit Type High`[covid_pts_census_site$Site=="MSB"]),
                                selected = "COVID19", width =250),
                    dateRangeInput("CensusDate2", strong("Select Date range"), start = "2021-08-01",
                                   end = max(covid_pts_census_site$CensusDate),  min = "2020-03-12", max = Sys.Date()-1, width =250)))),
             
             fluidRow(
               column(11,
               box(plotOutput("msb_plot4"),  width= 250,
                   title = "MSB Adult Surg Med: Total COVID-19 Patients Census", status = "primary",
                   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                   selectInput(inputId = "Unit", label = strong("Select Unit"),
                               choices = unique(covid_pts$Unit[covid_pts$Site== "MSB"]),
                               selected = "MSB 2 EAST", width =250),
                   dateRangeInput("CensusDate3", strong("Select Date range"), start ="2021-08-01",
                                  end = max(covid_pts$CensusDate),  min = "2020-03-12", max = Sys.Date()-1, width =250)))),
             
             fluidRow(
               column(11,
               box(plotOutput("msb_plot5"),  width= 250,
                   title = " MSB Virtual Units: Total COVID-19 Patients Census", status = "primary",
                   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                   h5("Epic virtual units with > 0 beds occupied."),
                   selectInput(inputId = "Unit1", label = strong("Select Unit"),
                               choices = unique(covid_pts_vu$Unit[covid_pts_vu$Site== "MSB"]),
                               selected =  "MSB PACU VU1", width =250),
                   dateRangeInput("CensusDate4", strong("Select Date range"), start = min(covid_pts_vu$CensusDate), 
                                  end = max(covid_pts_vu$CensusDate),  min = "2020-03-12", max = Sys.Date()-1, width =250)))),
      )
             
     )
      
  )
                         
)



 


server <- function(input, output) { 
  
  output$main_plot1 <- renderPlot({
    p1 <- ggplot(covid_status, 
                 aes(x=Site, y=total, fill=factor(variable,levels=c("PUI","SUSC","COVID19"))))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_manual(values=c("#7f7f7f","#E69F00","#212070"))+
      ggtitle(label="\nBy Infection Status")+
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
    
    p2 <- ggplot(covid_loc, 
                 aes(x=Site, y=total, fill=factor(`Unit Type High`,levels=c("ED","IP","Other"))))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_manual(values=c("#00aeef","#d80b8c","#863198"))+
      ggtitle(label="\nBy ED vs. IP")+
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
    
    grid.arrange(p1, p2, ncol=2)
    
  })
  
  
  
  # MSHS Hospitalized Census by Infection Status 
  pts_trends <- reactive({
    req(input$Date)
    validate(need(!is.na(input$Date[1]) & !is.na(input$Date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$Date[1] < input$Date[2], "Error: Start date should be earlier than end date."))
    covid_pts_trend %>%
      filter(
        Date > as.Date(input$Date[1]) & Date < as.Date(input$Date[2] ))
  })
  
  
  output$main_plot2 <- renderPlot({
    ggplot(pts_trends(), 
           aes(x=Date, y=value, fill=factor(`Patient Type`,levels=c("PUM","PUI","SUSC","COVID19"))))+
      geom_bar(position="stack",stat="identity", width=0.5)+
      scale_fill_manual(values=c("#d80b8c",	"#00aeef","#E69F00","#212070"))+
      labs(x=NULL, y="Beds Occupied")+
      guides(fill=guide_legend(title="Patient Type"))+
      theme_bw()+
      theme(legend.position='top', 
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
  
  
  # MSHS Hospitalized Census by Infection Status 
  census_trends <- reactive({
    req(input$CensusDate)
    validate(need(!is.na(input$CensusDate[1]) & !is.na(input$CensusDate[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$CensusDate[1] < input$CensusDate[2], "Error: Start date should be earlier than end date."))
    covid_pts_census %>%
      filter(
        CensusDate > as.Date(input$CensusDate[1]) & CensusDate < as.Date(input$CensusDate[2] ))
  })
  
  
  output$main_plot3 <- renderPlot({
    ggplot(census_trends(), 
           aes(x=CensusDate, y=total, fill=factor(`Unit Type High`,levels=c("ED","IP","Other"))))+
      geom_bar(position="stack",stat="identity", width=0.6)+
      scale_fill_manual(values=c("#d80b8c",	"#00aeef", "#863198"))+
      labs(x=NULL, y="Beds Occupied")+
      guides(fill=guide_legend(title="Unit Type"))+
      theme_bw()+
      theme(legend.position='top', 
            legend.justification='left',
            legend.direction='horizontal',
            legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 10),
            panel.grid.major = element_line(color = "lightgrey"),
            panel.grid.minor = element_line(color = "lightgrey"))+
      scale_y_continuous(limits = c(0, max(covid_census_max$sum)*1.2))+
      scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))+
      geom_text(aes(label=total), color="white", 
                size=2, position = position_stack(vjust = 0.5))+
      stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = CensusDate), 
                   geom="text", color="black", 
                   size=3)
    })
  
  
  #Hospitalized covid19 patients census trend by unit type 
  pts_census_trends <- reactive({
    req(input$CensusDate0)
    validate(need(!is.na(input$CensusDate0[1]) & !is.na(input$CensusDate0[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$CensusDate0[1] < input$CensusDate0[2], "Error: Start date should be earlier than end date."))
    covid_pts_census %>%
      filter(
        `Unit Type High` == input$UnitTypeHigh,
        CensusDate > as.Date(input$CensusDate0[1]) & CensusDate < as.Date(input$CensusDate0[2] ))
  })
 
  output$main_plot4 <- renderPlot({
  ggplot(pts_census_trends()) +
    geom_point(aes(x = CensusDate, y = total, shape = `Unit Type High`, color = `Unit Type High`)) +
    geom_line(aes(x = CensusDate, y = total, color = `Unit Type High`)) +
    scale_shape_manual(values = 1:4)+
    scale_color_manual(values=c("#d80b8c",	"#00aeef", "#863198"))+
    labs(x = NULL, y = "Census", shape = "Unit Type", color = "Unit Type") +
    scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))+
    theme_bw() +
    theme(legend.position='top', 
          legend.justification='left',
          legend.direction='horizontal',
          axis.text.x = element_text(size = 10, angle = 45,hjust = 1), 
          axis.text.y = element_text(size = 10),
          panel.grid.major = element_line(color = "lightgrey"),
          panel.grid.minor = element_line(color = "lightgrey"))
  
  })
  
  
  # MSB
  pts_trends_msb <- reactive({
    req(input$Date1)
    validate(need(!is.na(input$Date1[1]) & !is.na(input$Date1[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$Date1[1] < input$Date1[2], "Error: Start date should be earlier than end date."))
    covid_pts_trend_site %>%
      filter(Site== "MSB",
        Date > as.Date(input$Date1[1]) & Date < as.Date(input$Date1[2] ))
  })
  
  
  output$msb_plot1 <- renderPlot({
    ggplot(pts_trends_msb(), 
           aes(x=Date, y=value, fill=factor(`Patient Type`,levels=c("PUM","PUI","SUSC","COVID19"))))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_manual(values=c("#d80b8c",	"#00aeef","#E69F00","#212070"))+
      labs(x=NULL, y="Beds Occupied")+
      guides(fill=guide_legend(title="Patient Type"))+
      theme_bw()+
      theme(legend.position='top', 
            legend.justification='left',
            legend.direction='horizontal',
            legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 10),
            panel.grid.major = element_line(color = "lightgrey"),
            panel.grid.minor = element_line(color = "lightgrey"))+
      scale_y_continuous(limits = c(0, max(covid_pts_trend_max_site$sum[covid_census_max_site$Site== "MSB"])*1.2))+
      scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))+
      geom_text(aes(label=value), color="white", 
                size=2, position = position_stack(vjust = 0.5))+
      stat_summary(fun= sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Date), 
                   geom="text", color="black", 
                   size=3)
    
  })
  
  # MSB Hospitalized Census by Infection Status 
  census_trends_msb <- reactive({
    req(input$CensusDate1)
    validate(need(!is.na(input$CensusDate1[1]) & !is.na(input$CensusDate1[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$CensusDate1[1] < input$CensusDate1[2], "Error: Start date should be earlier than end date."))
    covid_pts_census_site %>%
      filter( Site=="MSB",
        CensusDate > as.Date(input$CensusDate1[1]) & CensusDate < as.Date(input$CensusDate1[2] ))
      }) 
  
  
 output$msb_plot2 <- renderPlot({
  ggplot(census_trends_msb() , 
         aes(x=CensusDate, y=total, fill=`Unit Type High`))+
    geom_bar(position="stack",stat="identity", width=0.7)+
    scale_fill_manual(values=c("#d80b8c",	"#00aeef","#212070","#7f7f7f"))+
    labs(x=NULL, y="Beds Occupied")+
    guides(fill=guide_legend(title="Unit Type"))+
    theme_bw()+
    theme(legend.position='top', 
          legend.justification='left',
          legend.direction='horizontal',
          legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
          axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10),
          panel.grid.major = element_line(color = "lightgrey"),
          panel.grid.minor = element_line(color = "lightgrey"))+
    scale_y_continuous(limits = c(0, max(covid_census_max_site$sum[covid_census_max_site$Site=="MSB"])*1.2))+
    scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))+
    geom_text(aes(label=total), color="white", 
              size=2, position = position_stack(vjust = 0.5))+
    stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = CensusDate), 
                 geom="text", color="black",                  size=3)
  
})  


#Hospitalized covid19 patients census trend by unit type
 
 pts_census_trends_msb <- reactive({
   req(input$CensusDate2)
   validate(need(!is.na(input$CensusDate2[1]) & !is.na(input$CensusDate2[2]), "Error: Please provide both a start and an end date."))
   validate(need(input$CensusDate2[1] < input$CensusDate2[2], "Error: Start date should be earlier than end date."))
   covid_pts_census_site %>%
     filter( Site== "MSB",
       `Unit Type High` == input$UnitTypeHigh1,
       CensusDate > as.Date(input$CensusDate2[1]) & CensusDate < as.Date(input$CensusDate2[2] ))
 })
 
 output$msb_plot3 <- renderPlot({
 ggplot(pts_census_trends_msb()) +
  geom_point(aes(x = CensusDate, y = total, shape = `Unit Type High`, color = `Unit Type High`)) +
  geom_line(aes(x = CensusDate, y = total, color = `Unit Type High`)) +
  scale_shape_manual(values = 1:4)+
  scale_colour_manual(values=c("#d80b8c",	"#00aeef","#212070","#7f7f7f"))+
  labs(x = NULL, y = "Census", shape = "Unit Type", color = "Unit Type") +
  scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))+
  theme_bw() +
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal',
        legend.title = element_text(size = 10), legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45,hjust = 1), 
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey"))


 })
 
 
 
 msb_trends <- reactive({
   req(input$CensusDate3)
   validate(need(!is.na(input$CensusDate3[1]) & !is.na(input$CensusDate3[2]), "Error: Please provide both a start and an end date."))
   validate(need(input$CensusDate3[1] < input$CensusDate3[2], "Error: Start date should be earlier than end date."))
   covid_pts %>%
     filter( Site == "MSB",
       Unit == input$Unit,
       CensusDate > as.Date(input$CensusDate3[1]) & CensusDate < as.Date(input$CensusDate3[2] ))
 })
 
 output$msb_plot4 <- renderPlot({
   ggplot( msb_trends() )+
     geom_point(aes(x=CensusDate, y=total, color=Unit))+
     geom_line(aes(x=CensusDate, y=total, color=Unit))+
     scale_colour_manual(values=c("#00aeef"))+
    # scale_shape_manual(values = 1:length(unique(Unit)),
     #                   guide = guide_legend(nrow = ceiling(length(unique(Unit))/6)))+
     labs(x=NULL, y="Beds Occupied")+
     theme_bw()+
     theme(legend.position='top', 
           legend.justification='left',
           legend.direction='horizontal',
           legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
           axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
           axis.text.y = element_text(size = 10),
           panel.grid.major = element_line(color = "lightgrey"),
           panel.grid.minor = element_line(color = "lightgrey"))+
     scale_y_continuous(limits = c(0, max(covid_pts$total[covid_pts$Site=="MSB"])*1.2), breaks= pretty_breaks())+
     scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))
 
})
 
 
 msb_trends_vu <- reactive({
   req(input$CensusDate4)
   validate(need(!is.na(input$CensusDate4[1]) & !is.na(input$CensusDate4[2]), "Error: Please provide both a start and an end date."))
   validate(need(input$CensusDate4[1] < input$CensusDate4[2], "Error: Start date should be earlier than end date."))
   covid_pts_vu %>%
     filter( Site == "MSB",
             Unit == input$Unit1,
             CensusDate > as.Date(input$CensusDate4[1]) & CensusDate < as.Date(input$CensusDate4[2] ))
 })
 
 
 output$msb_plot5 <- renderPlot({
   
   ggplot(msb_trends_vu())+
     geom_point(aes(x=CensusDate, y=total, shape=Unit, color=Unit))+
     geom_line(aes(x=CensusDate, y=total, color=Unit))+
     scale_shape_manual(values = 1:length(unique(msb_trends_vu()$Unit)),
                        guide = guide_legend(nrow = ceiling(length(unique(msb_trends_vu()$Unit))/4)))+
     labs(x=NULL, y="Beds Occupied")+
     theme_bw()+
     theme(legend.position='top', 
           legend.justification='left',
           legend.direction='horizontal',
           legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
           axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
           axis.text.y = element_text(size = 10),
           panel.grid.major = element_line(color = "lightgrey"),
           panel.grid.minor = element_line(color = "lightgrey"))+
     scale_y_continuous(limits = c(0, max(msb_trends_vu()$total)*1.2), breaks= pretty_breaks())+
     scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "2 days", date_minor_breaks = "1 day", expand = c(0, 0.6))
 })

} 

shinyApp(ui, server)
    