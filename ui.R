### Version 28.02.2015

library("shiny")
library("lubridate")
library("mgcv")
library("htmltools")
library(shinydashboard)
library(dygraphs)

# load helpers.R file
source("helpers.R")

#Datamining281114 <- read.delim("Datamining281114.csv", header = TRUE, sep = ";", dec=".")
Dataset <- read.delim("DMFeb.csv", header = TRUE, sep = ";", dec=".")


# Call function to create the dataset for analysis
df <- create_dataset(Dataset)

#--------------Header------------------------------------------------------

header <- dashboardHeader(
  title = "Aqua Miner",
  dropdownMenu( badgeStatus = "warning",icon = icon("users"),
    
    .list = list(
      tags$li(sliderInput("orders", "Orders", min = 1, max = 500, value = 120)),
      tags$li(sliderInput("rangeAvWeight", "End.Av.Weight:", min = min(as.double(df$End.Av.Weight)), 
                          max = max(as.double(df$End.Av.Weight)), 
                          value = c(min(as.double(df$End.Av.Weight)), max(as.double(df$End.Av.Weight))),
                          step=1.0, round=TRUE, sep=".")),
      tags$li(radioButtons("radioDimUni", label = h3("Separate The Dataset By:"), 
                           choices = list("None", "Orientation", "System", "Section", "Batch", "Hatchery",
                                          "Origin.Month", "Origin.Year", "Start.Av.Weight.BioCat", 
                                          "End.Av.Weight.BioCat", "Actual.Feed"), selected = "None")),
      tags$li(sliderInput("orders", "Orders", min = 1, max = 500, value = 120))
      
    )
    ),
  # Messages
  dropdownMenu(type = "messages", badgeStatus = "success",
               
               messageItem("Support Team",
                           "Message content here.",
                           time = "5 mins"
               ),
               messageItem("Support Team",
                           "Message content here.",
                           time = "2 hours"
               ),
               messageItem("New User",
                           "Message content here.",
                           time = "Today"
               )
  ),
  # Notifications
  dropdownMenu(type = "notifications", badgeStatus = "warning",
               notificationItem(icon = icon("users"), status = "info",
                                "5 new members joined today"
               ),
               notificationItem(icon = icon("warning"), status = "danger",
                                "Very long description here that may not fit into the page and may cause design problems"
               ),
               notificationItem(icon = icon("users"), status = "warning",
                                "5 new members joined"
               ),
               notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),
                                status = "success", "25 sales made"
               ),
               notificationItem(icon = icon("user", lib = "glyphicon"),
                                status = "danger", "You changed your username"
               ),
               notificationItem(icon = icon("user", lib = "glyphicon"),
                                status = "danger", selectInput(inputId='groupOrientation', label='Orientation', choices=c("All", unique(as.character(df$Orientation))), selected="All", multiple=TRUE)
               )
  ),
  # Tasks
  dropdownMenu(type = "tasks", badgeStatus = "danger",
               taskItem(value = 20, color = "aqua",
                        "Create a nice theme"
               ),
               taskItem(value = 40, color = "green",
                        "Design some buttons"
               ),
               taskItem(value = 60, color = "yellow",
                        "Another task"
               ),
               taskItem(value = 80, color = "red",
                        "And yet another task"
               )
  )
)
#--------------End Header-----
#---------------Sidebar----------------------------------------------------------------------

sidebar <- dashboardSidebar(
  img(src="Aquamanager-logo.png" ,class = "img-responsive"),
  actionButton(inputId = 'goUniPlot',  label = ' Refresh Univariate plots',icon =icon("signal")),
  #actionButton("goButton", "Go!"),
  #---sidebarSearchForm(label = "Enter approval number", "approvalText", "approvalButton"),
  
  sidebarMenu(
    menuItem("Navigation",  icon = icon("navicon"),
             menuSubItem("Univariate", tabName = "Univariate",icon = icon("signal")),
             menuSubItem("Sub-item 2", tabName = "subitem2")
             ),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
             badgeColor = "green"),
    menuItem("Charts", icon = icon("bar-chart-o"),
             menuSubItem("Sub-item 1", tabName = "subitem1"),
             menuSubItem("Sub-item 2", tabName = "subitem2")
             
    ),
    menuItem("Filters",icon = icon("shield"),
             menuSubItem( icon=NULL,sliderInput("orders", "Orders", min = 1, max = 500, value = 120)),
             menuSubItem(icon=NULL, selectInput("progress", "Progress",
                                      choices = c("0%" = 0, "20%" = 20, "40%" = 40, "60%" = 60, "80%" = 80,
                                                  "100%" = 100)
                         ) )
    ),
  
    menuItem("Dimensions",icon = icon("cubes"),
             menuSubItem(icon=NULL,
               selectInput(inputId='groupOrientation', label='Orientation', choices=c("All", unique(as.character(df$Orientation))), selected="All", multiple=TRUE)),
             menuSubItem(icon = NULL,
               selectInput(inputId='groupSystem', label='System', choices=c("All", unique(as.character(df$System))), selected="All", multiple=TRUE)),
             menuSubItem(icon = NULL,
              selectInput(inputId='groupBatch', label='Batch', choices=c("All", unique(as.character(df$Batch))), selected="All", multiple=TRUE)),
             menuSubItem(icon=NULL,
              selectInput(inputId='groupSection', label='Section', choices=c("All", unique(as.character(df$Section))), selected="All", multiple=TRUE)),
             menuSubItem(icon = NULL,
              selectInput(inputId='groupOriginMonth', label='Origin.Month', choices=c("All", unique(as.character(df$Origin.Month))), selected="All", multiple=TRUE)),
             menuSubItem(icon = NULL,
              selectInput(inputId='groupOriginYear', label='Origin.Year', choices=c("All", unique(as.character(df$Origin.Year))), selected="All", multiple=TRUE))
             ),
    menuItem("Dimensions",icon = icon("cubes"),
             menuSubItem(icon=NULL,
                         selectInput(inputId='groupOrientation', label='Orientation', choices=c("All", unique(as.character(df$Orientation))), selected="All", multiple=TRUE)),
             menuSubItem(icon = NULL,
                         selectInput(inputId='groupSystem', label='System', choices=c("All", unique(as.character(df$System))), selected="All", multiple=TRUE)),
             menuSubItem(icon = NULL,
                         selectInput(inputId='groupBatch', label='Batch', choices=c("All", unique(as.character(df$Batch))), selected="All", multiple=TRUE)),
             menuSubItem(icon=NULL,
                         selectInput(inputId='groupSection', label='Section', choices=c("All", unique(as.character(df$Section))), selected="All", multiple=TRUE)),
             menuSubItem(icon = NULL,
                         selectInput(inputId='groupOriginMonth', label='Origin.Month', choices=c("All", unique(as.character(df$Origin.Month))), selected="All", multiple=TRUE)),
             menuSubItem(icon = NULL,
                         selectInput(inputId='groupOriginYear', label='Origin.Year', choices=c("All", unique(as.character(df$Origin.Year))), selected="All", multiple=TRUE))
    )
  )
  
  
)
#---------------Body--------------------------------------------------------------------
body <- dashboardBody(
  tabItems(
    tabItem("Univariate",
            mainPanel(
              tabsetPanel(
                tabPanel("Histograms",
                         fluidRow(column(3,sliderInput("numbins", "Number of bins:", 
                                                       min = 5, max = 100, 
                                                       value = 20, step=1))
                                 ), 
                         fluidRow(plotOutput("histPlotAvWeight")),
                         fluidRow( plotOutput("histPlotPeriod.FCR")),
                         fluidRow( plotOutput("histPlotEcon.FCR")),
                         fluidRow( plotOutput("histPlotPeriod.SFR")),
                         fluidRow( plotOutput("histPlotPeriod.SGR")),
                         fluidRow( plotOutput("histPlotMortality")),
                         fluidRow( plotOutput("histPlotPeriod.Day.Degrees")),
                         fluidRow( plotOutput("histPlotAvg.Temperature"))
                ), # end tabPanel Histograms 
                tabPanel("Density Plots",
                         fluidRow(plotOutput("densPlotAvWeight")),
                         fluidRow(plotOutput("densPlotPeriod.FCR")),
                         fluidRow(plotOutput("densPlotEcon.FCR")),
                         fluidRow(plotOutput("densPlotPeriod.SFR")),
                         fluidRow(plotOutput("densPlotPeriod.SGR")),
                         fluidRow(plotOutput("densPlotMortality")),
                         fluidRow(plotOutput("densPlotPeriod.Day.Degrees")),
                         fluidRow(plotOutput("densPlotAvg.Temperature"))
                ), # end tabPanel Density Plots
                tabPanel("Boxplots",
                         fluidRow(plotOutput("boxPlotAvWeight")),
                         fluidRow(plotOutput("boxPlotPeriod.FCR")),
                         fluidRow(plotOutput("boxPlotEcon.FCR")),
                         fluidRow(plotOutput("boxPlotPeriod.SFR")),
                         fluidRow(plotOutput("boxPlotPeriod.SGR")),
                         fluidRow(plotOutput("boxPlotMortality")),
                         fluidRow(plotOutput("boxPlotPeriod.Day.Degrees")),
                         fluidRow(plotOutput("boxPlotAvg.Temperature"))
                ), # end tabPanel BoxPlots
                tabPanel("Summary Statistics", 
                         h4("End Average Weight:"),
                         tableOutput("summary_stats_EndAvWeight"),
                         hr(),
                         h4("Period FCR:"),
                         tableOutput("summary_stats_PeriodFCR"),
                         hr(),
                         h4("LTD Econ FCR:"),
                         tableOutput("summary_stats_EconFCR"),
                         hr(),
                         h4("Period SFR:"),
                         tableOutput("summary_stats_PeriodSFR"),
                         hr(),
                         h4("Period SGR:"),
                         tableOutput("summary_stats_PeriodSGR"),
                         hr(),
                         h4("LTD Mortality:"),
                         tableOutput("summary_stats_Mortality"),
                         hr(),
                         h4("Period Thermal Age:"),
                         tableOutput("summary_stats_Period.Day.Degrees"),
                         hr(),
                         h4("Avg. Temperature:"),
                         tableOutput("summary_stats_Avg.Temp")
                ), # end tabPanel Summary Statistics
                tabPanel("Data", 
                         textOutput("Dataset for processing..."),
                         hr(),
                         dataTableOutput("dataset") 
                )  # end tabPanel Data
              ) # end tabsetPanel
            )
 ),
    
    tabItem("widgets",
            fluidRow(
              box(status = "primary", width = 8, dygraphOutput("dygraph", height = 250)),
              
              box(title = "Controls for dygraph", background = "teal",
                  sliderInput("months", label = "Months to Predict",
                              value = 72, min = 12, max = 144, step = 12, ticks = FALSE),
                  selectInput("interval", label = "Prediction Interval",
                              choices = c("0.80", "0.90", "0.95", "0.99"),
                              selected = "0.95", selectize = TRUE),
                  checkboxInput("showgrid", label = "Show Grid", value = TRUE)
              )
            ),
            
            fluidRow(
              # Box with textOutput
              box(title = "Status summary", background = "red", textOutput("status")),
              # Box with HTML output, when finer control over appearance is needed
              box(
                title = "Status summary 2",
                uiOutput("status2"),
                background = "blue"
              )
            )
    ),
    
    tabItem("subitem1",
            "Sub-item 1 tab content"
    ),
    
    tabItem("subitem2",
            "Sub-item 2 tab content"
    )
  )
) 


#--------------------------------------------
dashboardPage(header, sidebar, body)
