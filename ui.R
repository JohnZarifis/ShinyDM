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

#-------------------------------Header------------------------------------------------------

header <- dashboardHeader(
  title = "Aqua Miner",
  dropdownMenu(badgeStatus = "success",
               menuItem("Dashboard",tabName = "dashboard", icon = icon("dashboard")),
               menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
                        badgeColor = "green"),
               menuItem("Charts", icon = icon("bar-chart-o"),
                        menuSubItem("Sub-item 1", tabName = "subitem1"),
                        menuSubItem("Sub-item 2", tabName = "subitem2"),
                        menuSubItem(sliderInput("orders", "Orders", min = 1, max = 500, value = 120))
               )
               
               
               ),
  # Messages
  dropdownMenu(type = "messages", badgeStatus = "success",
               menuItem("Dashboard",tabName = "dashboard", icon = icon("dashboard")),
               menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
                        badgeColor = "green"),
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
                                "5 new members joined today",href = "#shiny-tab-widgets"
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

#---------------Sidebar----------------------------------------------------------------------

sidebar <- dashboardSidebar(
  #---sidebarSearchForm(label = "Enter approval number", "approvalText", "approvalButton"),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
             badgeColor = "green"),
    menuItem("Charts", icon = icon("bar-chart-o"),
             menuSubItem("Sub-item 1", tabName = "subitem1"),
             menuSubItem("Sub-item 2", tabName = "subitem2"),
             sliderInput("orders1", "Orders", min = 1, max = 500, value = 12),
             menuSubItem(sliderInput("months1", label = "Months to Predict",
                         value = 72, min = 12, max = 144, step = 12, ticks = FALSE))
    ),
    menuItem("Source code for app", icon = icon("file-code-o"),
             href = "https://gist.github.com/wch/8957ee5e2d79770abf9a")
  )
)
#-----------------------Body--------------------------------------------------------------------
body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            # valueBoxes
            fluidRow(
              valueBox(
                uiOutput("orderNum"), "New Orders", icon = icon("credit-card")
              ),
              valueBox(
                uiOutput("progress"), "Progress", icon = uiOutput("progressIcon"),
                color = "purple"
              ),
              # An entire box can be in a uiOutput
              uiOutput("approvalBox")
            ),
            
            # Boxes
            fluidRow(
              box(status = "primary", width = 6,
                  sliderInput("orders", "Orders", min = 1, max = 500, value = 420),
                  selectInput("progress", "Progress",
                              choices = c("0%" = 0, "20%" = 20, "40%" = 40, "60%" = 60, "80%" = 80,
                                          "100%" = 100)
                  )
              ),
              box(title = "Histogram box title", width = 6,
                  status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  plotOutput("plot", height = 250)
              )
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
sidebarUni <-dashboardPage(header, sidebar, body)
