library("shiny")

shinyUI(fluidPage(
  title = 'Creating a UI from a loop',
  
  sidebarLayout(
    sidebarPanel(
      h2("Dimensions"),
      uiOutput("explanatoryVar"),
      actionButton(inputId = 'goSetValues',  label = 'Start Set Values')
      ),
    
    mainPanel(
      h2("Number of predictors"),
      verbatimTextOutput("numpreds"),
      hr(),
      
      h2("Set values to predictors"),
      # UI output
      uiOutput("dyn_input"),
      
      br(),
      h2("Prediction..."),
      fluidRow(column(12, verbatimTextOutput("value")))
      
    )
  )
))