### Version 28.02.2015

library("shiny")
library("lubridate")
library("mgcv")
library("htmltools")


# load helpers.R file
source("helpers.R")

#Datamining281114 <- read.delim("Datamining281114.csv", header = TRUE, sep = ";", dec=".")
Dataset <- read.delim("DMFeb.csv", header = TRUE, sep = ";", dec=".")


# Call function to create the dataset for analysis
df <- create_dataset(Dataset)

#-----------------------------------------------------------------------------------------------------
sidebarUni <- sidebarPanel(
  #fixed responsive img #added class img
  img(src="Aquamanager-logo.png" ,class = "img-responsive"),
  
  h2("Dimensions"),
  fluidRow(column(6,
                  selectInput(inputId='groupOrientation', label='Orientation', choices=c("All", unique(as.character(df$Orientation))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupSystem', label='System', choices=c("All", unique(as.character(df$System))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupBatch', label='Batch', choices=c("All", unique(as.character(df$Batch))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupSection', label='Section', choices=c("All", unique(as.character(df$Section))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupOriginMonth', label='Origin.Month', choices=c("All", unique(as.character(df$Origin.Month))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupOriginYear', label='Origin.Year', choices=c("All", unique(as.character(df$Origin.Year))), selected="All", multiple=TRUE)),
           column(6,
                  selectInput(inputId='groupHatchery', label='Hatchery', choices=c("All", unique(as.character(df$Hatchery))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupFood', label='Actual.Feed', choices=c("All", unique(as.character(df$Actual.Feed))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupFood.Category', label='Feed.Category', choices=c("All", unique(as.character(df$Feed.Category))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupSupplier', label='Supplier', choices=c("All", unique(as.character(df$Supplier))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupStartAvWeightBioCat', label='Start.Av.Weight.BioCat', choices=c("All", unique(as.character(df$Start.Av.Weight.BioCat))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupEndAvWeightBioCat', label='End.Av.Weight.BioCat', choices=c("All", unique(as.character(df$End.Av.Weight.BioCat))), selected="All", multiple=TRUE))),
  dateRangeInput('dateRangeFrom',
                 label = paste(' From: '),
                 start = min( ymd(df$From)-days(0) ), 
                 end = max( ymd(df$From)+days(1) ),
                 min = min( ymd(df$From)-days(0) ),
                 max = max( ymd(df$From)+days(1)),
                 separator = " to ", format = "dd/mm/yyyy",
                 startview = 'year', language = 'pt', weekstart = 0
  ),
  dateRangeInput('dateRangeTo',
                 label = paste(' To: '),
                 start = min( ymd(df$To)-days(1) ), 
                 end = max( ymd(df$To)+days(1) ),
                 min = min( ymd(df$To)-days(1) ),
                 max = max( ymd(df$To)+days(1) ),
                 separator = " to ", format = "dd/mm/yyyy",
                 startview = 'year', language = 'pt', weekstart = 0
  ),
  
  hr(),
  
  h2('Measures'),
  fluidRow(column(6,
                  sliderInput("rangeAvWeight", "End.Av.Weight:", min = min(as.double(df$End.Av.Weight)), 
                              max = max(as.double(df$End.Av.Weight)), 
                              value = c(min(as.double(df$End.Av.Weight)), max(as.double(df$End.Av.Weight))),
                              step=1.0, round=TRUE, sep="."), 
                  sliderInput("rangeAvWeightDev", "Av.Weight.Deviation:", 
                              min = min(as.double(df$Av.Weight.Deviation)), 
                              max = max(as.double(df$Av.Weight.Deviation)), 
                              value = c(min(as.double(df$Av.Weight.Deviation)), 
                                        max(as.double(df$Av.Weight.Deviation))),
                              step=1.0, round=-2, sep=".", post="%"),
                  sliderInput("rangePeriod.FCR", "Econ.FCR.Period:", min = min(as.double(df$Econ.FCR.Period)), 
                              max = max(as.double(df$Econ.FCR.Period)), 
                              value = c(min(as.double(df$Econ.FCR.Period)), max(as.double(df$Econ.FCR.Period))), 
                              step=0.1, round=-2, sep="."),
                  sliderInput("rangeLTD.Econ.FCR", "LTD.Econ.FCR:", min = min(as.double(df$LTD.Econ.FCR)), 
                              max = max(as.double(df$LTD.Econ.FCR)), 
                              value = c(min(as.double(df$LTD.Econ.FCR)), max(as.double(df$LTD.Econ.FCR))), 
                              step=0.5, round=-2, sep="."),
                  sliderInput("rangeAvg.Temp", "Avg.Temperature:", min = min(as.double(df$Avg.Temperature)), 
                              max = max(as.double(df$Avg.Temperature)), 
                              value = c(min(as.double(df$Avg.Temperature)), max(as.double(df$Avg.Temperature))), 
                              step=0.5, round=-2, sep=".")),
           column(6,
                  sliderInput("rangePeriod.SFR", "Period.SFR:", min = min(as.double(df$SFR.Period)), 
                              max = max(as.double(df$SFR.Period)), 
                              value = c(min(as.double(df$SFR.Period)), max(as.double(df$SFR.Period))), step=0.1, 
                              round=-2, sep="."),
                  sliderInput("rangePeriod.SGR", "Period.SGR:", min = min(as.double(df$SGR.Period)), 
                              max = max(as.double(df$SGR.Period)), 
                              value = c(min(as.double(df$SGR.Period)), max(as.double(df$SGR.Period))), step=0.1, 
                              round=-2, sep="."),
                  sliderInput("rangeLTD.Mortality", "LTD.Mortality:", 
                              min = min(as.double(df$LTD.Mortality)),
                              max = max(as.double(df$LTD.Mortality)), 
                              value = c(min(as.double(df$LTD.Mortality)), 
                                        max(as.double(df$LTD.Mortality))), 
                              step=1, round=-2, sep="."),
                  sliderInput("rangePeriod.Day.Degrees", "Period.Day.Degrees:", 
                              min = min(as.double(df$Period.Day.Degrees), na.rm=TRUE), 
                              max = max(as.double(df$Period.Day.Degrees), na.rm=TRUE), 
                              value = c(min(as.double(df$Period.Day.Degrees)), 
                                        max(as.double(df$Period.Day.Degrees))), 
                              step=10, round=TRUE, sep="."),
                  sliderInput("rangePeriod.Feed.Qty", "Period.Feed.Qty:", 
                              min = min(as.double(df$Period.Feed.Qty), na.rm=TRUE), 
                              max = max(as.double(df$Period.Feed.Qty), na.rm=TRUE), 
                              value = c(min(as.double(df$Period.Feed.Qty)), 
                                        max(as.double(df$Period.Feed.Qty))), 
                              step=10, round=TRUE, sep="."))
  ), # end fluid row
  
  hr(),
  radioButtons("radioDimUni", label = h3("Separate The Dataset By:"), 
               choices = list("None", "Orientation", "System", "Section", "Batch", "Hatchery",
                              "Origin.Month", "Origin.Year", "Start.Av.Weight.BioCat", 
                              "End.Av.Weight.BioCat", "Actual.Feed"), selected = "None"),
  
  hr(),
  actionButton(inputId = 'goUniPlot',  label = 'Refresh Univariate plots')
  
) # end sidebarUni function

#-------------------------------------------------------------------------------
# sidebarMulti <- sidebarPanel(
#   
#   img(src="Aquamanager-logo.png"),
#   
#   radioButtons("radioDimMulti", label = h3("Separate The Dataset By:"), 
#                choices = list("Orientation", "System", 'Section', "Hatchery",
#                               "Origin.Month", "Origin.Year"), selected = "Orientation", inline = TRUE),
#   
#    actionButton(inputId = 'goMultiPlot',  label = 'Refresh Multivariate plots')
#  
# ) # end sidebarMulti function


#----------------------------------------------------
# 
shinyUI( 
  navbarPage( theme = "bootstrap.css",
              "Aquamanager Data Miner", 
              
              #---------------------------------------------------------- First MenuPage
              tabPanel(" Univariate Statistics ", id="MenuPage_1", 
                       fluidPage( theme = "bootstrap.css", 
                                 # titlePanel("Exploratory Data Analysis"),
                                  sidebarLayout(
                                    sidebarUni,
                              
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Histograms",
                                                   fluidRow(column(3, sliderInput("numbins", "Number of bins:", 
                                                                                  min = 5, max = 100, 
                                                                                  value = 20, step=1))), 
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
                                    )# end mainPanel
                                  ) # end sidebarLayout
                       )  # end fluidPage
              ), # end tabPanel "Univariate Statistics"

# ---------------------------------------------------------- Second MenuPage
            tabPanel(" Multivariate Statistics ", id="MenuPage_2", 
                fluidPage( #theme = shinytheme("cerulean"),
                          #titlePanel("Exploratory Data Analysis"),
                          fluidRow( column(9, radioButtons("radioDimMulti", label = h3("Separate The Dataset By:"), 
                                                      choices = list("None", "Orientation", "System", "Batch", "Section", "Hatchery",
                                                        "Origin.Month", "Origin.Year", "Start.Av.Weight.BioCat", 
                                                        "End.Av.Weight.BioCat", "Actual.Feed"), selected = "None", inline = TRUE)),
                                    column(3, actionButton(inputId = 'goMultiPlot',  label = 'Refresh Multivariate plots'))
                          ), # end fluidRow
                          hr(),
                          fluidRow(
                              tabsetPanel( 
                                 tabPanel("Scatter Matrix Plots",
                                           plotOutput("scatterMatrixPlot",height="600px") 
                                 ), # end tabPanel "Scatter Matrix Plots"
                                
                                 tabPanel("Scatter Plots",
                                         wellPanel(
                                         fluidRow(column(9, plotOutput("scatterPlot.EndAvWeight.PeriodFCR")),
                                                  column(3, verbatimTextOutput("cor.stats.EndAvWeight.PeriodFCR")) 
                                                 )
                                         ),  
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EndAvWeight.PeriodSFR")),
                                                    column(3, verbatimTextOutput("cor.stats.EndAvWeight.PeriodSFR")) 
                                           )
                                         ),  
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EndAvWeight.PeriodSGR")),
                                                    column(3, verbatimTextOutput("cor.stats.EndAvWeight.PeriodSGR")) 
                                                   )
                                         ),  
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EndAvWeight.AvgTemp")),
                                                    column(3, verbatimTextOutput("cor.stats.EndAvWeight.AvgTemp")) 
                                           )
                                         ),
                                         #..........................................................................
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.PeriodEcon.FCR.PeriodSFR")),
                                                    column(3, verbatimTextOutput("cor.stats.PeriodEcon.FCR.PeriodSFR")) 
                                           )
                                         ),  
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.PeriodEcon.FCR.PeriodSGR")),
                                                    column(3, verbatimTextOutput("cor.stats.PeriodEcon.FCR.PeriodSGR")) 
                                           )
                                         ),  
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.PeriodFCR.AvgTemp")),
                                                    column(3, verbatimTextOutput("cor.stats.PeriodFCR.AvgTemp")) 
                                           )
                                         ),  
                                         #..........................................................................
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.PeriodSFR.PeriodSGR")),
                                                    column(3, verbatimTextOutput("cor.stats.PeriodSFR.PeriodSGR")) 
                                           )
                                         ),  
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.PeriodSFR.AvgTemp")),
                                                    column(3, verbatimTextOutput("cor.stats.PeriodSFR.AvgTemp")) 
                                           )
                                         ),  
                                         #..........................................................................
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.PeriodSGR.AvgTemp")),
                                                    column(3, verbatimTextOutput("cor.stats.PeriodSGR.AvgTemp")) 
                                           )
                                         ),
                                         #..........................................................................
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EconFCR.EndAvWeight")),
                                                    column(3, verbatimTextOutput("cor.stats.EconFCR.EndAvWeight")) 
                                           )
                                         ),
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EconFCR.FCRPeriod")),
                                                    column(3, verbatimTextOutput("cor.stats.EconFCR.FCRPeriod")) 
                                           )
                                         ),
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EconFCR.SFRPeriod")),
                                                    column(3, verbatimTextOutput("cor.stats.EconFCR.SFRPeriod")) 
                                           )
                                         ),
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EconFCR.SGRPeriod")),
                                                    column(3, verbatimTextOutput("cor.stats.EconFCR.SGRPeriod")) 
                                           )
                                         ),
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EconFCR.AvgTemp")),
                                                    column(3, verbatimTextOutput("cor.stats.EconFCR.AvgTemp")) 
                                           )
                                         )
                                       ) # end tabPanel "Scatter Plots"
                              ) # end tabsetPanel
                          ) # end fluidRow
                    ) # end fluidPage
            ),  # end tabPanel " Multivariate Statistics "  

#---------------------------------------------------------- Third MenuPage
            tabPanel(" Multidimensional Dashboard ", id="MenuPage_3", 
                      fluidPage( #theme = shinytheme("cerulean"),
                                 plotOutput('plotDashboard',height="600px"),
                                 hr(),
                                 fluidRow(
                                   column(3,
                                          h4("Multidimensional Exploration"),
                                          dateRangeInput('MD.dateRangeFrom',
                                                         label = paste(' From: '),
                                                         start = min( ymd(df$From)-days(0) ), 
                                                         end = max( ymd(df$From)+days(1) ),
                                                         min = min( ymd(df$From)-days(0) ),
                                                         max = max( ymd(df$From)+days(1) ),
                                                         separator = " to ", format = "dd/mm/yyyy",
                                                         startview = 'year', language = 'pt', weekstart = 0
                                          ),
                                          br(),
                                          dateRangeInput('MD.dateRangeTo',
                                                         label = paste(' To: '),
                                                         start = min( ymd(df$To)-days(1) ), 
                                                         end = max( ymd(df$To)+days(1) ),
                                                         min = min( ymd(df$To)-days(1) ),
                                                         max = max( ymd(df$To)+days(1) ),
                                                         separator = " to ", format = "dd/mm/yyyy",
                                                         startview = 'year', language = 'pt', weekstart = 0
                                          )
                                   ),
                                   column(3, 
                                          selectInput('x', 'X', choices=names(df), selected="To"),
                                          selectInput('y', 'Y', choices=names(df), selected="End.Av.Weight"),
                                          selectInput('color', 'Color', choices=c('None', names(df)))
                                   ),
                                   column(3,
                                          selectInput('facet_row', 'Facet Row',
                                                      c(None='.', names(df[sapply(df, is.factor)]))),
                                          selectInput('facet_col', 'Facet Column',
                                                      c(None='.', names(df[sapply(df, is.factor)])))
                                   ),
                                   column(3,
                                          checkboxInput('xmeans', 'X-axis mean'),
                                          checkboxInput('ymeans', 'Y-axis mean'),
                                          checkboxInput('total.xmeans', 'Total X-axis mean'),
                                          checkboxInput('total.ymeans', 'Total Y-axis mean'),
                                          checkboxInput('smooth', 'Smooth'),
                                          checkboxInput('comp.ranges', 'Compare Ranges'),
                                          checkboxInput('benchmarker', 'Benchmarker')
                                   ),
                                   fluidRow(column(12, hr()))
                                 ) # end fluidRow
                      ) # end fluidPage
            ),  # end tabPanel Multidimensional Dashboard    
            
#---------------------------------------------------------- Third_Interactive MenuPage
# tabPanel(" Multidimensional Interactive Dashboard ", id="MenuPage_3i", 
#          fluidPage( theme = shinytheme("cerulean"),
#                     metricsgraphicsOutput('plot.Interactive.Dashboard'),
#                     hr(),
#                     fluidRow(
#                       column(3,
#                              h4("Multidimensional Exploration"),
#                              dateRangeInput('MD.dateRangeFrom',
#                                        label = paste(' From: '),
#                                        start = min( ymd(df$From)-days(0) ), 
#                                        end = max( ymd(df$From)+days(1) ),
#                                        min = min( ymd(df$From)-days(0) ),
#                                        max = max( ymd(df$From)+days(1) ),
#                                        separator = " to ", format = "dd/mm/yyyy",
#                                        startview = 'year', language = 'pt', weekstart = 0
#                                       ),
#                                       br(),
#                                       dateRangeInput('MD.dateRangeTo',
#                                           label = paste(' To: '),
#                                           start = min( ymd(df$To)-days(1) ), 
#                                           end = max( ymd(df$To)+days(1) ),
#                                           min = min( ymd(df$To)-days(1) ),
#                                           max = max( ymd(df$To)+days(1) ),
#                                           separator = " to ", format = "dd/mm/yyyy",
#                                           startview = 'year', language = 'pt', weekstart = 0
#                                       )
#                                ),
#                                column(3, 
#                                       selectInput('x', 'X', choices=names(df), selected="To"),
#                                       selectInput('y', 'Y', choices=names(df), selected="End.Av.Weight"),
#                                       selectInput('color', 'Color', choices=c('None', names(df)))
#                                ),
#                                column(3,
#                                       selectInput('facet_row', 'Facet Row',
#                                                   c(None='.', names(df[sapply(df, is.factor)]))),
#                                       selectInput('facet_col', 'Facet Column',
#                                                   c(None='.', names(df[sapply(df, is.factor)])))
#                                ),
#                                column(3,
#                                       checkboxInput('xmeans', 'X-axis mean'),
#                                       checkboxInput('ymeans', 'Y-axis mean'),
#                                       checkboxInput('total.xmeans', 'Total X-axis mean'),
#                                       checkboxInput('total.ymeans', 'Total Y-axis mean'),
#                                       checkboxInput('smooth', 'Smooth'),
#                                       checkboxInput('comp.ranges', 'Compare Ranges'),
#                                       checkboxInput('benchmarker', 'Benchmarker')
#                                ),
#                                fluidRow(column(12, hr()))
#                              ) # end fluidRow
#                       ) # end fluidPage
#                     ),  # end tabPanel Multidimensional Interactive Dashboard   

#---------------------------------------------------------- Forth MenuPage
tabPanel(" Regression Models ", id="MenuPage_4", 
          fluidPage( #theme = shinytheme("cerulean"),
                    sidebarPanel(
                      img(src="Aquamanager-logo.png",class = "img-responsive"),
                      hr(),
                      selectInput(inputId='responseVar', label=h3('Response Variable:'), 
                                  choices=c("End.Av.Weight", "Days", "Econ.FCR.Period", 
                                            "SFR.Period", "SGR.Period",
                                            "LTD.Mortality"), 
                                  selected=NULL, multiple=FALSE),
                      hr(),
                      uiOutput("explanatoryVar"),
                      hr(),
                      radioButtons("radioModel", label = h3("Regression model..."), 
                                    choices = list("Multiple Linear Regression"=1, "Generalized Linear Models (GLM)"=2, 
                                                   "Generalized Additive Models (GAM)" = 3), selected = 1),
                      hr(),
                      actionButton(inputId = 'goRegression',  label = 'Build Regression Model')
                      
                    ),  # end sidebarPanel 
                    mainPanel(tabsetPanel( 
                        tabPanel("Build the Model", 
                               h4('Formula:'),   
                               fluidRow(column(12, verbatimTextOutput("fmla"))),
                               hr(),
                               fluidRow(
                                   column(12, h4('Percentage of Variation explained by the regression line:'),
                                          verbatimTextOutput("regression_R")),
                                   column(12, h4('Residuals:'), 
                                          verbatimTextOutput("regression_Table_residuals"))),
                               hr(),
                               fluidRow(column(12, plotOutput("plot_lm_13"))),
                               hr(),
                               fluidRow(column(12, plotOutput("plot_lm_24"))),
                               hr(),
                               conditionalPanel( "input.radioModel != 3 ", 
                                      fluidRow(column(12,h4('Significance of the Regression Coefficients at LM or GLM model:'),
                                                  verbatimTextOutput("regression_Table_sign_coeff")
                                               ))),
                               conditionalPanel( "input.radioModel == 3 ", 
                                      fluidRow(column(12, h4('Parametric coefficients at GAM model:'),
                                                  verbatimTextOutput("regression_Table_sign_param_gam"))),
                                      fluidRow(column(12, h4('Approximate significance of smooth terms at GAM model:'),
                                                  verbatimTextOutput("regression_Table_sign_coeff_gam")))
                               ),
                               hr(),
                               conditionalPanel( "input.radioModel != 3 ", 
                                        fluidRow(column(12, h4('Confidence Intervals:'),
                                                 verbatimTextOutput("regression_CI"))),
                                        hr(),
                                        fluidRow(column(12, h4('Analysis of Variance:'),
                                                 verbatimTextOutput("regression_Anova"))),
                                        hr(),
                                        fluidRow(column(12, h4('Outliers Observations:'),
                                                 verbatimTextOutput("regression_outliers"))),
                                        hr(),
                                        h4(' Influential Observations:'),                                       
                                        fluidRow(column(12, plotOutput("plot_Infl"))),
                                        hr(),
                                        fluidRow(column(12, dataTableOutput("table_Infl")))
                               ),
                               hr(),
                               conditionalPanel( "input.radioModel == 3 ",
                                                 fluidRow(column(12, h4('Coefficients'),
                                                 verbatimTextOutput("gam.mod.coeffs")))
                               ),                   
                               hr(),
                               h4(' Relative Importance:'),      
                               fluidRow(column(12, plotOutput("bar.Rel.Impo"))),
                               fluidRow(column(12, verbatimTextOutput("Rel.Impo"))),              
                               hr()
                                                   
                        ), # end tabPanel "Build"
                        tabPanel("Predict with it",
                                 
                                 # prediction using Training data set
                                 h2("Prediction using Training Data Set:"),
                                 fluidRow(column(12, plotOutput("pred.actual.plot"))),
                                 hr(),
                                 # predict response value using user-defined values for each predictor  
                                 h2("Numeric inputs:"),
                                 fluidRow(column(3, numericInput('num_Start.Av.Weight', label = h4("Start.Av.Weight"), value = NA)),
                                          column(3, numericInput("num_Days", label = h4("Days"), value = NA)),
                                          column(3, numericInput('num_Period.Feed.Qty', label = h4("Period.Feed.Qty"), value = NA)),
                                          column(3, numericInput('num_Suggested.Feed.Qty', label = h4("Suggested.Feed.Qty"), value = NA))
                                 ),
                                 fluidRow(column(3, numericInput('num_FCR', label = h4("Econ.FCR.Period"), value = NA)),
                                          column(3, numericInput('num_SFR.Period', label = h4("SFR.Period"), value = NA)),
                                          column(3, numericInput('num_SGR.Period', label = h4("SGR.Period"), value = NA))
                                 ),
                                 fluidRow(column(3, numericInput('num_LTD.Mortality', label = h4("LTD.Mortality"), value = NA)),
                                          column(3, numericInput("num_Avg.Temperature", label = h4("Avg.Temperature"), value = NA)),
                                          column(3, numericInput('num_End.Av.Weight', label = h4("End.Av.Weight"), value = NA))
                                 ),
                                 hr(),
                                 actionButton(inputId = 'goPredict',  label = 'Start prediction'),
                                 hr(),
                                 h2("Prediction..."),
                                 fluidRow(column(12, verbatimTextOutput("value")))
                               
                        ) # end tabPanel "Predict"
                      ) # end tabsetPanel 
                    ) # end mainPanel

            ) # end fluidPage
),  # end tabPanel " Regression Models "  


#---------------------------------------------------------- fifth MenuPage
tabPanel(" Analysis Of Variance ", id="MenuPage_5", 
        fluidPage( # theme = shinytheme("cerulean"),
              sidebarPanel(
                     img(src="Aquamanager-logo.png",class = "img-responsive"),
                     hr(),
                     checkboxGroupInput(inputId='Dep.Var', label=h3('Dependent Variable:'), 
                                        choices=c("Econ.FCR.Period", "LTD.Econ.FCR", "SFR.Period", "SGR.Period"), 
                                        selected="Econ.FCR.Period"),
                     hr(),
                     checkboxGroupInput(inputId='Ind.Vars', label=h3('Independent Variable(s):'), 
                                        choices=c("Actual.Feed", "End.Av.Weight.BioCat", "Feed.Category", 
                                                     "Feeding.Policy", "System", "Orientation", "Origin.Month", "Origin.Year"), 
                                        selected="Actual.Feed"),
                     hr(),
                     radioButtons(inputId="anova.test", label = h3("Anova Test..."), 
                                  choices = list("One-way ANOVA"=1, "Two-way ANOVA"=2, 
                                                 "Factorial ANOVA"=3), selected=1),
                     hr(),
                     actionButton(inputId = 'goANOVA',  label = 'Start Analysis')
                     
              ),  # end sidebarPanel 
              mainPanel(    
                     h4('Formula:'),   
                     fluidRow(column(12, verbatimTextOutput("fmla.aov"))),
                     hr(),
                     h4('Summary:'),
                     fluidRow(column(12, verbatimTextOutput("summary.aov"))),
                     hr(),
                     h4('Tukey’s honest significant differences:'),
                     fluidRow(column(12, plotOutput("plot.TukeyHSD"))),
                     hr(),
                     fluidRow(column(12, verbatimTextOutput("sign.diffs.Tukey"))),
                     hr(),
                     fluidRow(column(12, plotOutput("plot.aov")))
                     
            ) # end mainPanel
                
        ) # end fluidPage
),  # end tabPanel ANOVA   

#---------------------------------------------------------- sixth MenuPage
tabPanel(" Classification ", id="MenuPage_6",
         fluidPage( # theme = shinytheme("cerulean"),
           sidebarPanel(
             img(src="Aquamanager-logo.png",class = "img-responsive"),
             hr(),
             radioButtons("radioDesTree", label = h3("Decision Tree for:"), choices = list("Classification"=1, "Regression"=2), selected=1),
             hr(),
             uiOutput("targs.Variables"),
             hr(),
             uiOutput("preds.Variables"),
             hr(),
             actionButton(inputId = 'goDT',  label = 'Start Analysis')
           ),  # end sidebarPanel 
          mainPanel(tabsetPanel( 
                tabPanel("Build the Tree",
                       h4('Formula:'), 
                       fluidRow(column(12, verbatimTextOutput("fmla.dec.Trees"))),
                       hr(),
                       fluidRow(column(12, plotOutput("plot_dec.Tree") )),
                       
                       h4(' Information on the predictive accuracy of the Tree:'),
                       fluidRow(column(12, verbatimTextOutput("info_tree_abs_relacc") )),
                       hr(),
                       h4(' Relative Importance:'),      
                       fluidRow(column(12, plotOutput("plot_RegClass.Rel.Impo"))),
                       hr(),
                       fluidRow(column(12, verbatimTextOutput("RegClass.Rel.Impo"))),              
                       hr(),
                       fluidRow(column(12, dataTableOutput("info_tree_acc") )),
                       hr(),
                       h4(' Rules of the Tree:'),
                       fluidRow(column(12, verbatimTextOutput("print_Tree.rules") ))
                       
                      ) # end tabPanel "Build"
                # tabPanel("Predict with it"
                      # fluidRow(column(12, dataTableOutput(outputId="test.set.class.prediction")))  
                # ) # end tabPanel "Predict"
              ) # end tabsetPanel
           ) # end mainPanel
   
         ) # end fluidPage
) # end tabPanel " Classification "





  ) # end navbarPage
) # end shinyUI                                               
                                                   

                                       
                                                   