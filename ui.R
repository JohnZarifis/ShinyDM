### Version Sea-8 (Sol)

source("SidebarUi.R")

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
              "Aqua Miner", 
              
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
                                                                                  value = 50, step=1))), 
                                                    bsCollapse(id = "collapseHistPlot" , # open = "Av. Weight", 
                                                            bsCollapsePanel("Av. Weight", style = "primary" ,
                                                   fluidRow(plotOutput("histPlotAvWeight")),
                                                   fluidRow(plotOutput("histPlotAvWeightDeviation"))),
                                                            bsCollapsePanel("KPI's", style = "primary" ,
                                                   fluidRow( plotOutput("histPlotPeriod.FCR")),
                                                   fluidRow( plotOutput("histPlotEcon.FCR")),
                                                   fluidRow( plotOutput("histPlotPeriod.SFR")),
                                                   fluidRow( plotOutput("histPlotPeriod.SGR")),
                                                   fluidRow( plotOutput("histPlotMortality"))),
                                                            bsCollapsePanel("Envirnomental", style = "primary" ,
                                                   fluidRow( plotOutput("histPlotPeriod.Day.Degrees")),
                                                   fluidRow( plotOutput("histPlotAvg.Temperature")),
                                                   fluidRow( plotOutput("histPlotPh")),
                                                   fluidRow( plotOutput("histPlotCAUDAL.O3")),
                                                   fluidRow( plotOutput("histPlotWATER.RENEWAL")),
                                                   fluidRow( plotOutput("histPlotNH3")),
                                                   fluidRow( plotOutput("histPlotNO2")))
                                        )), # end tabPanel Histograms 
                                        tabPanel("Density Plots",
                                                   fluidRow(plotOutput("densPlotAvWeight")),
                                                   fluidRow(plotOutput("densPlotAvWeightDeviation")),
                                                   fluidRow(plotOutput("densPlotPeriod.FCR")),
                                                   fluidRow(plotOutput("densPlotEcon.FCR")),
                                                   fluidRow(plotOutput("densPlotPeriod.SFR")),
                                                   fluidRow(plotOutput("densPlotPeriod.SGR")),
                                                   fluidRow(plotOutput("densPlotMortality")),
                                                   fluidRow(plotOutput("densPlotPeriod.Day.Degrees")),
                                                   fluidRow(plotOutput("densPlotAvg.Temperature")),
                                                   fluidRow( plotOutput("densPlotPh")),
                                                   fluidRow( plotOutput("densPlotCAUDAL.O3")),
                                                   fluidRow( plotOutput("densPlotWATER.RENEWAL")),
                                                   fluidRow( plotOutput("densPlotNH3")),
                                                   fluidRow( plotOutput("densPlotNO2"))
                                        ), # end tabPanel Density Plots
                                        tabPanel("Boxplots",
                                                   fluidRow(plotOutput("boxPlotAvWeight")),
                                                   fluidRow(plotOutput("boxPlotAvWeightDeviation")),  
                                                   fluidRow(plotOutput("boxPlotPeriod.FCR")),
                                                   fluidRow(plotOutput("boxPlotEcon.FCR")),
                                                   fluidRow(plotOutput("boxPlotPeriod.SFR")),
                                                   fluidRow(plotOutput("boxPlotPeriod.SGR")),
                                                   fluidRow(plotOutput("boxPlotMortality")),
                                                   fluidRow(plotOutput("boxPlotPeriod.Day.Degrees")),
                                                   fluidRow(plotOutput("boxPlotAvg.Temperature")),
                                                   fluidRow( plotOutput("boxPlotPh")),
                                                   fluidRow( plotOutput("boxPlotCAUDAL.O3")),
                                                   fluidRow( plotOutput("boxPlotWATER.RENEWAL")),
                                                   fluidRow( plotOutput("boxPlotNH3")),
                                                   fluidRow( plotOutput("boxPlotNO2"))
                                        ), # end tabPanel BoxPlots
                                        tabPanel("Summary Statistics", 
                                                    h4("End Average Weight:"),
                                                    tableOutput("summary_stats_EndAvWeight"),
                                                    hr(),
                                                    h4("Average Weight Deviation:"),
                                                    tableOutput("summary_stats_AvWeightDeviation"),
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
                                                    tableOutput("summary_stats_Avg.Temp"),
                                                    hr(),
                                                    h4("Ph:"),
                                                    tableOutput("summary_stats_Ph"),
                                                    hr(),
                                                    h4("CAUDAL O3 (Nm3/H):"),
                                                    tableOutput("summary_stats_CAUDAL.O3"),
                                                    hr(),
                                                    h4("WATER RENEWAL (l./min.):"),
                                                    tableOutput("summary_stats_WATER.RENEWAL"),
                                                    hr(),
                                                    h4("NH3 (ppm.):"),
                                                    tableOutput("summary_stats_NH3"),
                                                    hr(),
                                                    h4("NO2 (ppm.):"),
                                                    tableOutput("summary_stats_NO2")
                                                 
                                        ), # end tabPanel Summary Statistics
                                        tabPanel("Data", 
                                                    DT::dataTableOutput("dataset") 
                                        )
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
                                                      choices = list("None", "Region", "Site", "Batch", "Unit", "Hatchery",
                                                        "Origin.Month", "Origin.Year", "Current.Grading", 
                                                        "Feed Type"="Actual.Feed","Feed.Category","Feed Supplier"="Supplier"), selected = "None", inline = TRUE)),
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
                                         ),
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EconFCR.Ph")),
                                                    column(3, verbatimTextOutput("cor.stats.EconFCR.Ph")) 
                                           )
                                         )
                                       ) # end tabPanel "Scatter Plots"
                              ) # end tabsetPanel
                          ) # end fluidRow
                    ) # end fluidPage
            ),  # end tabPanel " Multivariate Statistics "  

#---------------------------------------------------------- Third MenuPage
            navbarMenu("Tools",
                       tabPanel("Dashboard ", id="MenuPage_3d", 
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
                       
                       # pivot Table
                       tabPanel("PivotTable", id="MenuPage_3piv", 
                                #rpivotTable::
                                rpivotTableOutput('pivTable', height = "800px") 
                        ) # end tabPanel Pivot Table
            ), # end navbarMenu Tools
            
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
                                          verbatimTextOutput("regression_R"),
                                   
                                          bsPopover(id="regression_R", title="Informations", 
                                                    content="R-Squared: measures the proportion of variability
                                                    in response variable that can be explained using predictors.",
                                                    placement = "right", trigger = "hover", 
                                                    options = list(container = "body"))
                                   )                               
                               ),
                              
                               fluidRow( column(12, h4('Residuals:'), 
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
                                 fluidRow(column(6,   
                                                 h3("Set values to Predictors:"),
                                                 uiOutput("dyn_input.Regression")
                                         ), 
                                         hr(),
                                         column(6, 
                                                actionButton(inputId = 'goRegressionPredict',  label = 'Start prediction'),
                                                hr(),
                                                h3("Prediction with Regression model..."),
                                                fluidRow(column(12, verbatimTextOutput("prediction.value.Regression")))
                                         ) # end column
                                 ) # end fluidRow 
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
                                        choices=c("Actual.Feed", "Current.Grading","End.Av.Weight.Category", "Feed.Category", 
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
              )  # mainPanel 
        ) # end fluidPage
),  # end tabPanel ANOVA   


#---------------------------------------------------------- sixth MenuPage
tabPanel(" Machine Learning Models ", id="MenuPage_6",
         fluidPage( # theme = shinytheme("cerulean"),
           sidebarPanel(
             img(src="Aquamanager-logo.png",class = "img-responsive"),
             hr(),
             radioButtons("radioML.task", label = h3("Choose Task:"), choices = list("Classification"=1,
                                                                                 "Regression"=2), selected=1),
             uiOutput("targs.ML.Variables"),
             hr(),
             uiOutput("preds.ML.Variables"),
             hr(),
             radioButtons("radioML.model", label = h3("Choose model:"), choices = list("Support Vector Machines"=1,
                                                        "GLMnet"=2), selected=1),
             hr(),
             radioButtons(inputId="testingOptions",label = h3("Testing Options:"), 
                          choices = list("Cross Validation" = 1, "Random Split" = 2), selected=1),
             hr(),
             uiOutput("TestOpts"),
             hr(),
             actionButton(inputId = 'goAnalysis',  label = 'Start Analysis')
           ),  # end sidebarPanel 
           mainPanel(tabsetPanel( 
             tabPanel("Build the Model",
                      h4('Formula:'), 
                      fluidRow(column(12, verbatimTextOutput("fmla.model"))),
                      hr(),
                      h4('Summary:'),
                      fluidRow(column(12, verbatimTextOutput("summary.model"))),
                      hr(),
                      h4('Evaluate the model on the whole Training Set:'),
                      fluidRow(column(12, verbatimTextOutput("validate.model"))),
                      
                      #------ relative importance
                      hr(),
                      h4(' Relative Importance:'),      
                      fluidRow(column(12, plotOutput("plot_ML.Rel.Impo",height="600px"))),
                      hr(),
                      fluidRow(column(12, verbatimTextOutput("ML.Rel.Impo")))
                      
             ), # end tabPanel "Build"
             tabPanel("Predict with it",
                      
              # predict response value using user-defined values for each predictor  
              fluidRow(column(6,   
                          h3("Set values to Predictors:"),
                          uiOutput("dyn_input")
                        ), 
                        hr(),
                        column(6, 
                          actionButton(inputId = 'goMLPredict',  label = 'Start prediction'),
                          hr(),
                          h3("Prediction with ML model..."),
                          fluidRow(column(12, verbatimTextOutput("prediction.value.ML")))
                        ) # end column
              ) # end fluidRow 
            ) # end tabPanel "Predict"
           ) # end tabsetPanel
         ) # end mainPanel
         
  ) # end fluidPage
), # end tabPanel SVMs
           

#---------------------------------------------------------- seventh MenuPage
tabPanel(" Classification ", id="MenuPage_7",
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
                       fluidRow(column(12, verbatimTextOutput("info_tree_acc") )),
                       hr(),
                       h4(' Rules of the Tree:'),
                       fluidRow(column(12, verbatimTextOutput("print_Tree.rules") ))
                       
                      ), # end tabPanel "Build"
                tabPanel("Predict with it",
                      
                      # predict response value using user-defined values for each predictor  
                      fluidRow(column(6,   
                                      h3("Set values to Predictors:"),
                                      uiOutput("dyn_input.DT")
                      ), 
                      hr(),
                      column(6, 
                             actionButton(inputId = 'goDTPredict',  label = 'Start prediction'),
                             hr(),
                             h3("Prediction with Classification/Regression Trees..."),
                             fluidRow(column(12, verbatimTextOutput("prediction.value.DT")))
                      ) # end column
                      ) # end fluidRow 
                ) # end tabPanel "Predict"
              ) # end tabsetPanel
           ) # end mainPanel
   
         ) # end fluidPage
) # end tabPanel " Classification "


  ) # end navbarPage
) # end shinyUI                                               
                                                   

                                       
                                                   