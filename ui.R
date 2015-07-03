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
                       )  # end tabPanel Multidimensional Dashboard    
                       
                       # pivot Table
                        ,tabPanel("PivotTable", id="MenuPage_3piv", 
                                # rpivotTable::rpivotTableOutput('pivTable', height = "800px") 
                                 rpivotTableOutput("pivTable", height = "800px") 
                      
                        ) # end tabPanel Pivot Table
                                               ,tabPanel("Interactive DashBoard"
                                                         ,fluidRow(column(12,dimpleOutput("dimple",width = "100%", height = "600px")))
                                                         ,fluidRow(column(12
                                                                          ,selectInput('xVar', 'X', choices=names(df), selected="Start.Av.Weight") # ToDO is numeric
                                                                         ,selectInput('yVar', 'Y', choices=names(df), selected="End.Av.Weight")
                                                                          ,selectInput('colori', 'Color', choices=names(df),selected="Origin.Year")
                                                                         ,selectInput('size', 'Size', choices=names(df), selected="Closing.Biomass")
                                                                          )))
                       ,tabPanel("HeatMap"
                                 
                                 ,d3heatmapOutput('HeatMap'
                                                  ,height = "800px"
                                                  
                                                  )
                                 ,selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues","Spectral"))
                                 ,checkboxInput("cluster", "Apply clustering")
                                 ,h1("-")
                                 
                       )

                                 
            ), # end navbarMenu Tools
            
  
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
                                         verbatimTextOutput("regression_R") ),
                                   bsPopover(id="regression_R", title=paste0("Information about the models goodness:") 
                                               ,content = paste0("<b>R-Squared:</b> Metric for evaluating the goodness of fit of your model. "
                                               , " Higher is better with 1 being the best. It measures the proportion of variability "
                                               , " in response variable that can be explained using predictors. " 
                                               , " Adjusted R-Squared: R-squared on data will be higher for models with more input "
                                               , " parameters, independently of whether the additional variables actually improve the "
                                               , " model or not. That&#39s why the adjusted R-squared is a more "
                                               , " realistic assessment of model&#39s effectiveness, due to the fact that it adjusts R-Squared "
                                               , " for the number of explanatory variables in the equation."
                                               , " Akaike&#39s Information Criterion - AIC: a log-likelihood value can be obtained, according to the formula "
                                               , " -2*log-likelihood + k*npar, where npar represents the number of parameters in the fitted model, " 
                                               , " and k = 2 for the usual AIC. The AIC is usually used to decide which and how many input variables to use "
                                               , " in the model. If you train many different models with different sets of variables on the same "
                                               , " training set, you can consider the model with the lowest AIC to be the best fit. ") 
                                               , placement = "bottom"
                                               , trigger = "click"
                                               , options = list(container = "body") )
                               ),
                              
                               fluidRow( column(12, h4('Residuals:'), 
                                          verbatimTextOutput("regression_Table_residuals")),
                                          bsPopover(id="regression_Table_residuals", title="Information about Residuals:",
                                                   content=paste0("The residuals are the difference between the actual values"
                                                                  , " of the variable you &#39re predicting and predicted values from your regression model."
                                                                  ),
                                                   placement = "bottom", trigger = "click", options = list(container = "body") )
                               ),
                               hr(),
                               fluidRow(column(12, plotOutput("plot_lm_13")),
                                        bsPopover(id="plot_lm_13", title="Information about plots:",
                                                  content=paste0("<b>Residual graphs</b> with predictions (Fitted values) on the x-axis " 
                                                  ,"gives a sense of when the model may be under- or overpredicting, " 
                                                  ,"based on the model’s output. " 
                                                  ,"The first graph (left) shows residuals on the y axis against fitted "
                                                  ,"values on the x axis. If the dependent variable is linearly related to the "
                                                  ,"independent variables, there should be no systematic relationship between the "
                                                  ,"residuals and the predicted (that is, fitted) values. Ideally, the points will "
                                                  ,"all lie very close to that line. Instead, if input variables don’t explain the output "
                                                  ,"too closely a wider cloud of points is received, which suggests that a quadratic "
                                                  ,"term ought to be added to the regression model. "
                                                  ,"The second graph (right) is a repeat of the first, but on a different scale; it shows "
                                                  ,"the square root of the standardized residuals (where all the values are positive) "
                                                  ,"against the fitted values. If there was a problem, such as the variance increasing "
                                                  ,"with the mean, then the points would be distributed inside a triangular shape, "
                                                  ,"with the scatter of the residuals increasing as the fitted values increase. "
                                                  ,"If the constant variance assumption (Homoscedasticity) is met, the points in "
                                                  ,"the Scale-Location graph (bottom left) should be a random band around a horizontal line."),
                                                  placement = "bottom", trigger = "click" )
                               ),
                               hr(),
                               fluidRow(column(12, plotOutput("plot_lm_24")),
                                        bsPopover(id="plot_lm_24", title="Information about plots:",
                                                  content=paste0("If the dependent variable is normally distributed for a fixed set of "
                                                  ,"predictor values, then the residual values should be normally distributed with a "
                                                  ,"mean of 0. The Normal Q-Q plot (left graph) is a probability plot of the standardized "
                                                  ,"residuals against the values that would be expected under normality. If "
                                                  ,"the normality assumption is met, the points on this graph should fall on the "
                                                  ,"straight 45-degree line. If the pattern were S-shaped or banana-shaped, we would need "
                                                  ,"to fit a different model to the data. "
                                                  ,"A Cook$#39s Distance plot (right graph) presents the influential observations, which are "
                                                  ,"observations that have a disproportionate impact on the determination of the model "
                                                  ,"parameters. Imagine finding that your model changes dramatically with the removal "
                                                  ,"of a single observation. Influential observations are identified using a statistic " 
                                                  ,"called Cook$#39s distance, or Cook$#39s D. Cook$#39s D values greater than 4/(n-k-1), where "
                                                  ,"n is the sample size and k is the number of predictor variables indicate influential observations."),
                                                  placement = "bottom", trigger = "click", options = list(container = "body") )
                               ),
                               hr(),
                               conditionalPanel( "input.radioModel != 3 ", 
                                      fluidRow(column(12,h4('Significance of the Regression Coefficients at LM or GLM model:'),
                                                  verbatimTextOutput("regression_Table_sign_coeff") ),
                                               bsPopover(id="regression_Table_sign_coeff", 
                                                         title=paste0("Information about Significance of the Regression Coefficients:"),
                                                         content=paste0("Each model coefficient forms a row of the summary coefficients table. " 
                                                         ,"The columns report the estimated coefficient (column Estimate) "
                                                         ,"for the intercept and each independent variable, the uncertainty of the "
                                                         ,"estimate (column Std. Error), how large the coefficient is relative to the uncertainty " 
                                                         ,"(column t value), and how likely (probability) such a ratio would be due to mere "
                                                         ,"chance (column p-value). The p-value gauges the likelihood that the coefficient is not "
                                                         ,"significant, so smaller is better. Big is bad because it indicates a high likelihood of "
                                                         ,"insignificance (the variable is worthless; it adds nothing to the model). "
                                                         ,"In common practice, a cutoff 0.05 is used to determine statistical significance. " 
                                                         ,"In general, it is better to have significant coefficients and models, because "
                                                         ,"statistical significance indicates that our results are more likely to be genuine " 
                                                         ,"and unlikely to have occurred by random chance."),
                                                         placement = "bottom", trigger = "click", options = list(container = "body") )
                                      )
                               ),
                               conditionalPanel( "input.radioModel == 3 ", 
                                      fluidRow(column(12, h4('Parametric coefficients at GAM model:'),
                                                  verbatimTextOutput("regression_Table_sign_param_gam")) ),
                                      fluidRow(column(12, h4('Approximate significance of smooth terms at GAM model:'),
                                                  verbatimTextOutput("regression_Table_sign_coeff_gam")) )
                               ),
                               hr(),
                               conditionalPanel( "input.radioModel != 3 ", 
                                        fluidRow(column(12, h4('Confidence Intervals:'),
                                                 verbatimTextOutput("regression_CI")),
                                                 bsPopover(id="regression_CI", 
                                                           title="Information about Confidence Intervals:",
                                                           content="Confidence intervals for the regression coefficients.",
                                                           placement = "bottom", trigger = "click", options = list(container = "body"))
                                        ),
                                        hr(),
                                        fluidRow(column(12, h4('Analysis of Variance:'),
                                                 verbatimTextOutput("regression_Anova")),
                                                 bsPopover(id="regression_Anova", 
                                                           title="Information about ANOVA test:",
                                                           content=paste0("A sequential ANalysis Of VAriance (ANOVA) table assesses the contribution "
                                                           ,"of each predictor variable to the model in turn, assuming inclusion of previously " 
                                                           ,"assessed variables. The F-statistic is used to measure whether the regression model " 
                                                           ,"predicts outcome better than the constant mode (the mean value of y). "
                                                           ,"The F-statistic gets its name from the F-test, which is the technique used to check "
                                                           ,"if the variance of the residuals from the constant model and the variance of the "
                                                           ,"residuals from the linear model are significantly different. The model "
                                                           ,"is significant if any of the coefficients are nonzero. It is insignificant if all "
                                                           ,"coefficients are zero. "),
                                                           placement = "bottom", trigger = "click", options = list(container = "body"))
                                        ),
                                        hr(),
                                        fluidRow(column(12, h4('Outliers Observations:'),
                                                 verbatimTextOutput("regression_outliers")),
                                                 bsPopover(id="regression_outliers", 
                                                           title="Information about Outliers:",
                                                           content=paste0(" Outliers are observations that aren’t predicted well by the model. " 
                                                           ,"They have either unusually large positive or negative residuals. Positive "
                                                           ,"residuals indicate that the model is underestimating the response value, "
                                                           ,"while negative residuals indicate an overestimation."),
                                                           placement = "bottom", trigger = "click", options = list(container = "body"))
                                                 ),
                                        hr(),
                                        h4(' Influential, Leverage and Outliers Observations:'),                                       
                                        fluidRow(column(12, plotOutput("plot_Infl",height="600px"))),
                                        bsPopover(id="plot_Infl", 
                                                  title="Information about Leverage and Influencial Observations:",
                                                  content=paste0("Leverage points are those observations, if any, made at extreme or outlying values of the "
                                                  ,"independent variables such that the lack of neighboring observations means that the fitted "
                                                  ,"regression model will pass close to that particular observation. "
                                                  ,"Observations that have high leverage are outliers with regard to the other predictors. "
                                                  ,"In other words, they have an unusual combination of predictor values. The response "
                                                  ,"value is not involved in determining leverage. High leverage observations may or may not be "
                                                  ,"influential observations. That will depend on whether they are also outliers. "
                                                  ,"Influential observations are observations that have a disproportionate impact on the "
                                                  ,"values of the model parameters. Imagine finding that your model changes dramatically "
                                                  ,"with the removal of a single observation. It is this concern that leads you to examine "
                                                  ,"your data for influential points. "
                                                  ,"<b>Influence plot:</b> States above +2 or below –2 on the Studentized-residual scale (vertical axis) "
                                                  ,"are considered outliers. States above 2 or 3 times the average hat value (horizontal axis) have high "
                                                  ,"leverage (unusual combinations of predictor values). Circle size is proportional "
                                                  ,"to influence. Observations depicted by large circles may have disproportionate "
                                                  ,"influence on the parameters estimates of the model."),
                                                  placement = "bottom", trigger = "click", options = list(container = "body"))
                                        ),
                                        hr(),
                                        fluidRow(column(12, dataTableOutput("table_Infl"))),
                                        
                               hr(),
                               conditionalPanel( "input.radioModel == 3 ",
                                                 fluidRow(column(12, h4('Coefficients'),
                                                 verbatimTextOutput("gam.mod.coeffs")))
                               ),                   
                               hr(),
                               h4(' Relative Importance:'),      
                               fluidRow(column(12, plotOutput("bar.Rel.Impo"))),
                               fluidRow(column(12, verbatimTextOutput("Rel_Impo")),
                                        bsPopover(id="Rel_Impo", 
                                                  title="Which variables are most important in predicting the outcome?",
                                                  content= paste0("Rank-order the predictors in terms of relative importance. "
                                                  ,"Relative importance can be thought of as the contribution each predictor "
                                                  ,"makes to R-square, both alone and in combination with other predictors."),
                                                  placement = "bottom", trigger = "click", options = list(container = "body"))),              
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
                     fluidRow(column(12, verbatimTextOutput("summary_aov")),
                            bsPopover(id="summary_aov", 
                                      title="Analysis of Variance:",
                                       content= paste0("<b>One-way ANOVA:</b> when the explanatory variable is categorical (factor) "
                                                        ," with three or more levels. Here, we test the null hypothesis (H0) that "
                                                        ," the means of all levels are equal. The alternative hypothesis is that "
                                                        ," at least one of the means is significantly different from the others. "
                                                        ," F ratio can be used to determine whether the statistically significant "
                                                        ," differenced exist between the levels. The p-value indicates the likelihood "
                                                        ," that the level is not significant, so smaller is better. If p-value is less "
                                                        ," than the significance level a, then the null hypothesis is rejected in favor "
                                                        ," of the alternative. "
                                                        ,"<b>Two-way ANOVA:</b> when the explanatory variables are categorical (factors). "
                                                        ," Crossing two factors produces a two-way ANOVA."
                                                        ,"<b>Factorial ANOVA:</b> crossing two or more factors. For each factor, we have "
                                                        ," F-tests for each factor and for each interaction between factors."),
                                      placement = "bottom", trigger = "click", options = list(container = "body"))
                     ),
                     hr(),
                     h4('Tukey’s honest significant differences:'),
                     fluidRow(column(12, plotOutput("plot_TukeyHSD")),
                              bsPopover(id="plot_TukeyHSD", 
                                        title="Tukey’s honest significant differences:",
                                        content= paste0("Plot indicates paired significances. Those intervals that cross zero indicate  "
                                                        ,"that the differences between the means of one level and another are not significant."))  
                     ),
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
                      fluidRow(column(12, verbatimTextOutput("fmla_model")),
                               bsPopover(id="fmla_model", 
                                         title="About model:",
                                         content= paste0("<b>Support Vector Machines (SVM):</b> supervised learninng models "
                                                         ," used for classification and regression analysis. Given a set of training examples, " 
                                            ," for belonging to one of two categories, an SVM training algorithm builds a model that "
                                            ," assigns new examples into one category or the other, making it a non-probabilistic binary "
                                            ," linear classifier. An SVM model is a representation of the examples as points in space, "
                                            ," mapped so that the examples of the separate categories are divided by a clear gap that is as "
                                            ," wide as possible. New examples are then mapped into that same space and predicted to belong to " 
                                            ," a category based on which side of the gap they fall on. "
                                            ," SVMs can efficiently perform a non-linear classification using kernels, implicitly mapping their "
                                            ," inputs into high-dimensional feature spaces via a non-linear transformation. "
                                            ," <b>Generalized Linear Models (GLMnet):</b> is a flexible generalization of ordinary linear regression "
                                            ," that allows for response variables that have error distribution models other than a normal distribution. " 
                                            ," The GLM generalizes linear regression by allowing the linear model to be related to the response variable "
                                            ," via a link function and by allowing the magnitude of the variance of each measurement to be a function of "
                                            ," its predicted value. "),
                                         placement = "bottom", trigger = "click", options = list(container = "body") )
                               ),
                      hr(),
                      h4('Summary:'),
                      fluidRow(column(12, verbatimTextOutput("summary_model")),
                               bsPopover(id="summary_model", 
                                         title="About model:",
                                         content= paste0("For <b>SVMs model</b>, <b>sigma</b> and <b>C</b> are the parameters of the model. "
                                                         ," <b>Accuracy</b> and <b>Kappa</b> are two measures that evaluate the performance of the model."
                                                         ," High accuracy means that the model is more reliable."),
                                         placement = "bottom", trigger = "click", options = list(container = "body") )
                               ),
                      hr(),
                      h4('Evaluate the model on the whole Training Set:'),
                      fluidRow(column(12, verbatimTextOutput("validate_model")),
                               bsPopover(id="validate_model", 
                                         title="About model:",
                                         content= paste0( ),
                               
                                         placement = "bottom", trigger = "click", options = list(container = "body") )
                               ),
                      
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
                                                   

                                       
                                                   