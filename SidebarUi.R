# load helpers.R file
source("helpers.R")

# Call function to create the dataset for analysis
df <- create.dataset(Dataset)
#View(df)

sidebarUni <- sidebarPanel(
  #fixed responsive img #added class img
  img(src="Aquamanager-logo.png", class = "img-responsive"),
  
  bsCollapse(id = "collapseSidebar" ,  #open = "Dimensions", 
             bsCollapsePanel("Dimensions", style = "primary" ,
  #h2("Dimensions"),
  fluidRow(column(6,
                  selectInput(inputId='groupUnit', label='Unit', choices=c("All", unique(as.character(df$Unit))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupBatch', label='Batch', choices=c("All", unique(as.character(df$Batch))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupFood', label='Actual.Feed', choices=c("All", unique(as.character(df$Actual.Feed))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupFood.Category', label='Feed.Category', choices=c("All", unique(as.character(df$Feed.Category))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupHatchery', label='Hatchery', choices=c("All", unique(as.character(df$Hatchery))), selected="All", multiple=TRUE)),
           column(6,
                  selectInput(inputId='groupOriginMonth', label='Origin.Month', choices=c("All", unique(as.character(df$Origin.Month))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupOriginYear', label='Origin.Year', choices=c("All", unique(as.character(df$Origin.Year))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupSupplier', label='Supplier', choices=c("All", unique(as.character(df$Supplier))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupStart.Av.Weight.Category', label='Start.Av.Weight.Category', choices=c("All", unique(as.character(df$Start.Av.Weight.Category))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupEnd.Av.Weight.Category', label='End.Av.Weight.Category', choices=c("All", unique(as.character(df$End.Av.Weight.Category))), selected="All", multiple=TRUE))))),
  
  dateRangeInput('dateRangeFrom',
                 label = paste(' From: '),
                 start = min( ymd(df$From)-days(0) ), 
                 end = max( ymd(df$From)+days(1) ),
                 min = min( ymd(df$From)-days(0) ),
                 max = max( ymd(df$From)+days(1)),
                 separator = " to ", format = "dd/mm/yyyy",
                 startview = 'year', language = 'el', weekstart = 0
  ),
  dateRangeInput('dateRangeTo',
                 label = paste(' To: '),
                 start = min( ymd(df$To)-days(1) ), 
                 end = max( ymd(df$To)+days(1) ),
                 min = min( ymd(df$To)-days(1) ),
                 max = max( ymd(df$To)+days(1) ),
                 separator = " to ", format = "dd/mm/yyyy",
                 startview = 'year', language = 'el', weekstart = 0
  ),
  
  hr(),
  bsCollapsePanel('Measures', style = "primary" ,
  #h2('Measures'),
  fluidRow(column(6,
                  sliderInput("rangeStAvWeight", "Start.Av.Weight:", min = min(as.double(df$Start.Av.Weight)), 
                              max = max(as.double(df$Start.Av.Weight)), 
                              value = c(min(as.double(df$Start.Av.Weight)), max(as.double(df$Start.Av.Weight))),
                              step=1.0, round=TRUE, sep="."),
                  sliderInput("rangeAvWeightDev", "Av.Weight.Deviation:", 
                              min = min(as.double(df$Av.Wt.Deviation.Perc)), 
                              max = max(as.double(df$Av.Wt.Deviation.Perc)), 
                              value = c(min(as.double(df$Av.Wt.Deviation.Perc)), 
                                        max(as.double(df$Av.Wt.Deviation.Perc))),
                              step=1.0, round=-2, sep=".", post="%"),
                  sliderInput("rangePeriod.FCR", "Econ.FCR.Period:", min = min(as.double(df$Econ.FCR.Period)), 
                              max = max(as.double(df$Econ.FCR.Period)), 
                              value = c(min(as.double(df$Econ.FCR.Period)), max(as.double(df$Econ.FCR.Period))), 
                              step=0.1, round=-2, sep="."),
                  sliderInput("rangeLTD.Econ.FCR", "LTD.Econ.FCR:", min = min(as.double(df$LTD.Econ.FCR)), 
                              max = max(as.double(df$LTD.Econ.FCR)), 
                              value = c(min(as.double(df$LTD.Econ.FCR)), max(as.double(df$LTD.Econ.FCR))), 
                              step=0.5, round=-2, sep="."),
                  sliderInput("rangePeriod.Feed.Qty", "Period.Feed.Qty:", 
                              min = min(as.double(df$Period.Feed.Qty), na.rm=TRUE), 
                              max = max(as.double(df$Period.Feed.Qty), na.rm=TRUE), 
                              value = c(min(as.double(df$Period.Feed.Qty)), 
                                        max(as.double(df$Period.Feed.Qty))), 
                              step=10, round=TRUE, sep=".")
  ),
  column(6,
         sliderInput("rangeAvWeight", "End.Av.Weight:", min = min(as.double(df$End.Av.Weight)), 
                     max = max(as.double(df$End.Av.Weight)), 
                     value = c(min(as.double(df$End.Av.Weight)), max(as.double(df$End.Av.Weight))),
                     step=1.0, round=TRUE, sep="."),
         sliderInput("rangePeriod.SFR", "Period.SFR:", min = min(as.double(df$SFR.Period.Perc)), 
                     max = max(as.double(df$SFR.Period.Perc)), 
                     value = c(min(as.double(df$SFR.Period.Perc)), max(as.double(df$SFR.Period.Perc))), step=0.1, 
                     round=-2, sep="."),
         sliderInput("rangePeriod.SGR", "Period.SGR:", min = min(as.double(df$SGR.Period.Perc)), 
                     max = max(as.double(df$SGR.Period.Perc)), 
                     value = c(min(as.double(df$SGR.Period.Perc)), max(as.double(df$SGR.Period.Perc))), step=0.1, 
                     round=-2, sep="."),
         sliderInput("rangeLTD.Mortality", "LTD.Mortality:", 
                     min = min(as.double(df$LTD.Mortality.Perc)),
                     max = max(as.double(df$LTD.Mortality.Perc)), 
                     value = c(min(as.double(df$LTD.Mortality.Perc)), 
                               max(as.double(df$LTD.Mortality.Perc))), 
                     step=1, round=-2, sep="."),
         sliderInput("rangePeriod.Day.Degrees", "Period.Day.Degrees:", 
                     min = min(as.double(df$Period.Day.Degrees), na.rm=TRUE), 
                     max = max(as.double(df$Period.Day.Degrees), na.rm=TRUE), 
                     value = c(min(as.double(df$Period.Day.Degrees)), 
                               max(as.double(df$Period.Day.Degrees))), 
                     step=10, round=TRUE, sep="."),
         sliderInput("rangeAvgTemp", "Avg.Temperature:", min = min(as.double(df$Avg.Temp)), 
                     max = max(as.double(df$Avg.Temp)), 
                     value = c(min(as.double(df$Avg.Temp)), max(as.double(df$Avg.Temp))), 
                     step=0.5, round=-2, sep=".")
  ) # end column
  ) # end fluid row
  ), # end of colapsePanel
  
  hr(),
  bsCollapsePanel("Separate By:", style = "primary" ,
  radioButtons("radioDimUni", label = h3("Separate The Dataset By:"), 
               choices = list("None", "Batch", "Hatchery",
                              "Origin.Month", "Origin.Year", "Start.Av.Weight.Category", 
                              "End.Av.Weight.Category", "Actual.Feed"), selected = "None")),# end of ColapsePanel
  
  hr(),
  actionButton(inputId = 'goUniPlot',  label = 'Refresh Univariate plots')
  
) # end sidebarUni function