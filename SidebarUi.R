# load helpers.R file
source("helpers.R")

# Call function to create the dataset for analysis
df <- create_dataset(Dataset)

sidebarUni <- sidebarPanel(
  #fixed responsive img #added class img
  img(src="Aquamanager-logo.png", class = "img-responsive"),
  
  bsCollapse(id = "collapseSidebar" ,  #open = "Dimensions", 
             bsCollapsePanel("Dimensions", style = "primary" ,
  #h2("Dimensions"),
  fluidRow(column(6,
                  selectInput(inputId='groupOrientation', label='Orientation', choices=c("All", unique(as.character(df$Orientation))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupSystem', label='System', choices=c("All", unique(as.character(df$System))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupCage', label='Cage', choices=c("All", unique(as.character(df$Cage))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupUnit', label='Unit', choices=c("All", unique(as.character(df$Unit))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupBatch', label='Batch', choices=c("All", unique(as.character(df$Batch))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupSection', label='Section', choices=c("All", unique(as.character(df$Section))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupHatchery', label='Hatchery', choices=c("All", unique(as.character(df$Hatchery))), selected="All", multiple=TRUE)),
           column(6,
                  selectInput(inputId='groupOriginMonth', label='Origin.Month', choices=c("All", unique(as.character(df$Origin.Month))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupOriginYear', label='Origin.Year', choices=c("All", unique(as.character(df$Origin.Year))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupFood', label='Actual.Feed', choices=c("All", unique(as.character(df$Actual.Feed))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupFood.Category', label='Feed.Category', choices=c("All", unique(as.character(df$Feed.Category))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupSupplier', label='Supplier', choices=c("All", unique(as.character(df$Supplier))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupStartAvWeightBioCat', label='Start.Av.Weight.BioCat', choices=c("All", unique(as.character(df$Start.Av.Weight.BioCat))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupEndAvWeightBioCat', label='End.Av.Weight.BioCat', choices=c("All", unique(as.character(df$End.Av.Weight.BioCat))), selected="All", multiple=TRUE))))),
  
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
                     step=10, round=TRUE, sep=".")
  ) # end column
  ) # end fluid row
  ), # end of colapsePanel
  
  hr()
  ,bsCollapsePanel('Environmental Measures', style = "primary" 
  ,h2('Environmental Measures')
  ,bsTooltip(c("rangeCAUDAL.O3","rangePh","rangeNO2") , "Remove -1 in order to remove empty values",
            "right", options = list(container = "body"))
  ,bsTooltip(c("rangePh","rangeNO2","rangeWATERRENEWAL","rangeNH3","rangeAvgTemp","rangeCAUDALO3") , "Remove -1 in order to remove empty values",
             "right", options = list(container = "body"))
  ,fluidRow(column(6,
                  sliderInput("rangePh", "Ph:", min = min(as.double(df$Ph)), 
                              max = max(as.double(df$Ph)), 
                              value = c(min(as.double(df$Ph)), max(as.double(df$Ph))), 
                              step=0.1, round=-2, sep="."),
                  sliderInput("rangeCAUDALO3", "CAUDAL O3 (Nm3/H):", min = min(as.double(df$CAUDAL.O3)), 
                              max = max(as.double(df$CAUDAL.O3)), 
                              value = c(min(as.double(df$CAUDAL.O3)), max(as.double(df$CAUDAL.O3))), 
                              step=10, round=0, sep="."),
                  sliderInput("rangeWATERRENEWAL", "WATER RENEWAL:", min = min(as.double(df$WATER.RENEWAL)), 
                              max = max(as.double(df$WATER.RENEWAL)), 
                              value = c(min(as.double(df$WATER.RENEWAL)), max(as.double(df$WATER.RENEWAL))), 
                              step=1, round=0, sep=".")
  ),
  column(6, 
         sliderInput("rangeNO2", "NO2:", min = min(as.double(df$NO2)), 
                     max = max(as.double(df$NO2)), 
                     value = c(min(as.double(df$NO2)), max(as.double(df$NO2))), 
                     step=0.01, round=-2, sep="."),
         sliderInput("rangeNH3", "NH3:", min = min(as.double(df$NH3)), 
                     max = max(as.double(df$NH3)), 
                     value = c(min(as.double(df$NH3)), max(as.double(df$NH3))), 
                     step=0.5, round=-2, sep="."),
         sliderInput("rangeAvgTemp", "Avg.Temperature:", min = min(as.double(df$Avg.Temperature)), 
                     max = max(as.double(df$Avg.Temperature)), 
                     value = c(min(as.double(df$Avg.Temperature)), max(as.double(df$Avg.Temperature))), 
                     step=0.5, round=-2, sep=".")
  ) # end column
  ) # end fluid row
  ), # end of ColapsePanel
  
  hr(),
  bsCollapsePanel("Separate By:", style = "primary" ,
  radioButtons("radioDimUni", label = h3("Separate The Dataset By:"), 
               choices = list("None", "Orientation", "System", "Section", "Batch", "Hatchery",
                              "Origin.Month", "Origin.Year", "Start.Av.Weight.BioCat", 
                              "End.Av.Weight.BioCat", "Actual.Feed"), selected = "None")),# end of ColapsePanel
  
  hr(),
  actionButton(inputId = 'goUniPlot',  label = 'Refresh Univariate plots')
  
) # end sidebarUni function