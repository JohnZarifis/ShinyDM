### Version Sea8 
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library("shiny")
library("shinythemes")
library("graphics")
library("ggplot2")
library("lattice")
library("plotrix")
library("psych")
library("RColorBrewer")
library("lubridate")
library("plyr")
library("dplyr")
library("GGally")
library("e1071")
library("caret")
library("pROC")
library("glmnet")
library("rpart")
library("party")
library("partykit")
library("Hmisc")
library("effects")
library("car")
library("relaimpo")
library("ROCR")
library("fpc")
library("randomForest")
library("maptree")
library("nlme")
library("mgcv")
library("htmltools")
library(rpivotTable)
library(readxl)


# load helpers.R file
source("helpers.R")

# load dataset 
#Datamining281114 <- read.delim("Datamining281114.csv", header = TRUE, sep = ";", dec=".")

#Dataset <- read.delim("DMFeb.csv", header = TRUE, sep = ";", dec=".")
#Dataset <- readWorksheetFromFile("TSIPOYRA-2014 BATCHES-ANON.xlsx",sheet =1)
#Dataset <- read.delim("bream2014.csv", header = TRUE, sep = ";", dec=",")
#Dataset <- read_excel("bream2014.xlsx",sheet = 1 ,col_names = TRUE)

pathname = paste(getwd(), "bream2014.xlsx", sep="/")
Dataset <- read_excel(pathname, sheet = 1 ,col_names = TRUE, na='na')

# Call function to create the dataset for analysis
data <- create_dataset(Dataset)
#

#----------------------------------------------------------------------------------
# the histogram plot function
#  
histPlot <- function( ds, x, nbins, group_var ){  
  range_var = diff(range(as.numeric(ds[,x])))/nbins 

  if (group_var!="None")
  {
    cdf <- ddply( ds, group_var, function(df) mean(df[,x]) )
    colnames(cdf)<-c("gv", "x.means")
  
    h <- ggplot(ds, aes_string(x=x, fill=group_var)) +
              geom_histogram( aes( y=..density.. ), binwidth=range_var, alpha=.5, position="identity" ) +  
              scale_x_continuous(limits=c(min(as.numeric(ds[,x])),max(as.numeric(ds[,x])))) +
              geom_vline(data=cdf, aes_string(xintercept="x.means", colour="gv"), linetype="dashed", size=1)
              
  }else{
    h <- ggplot(ds, aes_string(x=x)) + geom_histogram( aes( y=..density..,fill=..count.. ), binwidth=range_var ) +
              scale_x_continuous(limits=c(min(as.numeric(ds[,x])),max(as.numeric(ds[,x])))) +
              geom_vline(aes_string(xintercept=mean(ds[,x], na.rm=T)), color="red", linetype="dashed", size=1) 
  }
    
}
#----------------------------------------------------------------------------------
# the density plot function
# 
densityPlot <- function( ds, x, group_var ){  
 
  if (group_var!="None")
  {
    cdf <- ddply( ds, group_var, function(df)mean(df[,x]) )
    colnames(cdf)<-c("gv", "x.means")
    
    d <- ggplot(ds, aes_string(x=x, color=group_var)) + geom_density(size=1,alpha=0.8) + 
              geom_vline(data=cdf, aes_string(xintercept="x.means", colour="gv"), linetype="dashed", size=0.5)
  }else{
    
    d <- ggplot(ds, aes_string(x=x)) + geom_density(size=1,color="blue") +
             geom_vline( aes_string(xintercept=mean(ds[,x], na.rm=T)), linetype="dashed", size=0.5 )
  }
}

#----------------------------------------------------------------------------------
# the Boxplot function
# 
boxPlots <- function( ds, x, group_var ){  
  if (group_var!="None")
  {
    d <- ggplot(ds, aes_string(x=group_var, y=x, fill=group_var)) + 
                geom_boxplot(notch=TRUE, width=0.5, outlier.size=1.5) +
                stat_summary(fun.y=mean, geom="point", shape=5, size=4)
  }else{
    d <- ggplot(ds, aes_string(x=1, y=x)) + 
                geom_boxplot(notch=TRUE, width=0.5, outlier.size=1.5) +
                stat_summary(fun.y=mean, geom="point", shape=5, size=4)
  }
  
  r <- max(ds[,x]) - min(ds[,x])
  d <- d # + scale_y_continuous(breaks=seq(min(ds[,x]), max(ds[,x]), round(r/5)))  # todo not working in some cases
 
}

#----------------------------------------------------------------------------------
# the scatter plot function
#
scatterPlot <- function(ds, x, y, colour, size, regr.method)
{
  if (colour!="None")
  {
      p <- ggplot(ds, aes_string(x=x, y=y)) 
      p <- p + aes_string(color=colour, size=size) + geom_point()
      p <- p + geom_smooth(method = regr.method, size = 1)  
      p <- p + scale_color_brewer(type="qual",palette='Set1') + scale_fill_brewer()
  }else{
      p <- ggplot(ds, aes_string(x=x, y=y)) 
      p <- p + geom_point()
      p <- p + geom_smooth(method = regr.method, size = 1)  
      p <- p + scale_color_brewer(type="qual",palette='Set1') + scale_fill_brewer() 
  }
}

#----------------------------------------------------------------------------------
# the scatter matrix plot function
# 
scatterMatrixPlot <- function(ds, dim_vars, group_by_var)
{
  if ( group_by_var != "None"){
    ds <- ds[,c(dim_vars,group_by_var)]
    p <- ggpairs(ds, columns=1:length(dim_vars), 
                 upper = list(continuous='cor'),
                 lower = list(continuous = "smooth"), color = group_by_var,params=c(size=3),
                 axisLabels='internal', title = "Matrix Scatter Plot")
  }else{
    ds <- ds[,dim_vars]
    p <- ggpairs(ds, columns=1:length(dim_vars), 
                 upper = list(continuous='cor'),
                 lower = list(continuous = "smooth"),params=c(size=3),
                 axisLabels='internal', title = "Matrix Scatter Plot")
  }
    
}  
##----------------------------------------------------------------------------------
##                 Summary Mutlivariate Statistics function
##
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
sum_stats <- function(data, measurevar, groupvars, na.rm=FALSE, conf.interval=.95, .drop=TRUE)
{
 
if ( groupvars != "None" ){
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, min, max, mean, median, sd, CV, kurtosis, skewness, Q1, Q3, IR, se and ci
  data_stats <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N      = length2(xx[[col]], na.rm=na.rm),
                     min    = min(xx[[col]], na.rm=na.rm),
                     max    = max(xx[[col]], na.rm=na.rm),
                     mean   = mean(xx[[col]], na.rm=na.rm),
                     median = median(xx[[col]], na.rm=na.rm),
                     sd     = sd(xx[[col]], na.rm=na.rm),
                     CV     = ( sd(xx[[col]], na.rm=na.rm)/mean(xx[[col]], na.rm=na.rm) )*100,
                     kurtosis = kurtosis(xx[[col]], na.rm=na.rm),
                     skewness = skewness(xx[[col]], na.rm=na.rm),
                     Q1       = quantile(xx[[col]], 1/4, na.rm=na.rm, names=FALSE),
                     Q3       = quantile(xx[[col]], 3/4, na.rm=na.rm, names=FALSE),
                     IR       = IQR(xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  data_stats$se <- data_stats$sd / sqrt(data_stats$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, data_stats$N-1)
  data_stats$ci <- data_stats$se * ciMult
  
}else{
  
  # This does the summary for all records of the data set at the variable measurevar 
  # N, min, max, mean, median, sd, CV, kurtosis, skewness, Q1, Q3, IR, se and ci
  
  ds <- data[,measurevar]
  data_stats <- data.frame(N     = length(ds),
                           min   = min(ds, na.rm=na.rm),
                           max    = max(ds, na.rm=na.rm),
                           mean   = mean(ds, na.rm=na.rm),
                           median = median(ds, na.rm=na.rm),
                           sd     = sd(ds, na.rm=na.rm),
                           CV     = ( sd(ds, na.rm=na.rm)/mean(ds, na.rm=na.rm) )*100,
                           kurtosis = kurtosis(ds, na.rm=na.rm),
                           skewness = skewness(ds, na.rm=na.rm),
                           Q1       = quantile(ds, 1/4, na.rm=na.rm, names=FALSE),
                           Q3       = quantile(ds, 3/4, na.rm=na.rm, names=FALSE),
                           IR       = IQR(ds, na.rm=na.rm)
                         )
                      
  
  data_stats$se <- data_stats$sd / sqrt(data_stats$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, data_stats$N-1)
  data_stats$ci <- data_stats$se * ciMult
  
  rownames(data_stats)<-"Total"
  
}

return(data_stats)

}
##----------------------------------------------------------------------------------

#---------------------------------------------------------------------------------- shinyServer....
#
shinyServer(function(input, output, session){
 
  #---------------------------------------------------------------------------------------------------
  #     Subset of Dataset 
  #---------------------------------------------------------------------------------------------------
  passData <- reactive({
       
    if (input$groupRegion != "All"){ 
      data <- subset(data, Region %in% c(input$groupOrientation)) 
    }
    if (input$groupSite != "All"){ 
      data <- subset(data, Site %in% c(input$groupSystem))
    }
    if (input$groupBatch != "All"){ 
      data <- subset(data, Batch %in% c(input$groupBatch))
    }
    if (input$groupUnit != "All"){ 
      data <- subset(data, Unit %in% c(input$groupSection))
    }
    if (input$groupHatchery != "All"){ 
      data <- subset(data, Hatchery %in% c(input$groupHatchery))
    }
    if (input$groupOriginMonth != "All"){ 
      data <- subset(data, Origin.Month %in% c(input$groupOriginMonth)) 
    }
    if (input$groupOriginYear != "All"){ 
      data <- subset(data, Origin.Year %in% c(input$groupOriginYear)) 
    }
    if (input$groupFood != "All"){ 
      data <- subset(data, Actual.Feed %in% c(input$groupFood))
    }
    if (input$groupFood.Category != "All"){ 
      data <- subset(data, Feed.Category %in% c(input$groupFood.Category))
    }
    if (input$groupSupplier != "All"){ 
      data <- subset(data, Supplier %in% c(input$groupSupplier))
    }
    if (input$groupCurrent.Grading != "All"){ 
      data <- subset(data, Current.Grading %in% c(input$groupCurrent.Grading))
    }
    
    
    data <- data[ data$End.Av.Weight >= as.numeric(input$rangeAvWeight[1]) & data$End.Av.Weight <= as.numeric(input$rangeAvWeight[2])
               & data$Start.Av.Weight >= as.numeric(input$rangeStAvWeight[1]) & data$Start.Av.Weight <= as.numeric(input$rangeStAvWeight[2])
               & data$Av.Weight.Deviation >= as.numeric(input$rangeAvWeightDev[1])
               & data$Av.Weight.Deviation <= as.numeric(input$rangeAvWeightDev[2]) 
               & data$Econ.FCR.Period >= as.numeric(input$rangePeriod.FCR[1]) & data$Econ.FCR.Period <= as.numeric(input$rangePeriod.FCR[2]) 
               & data$LTD.Econ.FCR >= as.numeric(input$rangeLTD.Econ.FCR[1]) & data$LTD.Econ.FCR <= as.numeric(input$rangeLTD.Econ.FCR[2])  
               & data$SGR.Period >= as.numeric(input$rangePeriod.SGR[1]) & data$SGR.Period <= as.numeric(input$rangePeriod.SGR[2]) 
               & data$SFR.Period >= as.numeric(input$rangePeriod.SFR[1]) & data$SFR.Period <= as.numeric(input$rangePeriod.SFR[2]) 
               & data$LTD.Mortality >= as.numeric(input$rangeLTD.Mortality[1]) & data$LTD.Mortality <= as.numeric(input$rangeLTD.Mortality[2])
               & data$Period.Day.Degrees >= as.numeric(input$rangePeriod.Day.Degrees[1]) 
               & data$Period.Day.Degrees <= as.numeric(input$rangePeriod.Day.Degrees[2])
               & data$Avg.Temperature >= as.numeric(input$rangeAvg.Temp[1]) 
               & data$Avg.Temperature <= as.numeric(input$rangeAvg.Temp[2])
               #& (data$From >= input$dateRangeFrom[1] & data$From <= input$dateRangeFrom[2]) 
               #& (data$To >= input$dateRangeTo[1] & data$To <= input$dateRangeTo[2])
               & (data$From >= ymd(input$dateRangeFrom[1]) & data$From <= ymd(input$dateRangeFrom[2])) 
               & (data$To >= ymd(input$dateRangeTo[1]) & data$To <= ymd(input$dateRangeTo[2]))
               & data$Period.Feed.Qty >= as.numeric(input$rangePeriod.Feed.Qty[1]) 
               & data$Period.Feed.Qty <= as.numeric(input$rangePeriod.Feed.Qty[2])
               & data$FastingsPerc >= as.numeric(input$rangeFastingsPerc[1]) 
               & data$FastingsPerc <= as.numeric(input$rangeFastingsPerc[2])
               & data$Fastings.No >= as.numeric(input$rangeFastings.No[1]) 
               & data$Fastings.No <= as.numeric(input$rangeFastings.No[2])
               & data$LTD.Day.Degrees >= as.numeric(input$rangeLTD.Day.Degrees[1]) 
               & data$LTD.Day.Degrees <= as.numeric(input$rangeLTD.Day.Degrees[2])
               & data$Period.Mortality >= as.numeric(input$rangePeriod.Mortality[1]) 
               & data$Period.Mortality <= as.numeric(input$rangePeriod.Mortality[2])
               & data$Age >= as.numeric(input$rangeAge[1]) 
               & data$Age <= as.numeric(input$rangeAge[2])
              , ]
    
    class(data$ProductionTimeDays)
    
#   For debugging  
# View(data)
#   str(data)
#   print(nrow(data))
    return(data)
  })  
  
  
#---------------------------------------------------------------------------------------------------
#     Histograms
#---------------------------------------------------------------------------------------------------
#
#...................................................... H1
output$histPlotAvWeight <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="End.Av.Weight", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H2
output$histPlotAvWeightDeviation <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Av.Weight.Deviation", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})

#...................................................... H3
output$histPlotPeriod.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Econ.FCR.Period", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H4
output$histPlotEcon.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="LTD.Econ.FCR", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H5
output$histPlotPeriod.SFR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="SFR.Period", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H6
output$histPlotPeriod.SGR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="SGR.Period", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H7
output$histPlotMortality <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="LTD.Mortality", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H8
output$histPlotPeriod.Day.Degrees <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Period.Day.Degrees", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H9
output$histPlotAvg.Temperature <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Avg.Temperature", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})

#---------------------------------------------------------------------------------------------------
#     Density Plots
#---------------------------------------------------------------------------------------------------
#
#...................................................... D1
output$densPlotAvWeight <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- densityPlot( graphData, x="End.Av.Weight", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D1
output$densPlotAvWeightDeviation <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- densityPlot( graphData, x="Av.Weight.Deviation", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D3
output$densPlotPeriod.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData() 
      theGraph <- densityPlot(graphData, x="Econ.FCR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D4
output$densPlotEcon.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData() 
      theGraph <- densityPlot(graphData, x="LTD.Econ.FCR", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D5
output$densPlotPeriod.SFR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="SFR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D6
output$densPlotPeriod.SGR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="SGR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D7
output$densPlotMortality <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="LTD.Mortality", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D8
output$densPlotPeriod.Day.Degrees <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="Period.Day.Degrees", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D9
output$densPlotAvg.Temperature <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="Avg.Temperature", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})

#---------------------------------------------------------------------------------------------------
#     BoxPlots
#---------------------------------------------------------------------------------------------------
#
#...................................................... B1
output$boxPlotAvWeight <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="End.Av.Weight", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B2
output$boxPlotAvWeightDeviation <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Av.Weight.Deviation", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B3
output$boxPlotPeriod.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Econ.FCR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B4
output$boxPlotEcon.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="LTD.Econ.FCR", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B5
output$boxPlotPeriod.SFR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="SFR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B6
output$boxPlotPeriod.SGR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="SGR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B7
output$boxPlotMortality <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="LTD.Mortality", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B8
output$boxPlotPeriod.Day.Degrees <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Period.Day.Degrees", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B9
output$boxPlotAvg.Temperature <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Avg.Temperature", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})

#---------------------------------------------------------------------------------------------------
#     Summary Univariate Statistics
#---------------------------------------------------------------------------------------------------
output$summary_stats_EndAvWeight <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="End.Av.Weight", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
})  
output$summary_stats_AvWeightDeviation <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Av.Weight.Deviation", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
})  
output$summary_stats_PeriodFCR <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Econ.FCR.Period", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_EconFCR <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="LTD.Econ.FCR", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_PeriodSFR <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="SFR.Period", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_PeriodSGR <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="SGR.Period", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_Mortality <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="LTD.Mortality", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
})  
output$summary_stats_Period.Day.Degrees <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Period.Day.Degrees", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_Avg.Temp <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Avg.Temperature", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 



#---------------------------------------------------------------------------------------------------
#     Dislpay dataset (Data)
#---------------------------------------------------------------------------------------------------
# Dislpay dataset
output$dataset <- renderDataTable({
  data <- passData() 
})

#---------------------------------------------------------------------------------------------------
#     Dislpay Pivot (Data)
#---------------------------------------------------------------------------------------------------
# Dislpay pivot


# output$foo <- rpivotTable::renderRpivotTable({
#   rpivotTable::rpivotTable(data <- passData())
# })



#---------------------------------------------------------------------------------------------------
#     Scatter Matrix Plots & Scatter Plots
#---------------------------------------------------------------------------------------------------
output$scatterMatrixPlot <- renderPlot({
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      
      dim_vars = c("End.Av.Weight", "Econ.FCR.Period", "SFR.Period", "SGR.Period", "LTD.Econ.FCR", 
                   "LTD.Mortality", "Avg.Temperature", "Period.Day.Degrees")
      group_by_var = input$radioDimMulti
      theGraph <- scatterMatrixPlot(graphData, dim_vars, group_by_var)
      print(theGraph)
   })
  }
})
 
#...................................................... S1
output$scatterPlot.EndAvWeight.PeriodFCR <- renderPlot({ 
#Re-run when button is clicked
if (input$goMultiPlot == 0){ 
  return() }
else{ 
  isolate({    
    graphData <- passData()
    p <- scatterPlot(graphData, x="End.Av.Weight", y="Econ.FCR.Period", colour=input$radioDimMulti,
                     size = "Closing.Biomass", regr.method="loess") 
   print(p)
  })
 }
})
output$cor.stats.EndAvWeight.PeriodFCR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
          data <- passData()
          if ( input$radioDimMulti != "None"){
            d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=End.Av.Weight, y=Econ.FCR.Period))
          }else{
            d <- data.frame("Pearson Correlation" = cor(x=data$End.Av.Weight, y=data$Econ.FCR.Period))
          }
          return( d ) 
    })  
  }
})  
#.......................................................... S2
output$scatterPlot.EndAvWeight.PeriodSFR <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="End.Av.Weight", y="SFR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EndAvWeight.PeriodSFR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=End.Av.Weight, y=SFR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$End.Av.Weight, y=data$SFR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S3
output$scatterPlot.EndAvWeight.PeriodSGR <- renderPlot({ 
#Re-run when button is clicked
if (input$goMultiPlot == 0){ 
    return() }
else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="End.Av.Weight", y="SGR.Period", colour=input$radioDimMulti,
                  size = "Closing.Biomass", regr.method="loess") 
      print(p)
      })
    }
})
output$cor.stats.EndAvWeight.PeriodSGR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=End.Av.Weight, y=SGR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$End.Av.Weight, y=data$SGR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S4
output$scatterPlot.EndAvWeight.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="End.Av.Weight", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EndAvWeight.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=End.Av.Weight))
      }else{
      d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$End.Av.Weight))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S5
output$scatterPlot.PeriodEcon.FCR.PeriodSFR <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Econ.FCR.Period", y="SFR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodEcon.FCR.PeriodSFR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Econ.FCR.Period, y=SFR.Period))
      }else{
          d <- data.frame("Pearson Correlation" = cor(x=data$Econ.FCR.Period, y=data$SFR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S6
output$scatterPlot.PeriodEcon.FCR.PeriodSGR <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Econ.FCR.Period", y="SGR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodEcon.FCR.PeriodSGR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Econ.FCR.Period, y=SGR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Econ.FCR.Period, y=data$SGR.Period))
      }
      return( d )
    })  
  }
})
#.......................................................... S7
output$scatterPlot.PeriodFCR.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="Econ.FCR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodFCR.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=Econ.FCR.Period))
      }else{
          d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$Econ.FCR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S8
output$scatterPlot.PeriodSFR.PeriodSGR <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="SFR.Period", y="SGR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodSFR.PeriodSGR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=SFR.Period, y=SGR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$SFR.Period, y=data$SGR.Period))
      }
      return( d )      
    })  
  }
})
#.......................................................... S9
output$scatterPlot.PeriodSFR.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="SFR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodSFR.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=SFR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$SFR.Period))
      }
      return( d )      
    })  
  }
})
#.......................................................... S10
output$scatterPlot.PeriodSGR.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="SGR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodSGR.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=SGR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$SGR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S11
output$scatterPlot.EconFCR.EndAvWeight <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="End.Av.Weight", y="LTD.Econ.FCR", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.EndAvWeight <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=End.Av.Weight, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$End.Av.Weight, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S12
output$scatterPlot.EconFCR.FCRPeriod <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Econ.FCR.Period", y="LTD.Econ.FCR", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.FCRPeriod <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Econ.FCR.Period, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Econ.FCR.Period, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S13
output$scatterPlot.EconFCR.SFRPeriod <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="SFR.Period", y="LTD.Econ.FCR", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.SFRPeriod <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=SFR.Period, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$SFR.Period, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S14
output$scatterPlot.EconFCR.SGRPeriod <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="SGR.Period", y="LTD.Econ.FCR", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.SGRPeriod <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=SGR.Period, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$SGR.Period, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S15
output$scatterPlot.EconFCR.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="LTD.Econ.FCR", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})

#---------------------------------------------------------------------------------------------------
#     Multidimensional Dashboard
#---------------------------------------------------------------------------------------------------

datasetMD <- reactive({
  data <- passData()
#  data <- data[sample(nrow(data), input$sampleSize),]
  data <- data[  (data$From >= ymd(input$MD.dateRangeFrom[1]) & data$From <= ymd(input$MD.dateRangeFrom[2])) 
               & (data$To >= ymd(input$MD.dateRangeTo[1]) & data$To <= ymd(input$MD.dateRangeTo[2])) , ]
  
})

output$plotDashboard <- renderPlot({
 
  dsMD <- datasetMD()
  p <- ggplot(dsMD, aes_string(x=input$x, y=input$y)) + geom_point(size=2.5) 
  
  if (input$color != 'None')
    p <- p + aes_string(color=input$color)
  
  facets <- paste(input$facet_row, '~', input$facet_col)
  if (facets != '. ~ .')
    p <- p + facet_grid(facets)

  if (input$xmeans)
      p <- p + geom_line( stat = "vline", xintercept="mean")
  
  if (input$ymeans)
      p <- p + geom_line( stat = "hline", yintercept="mean")
  
  if (input$total.xmeans)
  { 
    avgx = mean(as.numeric(dsMD[,input$x]))
    p <- p + geom_vline( xintercept=avgx, color="darkred", linetype="dashed", size=1.5)
  }
  if (input$total.ymeans)
  { 
    avgy = mean(as.numeric(dsMD[,input$y]))
    p <- p + geom_hline( yintercept=avgy, color="darkred", linetype="dashed", size=1.5)
  }
  if (input$smooth)
      p <- p + geom_smooth()

  if ( input$comp.ranges)
  {   
    smtr <- stat_summary(fun.data ="mean_cl_boot", geom="crossbar", conf.int=0.95, width=0.3, B=1000, na.rm=T, reps=F) 
    p <- p + smtr
  }    
  
  if ( input$benchmarker)  
  {
    p <- p + geom_abline(intercept=0, slope=1)
  }
  print(p)
  
})


#---------------------------------------------------------------------------------------------------
#     Multidimensional Interactive Dashboard
#---------------------------------------------------------------------------------------------------

#  datasetMD <- reactive({
#   data <- passData()
#   #  data <- data[sample(nrow(data), input$sampleSize),]
#   data <- data[  (data$From >= ymd(input$MD.dateRangeFrom[1]) & data$From <= ymd(input$MD.dateRangeFrom[2])) 
#                  & (data$To >= ymd(input$MD.dateRangeTo[1]) & data$To <= ymd(input$MD.dateRangeTo[2])) , ]
#  })
# 
# 
#  output$plot.Interactive.Dashboard <- renderMetricsgraphics({
#     
#     dsMD <- datasetMD()
#     
#     if (input$color != 'None'){
#       dsMD %>% 
#           mjs_plot(x= input$x, y=input$y) %>% 
#             mjs_point(point_size = 1.5, color_accessor=input$color) -> plot.dashboard
#     }else{
#       dsMD %>% 
#         mjs_plot(x= input$x, y=input$y) %>% 
#         mjs_point(point_size = 1.5) -> plot.dashboard
#     }
#     
#     print(plot.dashboard)
#     
#  })
# 
# #  print(plot.dashboard)
# #    mjs_point(point_size = 1.5, color_accessor=input$color, color_type="category", 
# #               size_accessor=Biomass, least_squares=TRUE) %>% mjs_labs(x="Average Weight", y="FCR")
# #   
#   
#   
# ##})



#---------------------------------------------------------------------------------------------------
#     Regression
#---------------------------------------------------------------------------------------------------
# Tab: Build
#

output$explanatoryVar <- renderUI({
  if (is.null(input$responseVar)) { return() }
  regressors <- list("End.Av.Weight", "Start.Av.Weight", "Days", "Period.Feed.Qty",
                     "Suggested.Feed.Qty", "Econ.FCR.Period", "SFR.Period", "SGR.Period",
                     "LTD.Mortality", "Avg.Temperature", "Age")
  regressors <- regressors[ regressors != input$responseVar ]   
  checkboxGroupInput(inputId='explanatory.Variables', label=h3('Explanatory Variable(s):'), 
                     choices=regressors, selected=regressors[1])
})


runRegression <- reactive({
        if (is.null(input$explanatory.Variables)) return()
        regres <- list(input$responseVar,input$explanatory.Variables)
        
        # "data": dataset that based on the user choices in the first page
        data <- passData()  
        ds <- data[ , names(data) %in% unlist(regres) ]
        
        if ( input$radioModel!=3 ){
            fmla = as.formula(paste(input$responseVar," ~ ",paste(input$explanatory.Variables, collapse="+")))
            if ( input$radioModel==1 ) # "Linear"
            {
              model <- lm(formula=fmla, data=ds )
              return(model)
            }else if (input$radioModel==2 )  # "Generalized Linear Models"
            {
              model <- glm(formula=fmla, family=gaussian(link='identity'), data=ds, control=glm.control(maxit=1000,trace=FALSE), model=TRUE)
              return(model)
            }
        }else if ( input$radioModel==3 ){ # "Generalized Additive Models"
            fmla = as.formula(paste(input$responseVar,"~",paste("s(", input$explanatory.Variables, ", bs= \"cr\" )", collapse="+") )) 
            model <- gam(formula=fmla, data=ds)
            return(model)
        } 
})

output$fmla <- renderText({
  if (input$goRegression == 0){
    return() }
  else{ 
   isolate({    
     if ( input$radioModel!=3 ){
        paste( as.character(input$responseVar), paste(input$explanatory.Variables, collapse=" + "), sep=" ~ ")  
     }else{
        paste(input$responseVar," ~ ",paste("s(", input$explanatory.Variables, ", bs=\"cr\" )", collapse="+"))
     }  
  })  # end isolate
  } # end if...else
})

output$regression_R <- renderPrint({
  
  if (input$goRegression == 0){
    return() }
  else{ 
     isolate({ 
  
        if( !is.null(input$responseVar)){
          
          mod <- runRegression()
          s<-summary(mod)
          
          if ( input$radioModel==1 ){ # "Linear"
            res.cor <- matrix( c( s$r.squared*100, s$adj.r.squared*100, AIC(mod) ), c(1,3) )
            colnames(res.cor) <- c( " R-Squared ",  " Adjusted R-Squared ", "AIC" )
            print(res.cor[1,], row.names=FALSE, digits=5)
          }else if (input$radioModel==2 ){  # "GLM"
            res.cor <- matrix( c( s$aic, s$deviance), c(1,2) )
            colnames(res.cor) <- c("AIC", "Residuals Deviance")
            print(res.cor[1,], row.names = FALSE, justify='centred', nsmall=3 )
          }else if (input$radioModel==3 ){  # "GAM"
            if (mod$converged == TRUE){
                  res.cor <- matrix( c( s$r.sq*100, mod$aic, mod$gcv.ubre ), c(1,3) )
                  colnames(res.cor) <- c(" Adjusted R-Squared ", "AIC", "Generalized Cross Validation - GCV")
                  res.cor.df <- as.data.frame(res.cor) 
                  print(res.cor.df[1,], row.names = FALSE, justify='centred', nsmall=3 )
            }else{
                  print(data.frame(Warning="The GAM method could not converged. Try again using another model."))
            }  
          }
        } 
        else
        {
          print(data.frame(Warning="Please select Model Parameters."))
        }
  
    }) # end isolate
  } # end if...else
  
})

output$regression_Table_residuals <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
        if( !is.null(input$responseVar)){
          resids<-summary(runRegression()$residuals)
          print(resids)
        } else {
          print(data.frame(Warning="Please select Model Parameters."))
        }
      }) # end isolate
    } # end if...else
})

output$plot_lm_13 <- renderPlot({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      mod <- runRegression()
      if ( input$radioModel==1 || input$radioModel==2 ){ # "Linear" or "GLM"  
          par(mfrow=c(1,2))
          plot(mod, which = c(1,3), id.n=10)
      }
      else if ( input$radioModel==3 ){ # "GAM"
        if (mod$converged == TRUE){
          if ( length(input$explanatory.Variables)%%2 == 0 )
          {
              par(mfrow=c(length(input$explanatory.Variables)/2, 2),oma=c(1,1,1,0), mar=c(2,1,1,2), tcl=-0.1, mgp=c(2,1,0) )
              plot.gam(mod, residuals=TRUE, pch=19, height="800px")
          }
          else{
              par(mfrow=c( length(input$explanatory.Variables)%/%2 + 1, 2),oma=c(1,1,1,0), mar=c(2,1,1,2),tcl=-0.1,mgp=c(2,1,0) )
              plot.gam(mod, residuals=TRUE, pch=19, height="800px")
          }
        }
      }
    }) # end isolate
  } # end if...else
})

output$plot_lm_24 <- renderPlot({
  if (input$goRegression == 0){
    return() }
  else{ 
     isolate({  
       fit <- runRegression()
       if ( input$radioModel==1 || input$radioModel==2 ){ # "Linear" or "GLM"  
          par(mfrow=c(1,2))
          plot(fit, which = 2, id.n=10)
          cutoff <- 4/(nrow(fit$model)-length(fit$coefficients)-2)
          plot(fit, which=4, cook.levels=cutoff)
          abline(h=cutoff, lty=2, col="blue")
       }else if ( input$radioModel==3 ){ # "GAM" 
         if (fit$converged == TRUE){
           par(mfrow=c(2,2))
           gam.check(fit)
         }
       }
    }) # end isolate
  } # end if...else
})

# output$regression_Table_coeff <- renderPrint({
#   if( !is.null(input$responseVar)){
#     coefs<-coef(runRegression())
#     print(coefs)
#   } else {
#     print(data.frame(Warning="Please select Model Parameters."))
#   }
# }) 

output$regression_Table_sign_coeff <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
        if( !is.null(input$responseVar)){
          mod <- runRegression()
          s<-summary(mod)
          print(s$coefficients)
        }else{
          print(data.frame(Warning="Please select Model Parameters."))
        }
    }) # end isolate
  } # end if...else
})


output$regression_Table_sign_param_gam <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      if( !is.null(input$responseVar)){
        mod <- runRegression()
        s<-summary(mod)
        if (mod$converged == TRUE){ 
          print(s$p.table)
        }
      }else{
          print(data.frame(Warning="Please select Model Parameters."))
      }
    }) # end isolate
  } # end if...else
})

output$regression_Table_sign_coeff_gam <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      if( !is.null(input$responseVar)){
        mod <- runRegression()
        s<-summary(mod)
        if (mod$converged == TRUE){ 
            print(s$s.table)
        }
      }else{
            print(data.frame(Warning="Please select Model Parameters."))
      }
    }) # end isolate
  } # end if...else
})

output$regression_CI <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      
      if( !is.null(input$responseVar)){
        ci <- confint(runRegression())
        print(ci)
      } else {
        print(data.frame(Warning="Please select Model Parameters."))
      }
  }) # end isolate
  } # end if...else
})

output$regression_Anova <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      if( !is.null(input$responseVar)){
        anv <- anova(runRegression())
        print(anv)
      } else {
        print(data.frame(Warning="Please select Model Parameters."))
      }
    }) # end isolate
  } # end if...else
})

output$regression_outliers <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({
      if( !is.null(input$responseVar)){
        outliers <- outlierTest(runRegression())
        print(outliers)
      } else {
        print(data.frame(Warning="Please select Model Parameters."))
      }
  }) # end isolate
  } # end if...else
})

output$plot_Infl <- renderPlot({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({
      influencePlot(runRegression(),  main="Influence Plot", scale=5, id.n=10, id.cex=.75, id.col='blue',
                    sub="Circle size is proportional to Cook’s distance")
  }) # end isolate
  } # end if...else
})

output$table_Infl <- renderDataTable({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({
      p<-influencePlot(runRegression(), id.n=10)
  return(p) 
  }) # end isolate
  } # end if...else
})

output$gam.mod.coeffs <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      if( !is.null(input$responseVar)){
        mod <- runRegression()
        if (mod$converged == TRUE){ 
          cof.df <- data.frame(mod$coefficients)
          names(cof.df)<- "Coefficients"
          print(cof.df)
        }
      }else{
        print(data.frame(Warning="Please select Model Parameters."))
      }
    }) # end isolate
  } # end if...else
})

run_Relative.Importance <- reactive({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({
      if (is.null(input$explanatory.Variables) || length(input$explanatory.Variables) <2) return()
      fit <- runRegression()
      metrics <- calc.relimp(fit, type = "car")
      return(metrics) 
    }) # end isolate
  } # end if...else
})


output$Rel.Impo <- renderPrint({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({
        if (is.null(input$explanatory.Variables) || length(input$explanatory.Variables) <2) 
          return(print(data.frame(Warning="Please select more Regressors Parameters.")))
        metrics <- run_Relative.Importance()
        m<-metrics$car*100
        print(round(m, digits = 2) )
  }) # end isolate
  } # end if...else
})  

output$bar.Rel.Impo <- renderPlot({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({
      if (is.null(input$explanatory.Variables) || length(input$explanatory.Variables) <2) 
        return(print(data.frame(Warning="Please select more Regressors Parameters.")))
      metrics <- run_Relative.Importance()
      m<-metrics$car*100
      barplot(m, xlab='% of Responce Variance', main=paste('Relative Importances for', input$responseVar, sep=' '), 
              horiz=TRUE, las=1,cex.names=0.8, col='blue')
    }) # end isolate
  } # end if...else
})

#---------------------------------------------------------------------------------------------------
# Tab: Predict with Regression Model
#
output$pred.actual.plot <- renderPlot({
  if (input$goRegression == 0){
    return() }
  else{ 
    isolate({ 
      fit <- runRegression()
      data <- passData()
      newds <- subset(data, select=input$explanatory.Variables)
      
      preds <- predict(fit,newdata = newds)
      
      df <- data.frame( subset(data,select=input$responseVar), preds ) 
      names(df) <- c( "Actual", "Prediction" )
      ggplot(df, aes_string(x="Actual", y="Prediction") ) + geom_point() + geom_abline(col="red")
    })
  }
})


output$dyn_input.Regression <- renderUI({
  
  data <- passData()
  list.predictors <- input$explanatory.Variables
  num.preds <- length(list.predictors)
  
  inputs <- lapply(1:num.preds, function(i) {
    input_name <- paste0("input", i, sep="")
    fluidRow(column(width=6, 
                    if ( is.factor( data[, list.predictors[[i]]] ) )
                    {
                      list.values <- unique( data[, list.predictors[[i]]] )
                      selectInput(inputId=input_name, label=h4( as.character(list.predictors[[i]]) ), 
                                  choices=as.character(list.values), multiple=FALSE)
                    }else{  
                      numericInput( input_name, label = h4( as.character(list.predictors[[i]]) ), value = NA)
                    } # end if...else
    ) # end column
    ) # end fluidRow
  } # end function
  ) # end lapply
  
  do.call(tagList, inputs)
}) 

# predict value regarding the predictors' values
output$prediction.value.Regression <- renderPrint({ 
  
  if (input$goRegressionPredict == 0){
    return() }
  else{ 
    isolate({
      
      # load the model SVM or GLMnet 
      fit <- runRegression()
      
      # create an instance from the input values 
      list.predictors <- input$explanatory.Variables
      num.preds <- length(list.predictors)
      
      newdata <- as.data.frame(matrix(0, nrow = 1, ncol=num.preds))
      newdata <- lapply(1:num.preds, function(i) {
        input_name <- paste0("input", i, sep="")
        input[[ input_name ]]
      } # end function
      )# end lapply
      names(newdata) <- list.predictors
      
      pred_val <- predict(fit, newdata)
      names(pred_val) <- as.character(input$responseVar)
      
      regression.response <- data.frame(pred_val, stringsAsFactors = FALSE)
      print( regression.response )
      
    }) # end isolate
  } # end if...else
  
})


#---------------------------------------------------------------------------------------------------
#     Analysis of Variance (ANOVA, Factorial ANOVA, MANOVA)
#---------------------------------------------------------------------------------------------------

 runANOVA <- reactive({
  if (is.null(input$Ind.Vars)){ return() }
  aov.list.vars <- list(input$Dep.Var, input$Ind.Vars)
  
  # "data": dataset that based on the user choices in the first page
  data <- passData()  
  ds <- data[ , names(data) %in% unlist(aov.list.vars) ]
 
  if ( input$anova.test==1 || input$anova.test==2 ){
       fmla = paste( paste(as.character(input$Dep.Var),collapse=" + "), paste(as.character(input$Ind.Vars), collapse=" + "), sep=" ~ " ) 
  }else{
       fmla = paste( paste(input$Dep.Var,collapse=" + "), paste(as.character(input$Ind.Vars), collapse=" * "), sep=" ~ " )
  }
  
  model.aov <- aov(formula=as.formula(fmla), data=ds)
 
  return(model.aov)

})
  
output$fmla.aov <- renderText({
  if (input$goANOVA == 0){
    return() }
  else{ 
    isolate({   
      
      if ( input$anova.test==1 || input$anova.test==2 ){
        fmla = paste( paste(as.character(input$Dep.Var),collapse=" + "), paste(as.character(input$Ind.Vars), collapse=" + "), sep=" ~ " )
      }else{
        fmla = paste( paste(as.character(input$Dep.Var),collapse=" + "), paste(as.character(input$Ind.Vars), collapse=" * "), sep=" ~ " )
      }
      
    })  # end isolate
  } # end if...else
})


output$summary.aov <- renderPrint({
  if (input$goANOVA == 0){
    return() }
  else{ 
    isolate({   
    if ( !is.null(input$Dep.Var) ){
         mod <- runANOVA()
         s <- summary(mod)
         print(s)
     }
     else{ 
         print(data.frame(Warning="Please select Model Parameters."))
     }
    }) # end isolate
  } # end if...else
})    

output$plot.aov <- renderPlot({
  if (input$goANOVA == 0){
    return() }
  else{ 
    isolate({ 
      mod <- runANOVA()
      par(mfrow=c(2,2))
      plot(mod)
    })
  }
})

output$sign.diffs.Tukey <- renderPrint({
  if (input$goANOVA == 0){
    return() }
  else{ 
    isolate({ 
      mod <- runANOVA()
      sign.diffs.Tukey <- TukeyHSD(mod)
      print(sign.diffs.Tukey)
    })
  }
})

output$plot.TukeyHSD <- renderPlot({

  if (input$goANOVA == 0){
    return() }
  else{ 
    isolate({ 
      mod <- runANOVA()
      par(las=1)
      plot(TukeyHSD(mod))
    })
  }  
})

#---------------------------------------------------------------------------------------------------
#     Machine Learning Models (Support Vector Machines, Generalised Linear Model)
#     ---------------------------------------------------------------------------
#     Study only classification problems then the targets variable are categorical
#----------------------------------------------------------------------------------------------------
output$targs.ML.Variables <- renderUI({ 
    var <- list("Class")
    radioButtons(inputId='Targ.ML.Var', label=h3('Target Variable:'), choices=var, selected=var[[1]])
})  # end renderUI targs.Variables

output$preds.ML.Variables <- renderUI({
  data <- passData() 
  Vars <- names(data)
  ind.vars <- Vars[ Vars != input$Targ.ML.Var] 
  selectInput(inputId='preds.ML.Vars', label=h3('Predictors:'), choices=ind.vars, multiple=TRUE)
})  # end renderUI preds.Variables

output$TestOpts <- renderUI({
  if (is.null(input$testingOptions))
    return()
  
  # Depending on input$testingOptions, we'll generate a different
  # UI component and send it to the client.
  switch(input$testingOptions,
         #"Cross Validation" 
         "1" = sliderInput("folds", "Folds:",min = 1, max = 20, value = 10),
         #"Random Split" 
         "2" = sliderInput("percentage", "Percentage for training:", min = 0, max = 100, value = 80)
  )
})

output$fmla.model <- renderText({
  if (input$goAnalysis == 0){
    return() }
  else{ 
    isolate({   
      fmla = paste( as.character(input$Targ.ML.Var), 
                    paste(as.character(input$preds.ML.Vars), collapse=" + "), sep=" ~ " )
    })  # end isolate
  } # end if...else
})

#------------------------------------- SVM function
runSVM <- reactive({
  
  list.vars <- list(input$Targ.ML.Var, input$preds.ML.Vars)
  Class <- input$Targ.ML.Var
  
  # "data": dataset that based on the user choices in the first page
  data <- passData()  
  dset.train <- data[ , names(data) %in% unlist(list.vars) ]
  
  fmla <- as.formula( paste(input$Targ.ML.Var, paste(input$preds.ML.Vars, collapse="+"), sep=" ~ ") )      
  
  # cross-validation
  if ( input$testingOptions == 1 ){
    
    dummy.ds <- dummyVars(fmla,data=dset.train, fullRank=F)
    dummy.dset.train <- data.frame(predict(dummy.ds, newdata = dset.train),"Class"= dset.train$Class) #input$Targ.ML.Var)
   
    # dummy.dset.train$Class <- ifelse(dummy.dset.train$Class=='GOOD',1,0)
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = input$folds,
      ## repeated ten times
      repeats = 10, 
      classProbs = TRUE,
      returnData = TRUE)
   
    svmFit <- train(Class~., data=dummy.dset.train, method = "svmRadial", trControl = fitControl, metric="ROC")
   
  }
  else{
    # random split
    perc <- input$percentage/100
    set.seed(998)
    Class <- input$Targ.ML.Var
    
    dummy.ds <- dummyVars(fmla,data=dset.train, fullRank=F)
    dummy.dset.train <- data.frame(predict(dummy.ds, newdata = dset.train),"Class"= dset.train$Class) #input$Targ.ML.Var)
    
    # dummy.dset.train$Class <- ifelse(dummy.dset.train$Class=='GOOD',1,0)
    
    inTraining <- createDataPartition(dummy.dset.train$Class, p = perc, list = FALSE)
    training <- dummy.dset.train[ inTraining,]
    testing  <- dummy.dset.train[-inTraining,]
    
    fitControl <- trainControl(classProbs = TRUE, returnData = TRUE)

    svmFit <- train(Class~., data=training, method = "svmRadial", trControl = fitControl, metric="ROC")
    
  } 
  
  svm.model <- svmFit
  
  return(svm.model)
})
#----------------------------------------------------

#------------------------------------- GLM function
runGLM <- reactive({
  
  list.vars <- list(input$Targ.ML.Var, input$preds.ML.Vars)
  Class <- input$Targ.ML.Var
  
  # "data": dataset that based on the user choices in the first page
  data <- passData()  
  dset.train <- data[ , names(data) %in% unlist(list.vars) ]
  
  fmla <- as.formula( paste(input$Targ.ML.Var, paste(input$preds.ML.Vars, collapse="+"), sep=" ~ ") )      
 
  # cross-validation
  if ( input$testingOptions == 1 ){
    dummy.ds <- dummyVars(fmla,data=dset.train, fullRank=F)
    dummy.dset.train <- data.frame(predict(dummy.ds, newdata = dset.train),"Class"= dset.train$Class) #input$Targ.ML.Var)
  
    dummy.dset.train$Class <- ifelse(dummy.dset.train$Class=='GOOD',1,0)
    
    fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = 10,
      ## repeated ten times
      repeats = 10)
    
    glmnetFit <- train(Class~., data=dummy.dset.train, method = "glmnet", trControl = fitControl)
    
  }else{
    # random split

    perc <- input$percentage/100
    set.seed(998)
    Class <- input$Targ.ML.Var
    
    dummy.ds <- dummyVars(fmla,data=dset.train, fullRank=F)
    dummy.dset.train <- data.frame(predict(dummy.ds, newdata = dset.train),"Class"= dset.train$Class) #input$Targ.ML.Var)
    
    dummy.dset.train$Class <- ifelse(dummy.dset.train$Class=='GOOD',1,0)

    inTraining <- createDataPartition(dummy.dset.train$Class, p = perc, list = FALSE)
    training <- dummy.dset.train[ inTraining,]
    testing  <- dummy.dset.train[-inTraining,]
    
    glmnetFit <- train(Class~., data=training, method = "glmnet") 
    
   }
  
   glmnet.model <- glmnetFit  
   return(glmnet.model)
     
})

#----------------------------------------------------
output$summary.model <- renderPrint({
  if (input$goAnalysis == 0){
    return() }
  else{ 
    isolate({   
      if ( !is.null(input$Targ.ML.Var) ){
        # if SVM 
        if (input$radioML == 1){
            svm.mod <- runSVM()
            res <- svm.mod$results[rownames(svm.mod$bestTune),]
        }else{
        # if GLM
            gml.mod <- runGLM()
            res <- gml.mod$results[rownames(gml.mod$bestTune),]
        }
        
        print(res) 
                     
      }else{ 
        print(data.frame(Warning="Please select Model Parameters."))
      }
    }) # end isolate
  } # end if...else
})    

output$validate.model <- renderPrint({
  if (input$goAnalysis == 0){
    return() }
  else{ 
    isolate({   
      if ( !is.null(input$Targ.ML.Var) ){ 
        # list of variables to examine
        list.vars <- list(input$Targ.ML.Var, input$preds.ML.Vars)
        fmla <- as.formula( paste(input$Targ.ML.Var, paste(input$preds.ML.Vars, collapse="+"), sep=" ~ ") )
        
        # "data": dataset that based on the user choices in the first page
        data <- passData()  
        dset <- data[ , names(data) %in% unlist(list.vars) ]
        targ <- input$Targ.ML.Var
        
        # if SVM 
        if (input$radioML == 1){
        
          dummy.ds <- dummyVars(fmla, data=dset, fullRank=F)
          dummy.dset <- data.frame(predict(dummy.ds, newdata = dset),"Class"= dset$Class) #input$Targ.ML.Var)
          
          predictorsNames <- names(dummy.dset)[names(dummy.dset) != targ]  
          
          nr=nrow(dummy.dset)
          perc = 1 #50/100
          ids <- sort(ceiling(sample( seq(1,nr), nr*perc, replace = FALSE)))
          dummy.ds.test <- dummy.dset[ ids, ]
      
          # call svm model
          svm.mod <- runSVM()
          testPred <- predict(svm.mod, dummy.ds.test )
          confmat <- confusionMatrix(testPred, dummy.ds.test[ ,targ])
          
          results.model<-confmat
          
        }else{
          # if GLM
         
          dummy.ds <- dummyVars(fmla, data=dset, fullRank=F)
          dummy.dset <- data.frame(predict(dummy.ds, newdata = dset),"Class"= dset$Class) #input$Targ.ML.Var)
          
          predictorsNames <- names(dummy.dset)[names(dummy.dset) != targ]  
          
          nr=nrow(dummy.dset)
          perc = 1 #50/100
          ids <- sort(ceiling(sample( seq(1,nr), nr*perc, replace = FALSE)))
          dummy.ds.test <- dummy.dset[ ids, ]
          
          gml.mod <- runGLM()
          testPred <- predict(gml.mod, dummy.ds.test[ , predictorsNames] )
          auc <- roc(dummy.ds.test[,targ], testPred)
          
          results.model<-auc$auc
          
        }
      
        print( results.model )
      }else{ 
        print(data.frame(Warning="Please select Model Parameters."))
      }
      
    }) # end isolate
  } # end if...else
})    


output$plot_ML.Rel.Impo <- renderPlot({ 
  if (input$goAnalysis == 0){
    return() }
  else{ 
    isolate({ 
      # if SVM 
      if (input$radioML == 1){
        
          svm.mod <- runSVM()
          RocImp <- varImp(svm.mod, scale = FALSE)
          plot(RocImp) 
      
      }
      else{
          # if GLM
          gml.mod <- runGLM()
          RocImp <- varImp(gml.mod, scale = FALSE)
          
          results <- data.frame(row.names(RocImp$importance),RocImp$importance$Overall)
          results$VariableName <- rownames(RocImp)
          colnames(results) <- c('VariableName','Class')
          results <- results[order(results$Class),]
          results <- results[(results$Class != 0),]
          
          par(mar=c(5,15,4,2)) # increase y-axis margin. 
          xx <- barplot(results$Class, width = 0.25, 
                        main = paste("Variable Importance using GLM model"), horiz = T, 
                        xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = TRUE, 
                        col = ifelse((results$Class > 0), 'blue', 'red')) 
          axis(2, at=xx, labels=results$VariableName, tick=FALSE, las=2, line=-0.3, cex.axis=0.6) 
          
      }
    })
  }
})

output$ML.Rel.Impo <- renderPrint({ 
  if (input$goAnalysis == 0){
    return() }
  else{ 
    isolate({  
      # if SVM 
      if (input$radioML == 1){
        svm.mod <- runSVM()
        RocImp <- varImp(svm.mod, scale = FALSE)
        print(RocImp, digits=3, justify="left")
      }
      else{
        # if GLMnet
        gml.mod <- runGLM()
        RocImp <- varImp(gml.mod, scale = FALSE)
        print(RocImp, digits=3, justify="left")
      }
   })
  }
})

#---------------------------------------------------------------------------------------------------
# Tab: Predict with Machine Learning Models
#
predict.with.ML.Model <- reactive({
  # load the model SVM or GLMnet 
  if (input$radioML == 1){
    ML.model <- runSVM()
  }else{
    ML.model <- runGLM()
  }
  
  # create an instance from the input values 
  list.predictors <- input$preds.ML.Vars
  num.preds <- length(list.predictors)
  targ <- input$Targ.ML.Var
  fmla <- as.formula( paste(" ", paste(input$preds.ML.Vars, collapse="+"), sep=" ~ ") )
  
  newdata <- as.data.frame(matrix(0, nrow = 1, ncol=num.preds))
  newdata <- lapply(1:num.preds, function(i) {
    input_name <- paste0("input", i, sep="")
    input[[ input_name ]]
  } # end function
  )# end lapply
  names(newdata) <- list.predictors
  
  dummy.newdata <- dummyVars(fmla, data=newdata, fullRank=F)
  dummy.newdata <- data.frame(predict(dummy.newdata, newdata = newdata))
  predict.Names <- names(dummy.newdata)
 
  pred_ML_model <- predict(ML.model, dummy.newdata)
  names(pred_ML_model) <- as.character(input$Targ.ML.Var)
  
  return(pred_ML_model)
  
})

output$dyn_input <- renderUI({
  
    data <- passData()
    list.predictors <- input$preds.ML.Vars
    num.preds <- length(list.predictors)
    
    inputs <- lapply(1:num.preds, function(i) {
    input_name <- paste0("input", i, sep="")
    fluidRow(column(width=6, 
                if ( is.factor( data[, list.predictors[[i]]] ) )
                {
                  list.values <- unique( data[, list.predictors[[i]]] )
                  selectInput(inputId=input_name, label=h4( as.character(list.predictors[[i]]) ), 
                              choices=as.character(list.values), multiple=FALSE)
                }else{  
                  numericInput( input_name, label = h4( as.character(list.predictors[[i]]) ), value = NA)
                } # end if...else
              ) # end column
            ) # end fluidRow
      } # end function
    ) # end lapply
    
    do.call(tagList, inputs)
}) 

# predict value regarding the predictors' values
output$prediction.value.ML <- renderPrint({ 
  
 if (input$goMLPredict == 0){
   return() }
 else{ 
     isolate({
       
        pred_val <- predict.with.ML.Model()
        names(pred_val) <- as.character(input$Targ.ML.Var)
        
        ml.response <- data.frame(pred_val, stringsAsFactors = FALSE)
        print( ml.response )
      
     }) # end isolate
  } # end if...else
  
})



#---------------------------------------------------------------------------------------------------
#     Classification
#---------------------------------------------------------------------------------------------------

# If the problem is for classification then the targets variable are categorical, else regression
output$targs.Variables <- renderUI({ 
  if (input$radioDesTree == 1){
    var <- list("Class", "Current.Grading")
    radioButtons(inputId='TargVar', label=h3('Target Variable:'), choices=var, selected=var[[1]])
  } else if (input$radioDesTree == 2){
    var <- list("Econ.FCR.Period", "LTD.Econ.FCR", "SFR.Period", "SGR.Period")
    radioButtons(inputId='TargVar', label=h3('Target Variable:'), choices=var, selected=var[[1]])
  } 
})  # end renderUI targs.Variables

output$preds.Variables <- renderUI({
  data <- passData() 
  Vars <- names(data)
  indep.vars <- Vars[ Vars != input$TargVar] 
  selectInput(inputId='preds.Vars', label=h3('Predictors:'), choices=indep.vars, multiple=TRUE)
})  # end renderUI preds.Variables


output$fmla.dec.Trees <- renderText({
  if (input$goDT == 0){
    return() }
  else{ 
    isolate({   
        fmla = paste( as.character(input$TargVar), 
                      paste(as.character(input$preds.Vars), collapse=" + "), sep=" ~ " )
    })  # end isolate
  } # end if...else
})

runClassRegTrees <- reactive({
  
  list.vars <- list(input$TargVar, input$preds.Vars)
  
  # "data": dataset that based on the user choices in the first page
  data <- passData()  
  dset.train <- data[ , names(data) %in% unlist(list.vars) ]
  
  fmla <- as.formula( paste(input$TargVar, paste(input$preds.Vars, collapse="+"), sep=" ~ ") )      
  
  if (input$radioDesTree == 1){
    dec.Tree <- rpart(formula=fmla, data=dset.train, method="class", model=T, parms = list(split = "gini"), 
                      control = rpart.control(minsplit = 50, minbucket = round(50/3), cp = 1e-3, xval = 20))
    
    # prune the tree
    opt <- which.min(dec.Tree$cptable[,"xerror"])
    cp <- dec.Tree$cptable[opt, "CP"]
    dec.Tree <- prune( dec.Tree, cp = cp)
    
  } else if (input$radioDesTree == 2){   
    dec.Tree <- rpart(formula=fmla, data=dset.train, method="anova", model=T, parms = list(split = "gini"), 
                      control = rpart.control(minsplit = 50, minbucket = round(50/3), cp = 1e-3, xval = 20))
    # prune the tree
    opt <- which.min(dec.Tree$cptable[,"xerror"])
    cp <- dec.Tree$cptable[opt, "CP"]
    dec.Tree <- prune( dec.Tree, cp = cp)
    
  }  
  return(dec.Tree)
})

output$plot_dec.Tree <- renderPlot({ 
  if (input$goDT == 0){
    return() }
  else{ 
    isolate({  
        class.reg.Tree <- runClassRegTrees()
        if (input$radioDesTree == 1){
            plot(as.party(class.reg.Tree))
        } else if (input$radioDesTree == 2){
            plot(as.party(class.reg.Tree))
        }
    })
  }    
})

output$info_tree_abs_relacc <- renderPrint({
  if (input$goDT == 0){
    return() }
  else{ 
    isolate({  
      class.reg.Tree <- runClassRegTrees()
      n=nrow(class.reg.Tree$model)
      Root.node.error = class.reg.Tree$frame$dev[1]/n
      rel.err <- min(class.reg.Tree$cptable[,3])
      resubstitution.error.rate = rel.err*Root.node.error*100
      xrel.err <- min(class.reg.Tree$cptable[,4])
      absolute.cross.validated.error = xrel.err*Root.node.error*100   
      
      res.mat <- matrix( c( rel.err*100, resubstitution.error.rate, xrel.err*100, absolute.cross.validated.error ), c(1,4) )
      colnames(res.mat) <- c( " Rel.Error (%) ", " Resubstitution Error Rate ",  " Cross-Val. Error (%)", " Abs. Cross-Val. Error " )
      print(res.mat[1,], row.names=FALSE, digits=3, justify="left")
    })
  }
})

output$info_tree_acc <- renderDataTable({  
  if (input$goDT == 0){
    return() }
  else{ 
    isolate({  
        class.reg.Tree <- runClassRegTrees()
        table <- class.reg.Tree$cptable   
        colnames(table)<-c("Complexity Parameter (CP)", "No Splits", "Relative Error", "Cross-Validated Error", "Cross-Validated Standard Deviation" )
        print(table, digits=3, justify="left", width='minimum')
    })
  }
})

output$RegClass.Rel.Impo <- renderPrint({
  if (input$goDT == 0){
    return() }
  else{ 
    isolate({  
        class.reg.Tree <- runClassRegTrees()
        rel.imp <- 100*class.reg.Tree$variable.importance/sum(class.reg.Tree$variable.importance)
        print(rel.imp, row.names=FALSE, digits=3, justify="left")
    })
  }
})          

output$plot_RegClass.Rel.Impo <- renderPlot({ 
  if (input$goDT == 0){
    return() }
  else{ 
    isolate({  
        class.reg.Tree <- runClassRegTrees()
        rel.imp <- 100*class.reg.Tree$variable.importance/sum(class.reg.Tree$variable.importance)
        barplot(rel.imp, xlab='% of Responce Variance', main=paste('Relative Importances for', input$responseVar, sep=' '), horiz=TRUE, 
                las=1,cex.names=0.8, col='blue')
    })
  }
})

output$print_Tree.rules <- renderPrint({
  if (input$goDT == 0){
    return() }
  else{ 
    isolate({  
        class.reg.Tree <- runClassRegTrees()
        print(class.reg.Tree)
    })
  }
})
#---------------------------------------------------------------------------------------------------
# Tab: Predict with the Classification Tree
#
output$dyn_input.DT <- renderUI({
  
  data <- passData()
  list.predictors <- input$preds.Vars
  num.preds <- length(list.predictors)
  
  inputs <- lapply(1:num.preds, function(i) {
    input_name <- paste0("input", i, sep="")
    fluidRow(column(width=6, 
                    if ( is.factor( data[, list.predictors[[i]]] ) )
                    {
                      list.values <- unique( data[, list.predictors[[i]]] )
                      selectInput(inputId=input_name, label=h4( as.character(list.predictors[[i]]) ), 
                                  choices=as.character(list.values), multiple=FALSE)
                    }else{  
                      numericInput( input_name, label = h4( as.character(list.predictors[[i]]) ), value = NA)
                    } # end if...else
    ) # end column
    ) # end fluidRow
  } # end function
  ) # end lapply
  
  do.call(tagList, inputs)
}) 

# predict value regarding the predictors' values
output$prediction.value.DT <- renderPrint({ 
  
  if (input$goDTPredict == 0){
    return() }
  else{ 
    isolate({
      
      # load the model 
      class.reg.Tree <- runClassRegTrees()
      
      # create an instance from the input values 
      list.predictors <- input$preds.Vars
      num.preds <- length(list.predictors)
      
      newdata <- as.data.frame(matrix(0, nrow = 1, ncol=num.preds))
      newdata <- lapply(1:num.preds, function(i) {
        input_name <- paste0("input", i, sep="")
        input[[ input_name ]]
      } # end function
      )# end lapply
      names(newdata) <- list.predictors
      
      print(newdata)
      print( class(newdata) )
      
      pred_val <- predict(class.reg.Tree, newdata)
      names(pred_val) <- as.character(input$TargVar)
      
      DT.response <- data.frame(pred_val) #, stringsAsFactors = FALSE)
      print( DT.response )
      
     }) # end isolate
  } # end if...else
  
}) 


















  
}) # end shinyServer