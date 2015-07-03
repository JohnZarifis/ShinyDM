### Version 30.05.2015
#
# load packages
#
library("shiny")
library("shinyFiles")
library("shinyBS")
library("lubridate")
library("htmltools")
library("d3heatmap")
library("DT") 
library("rpivotTable") #devtools::install_github("smartinsightsfromdata/rpivotTable")
library("htmlwidgets")
library("RColorBrewer") 
library("readxl") 
library("graphics")
library("ggplot2")
library("lattice")
library("mgcv")
library("plotrix")
library("psych")
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
library("ggthemr") # devtools::install_github('ggthemr', 'cttobin')
library("dimple") #devtools::install_github("Bart6114/dimple")
#library("jsonlite")
#library("RJSONIO")

#-----------------------------------------
# load dataset
pathname = paste(getwd(), "aquaData.xlsx", sep="/")
Dataset <- read_excel(pathname, sheet = 1 ,col_names = TRUE, na='na')

# ------------------------
# Create the dataset
# ------------------------
create_dataset <- function(dataset){
  
  data <- data.frame(
    'Orientation' = substr(dataset$Unit,1,1) 
    ,'System' = substr(dataset$Unit,2,2)
    ,'Cage' = substr(dataset$Unit,nchar(as.character(dataset$Unit))-1,nchar(as.character(dataset$Unit))-1)
    ,'Section' = substr(dataset$Unit,nchar(as.character(dataset$Unit)),nchar(as.character(dataset$Unit)))
    ,'Region' = dataset$Region
    ,'Site' = dataset$Site
    ,'Unit' = dataset$Unit
    ,'Batch' = dataset$Batch 
    ,"Hatchery" = dataset$Hatchery
    ,"Origin.Year" = as.character(dataset$"Origin Year")
    ,"Origin.Month" = dataset$'Origin Month'
    ,'Current.Grading' = dataset$'Current Grading'
    #,'From' = dmy(dataset$From)
    #,"To" = dmy(dataset$To)
    #,"From" = as.Date(dataset$From, origin="1899-12-30") 
    #,"To" = as.Date(dataset$To, origin="1899-12-30")
    ,"From" = ymd(as.Date(dataset$From, origin="1899-12-30")) 
    ,"To" = ymd(as.Date(dataset$To, origin="1899-12-30"))                    
    ,"Month.Sampling" = month(as.Date(dataset$To, origin="1899-12-30"),label = TRUE)
    ,"Start.Av.Weight" = dataset$'Start Av. Wt.'
    ,"End.Av.Weight" = dataset$'End Av.Wt.'
    ,"Model.End.Av.Weight.Act.Feed" = dataset$'Model End Av. Wt. Act. Feed' 
    ,"Av.Weight.Deviation" = dataset$'Av. Wt. Deviation (%)' 
    ,"Av.Weight.Before.Sampling" = dataset$'Av. Wt. Before Sampl.' 
    ,"Model.End.Av.Weight.Suggested.Feed" = dataset$'Model End Av. Wt. Sugg. Feed'
    ,"Actual.Feed" = dataset$'Actual Feed' 
    ,"Feed.Category" = dataset$'Feed Category' 
    ,"Supplier" = dataset$Supplier 
    ,"Period.Feed.Qty" = dataset$'Period Feed Qty'
    ,"Suggested.Feed.Qty" = dataset$'Suggested Feed Qty'
    ,"Opening.Fish.No" = dataset$'Opening Fish No' 
    ,"Opening.Biomass" = round(dataset$'Opening Biomass', digits=2)
    ,"Closing.Fish.No" = dataset$'Closing Fish No' 
    ,"Closing.Biomass" = round(dataset$'Closing Biomass', digits=2)     
    ,"Harvest.Biomass" = dataset$'Harvest Biomass' 
    ,"Biomass.Produced" = dataset$'Biomass Produced'     
    ,"Biomass.Produced.Before.Sampling" = dataset$'Biomass Produced Before Sampl.' 
    ,"Econ.FCR.Period" = dataset$'Econ. FCR Period'
    ,"FCR.Before.Sampling" = dataset$'Econ FCR Period Before Sampl.' 
    ,"Mortality.No" = dataset$'Mortality No' 
    ,"Model.Mortality.No" = dataset$'Model Mortality No' 
    ,"Mortality.Deviation" = dataset$'Mortality Deviation (%)'
    ,"SFR.Period" = dataset$'SFR Period (%)' 
    ,"SFR.Period.Before.Sampling" = dataset$'SFR Period (%) Before Sampl.'     
    ,"SGR.Period" = dataset$'SGR Period (%)' 
    ,"Max.Food.Qty" = dataset$'Max Feed Qty'
    ,"Food.Price" = dataset$'Food Price'
    ,"LTD.Econ.FCR" = round(dataset$'LTD Econ. FCR' , digits=2)
    ,"LTD.Mortality" = round(dataset$'LTD Mortality %', digits=2)  
    ,"LTD.Mortality.No" = dataset$'LTD Mortality No' 
    ,"Avg.Oxygene" = dataset$'Avg. Oxygene' 
    ,"Avg.Temperature" = dataset$'Avg. Temp.' 
    ,"Feeding.Policy" = dataset$'Feeding Policy' 
    ,"Period.Day.Degrees" = dataset$'Period Day Degrees'
    ,"Start.Av.Weight.Category" =  dataset$'Start Av. Weight Category'
    ,"End.Av.Weight.Category" = dataset$'End Av. Weight Category'                     
    ,"Age" = dataset$AGE                     
    ,"Days" = interval(as.Date(dataset$From, origin="1899-12-30"), as.Date(dataset$To, origin="1899-12-30") )%/%days(1)
    ,"Start.Av.Weight.BioCat" = dataset$"Start Av Weight BioCat"
    ,"End.Av.Weight.BioCat" = dataset$"End Av Weight BioCat"
    ,"Ph"=round(dataset$Ph, digits=2)
    ,"CAUDAL.O3" = round(dataset$"CAUDAL O3 (Nm3/H)")
    ,"WATER.RENEWAL"= round(dataset$"WATER RENEWAL (l./min.)")
    ,"NH3" = round(dataset$"NH3 (ppm.)", digits=2)
    ,"NO2" = round(dataset$"NO2 (ppm.)", digits=2)
    #"Class" = dataset$CLASS
    #"Period.Mortality" = dataset$'Period Mortality %'
    #"LTD.Day.Degrees" = dataset$'LTD Day Degrees',
    #"Fastings.No" = dataset$'Fastings No',
    # "FastingsPerc"=dataset$'Fastings %'
  )
  
  data$Class = ifelse( data$Av.Weight.Deviation > 0,"GOOD","BAD" )
  data$Class = as.factor(data$Class)
  #data$Ph = as.numeric(data$Ph)
  
  #  For debugging  
  #  View(data)
  #  str(data)
  # print(nrow(data))
  
  return(data)
  
  #"ProductionTimeDays" = paste(01,dataset$Origin.Month, dataset$Origin.Year, sep="-" )
}



#-----------------------------------------------------------------------------------------------------
##   Function that Summarizes data.
##   Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE,w,quant) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col, w) {
                   c(N         = length2(xx[[col]], na.rm=na.rm),
                     mean      = mean   (xx[[col]], na.rm=na.rm),
                     sd        = sd     (xx[[col]], na.rm=na.rm),
                     w.mean    = weighted.mean(xx[[col]],xx[[w]]),
                     sum.w     = sum(xx[[w]]),
                     min       = min(xx[[col]]),
                     max       = max(xx[[col]]),
                     quant     = quantile(xx[[col]],quant)
                   )
                 },
                 measurevar,w
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval  ?? t or z for us??: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  # Todo create function for percentiles...
  return(datac)
}

######-------end of function-----------####


###--------- Set ggplot2 theme and enrich the pallete --------###

ggthemr('flat dark')

set_swatch(c(   "#ecf0f1" # original Light grayish cyan.
                , "#3498db" # original Bright blue
                , "#2ecc71" # original Strong cyan - lime green.
                , "#f1c40f" # original Vivid yellow
                , "#e74c3c" # original Bright red.
                , "#9b59b6" # original Moderate violet.
                , "#1abc9c" # original Strong cyan.
                , "#f39c12" # original Vivid orange
                , "#2a0189" # Dark violet
                , "#221abc" # Strong blue
                , "#555555" # sgi darkgray 
                , "#3d3208" # Very dark yellow [Olive tone]
                , "#8E388E" #sgi beet
                , "#8470FF" #lightslateblue
                , "#8B8989" #Dark grayish red
                , "#1c8901" #Dark lime green
                , "#D1EEEE" #lightcyan 2
                , "#4b1abc" #Strong violet
                , "#B0171F" #indian red
                , "#8bbc1a" #Strong green
                , "#7D9EC0" #sgi lightblue
                , "#CAE1FF" #lightsteelblue 1
                , "#7171C6" #sgi slateblue
                , "#9c1abc" #Strong magenta
                , "#d35400" #original Strong orange
))

# in case we need to reset the theme.
# set_swatch(c("#ecf0f1", "#3498db", "#2ecc71", "#f1c40f" ,"#e74c3c", "#9b59b6", "#1abc9c", "#f39c12", "#d35400"))
# ggthemr_reset()



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


df <- create_dataset(Dataset)

View(df)
xx <- "None"
dimple(df,
       xMeasure="Age",
       xCategory=c("Age"),
       #xOrderRule = "Date",
       yMeasure="End.Av.Weight",
       #if (xx == "None"){} else{zMeasure="Closing.Biomass",}
       #zMeasure="Closing.Biomass",
       zMeasure = NULL,
       series=c("Unit","Hatchery"),
       legend=TRUE,
       chartType="bubble"
       #,seriesAggregate = "max"
       , width = 990, height = 600
)











