source("helpers.R")
library("lubridate")
library("e1071")
library("caret")

Dataset <- read.delim("TSIPOYRA-2014 BATCHES-ANON-2.csv", header = TRUE, sep = ";", dec=".")
data <- create_dataset(Dataset)

targ <- "Class"
preds <- c("Site", "Region", "Hatchery", "Days", "Econ.FCR.Period", "Actual.Feed", "End.Av.Weight", "Origin.Month")

fmla.cl<- as.formula(paste(targ, paste(preds, collapse="+"), sep=" ~ ") ) 

# perc = 80/100
# nr <- nrow(data)
# ids <- sort(ceiling(sample( seq(1,nr), nr*perc, replace = FALSE)))
# ds.tr1 <- data[ ids, names(data) %in% unlist(list(preds,targ))]
# ds.ts1 <- data[ -ids, names(data) %in% unlist(list(preds,targ))]
# 
# ds.tr1 <- data[ , names(data) %in% unlist(list(preds,targ))]
# 
# svm.fit.cl <- tune(svm, fmla.cl, data=ds.tr1, type="nu-classification", kernel="radial", 
#                     ranges=list(cost=c(0.1,1,10,50,100,150,200), gamma=c(0.5,1,2,3,4)),
#                                 nu=0.5, 
#                     tunecontrol = tune.control(sampling ="fix",fix=perc, 
#                     sampling.aggregate = mean, sampling.dispersion = sd) )  
#                    
# best.svm.fit.cl <- svm.fit.cl$best.model
# pred <- predict(best.svm.fit.cl, ds.ts1 )
# confmat <- table(true=ds.ts1[,targ],pred)
# print(confmat)
# 
# #print(summary(svm.fit.cl))
# 
# res<-list( "Support Vectors per class: "=best.svm.fit.cl$nSV, "Gamma: "= best.svm.fit.cl$gamma,"Cost: "=best.svm.fit.cl$cost,
#       "Method:"=best.svm.fit.cl$call$method, "Type:"=best.svm.fit.cl$call$type, "Kernel:"=best.svm.fit.cl$call$kernel  )
# 
# print(res)

#------------------------------------------------------
ds.tr1 <- data[ , names(data) %in% unlist(list(preds,targ))]
ds.tr1.Dummy <- dummyVars( " ~ Site + Region + Hatchery + Days + Econ.FCR.Period + Actual.Feed+ End.Av.Weight + Origin.Month", data=ds.tr1, fullRank=F)
train.ds <- data.frame(predict(ds.tr1.Dummy, newdata = ds.tr1))

train.dset <- data.frame(train.ds, "Class"=ds.tr1$Class)


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

  
svmFit <- train(x=ds.tr1.Dummy$vars, y="Class", data=train.dset, 
                  method = "svmRadial",
                  trControl = fitControl)
  
  
RocImp <- varImp(  svmFit, scale = FALSE)
RocImp

plot(RocImp, scale=F)


#--------------- cross validation
# cr=10
# ds.tr <- data[ , names(data) %in% unlist(list(preds,targ))]
# svm.fit.cl.cr <- tune(svm, fmla.cl, data=ds.tr, type="nu-classification", kernel="radial", 
#                    ranges=list(cost=c(0.1,1,10,50,100,150,200), gamma=c(0.5,1,2,3,4)),
#                    nu=0.5, tunecontrol = tune.control(sampling ="cross",cross=cr, 
#                    sampling.aggregate = mean, sampling.dispersion = sd) )  
# 
# best.svm.fit.cl.cr <- svm.fit.cl.cr$best.model
# # pred <- predict(best.svm.fit.cl.cr, ds.ts1 )
# # confmat <- table(true=ds.ts1[,targ],pred)
# # print(confmat)
# 
# print(summary(svm.fit.cl.cr))


#---------------------------------------------------------

# targ <- "Econ.FCR.Period"
# preds <- c("Days","SFR.Period","SGR.Period", "Feed.Category","End.Av.Weight", "Avg.Temperature")
# 
# fmla.reg<- as.formula(paste(targ, paste(preds, collapse="+"), sep=" ~ ") ) 
# 
# perc = 80/100
# nr <- nrow(data)
# ids <- sort(ceiling(sample( seq(1,nr), nr*perc, replace = FALSE)))
# ds.tr2 <- data[ ids, names(data) %in% unlist(list(preds,targ))]
# ds.ts2 <- data[ -ids, names(data) %in% unlist(list(preds,targ))]
# 
# svm.fit.reg <- tune(svm, fmla.reg, data=ds.tr2, type="nu-regression", kernel="radial", 
#                    ranges=list(cost=c(0.1,1,10,50,100,150,200), gamma=c(0.5,1,2,3,4)), nu=0.5, 
#                    tunecontrol = tune.control(sampling ="fix",fix=perc, 
#                                               sampling.aggregate = mean, sampling.dispersion = sd) )
# 
# best.svm.fit.reg <- svm.fit.reg$best.model


                   