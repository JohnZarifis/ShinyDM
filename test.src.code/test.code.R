source("helpers.R")
library("lubridate")
library("e1071")
library("caret")

Dataset <- read.delim("TSIPOYRA-2014 BATCHES-ANON-2.csv", header = TRUE, sep = ";", dec=".")
data <- create_dataset(Dataset)

#data <- data[data$Class=='GOOD', ]

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

# #------------------------------------------------------
# ds.tr1 <- data[ , names(data) %in% unlist(list(preds,targ))]
# 
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)
# 
#   
# svmFit <- train(fmla.cl, data=ds.tr1, method = "svmRadial", trControl = fitControl)
#   
# svmFit.best.acc.kappa <- svmFit$results[rownames(svmFit$bestTune),]
#   
# RocImp <- varImp(  svmFit, scale = FALSE)
# RocImp
# 
# plot(RocImp)
# 
# #------------------------------------------------------

ds.tr2 <- data[ , names(data) %in% unlist(list(preds,targ))]

targ <- "Class"
preds <- c("Site", "Region", "Hatchery", "Days", "Econ.FCR.Period", "Actual.Feed", "End.Av.Weight")

fmla <- as.formula(paste(" ",paste(preds, collapse="+"), sep=" ~ ") )
dummy.ds <- dummyVars(fmla,data=ds.tr2, fullRank=F)
dummy.ds.tr2 <- data.frame(predict(dummy.ds, newdata = ds.tr2),"Class"=ds.tr2$Class)
dummy.ds.tr2$Class <- ifelse(dummy.ds.tr2$Class=='GOOD',1,0)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
  
svmFit2 <- train(Class~., data=dummy.ds.tr2, method = "svmRadial", trControl = fitControl)
  
svmFit2.best.acc.kappa <- svmFit2$results[rownames(svmFit2$bestTune),]

RocImp2 <- varImp(svmFit2,scale=F)
RocImp2

plot(RocImp2)
# 
# results <- data.frame(row.names(RocImp2$importance),RocImp2$importance$Overall)
# results$VariableName <- rownames(RocImp2)
# colnames(results) <- c('VariableName','Class')
# results <- results[order(results$Class),]
# results <- results[(results$Class != 0),]
# 
# par(mar=c(5,15,4,2)) # increase y-axis margin. 
# xx <- barplot(results$Class, width = 0.85, 
#               main = paste("Variable Importance using SVM model"), horiz = T, 
#               xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = TRUE, 
#               col = ifelse((results$Class > 0), 'green', 'red')) 
# axis(2, at=xx, labels=results$VariableName, tick=FALSE, las=2, line=-0.3, cex.axis=0.6)  

#---------------------------------

# dummy.ds.good <- dummy.ds.tr2[dummy.ds.tr2$Class==1,]
# 
# targ <- "Class"
# preds <- c("Site", "Region", "Hatchery", "Days", "Econ.FCR.Period", "Actual.Feed", "End.Av.Weight")
# 
# fmla <- as.formula(paste(" ",paste(preds, collapse="+"), sep=" ~ ") )
# dummy.ds <- dummyVars(fmla,data=dummy.ds.good, fullRank=F)
# dummy.ds.good <- data.frame(predict(dummy.ds, newdata = dummy.ds.good),"Class"=dummy.ds.good$Class)
# 
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)
# 
# svmFitg <- train(Class~., data=dummy.ds.good, method = "svmRadial", trControl = fitControl)
# 
# svmFitg.best.acc.kappa <- svmFitg$results[rownames(svmFitg$bestTune),]
# 
# RocImpg <- varImp(svmFitg,scale=F)
# RocImpg
# 
# plot(RocImpg)
# 
# results <- data.frame(row.names(RocImpg$importance),RocImpg$importance$Overall)
# results$VariableName <- rownames(RocImpg)
# colnames(results) <- c('VariableName','Class')
# results <- results[order(results$Class),]
# results <- results[(results$Class != 0),]
# 
# par(mar=c(5,15,4,2)) # increase y-axis margin. 
# xx <- barplot(results$Class, width = 0.85, 
#               main = paste("Variable Importance using SVM model - good class"), horiz = T, 
#               xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = TRUE, 
#               col = ifelse((results$Class > 0), 'green', 'red')) 
# axis(2, at=xx, labels=results$VariableName, tick=FALSE, las=2, line=-0.3, cex.axis=0.6)  
# 
# 
# 
# dummy.ds.bad <- dummy.ds.tr2[dummy.ds.tr2$Class==0,]













#--------------------------------------------
# ds.tr3 <- data[ , names(data) %in% unlist(list(preds,targ))]
# 
# fmla <- as.formula(paste(targ,paste(preds, collapse="+"), sep=" ~ ") )
# dummy.ds <- dummyVars(fmla,data=ds.tr3, fullRank=F)
# dummy.ds.tr3 <- data.frame(predict(dummy.ds, newdata = ds.tr3),"Class"=ds.tr3$Class)
# 
# dummy.ds.tr3$Class <- ifelse(dummy.ds.tr3$Class=='GOOD',1,0)
# 
# View(dummy.ds.tr3)
# 
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)
# 
# glmnetFit <- train(Class~., data=dummy.ds.tr3, method = "glmnet", trControl = fitControl)
# 
# RocImp3 <- varImp(glmnetFit,scale=F)
# RocImp3
# 
# plot(RocImp3)
# 
# 
# results <- data.frame(row.names(RocImp3$importance),RocImp3$importance$Overall)
# results$VariableName <- rownames(RocImp3)
# colnames(results) <- c('VariableName','Class')
# results <- results[order(results$Class),]
# results <- results[(results$Class != 0),]
# 
# par(mar=c(5,15,4,2)) # increase y-axis margin. 
# xx <- barplot(results$Class, width = 0.85, 
#               main = paste("Variable Importance using GLM model"), horiz = T, 
#               xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = TRUE, 
#               col = ifelse((results$Class > 0), 'green', 'red')) 
# axis(2, at=xx, labels=results$VariableName, tick=FALSE, las=2, line=-0.3, cex.axis=0.6)  
# 


#--------------- cross validation
# cr=10
# ds.tr <- data[ , names(data) %in% unlist(list(preds,targ))]
# svm.fit.cl.cr <- tune(svm, fmla.cl, data=ds.tr, type="nu-classification", kernel="radial", 
#                    ranges=list(cost=c(0.1,1,10,50,100,150,200), gamma=c(0.5,1,2,3,4)),
#                    nu=0.5, tunecontrol = tune.control(sampling ="cross",cross=cr, 
#                    sampling.aggregate = mean, sampling.dispersion = sd) )  
# 
# best.svm.fit.cl.cr <- svm.fit.cl.cr$best.model
# 
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


                   