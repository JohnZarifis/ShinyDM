source("helpers.R")
library("lubridate")
library("e1071")
library("caret")
library("pROC")
library("glmnet")

Dataset <- read.delim("TSIPOYRA-2014 BATCHES-ANON-2.csv", header = TRUE, sep = ";", dec=".")
data <- create_dataset(Dataset)


#--------------------------- Regression Test

# regressors <- list("End.Av.Weight", "Start.Av.Weight", "Days", "Econ.FCR.Period", "SFR.Period", "SGR.Period",
#                    "Avg.Temperature", "Age")
# 
# response <- list("End.Av.Weight")
# predictors <- regressors[ regressors != unlist(response) ]
#  
# ds <- data[ , names(data) %in% unlist(regressors) ]
# 
# fmla = as.formula(paste(response," ~ ",paste(predictors, collapse="+")))
# 
# model <- glm(formula=fmla, family=gaussian(link='identity'), 
#               data=ds, control=glm.control(maxit=1000,trace=FALSE), model=TRUE)
# 
# 
# library("car")
# library("relaimpo")
# metrics <- calc.relimp(model, type = "car")


#--------------------------- End Regression Test


# targ <- "Class"
# #preds <- c("Site", "Region", "Hatchery", "Days", "Econ.FCR.Period", "Actual.Feed", "End.Av.Weight", "Origin.Month")
# preds <- c("Site", "Region", "Hatchery", "Days", "Econ.FCR.Period", "Actual.Feed", "End.Av.Weight")
# fmla.cl<- as.formula(paste(targ, paste(preds, collapse="+"), sep=" ~ ") ) 
# 
# list.vars <- list(targ, preds)
# ds.train <- data[, names(data) %in% unlist(list.vars) ]

# dec.Tree <- rpart(formula=fmla.cl, data=ds.train, method="class", model=T, parms = list(split = "gini"), 
#                   control = rpart.control(minsplit = 50, minbucket = round(50/3), cp = 1e-3, xval = 20))
# 
# plot(as.party(dec.Tree))
# 
# newdata <- ds.train[1,preds]
# newdata$End.Av.Weight <- 200
# newdata$Econ.FCR.Period <- 2.5
# newdata$Days <- 100
# 
# pred_val <- predict(dec.Tree, newdata, na.action = na.omit)
# ###############################################################





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
#     Caret package
#
# ds.tr1 <- data[ , names(data) %in% unlist(list(preds,targ))]
# 
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10, 
#   ## repeated ten times
#   repeats = 1,
#   ## Estimate class probabilities
#   classProbs = TRUE,
#   returnData = TRUE,
#   ## Evaluate performance using 
#   ## the following function
#   summaryFunction = twoClassSummary)
# 
# svmFit <- train(fmla.cl, data=ds.tr1, method = "svmRadial", trControl = fitControl, metric="ROC")
# 
# svmFit.best.acc.kappa <- svmFit$results[rownames(svmFit$bestTune),]
#   
# RocImp <- varImp(svmFit, scale = FALSE)
# RocImp
# 
# plot(RocImp)
# 
# 
# testPred <- predict(svmFit, ds.tr1[,preds])
# confusionMatrix(testPred, ds.tr1$Class)


# 
# 
# predictions <- predict(object=svmFit$finalModel, ds.tr1[,targ], type='prob')

# perc <- 80/100
# set.seed(998)
# Cl <- targ
# inTraining <- createDataPartition(ds.tr1$Cl, p = perc, list = FALSE)
# training <- ds.tr1[ inTraining,]
# testing  <- ds.tr1[-inTraining,]
# 
# fitControl <- trainControl(classProbs = TRUE)
# svmFit <- train(fmla.cl, data=training, method = "svmRadial", trControl = fitControl, metric="ROC")
# 
# predictions <- predict(object=svmFit, testing[,targ], type='prob')



#------------------------------------------------------

targ <- "Class"
preds <- c("Site", "Region", "Hatchery", "Days", "Econ.FCR.Period", "Actual.Feed", "End.Av.Weight")
ds.tr2 <- data[ , names(data) %in% unlist(list(preds,targ))]


fmla <- as.formula(paste(" ",paste(preds, collapse="+"), sep=" ~ ") )
dummy.ds <- dummyVars(fmla,data=ds.tr2, fullRank=F)
dummy.ds.tr2 <- data.frame(predict(dummy.ds, newdata = ds.tr2),"Class"=ds.tr2$Class)
dummy.ds.tr2$Class <- ifelse(dummy.ds.tr2$Class=='GOOD',1,0)


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10
  )

glmnetFit <- train(Class~., data=dummy.ds.tr2, method = "glmnet", trControl = fitControl)

perc <- 80/100
set.seed(998)
inTraining <- createDataPartition(dummy.ds.tr2$Class, p = perc, list = FALSE)
training <- dummy.ds.tr2[ inTraining,]
testing  <- dummy.ds.tr2[-inTraining,]

glmnetFit <- train(Class~., data=training, method = "glmnet") 
predictorsNames <- names(training)


testPred <- predict(glmnetFit, testing[ , predictorsNames] )
auc <- roc(testing[,targ], as.matrix(testPred))
print(auc$auc)

RocImp2 <- varImp(glmnetFit,scale=F)
RocImp2

plot(RocImp2)


#=======================================
# predict one arbitrary value
# ds.ts <- data[1,names(data) %in% c(preds,targ)]
# dum.ds.ts <- dummyVars(fmla,data=ds.ts , fullRank=F)
# dum.ds.ts1 <- data.frame(predict(dum.ds.ts, newdata = ds.ts, "Class"=data[1,targ]))
# predictors.Names <- names(dum.ds.ts1)[names(dum.ds.ts1) != targ]
# testPred.instance <- predict(svmFit, dum.ds.ts1[ , predictors.Names] )
#testPred.instance <- predict(glmnetFit, dum.ds.ts1[ , predictors.Names] )

#=======================================


#------------------------------------------------------------------
# nr=nrow(dummy.ds.tr2)
# perc = 80/100
# ids <- sort(ceiling(sample( seq(1,nr), nr*perc, replace = FALSE)))
# dummy.ds.test <- dummy.ds.tr2[ ids, ]
# predictorsNames <- names(training)
# testPred <- predict(glmnetFit, dummy.ds.test[ , predictorsNames] )
# 
# auc <- roc(dummy.ds.test[,targ], testPred)
# print(auc$auc)
#confmat <- confusionMatrix(testPred, dummy.ds.test[ ,targ])

#------------------------------------------------------------------
# perc <- 80/100
# set.seed(998)
# inTraining <- createDataPartition(dummy.ds.tr2$Class, p = perc, list = FALSE)
# training <- dummy.ds.tr2[ inTraining,]
# testing  <- dummy.ds.tr2[-inTraining,]
# 
# fitControl <- trainControl(classProbs = TRUE, returnData = TRUE, summaryFunction = twoClassSummary)
# 
# svmFit <- train(Class~., data=training, method = "svmRadial", trControl = fitControl, metric="ROC")


# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 5,
#   ## repeated ten times
#   repeats = 10,
#   ## Estimate class probabilities
#   classProbs = TRUE,
#   returnData = TRUE,
#   ## Evaluate performance using 
#   ## the following function
#   summaryFunction = twoClassSummary)
#   
# svmFit2 <- train(Class~., data=dummy.ds.tr2, method = "svmRadial", trControl = fitControl)
# #   
# svmFit2.best.acc.kappa <- svmFit2$results[rownames(svmFit2$bestTune),]
# 
# 
# RocImp2 <- varImp(svmFit,scale=F)
# RocImp2
# 
# plot(RocImp2)
# 
# results <- data.frame(row.names(RocImp2$importance),RocImp2$importance$Overall)
# results$VariableName <- rownames(RocImp2)
# colnames(results) <- c('VariableName','Class')
# results <- results[order(results$Class),]
# results <- results[(results$Class != 0),]
# 
# 
# par(mar=c(5,15,4,2)) # increase y-axis margin. 
# xx <- barplot(results$Class, width = 0.75, 
#               main = paste("Variable Importance using SVM model"), horiz = T, 
#               xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = TRUE, 
#               col = ifelse((results$Class > 0), 'blue', 'red')) 
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
# dummy.ds.bad <- dummy.ds.tr2[dummy.ds.tr2$Class==0,]



#--------------------------------------------
ds.tr3 <- data[ , names(data) %in% unlist(list(preds,targ))]

fmla <- as.formula(paste(targ,paste(preds, collapse="+"), sep=" ~ ") )
dummy.ds <- dummyVars(fmla,data=ds.tr3, fullRank=F)
dummy.ds.tr3 <- data.frame(predict(dummy.ds, newdata = ds.tr3),"Class"=ds.tr3$Class)

#dummy.ds.tr3$Class <- ifelse(dummy.ds.tr3$Class=='GOOD',1,0)

predictorsNames <- names(dummy.ds.tr3)[names(dummy.ds.tr3) != targ]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10
  )

glmnetFit <- train(Class~., data=dummy.ds.tr3, method = "glmnet", trControl = fitControl)

nr=nrow(dummy.ds.tr3)
perc = 1 #50/100
ids <- sort(ceiling(sample( seq(1,nr), nr*perc, replace = FALSE)))
dummy.ds.test <- dummy.ds.tr3[ ids, ]

testPred <- predict(glmnetFit, dummy.ds.test[ , predictorsNames] )

auc <- roc(dummy.ds.test[,targ], testPred)
print(auc$auc)
confmat <- confusionMatrix(testPred, dummy.ds.test[ ,targ])

#=======================================
# predict one arbitrary value
ds.ts <- data[1,names(data) %in% c(preds,targ)]
dum.ds.ts <- dummyVars(fmla,data=ds.ts , fullRank=F)
dum.ds.ts1 <- data.frame(predict(dum.ds.ts, newdata = ds.ts, "Class"=data[1,targ]))
predict.Names <- names(dum.ds.ts1)[names(dum.ds.ts1) != targ]
testPred.instance <- predict(glmnetFit, dum.ds.ts1[ , predictorsNames] )
#=======================================


RocImp3 <- varImp(glmnetFit,scale=F)
RocImp3

plot(RocImp3)


results <- data.frame(row.names(RocImp3$importance),RocImp3$importance$Overall)
results$VariableName <- rownames(RocImp3)
colnames(results) <- c('VariableName','Class')
results <- results[order(results$Class),]
results <- results[(results$Class != 0),]

par(mar=c(5,15,4,2)) # increase y-axis margin. 
xx <- barplot(results$Class, width = 0.25, 
              main = paste("Variable Importance using GLM model"), horiz = T, 
              xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = TRUE, 
              col = ifelse((results$Class > 0), 'blue', 'red')) 
axis(2, at=xx, labels=results$VariableName, tick=FALSE, las=2, line=-0.3, cex.axis=0.6)  









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


                   