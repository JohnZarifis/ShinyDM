#Setup a binary classification problem
require(randomForest)
data(iris)
set.seed(1)
dat <- iris
dat$Species <- factor(ifelse(dat$Species=='virginica','virginica','other'))
trainrows <- runif(nrow(dat)) > 0.3
train <- dat[trainrows,]
test <- dat[!trainrows,]

#Build a decision tree
require(rpart)
library("rpart.plot")
library("partykit")

model.rpart.1 <- rpart(formula=Species~., data=train, method="class", model=T, parms = list(split = "gini"), 
      control = rpart.control(minsplit = round(nrow(train)*0.1), cp = 1e-3, xval = 20))

plot(as.party(model.rpart.1))

# prune the tree
opt <- which.min(model.rpart.1$cptable[,"xerror"])
cp <- model.rpart.1$cptable[opt, "CP"]
dec.Tree <- prune( model.rpart.1, cp = cp)



#------------------------ Random Forest
model.rf <- randomForest(Species~., train, ntree=25, proximity=TRUE, importance=TRUE, nodesize=5)

getTree(model.rf, k=1, labelVar=TRUE)

library(ggplot2)
pSpecies <- predict(model.rf,test,'vote')[,2]
plotData <- lapply(names(test[,1:4]), function(x){
  out <- data.frame(
    var = x,
    type = c(rep('Actual',nrow(test)),rep('Predicted',nrow(test))),
    value = c(test[,x],test[,x]),
    species = c(as.numeric(test$Species)-1,pSpecies)
  )
  out$value <- out$value-min(out$value) #Normalize to [0,1]
  out$value <- out$value/max(out$value)
  out
})
plotData <- do.call(rbind,plotData)
qplot(value, species, data=plotData, facets = type ~ var, geom='smooth', span = 0.5)

#--------------------------------
importance(model.rf)
importance(model.rf, type=1)
importance(model.rf, type=2)

plot(model.rf)
plot(margin(model.rf)) 
MDSplot(model.rf, iris$Species, k=5)
plot(outlier(model.rf), type="h", col=c("red", "green", "blue")[as.numeric(dat$Species)])
#--------------------------------








