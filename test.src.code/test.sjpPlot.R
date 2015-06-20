library(sjmisc)
library(sjPlot)
data(efc)
# retrieve predictor variable labels
labs <- get_var_labels(efc)
predlab <- c(labs[['c161sex']],
             paste0(labs[['e42dep']], " (slightly)"),
             paste0(labs[['e42dep']], " (moderate)"),
             paste0(labs[['e42dep']], " (severely)"),
             labs[['barthtot']],
             paste0(labs[['c172code']], " (mid)"),
             paste0(labs[['c172code']], " (high)"))
# create binary response
y <- ifelse(efc$neg_c_7 < median(na.omit(efc$neg_c_7)), 0, 1)
# create dummy variables for educational status
edu.mid <- ifelse(efc$c172code == 2, 1, 0)
edu.high <- ifelse(efc$c172code == 3, 1, 0)
# create data frame for fitted model
mydf <- data.frame(y = as.factor(y),
                   sex = as.factor(efc$c161sex),
                   dep = as.factor(efc$e42dep),
                   barthel = as.numeric(efc$barthtot),
                   edu.mid = as.factor(edu.mid),
                   edu.hi = as.factor(edu.high))
# fit model
fit <- glm(y ~., data = mydf, family = binomial(link = "logit"))
# plot odds
sjp.glm(fit,
        title = labs[['neg_c_7']],
        axisLabels.y = predlab)

# plot probability curves (predicted probabilities)
# of coefficients
sjp.glm(fit,
        title = labs[['neg_c_7']],
        axisLabels.y = predlab,
        type = "prob")


sjp.glm(fit,
        title = labs[['neg_c_7']],
        type = 'bars',
        axisLabels.y = predlab,
        transformTicks=TRUE, geom.size = .3,
        gridBreaksAt=0.2)

mydf.1 <- data.frame(sex = as.factor(efc$c161sex),
                     dep = as.factor(efc$e42dep),
                     barthel = as.numeric(efc$barthtot),
                     edu.mid = as.factor(edu.mid),
                     edu.hi = as.factor(edu.high), 
                     Y1 = as.numeric(efc$neg_c_7) )
fit.1 <- glm( Y1~., data = mydf.1,  family = gaussian )


sjp.glm(fit.1,
        title = labs[['neg_c_7']],
        type = 'bars',
        axisLabels.y = predlab,
        transformTicks=TRUE, geom.size = .3,
        gridBreaksAt=0.2)
