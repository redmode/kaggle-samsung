#
# Clearing the workspace
#
rm(list = ls(all = TRUE))
gc(reset=TRUE)
set.seed(12345)

#
# Loading the data
#
train <- read.csv("data/train.csv")
test  <- read.csv("data/test.csv")

#
# Loading reqired packages
#
require(caret)

#
# Data preparation
#
set.seed(1)

## Splitting into training and testing sets
subjects.test  <- c(23,25,26,27,28,29,30)
test  <- subset(samsungData, samsungData$subject %in% subjects.test)
train <- subset(samsungData, !samsungData$subject %in% subjects.test)

## Excluding collinear variables
N <- ncol(train)
# ex1 <- nearZeroVar(train[,-c(N-1,N)])
# ex2 <- findCorrelation(cor(train[,-c(N-1,N)]), 0.90)
# ex <- c(ex1, ex2)
# 
# train <- train[,-ex]
# test  <- test[,-ex]
# N <- ncol(train)

#
# Feature selection (PCA)
#
xTrans <- preProcess(train[,-c(N-1,N)], method="pca", thresh=0.99)
train <- cbind(predict(xTrans, train[,-c(N-1,N)]), activity=train[,c(N)])
test  <- cbind(predict(xTrans, test[,-c(N-1,N)]),  activity=test[,c(N)])
N <- ncol(train)

#
# Building models
#

cvControl <- trainControl(method="cv", number=10)

svmFit <- train(train[,-c(N)],
                train[,N],
                method="svmRadial",
                tuneLength=3,
                trControl=cvControl,
                scaled=FALSE)

rfFit <- train(train[,-c(N)],
               train[,N],
               method="rf",
               tuneLength=5,
               trControl=cvControl,
               scaled=FALSE)

plsFit <- train(train[,-c(N)],
                train[,N],
                method="pls",
                tuneLength=N,
                trControl=cvControl,
                scaled=FALSE)

knnFit <- train(train[,-c(N)],
                train[,N],
                method="knn",
                tuneLength=5,
                trControl=cvControl)

gbmFit <- train(train[,-c(N)],
                train[,N],
                method="gbm",
                tuneGrid=expand.grid(.interaction.depth=(2:5)*2, .n.trees=100, .shrinkage=0.1),
                trControl=cvControl,
                verbose=TRUE)

models <- list(svm=svmFit, rf=rfFit, pls=plsFit, knn=knnFit, gbm=gbmFit)

# pred <- predict(models, newdata=test[,-c(N)], type="prob")
# probs <- extractProb(models, testX=test[,-c(N)], testY=test[,c(N)])
# test.probs <- subset(probs, dataType=="Test")
# plotClassProbs(test.probs)

##
## Single model prediction
##
pred.rf <- predict(rfFit$finalModel, newdata=test[,-c(N)], type="response")
confusionMatrix(pred.rf, test$activity)
##
##


pred <- predict(models, newdata=test[,-c(N)], type="raw")
preds <- extractPrediction(models, testX=test[,-c(N)], testY=test[,c(N)])
test.preds <- subset(preds, dataType=="Test")

# confusionMatrix(test.preds$pred, test.preds$obs)

M <- 5
K <- (nrow(test.preds)/M)
test.preds$id <- rep(1:K, M)
# head(test.preds[,c(2,5,6)],10)

rs <- reshape(test.preds[,c(2,5,6)], v.names="pred", idvar="id", timevar="object", direction="wide")
# head(rs)
# nrow(rs)

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

vote.pred <- sapply(1:K, function(i) mode(rs[i,-c(1)])[1,1])
# length(vote.pred)

cm <- confusionMatrix(vote.pred, test$activity)
cm$overall[1]

require(xtable)
xtable(cm$byClass)

confusionMatrix(test.preds$pred[test.preds$object=="svm"], test$activity)$overall[1]
confusionMatrix(test.preds$pred[test.preds$object=="rf"], test$activity)$overall[1]
confusionMatrix(test.preds$pred[test.preds$object=="pls"], test$activity)$overall[1]
confusionMatrix(test.preds$pred[test.preds$object=="knn"], test$activity)$overall[1]
confusionMatrix(test.preds$pred[test.preds$object=="gbm"], test$activity)$overall[1]

