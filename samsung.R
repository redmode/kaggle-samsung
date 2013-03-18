#
# Clearing the workspace
#
rm(list=ls(all=TRUE))
gc(reset=TRUE)
set.seed(12345)

#
# Loading reqired packages
#
require(caret)
require(doMC)
registerDoMC(2)

#
# Loading the data
#
data.train <- read.csv("data/train.csv")
data.test  <- read.csv("data/test.csv")

# data.train <- subset(data.train, subject %in% c(1,3,6))

#
# Train data preparation
#
subjects <- unique(data.train$subject)
subjects.validate <- sample(subjects, round(length(subjects)/5))
train <- !(data.train$subject %in% subjects.validate)
X <- data.train[,1:561]
Y <- as.factor(data.train[,563])

## X[train,] + Y[train]   - training set
## X[!train,] + Y[!train] - validate set

#
# Test data preparation
#
TX <- data.test[,1:561]

#
# Pre-processing and train controlling
#
pp <- preProcess(X, method=c("center","scale"))
X <- predict(pp, X)
cvCtrl <- trainControl(method='cv', number=10)

#
# Training models on X[train] + Validating on X[!train] set
#


model3 <- train(X[train,], Y[train], method='parRF', trControl=cvCtrl)


model4 <- train(X[train,], Y[train], method='mlpWeightDecay', trControl=myControl, trace=FALSE, preProcess=PP)
model5 <- train(X[train,], Y[train], method='ppr', trControl=myControl, preProcess=PP)
model6 <- train(X[train,], Y[train], method='earth', trControl=myControl, preProcess=PP)
model7 <- train(X[train,], Y[train], method='glm', trControl=myControl, preProcess=PP)

model9 <- train(X[train,], Y[train], method='gam', trControl=myControl, preProcess=PP)
model10 <- train(X[train,], Y[train], method='glmnet', trControl=myControl, preProcess=PP)

#
gbmFit <- train(X[train,], Y[train], method='gbm', 
                trControl=cvCtrl, preProcess=cvProc,
                tuneGrid=expand.grid(.interaction.depth=(2:5)*2, .n.trees=100, .shrinkage=0.1))
confusionMatrix(predict(gdmFit$finalModel, newdata=X[!train,], type="response"), Y[!train])$overall[1]
#
svmFit <- train(X[train,], Y[train], method="svmRadial",
                trControl=cvCtrl, preProcess=cvProc,
                tuneLength=3, scaled=FALSE)
confusionMatrix(predict(svmFit$finalModel, newdata=X[!train,], type="class"), Y[!train])$overall[1]
# 
rfFit <- train(X[train,], Y[train], method="rf",
               trControl=cvCtrl, tuneLength=3, scaled=FALSE)
confusionMatrix(predict(rfFit$finalModel, newdata=X[!train,], type="response"), Y[!train])$overall[1]
#
plsFit <- train(X[train,], Y[train], method="pls",
                trControl=cvCtrl, preProcess=cvProc,
                #tuneLength=ncol(X),
                tuneGrid=expand.grid(.ncomp=290:310),
                scaled=FALSE)
confusionMatrix(predict(plsFit$finalModel, newdata=X[!train,], type="class"), Y[!train])$overall[1]
#
knnFit <- train(X[train,], Y[train], method="knn",
                trControl=cvCtrl, preProcess=cvProc,
                tuneLength=5)
pr <- predict(knnFit$finalModel, newdata=X[!train,], type="class")
confusionMatrix(pr, Y[!train])$overall[1]


#
# Combining models
#
models <- list(gbmFit, svmFit, rfFit, plsFit, knnFit)


# pred <- predict(models, newdata=test[,-c(N)], type="raw")
preds <- extractPrediction(models, testX=test[,-c(N)], testY=test[,c(N)])
test.preds <- subset(preds, dataType=="Test")

# confusionMatrix(test.preds$pred, test.preds$obs)

M <- length(models)
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





