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
#require(doMC)
#registerDoMC(2)
require(doSNOW)


#
# Utility function
#
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#
# Loading the data
#
data.train <- read.csv("data/train.csv")
data.test  <- read.csv("data/test.csv")

#
# Start cluster
#
#cl <- makeCluster(c("localhost","inform-3"), type="SOCK")
#registerDoSNOW(cl)


inform3 <-
  list(host="192.168.130.169",
       rscript="C:/Program Files/R/R-2.15.3/bin/Rscript.exe",
       snowlib="C:/Program Files/R/R-2.15.3/library")

nodes <- c(lapply(1:2, function(i) inform3), rep("localhost",2))

cl <- makeCluster(nodes, type = "SOCK", manual=TRUE)
registerDoSNOW(cl)


#
# Small subset
#
data.train <- subset(data.train, subject %in% c(1,3,6))

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
#pp <- preProcess(X, method=c("center","scale"))
pp <- preProcess(X, method="pca", thresh=0.90)
X  <- predict(pp, X)
TX <- predict(pp, TX)
cvCtrl <- trainControl(method='cv', number=10, returnResamp="none", returnData=FALSE, verboseIter=TRUE)

#
# Training models on X[train] + Validating on X[!train] set
#

#
parrfFit <- train(X[train,], Y[train], method='parRF', trControl=cvCtrl, tuneLength=5)
#
mlpFit <- train(X[train,], Y[train], method='mlpWeightDecay', trControl=cvCtrl, tuneLength=5, trace=FALSE)
#
glmnetFit <- train(X[train,], Y[train], method='glmnet', trControl=cvCtrl, tuneLength=5)
#
gbmFit <- train(X[train,], Y[train], method='gbm', trControl=cvCtrl,
                tuneGrid=expand.grid(.interaction.depth=(2:5)*2, .n.trees=100, .shrinkage=0.1))
#
svmFit <- train(X[train,], Y[train], method="svmRadial", trControl=cvCtrl,
                tuneLength=5, scaled=FALSE)
# 
rfFit <- train(X[train,], Y[train], method="rf",
               trControl=cvCtrl, tuneLength=5, scaled=FALSE)
#
plsFit <- train(X[train,], Y[train], method="pls", trControl=cvCtrl,
                tuneLength=ncol(X),
                #tuneGrid=expand.grid(.ncomp=290:310),
                scaled=FALSE)
#
knnFit <- train(X[train,], Y[train], method="knn", trControl=cvCtrl, tuneLength=5)

#
# Stop cluster
#
stopCluster(cl)

#
# Combining models
#
models <- list(gbmFit, svmFit, rfFit, parrfFit, plsFit, knnFit, mlpFit, glmnetFit)

pred <- as.data.frame(predict(models, newdata=X[!train,], type="raw"))
colnames(pred) <- lapply(1:length(models), function(i) models[[i]]$method)
pred$vote <- sapply(1:nrow(pred), function(i) mode(pred[i,])[1,1])

res <- sapply(1:ncol(pred), function(i) caret::confusionMatrix(pred[,i], Y[!train])$overall[1])
names(res) <- colnames(pred)
res

##
## After prune
##
models <- models[-c(which(res<0.8))]

pred <- as.data.frame(predict(models, newdata=X[!train,], type="raw"))
colnames(pred) <- lapply(1:length(models), function(i) models[[i]]$method)
pred$vote <- sapply(1:nrow(pred), function(i) mode(pred[i,])[1,1])

res <- sapply(1:ncol(pred), function(i) caret::confusionMatrix(pred[,i], Y[!train])$overall[1])
names(res) <- colnames(pred)
res

