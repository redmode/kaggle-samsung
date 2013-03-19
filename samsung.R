##
## Clearing the workspace
##
rm(list=ls(all=TRUE))
gc(reset=TRUE)
set.seed(12345)

##
## Loading reqired packages & sources
##
require(caret)
source("utils.R")
source("cluster.R")

##
## Loading the data
##

# First-time loading
# data.train <- read.csv("data/train.csv")
# data.test  <- read.csv("data/test.csv")
# save(data.train, file="data/train.rda")
# save(data.test, file="data/test.rda")

# Fast loading
load("data/train.rda")
load("data/test.rda")

##
## Start cluster
##
start.cluster(mc=TRUE)

##
## Small subset (optional)
##
data.train <- subset(data.train, subject %in% c(1,2,3,4,30))

##
## Train data preparation
##
subjects <- unique(data.train$subject)
subjects.test <- sample(subjects, round(length(subjects)/5))
subjects.train <- subset(subjects, !(subjects %in% subjects.test))
train <- data.train$subject %in% subjects.train
X <- data.train[,1:561]
Y <- as.factor(data.train[,563])

## X[train,] + Y[train]   - training set
## X[!train,] + Y[!train] - validate set

##
## Test data preparation
##
TX <- data.test[,1:561]

##
## Feature selection and train controlling
##
#pp <- preProcess(X, method=c("center","scale"))
pp <- preProcess(X, method="pca", thresh=0.80)
X  <- predict(pp, X)
TX <- predict(pp, TX)
ind <- lapply(1:length(subjects.train), function(i) which(data.train[train,562]!=subjects.train[i]))
names(ind) <- sprintf("Fold%02d.Rep1", 1:length(subjects.train))
cvCtrl <- trainControl(method='cv', number=length(subjects.train), repeats=1,
                       returnResamp="none", returnData=FALSE, verboseIter=TRUE, index=ind)

##
## Training models on X[train]
##

clear.models()

#
parrfFit <- train(X[train,], Y[train], method='parRF', trControl=cvCtrl, tuneLength=5)
add.model(parrfFit)
#
mlpFit <- train(X[train,], Y[train], method='mlpWeightDecay', trControl=cvCtrl, tuneLength=7, trace=FALSE)
add.model(mlpFit)
#
glmnetFit <- train(X[train,], Y[train], method='glmnet', trControl=cvCtrl, tuneLength=5)
add.model(glmnetFit)
#
gbmFit <- train(X[train,], Y[train], method='gbm', trControl=cvCtrl,
                tuneGrid=expand.grid(.interaction.depth=(2:5)*2, .n.trees=100, .shrinkage=0.1))
add.model(gbmFit)
#
svmFit <- train(X[train,], Y[train], method="svmRadial", trControl=cvCtrl,
                tuneLength=5, scaled=FALSE)
# 
rfFit <- train(X[train,], Y[train], method="rf",
               trControl=cvCtrl, tuneLength=3, scaled=FALSE)
#
plsFit <- train(X[train,], Y[train], method="pls", trControl=cvCtrl,
                tuneLength=ncol(X),
                #tuneGrid=expand.grid(.ncomp=290:310),
                scaled=FALSE)
#
knnFit <- train(X[train,], Y[train], method="knn", trControl=cvCtrl, tuneLength=5)

##
## Stop cluster
##
stop.cluster()

##
## Testing models
##

# Majority vote
mv <- test.majority.vote(X[!train,], Y[!train])
mv
prune.models(mv)
test.majority.vote(X[!train,], Y[!train])

# Probability vote
