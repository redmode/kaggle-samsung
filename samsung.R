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
start.cluster(mc=F)

##
## Small subset (optional)
##
data.train <- subset(data.train, subject %in% c(1,2,3,4,30
                                                #,6,9,10,12,29
                                                ))

##
## Train data preparation
##
subjects <- unique(data.train$subject)
subjects.test <- sample(subjects, round(length(subjects)/5))
subjects.train <- subset(subjects, !(subjects %in% subjects.test))
train <- data.train$subject %in% subjects.train

X <- data.train[,1:561]
Y <- as.factor(data.train[,563])
TX <- data.test[,1:561]


##
## Pre-processing
##
pp <- preProcess(X, method=c("center","scale"))
#pp <- preProcess(X, method="pca", thresh=0.70)
X  <- predict(pp, X)
TX <- predict(pp, TX)


##
## Feature selection
##
subsets <- c(5,10,50,100,200,300)
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", number = 8, repeats = 3,
                   verbose = FALSE, returnResamp = "final")
profile <- rfe(X[train,], Y[train], sizes = subsets, rfeControl = ctrl)
X  <- X[,profile$optVariables]
TX <- TX[,profile$optVariables]

##
## Controlling cross-validation
##
ind <- lapply(1:length(subjects.train), function(i) which(data.train[train,562]!=subjects.train[i]))
names(ind) <- sprintf("Fold%02d.Rep1", 1:length(subjects.train))
cvCtrl <- trainControl(method='cv', number=length(subjects.train), repeats=1,
                       returnResamp="none", returnData=FALSE, verboseIter=TRUE, index=ind)

##
## Training models on X[train]
##

clear.models()

train.model(X[train,], Y[train], method='parRF', trControl=cvCtrl, tuneLength=5)
train.model(X[train,], Y[train], method='mlpWeightDecay', trControl=cvCtrl, tuneLength=7, trace=FALSE)
train.model(X[train,], Y[train], method='glmnet', trControl=cvCtrl, tuneLength=5)

# *** No probabilities ***
# train.model(X[train,], Y[train], method='gbm', trControl=cvCtrl,
#             tuneGrid=expand.grid(.interaction.depth=(2:5)*2, .n.trees=100, .shrinkage=0.1))

# *** Takes too long ***
# train.model(X[train,], Y[train], method="svmPoly", trControl=cvCtrl, tuneLength=5, scaled=FALSE)

train.model(X[train,], Y[train], method="rf", trControl=cvCtrl, tuneLength=5, scaled=FALSE)
train.model(X[train,], Y[train], method="pls", trControl=cvCtrl, tuneLength=ncol(X), scaled=FALSE)
train.model(X[train,], Y[train], method="knn", trControl=cvCtrl, tuneLength=10)
train.model(X[train,], Y[train], method="avNNet", trControl=cvCtrl, tuneLength=5)

# *** Swapping ***
# train.model(X[train,], Y[train], method="cforest", trControl=cvCtrl, tuneLength=3)

train.model(X[train,], Y[train], method="fda", trControl=cvCtrl, tuneLength=7)
train.model(X[train,], Y[train], method="lda2", trControl=cvCtrl, tuneLength=10)
train.model(X[train,], Y[train], method="multinom", trControl=cvCtrl, tuneLength=5, maxit=500)
train.model(X[train,], Y[train], method="pda", trControl=cvCtrl, tuneLength=10)

# xFit <- train(X[train,], Y[train], method="sda", trControl=cvCtrl, tuneLength=3)
# xFit

show.times()

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
pv <- test.prob.vote(X[!train,], Y[!train])
pv

prune.models(mv)

