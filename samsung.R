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
start.cluster(mc=T)

##
## Small subset (optional)
##
#data.train <- subset(data.train, subject %in% c(1,2,3,4,30
#                                                #,6,9,10,12,29
#                                                ))

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


dd <- ddply(data.train, activity~subject, nrow)
ag <- aggregate(V1~activity, data=dd, min)

sum(dd$V1)
sum(ag$V1)*length(unique(dd$subject))

##
## Pre-processing
##
pp <- preProcess(X, method=c("center","scale"))
X  <- predict(pp, X)
TX <- predict(pp, TX)

##
## Fast feature selection
##

##
## First time using randomForest with cross-validation
##

# ind <- lapply(1:length(subjects), function(i) which(data.train[,562]!=subjects[i]))
# names(ind) <- sprintf("Fold%02d.Rep1", 1:length(subjects))
# cvCtrl <- trainControl(method='cv', number=length(subjects), repeats=1,
#                        returnResamp="none", returnData=FALSE, verboseIter=TRUE, index=ind)
# 
# fs <- train.model(X, Y, method="rf", trControl=cvCtrl,
#                   tuneGrid=expand.grid(.mtry=c(18,20,22)),
#                   scaled=FALSE, importance=TRUE, ntree=1001)
# 
# sorted.names <- names(sort(fs$finalModel$importance[,"MeanDecreaseAccuracy"], decreasing=T))

# save(fs, file="data/featuresModel.rda")
# save(sorted.names, file="data/sortedNames.rda")

# load("data/featuresModel.rda")
load("data/sortedNames.rda")


##
## Feature selection (too slow)
##

# subsets <- c(50,100,150,200,250,300,400)
# subsets <- c(100)
# ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", number = 5, repeats = 3,
#                    verbose = TRUE, returnResamp = "final")
# profile <- rfe(X[train,], Y[train], sizes = subsets, rfeControl = ctrl)
# X  <- X[,profile$optVariables]
# TX <- TX[,profile$optVariables]

#var50 <- profile$variables[profile$variables$Variables==50,c("var","Resample")]
#require(reshape2)
#head(dcast(var50, Resample~var, length))


##
## Post-filtering
##

# require(randomForest)
# activepred1 <- predict(fs$finalModel, newdata = TX)
# write.table(x = activepred1, file = "fs_submission1.csv", row.names = FALSE, col.names = FALSE)
# activepred2 <- postfilter(activepred1)
# write.table(x = activepred2, file = "fs_submission2.csv", row.names = FALSE, col.names = FALSE)
# activepred3 <- postfilter(activepred1,2)
# write.table(x = activepred3, file = "fs_submission3.csv", row.names = FALSE, col.names = FALSE)



##
## Pre-processing
##

first100 <- sorted.names[1:100]
other <- sorted.names[-(1:100)]

pp <- preProcess(X[,other], method="pca", thresh=0.80)
X <- cbind(X[,first100], predict(pp, X[,other]))
TX <- cbind(TX[,first100], predict(pp, TX[,other]))



##
## Controlling cross-validation
##
ind <- lapply(1:length(subjects.train), function(i) which(data.train[train,562]!=subjects.train[i]))
names(ind) <- sprintf("Fold%02d.Rep1", 1:length(subjects.train))
cvCtrl <- trainControl(method='cv', number=length(subjects.train), repeats=1,
                       returnResamp="none", returnData=FALSE, verboseIter=FALSE, index=ind)

##
## Training models on X[train]
##

clear.models()
#train <- data.train$subject

train.model(X[train,], Y[train], method='parRF', trControl=cvCtrl, tuneLength=5, ntree=1001, scaled=FALSE)
train.model(X[train,], Y[train], method='mlpWeightDecay', trControl=cvCtrl, tuneLength=7, trace=FALSE)
train.model(X[train,], Y[train], method='glmnet', trControl=cvCtrl, tuneLength=5)

# *** No probabilities ***
# train.model(X[train,], Y[train], method='gbm', trControl=cvCtrl,
#             tuneGrid=expand.grid(.interaction.depth=(2:5)*2, .n.trees=100, .shrinkage=0.1))

# *** Takes too long ***
# train.model(X[train,], Y[train], method="svmPoly", trControl=cvCtrl, tuneLength=5, scaled=FALSE)

train.model(X[train,], Y[train], method="rf", trControl=cvCtrl, tuneLength=5, ntree=1001, scaled=FALSE)
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

##
## Stop cluster
##
stop.cluster()

##
## Testing models
##

##
## Save results
##

# models <- models.env$models
# save(models, file="models100_pca08.rda")
# times <- models.env$times
# save(times, file="times100_pca08.rda")

load("models100_pca08.rda")
load("times100_pca08.rda")
models.env$models <- models
models.env$times <- times

show.times()

# Majority vote
mv <- test.majority.vote(X[!train,], Y[!train])
mv
pv <- test.prob.vote(X[!train,], Y[!train])
pv

prune.models(mv,threshold=0.92)


prediction <- majority.vote(X[!train,])$majority.vote
res <- data.frame(y=Y[!train], prediction=prediction)

caret::confusionMatrix(res$y,res$prediction)$overall[1]
caret::confusionMatrix(res$y,postfilter(res$prediction,3))$overall[1]

res$prediction <- postfilter(prediction,3)


##
## 0.93xxxx
##
# subm4 <- majority.vote(TX)$majority.vote
# subm4 <- postfilter(subm4,3)
# subm4 <- as.data.frame(subm4)
# write.table(x = subm4, file = "fs100_pc08_submission4.csv", row.names = FALSE, col.names = FALSE)

start <- 1
N <- 600
x<-as.numeric(prediction[start:(start+N)])
y<-as.numeric((Y[!train])[start:(start+N)])

ts <- rep(0,length(x))
width<-5
for(i in width:(N-width)){
  m1 <- mean(x[(i-width+1):i])
  s1 <- sd(x[(i-width+1):i])
  m2 <- mean(x[(i+1):(i+width)])
  s2 <- sd(x[(i+1):(i+width)])
  ts[i] <- (m1-m2)/sqrt((s1^2 + s2^2)/width)
}

ts<-abs(ts)
ts[ts<width | is.nan(ts)] <- 0
ts[ts==Inf | ts==-Inf | ts>=width ] <- 10

# par(mfrow=c(1,1))
plot(ts, type="h")
points(x, col="red")
points(y, col="blue")

