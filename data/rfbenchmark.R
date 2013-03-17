### Example of submission code
require(randomForest)
### loading test and training set

### train.csv directly downloaded from kaggle
### test.csv directly downloaded from kaggle
train <- read.csv("train.csv")
test <- read.csv("test.csv")

### random forest
rfmodel <- randomForest(activity ~ . - subject, data = train)

### prediction
activpred <- predict(rfmodel, newdata = test)

### submit your prediction
write.table(x = activpred, file = "rf_submission.csv", row.names = FALSE, col.names = FALSE)




