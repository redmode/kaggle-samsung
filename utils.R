##
## Most frequent value in array
##
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

##
## Models' variables
##
models.env <- new.env()
models.env$models <- list()

clear.models <- function(){
  models.env$models <- list()
}

add.model <- function(model){
  ind <- 1 + length(models.env$models)
  models.env$models[[ind]] <- model
}

majority.vote <- function(x,y){
  pred <- as.data.frame(predict(models.env$models, newdata=x, type="raw"))
  colnames(pred) <- lapply(1:length(models.env$models), function(i) models.env$models[[i]]$method)
  pred$majority.vote <- sapply(1:nrow(pred), function(i) mode(pred[i,])[1,1])
  pred
}

test.majority.vote <- function(x,y){
  pred <- majority.vote(x,y)
  res<- sapply(1:ncol(pred), function(i) caret::confusionMatrix(pred[,i], y)$overall[1])
  names(res) <- colnames(pred)
  
  ret <- list()
  ret$votes <- res
  class(ret) <- "votes"
  ret
}

print.votes <- function(votes){
  print(votes$votes)
}

prune.models <- function(votes, threshold=0.80){
  stopifnot(class(votes)=="votes")
  
  scope <- c(which(votes$votes<threshold))
  if(length(scope)>0){
    models.env$models <- models.env$models[-scope]
  }  
}
