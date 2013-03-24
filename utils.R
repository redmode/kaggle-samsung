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
models.env$saved.time <- NULL
models.env$times <- list()

clear.models <- function(){
  models.env$models <- list()
  models.env$times <- list()
}

train.model <- function(...){
  models.env$saved.time <- Sys.time()
  
  model <- train(...)

  ind <- 1 + length(models.env$models)
  models.env$models[[ind]] <- model
  models.env$times[[ind]] <- as.character.POSIXt(Sys.time()-models.env$saved.time)
  
  model
}

show.times <- function(){
  show <- as.data.frame(models.env$times)
  colnames(show) <- lapply(1:length(models.env$models), function(i) models.env$models[[i]]$method)
  show
}

majority.vote <- function(x){
  pred <- as.data.frame(predict(models.env$models, newdata=x, type="raw"))
  colnames(pred) <- lapply(1:length(models.env$models), function(i) models.env$models[[i]]$method)
  pred$majority.vote <- sapply(1:nrow(pred), function(i) mode(pred[i,])[1,1])
  pred
}

test.majority.vote <- function(x,y){
  pred <- majority.vote(x)
  res <- sapply(1:ncol(pred), function(i) caret::confusionMatrix(pred[,i], y)$overall[1])
  names(res) <- colnames(pred)
  
  ret <- list()
  ret$votes <- res
  class(ret) <- "votes"
  ret
}

prob.vote <- function(x){
  # predicting
  pred <- predict(models.env$models, newdata=x, type="prob")
  # normalizing
  pred <- lapply(pred, function(lst) t(apply(lst,1,function(z) z/sum(z))))
  # summing list
  pred <- Reduce("+", pred)/length(models.env$models)
  # voting
  prob.vote <- as.factor(sapply(1:nrow(pred), function(i) names(which(pred[i,]==max(pred[i,])))))
  # combining
  data.frame(majority.vote(x)[,1:length(models.env$models)], prob.vote)
}

test.prob.vote <- function(x,y){
  pred <- prob.vote(x)
  res <- sapply(1:ncol(pred), function(i) caret::confusionMatrix(pred[,i], y)$overall[1])
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
    models.env$times <- models.env$times[-scope]
  }  
}

postfilter <- function(x, threshold=1){
  x <- as.data.frame(x)
  x$corrected <- x$x
  
  ## Head and tail correction
  # x[1,2] <- x[2,2] <- x[3,1]
  # x[nrow(x),2] <- x[nrow(x)-1,2] <- x[nrow(x)-2,1]
  
  ## One-inside correction
  for(i in 3:(nrow(x)-2)){
    if(x[i,1]!=x[i-1,1] & x[i,1]!=x[i+1,1] & x[i-1,1]==x[i-2,1] & x[i+1,1]==x[i+2,1]){
      x[i,2] <- x[i+1,1]
    }
  }
  
  ## Two-inside correction
  if(threshold>1){
    for(i in 4:(nrow(x)-4)){
      if(x[i,2]==x[i+1,2] &
           x[i,2]!=x[i-1,2]   & x[i-1,2]==x[i-2,2] & x[i-2,2]==x[i-3,2] &
           x[i+1,2]!=x[i+2,2] & x[i+2,2]==x[i+3,2] & x[i+3,2]==x[i+4,2]){
        x[i,2] <- x[i+1,2] <- x[i+2,2]
      }
    }
  }
  
  ## Tree-inside correction
  if(threshold>2){
    for(i in 4:(nrow(x)-5)){
      if(x[i,2]==x[i+1,2] & x[i+1,2]==x[i+2,2] &
         x[i,2]!=x[i-1,2] &
         all.equal(list(x[i-1,2],x[i-2,2],x[i-3,2]),list(x[i+3,2],x[i+4,2],x[i+5,2]))[1]==TRUE){
        x[i:(i+2),2] <- x[(i+3):(i+5),2]
      }
    }
  }
  
  x$corrected
}

