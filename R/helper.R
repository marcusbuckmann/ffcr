sigmoid <- function(x){
  return(1/(1+ exp(-x)))
}

dichotomize_data <- function(data_input, splittingFunction = "gini", weights = c(.5, .5)){
  data.out <- data_input
  splits <- findSplits(data_input, splittingFunction = splittingFunction, weights = weights)
  split_matrix <- splits@splits$matrix
  for(m in 1:nrow(split_matrix)){
    cue.ix <- split_matrix[m,1]
    if(is.numeric(data_input[,cue.ix+1])){
      data.out[,cue.ix + 1] <- ifelse(data_input[,cue.ix + 1] > split_matrix[m,2], 1, 0)
    } else {
      data.out[,cue.ix + 1] <- ifelse(!data_input[,cue.ix + 1]%in%splits@splits$categorical[[m]], 1, 0)
    }
  }
  output <- list(data.out,split_matrix,splits@splits$categorical)
  names(output) <- c("data","split_matrix","categorical")
  return(output)

}


normalizeCue <- function (x) {
  minx <- min(x, na.rm=TRUE)
  maxx <- max(x, na.rm=TRUE)
  if (maxx-minx == 0) {
    x <- (x - minx)
  } else {
    x <- (x - minx) / (maxx - minx)
  }
  return (x)
}

`%=%` <- function(x,y){
  if(length(y)>1)
    stop("compare against single double such as 0.5")
  vapply(x,function(x)(isTRUE(all.equal(x,y))),logical(1))
}


hasNoVariance <- function(a){
  return(all(duplicated(a)[-1]))
}

getCriterion <- function(data_input,is.factor=F){
  criterion <- data_input[,1]
  if(is.factor)
    criterion <- as.factor(criterion)

  return(criterion)
}

getCues <- function(data_input){
  return(data_input[,-1,drop=F])
}

getPrior <- function(data_input) {
  if(is.numeric(data_input[,1])){
    prior <- sum(data_input[,1] == 1) / nrow(data_input)
  } else {

    class_labels <- as.character(sort(unique(data_input[,1])))
    criterion <- ifelse(as.character(data_input[,1]) == class_labels[2], 1,0)
    prior <- sum(criterion == 1) / nrow(data_input)
  }
  return(prior)
}

createFolds <- function(n,n_folds=10,...){
  if(n < n_folds)
    n_folds <- n
  object_allocation <- sample(n)
  fold_index <- rep(1:n_folds,length.out=n)
  out <- cbind(fold_index, object_allocation)
  out <- out[order(out[,2]),]
  return(out)
}



