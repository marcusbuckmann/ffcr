
pruneLeaves <- function(model, depth = NULL, opposite_class = TRUE){
  if(is.null(depth))
    stop("Please specify depth!")

  mat <- model@tree$matrix
  cat <- model@tree$categorical

  if(nrow(mat)-1 <= depth)
    return(model)
  model@tree$matrix <- mat[1:depth,,drop = FALSE]
  model@tree$categorical <- cat[1:depth]
  model <- addLastLeaf(model)
  return(model)
}



pruneFit <- function(model, data.input, weights){
  #this method works for recursive trees only
  n.data <- nrow(data.input)
  modelMatrix <- model@tree$matrix
  n.splits <- nrow(modelMatrix)-1
  models <- list()
  #construct all models
  for(i in 1:n.splits){
    model.add <- model
    model.add@tree$matrix <- model.add@tree$matrix[1:i,,drop = F]
    model.add@tree$categorical <- model.add@tree$categorical[1:i]
    model.add <- addLastLeaf(model.add)
    models[[i]] <- model.add
  }
  performance <- t(sapply(models, function(x) (predict(x,data.input, weights = weights, type = "metric"))))
  model.ix <- which.max(performance[, "Accuracy"])
  model.out <- models[[model.ix]]
  return(model.out)
}


pruneCV <- function(model.input, data.input, costs = c(.5,.5), include_empty = TRUE){


  weights <- getWeightsFromCost(costs, getPrior(data.input))
  model.input <- pruneFit(model.input, data.input, weights = weights)
  n.objects <- nrow(data.input)

  if(n.objects < 3)
    return(model.input)
  if(nrow(model.input@tree$matrix) <= 2 & !include_empty)
    return(model.input)

  folds <- createFolds(n.objects)
  n.folds <- max(folds[,1])
  max.splits <- nrow(model.input@tree$matrix)-1

  if(include_empty)
    max.splits <- max.splits + 1

  cverror <- array(NA, dim = c(max.splits,n.folds))

  for(i in 1:n.folds){
    test.ix <- folds[folds[,1] == i,2]
    train.data <- data.input[-test.ix,]
    if(length(unique(train.data[,1]))==1)# both classes must be contained in training set
      next
    test.data <- data.input[test.ix,]
    max.tree <- fftree(train.data,
                       method = model.input@parameters$algorithm,
                       split_function = model.input@parameters$split_function,
                       weights = costs,
                       max_depth = model.input@parameters$max_depth,
                       use_features_once = model.input@parameters$use_features_once,
                       cross_entropy_parameters = model.input@parameters$cross_entropy_parameters,
                       cv = FALSE)

    model.list <- getFFTList(max.tree, include_empty = include_empty)
    n.splits <- sapply(model.list,function(x)x@parameters$n_nodes) + as.numeric(include_empty)
    # filter models that are bigger than inital models
    ix.out <- n.splits > max.splits
    n.splits <- n.splits[!ix.out]
    errors <- 1 - sapply(model.list[!ix.out], function(x) predict(x, test.data, weights = weights))
    cverror[n.splits,i] <- errors["Accuracy",]
  }
  meanCVerror <- apply(cverror,1,mean,na.rm = T)
  #only consider tree sizes that have been created at least three times in 10 folds
  ix3 <- apply(cverror,1,function(x)sum(!is.na(x))) < 3
  meanCVerror[ix3] <- 1

  best.ix <- which(meanCVerror == min(meanCVerror))
  best.ix <- (1:max.splits)[best.ix]
  if(length(best.ix) > 1){
    if(1 %in% best.ix & include_empty){
      best.ix <- best.ix[2]
    } else{
      best.ix <- best.ix[1]
    }
  }
  model.list <- getFFTList(model.input, include_empty = include_empty)
  model.output <- model.list[[best.ix]]

  # create empty model if pruning returns empty model or if both branches predict same class
  # only do this if we allow empty models
  if(nrow(model.output@tree$matrix)==2 &&
     round((model.output@tree$matrix[1,"exit"]) == round(model.output@tree$matrix[2,"exit"]))
  ) model.output@tree$matrix <- model.output@tree$matrix[-c(1:2),]
  return(model.output)
}


getAlphaVector <- function(model.input, data.input, weights, include_empty = include_empty){
  model.list <- getFFTList(model.input, include_empty = include_empty)
  model.list <- rev(model.list)
  fit.results <- sapply(model.list, function(x) predict(x, data.input, weights = weights))
  training.error <-  1 - fit.results["Accuracy",]
  n.splits <- nrow(model.input@tree$matrix) - 1
  tree.sizes <- n.splits:1
  if(include_empty)
    tree.sizes <- c(tree.sizes+1,1)

  alphas <- (training.error- training.error[1])/(tree.sizes-1)
  alphas[is.infinite(alphas)] <- 10^5
  alphas[is.nan(alphas)] <- 10^5
  return(alphas)
}

getFFTList <- function(model.input, include_empty = F){
  models <- list()
  n.splits <- nrow(model.input@tree$matrix) - 1
  for(i in 1:n.splits){
    model.add <- model.input
    model.add@tree$matrix <- model.add@tree$matrix[1:i, ,drop = F]
    model.add@tree$categorical <- model.add@tree$categorical[1:i]
    model.add@parameters$nSplits <- i
    model.add <- addLastLeaf(model.add)
    models[[i]] <- model.add
  }
  if(include_empty){
    empty <- models[[1]]
    empty@tree$matrix[,"exit"] <- sum(model.input@tree$matrix[1,c(">+", "<=+")])/sum(model.input@tree$matrix[1, c(">+",">-","<=+", "<=-")])
    empty@parameters$nSplits <- 0
    models <- c(empty,models)
  }
  return(models)
}

