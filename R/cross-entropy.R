
#' Hyperparmaeters for the cross-entropy models
#'
#'\code{cross_entropy_control} Returns a list of parameters that control the cross-entropy optimization procedure. The output of this function is be passed when training a fast-and-frugal tree or tallying model using the cross-entropy method (see examples).
#'@param starts Number of fast-and-frugal trees or tallying models generated with different seeds. The best-fitting model will be returned. The default is 10.
#'@param learning_rate Learning rate used in the optimization procedure. The higher the value, the quicker the model will converge. However, at higher learning rates, the model is more likely to converge to less accurate fast-and-frugal trees and tallying models. The default value is 0.1.
#'@param maximum_time Maximum training time (in seconds) to obtain the final model.
#'@param iterations The maximum number of iterations. The default is 500.
#'@param early_stopping Training stops early if the performance does not improve after this many iterations in a row. The default is 25.
#'@param thresholds The maximum number of numeric thresholds that are tested for a numeric feature. The default is 100.
#'@param split_percentiles When TRUE, the candidate thresholds at which features can be split are the percentiles of the features' distribution. When FALSE the candidate values are obtained by dividing a feature's values in equidistant bins. The default value is \code{FALSE}.
#'@param samples Number of models created in each iteration. The default is 100.
#'@param elite_samples The number of best-performing models that are used to update the parameter distribution. The default is 10.
#'@param threads Number of CPU cores used. The default is the number of available cores minus 1.

#'@examples
#'\dontrun{
#' data(liver)
#' liver$sex <- ifelse(liver$sex == "Female", 1,0) # Recoding categorical feature because the cross-entropy method only works with numeric features.
#' model_tree <- fftree(data = liver, formula = diagnosis~., method = "cross-entropy", cross_entropy_parameters = cross_entropy_control(starts = 5))
#' model_tally <- tally(data = liver, formula = diagnosis~., method = "cross-entropy", cross_entropy_parameters = cross_entropy_control(starts = 5))
#' }


#'@export
cross_entropy_control <- function(
  starts = 10,
  learning_rate = 0.05,
  maximum_time = 3600,
  iterations = 500,
  early_stopping = 25,
  thresholds = 100,
  split_percentiles = FALSE,
  samples = 100,
  elite_samples = 5,
  threads = parallel::detectCores() - 1

){
  if(elite_samples >= samples)
    stop("The number of elite_samples should be substantially smaller than the samples.")
  if(learning_rate >= 1 | learning_rate <= 0)
    stop("The learning must be greater than 0 and smaller than 1.")

  output <- list(
    starts = starts,
    learning_rate = learning_rate,
    maximum_time = maximum_time,
    iterations = iterations,
    early_stopping = early_stopping,
    thresholds = thresholds,
    split_percentiles = split_percentiles,
    samples = samples,
    elite_samples = elite_samples,
    threads = threads,
    verbose = FALSE # TODO for next version. https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
  )
  return(output)
}


buildExp <- function(pars, cols, failure.directions) {
  n <- length(pars)
  stopifnot((length(cols) == n) & (length(failure.directions == n)))
  rslt <- paste(cols[1], failure.directions[1], pars[1], sep=" ")

  if (n > 1)
    for (i in 2:n)
      rslt <- paste(rslt, "|", cols[i], failure.directions[i], pars[i], sep=" " )

  return (rslt)
}

tally_cross_entropy <- function(data_input,
                                maximum_size = 6,
                                samples = 100,
                                thresholds = 100,
                                elite_samples = 5,
                                iterations = 100,
                                costs = c(.5,.5),
                                learning_rate = 0.1,
                                maximum_time = 3600,
                                early_stopping = 25,
                                split_percentiles = F,
                                verbose = F,
                                inrep = NULL){
  if(verbose & !is.null(inrep)){
    cat("#################### \n")
    cat(paste0(inrep ,". tree \n" ))
    cat("#################### \n")
  }

  data_input_not_rescaled <- data_input
  boot = F
  minx <- apply(data_input[,-1, drop = F],2,min)
  maxx <- apply(data_input[,-1, drop = F],2,max)
  data_input[,-1] <- sapply(data_input[,-1], normalizeCue)
  nc <- ncol(data_input)-1
  cues <- data_input[,-1]

  no <- nrow(data_input)
  if(split_percentiles){
    cutValues <- lapply(1:ncol(cues), function(x)unique(quantile(cues[,x],c(0:thresholds)/thresholds)))
  } else {
    cutValues <- lapply(1:ncol(cues), function(x)c(0:thresholds)/thresholds)
  }

  indexToNode <- function(ix){
    cue.ix <- floor((ix-1)/(thresholds*2))+1
    temp <- ix - ((cue.ix-1)*thresholds*2); value <- ceiling(temp/2); side <- temp%%2
    cue.ix <- c(cue.ix)
    value <- c(value)
    side <- c(side)
    out <- list(cue.ix,value,side); names(out) <- c("cue.ix","value","side")
    return(out)
  }
  modPerf <- function(x){
    # x = index parameters
    modSpecs <- indexToNode(x)
    vals <- modSpecs$value
    cix <- modSpecs$cue.ix
    sideix <- modSpecs$side
    vals <- sapply(1:length(cix),function(x)cutValues[[cix[x]]][vals[x]])

    ixdup <- duplicated(cix) # duplicated cues are not allowed

    vals[ixdup] <- 1
    sideix[ixdup] <- 1

    nonempty <- 0
    pred <- rep(0,nrow(cues))
    for(i in 1:length(x)){
      if(sideix[i] == 1){
        adder <- ifelse(cues[,cix[i]]>vals[i],1,0)
        pred <- pred + adder
        nonempty <- nonempty + any(adder == 1)*1

      } else {
        adder <- ifelse(cues[,cix[i]]<=vals[i],1,0)
        pred <- pred + adder
        nonempty <- nonempty + any(adder == 1)*1
      }
    }

    predUni <- sort(unique(pred))
    ii <- sapply(predUni, function(x) performanceAccuracy(data_input[,1],pred, threshold = x, weights = weights,random = F))
    intercept <- predUni[which.max(ii)]
    pred <- pred - intercept
    return(c(performanceAccuracy(data_input[,1],pred, weights = weights,random = F,threshold = 0),length(x)-nonempty)) # regularizer included
  }

  weights <- getWeightsFromCost(costs, getPrior(data_input))
  outV <- rep(NA, iterations)

  allBest <- array(NA, dim = c(maximum_size,iterations))
  vv <- lapply(1:ncol(cues),function(x)table(cut(cues[,x],cutValues[[x]],include.lowest = T)))
  vv <- sapply(vv,function(x)c(x,rep(0,thresholds-length(x))))
  vv <- as.vector(vv)
  vv <- rep(vv, each = 2)


  distr <- array(0, dim = c(maximum_size,nc*thresholds*2))   # cue1, value1, side1 .... cue1, value1, side2 .... cue1, value2, side1 .... cue2, value1, side1
  ix <- vv>0

  value <- 1/sum(ix)
  for(d in 1:maximum_size)
    distr[d,ix] <- value
  no <- nrow(data_input); ncol <- nc*thresholds*2

  timeStart <- proc.time()
  for(i in 1:iterations){
    population <- replicate(samples,sapply(1:maximum_size, function(x) sample(1:ncol,size = 1,prob = distr[x,])))

    modperfs <- apply(population,2,function(x) modPerf(x))
    index_best <- order(modperfs[1,], modperfs[2,], decreasing = T)[1:elite_samples]
    bestTrees <- population[,index_best]
    distrNew <- distr*0
    for(treeIx in 1:elite_samples){
      for(node in 1:maximum_size){
        distrNew[node,bestTrees[node,treeIx]] = distrNew[node,bestTrees[node,treeIx]] + 1
      }
    }

    allBest[,i] <- bestTrees[,1]

    distr = learning_rate * distrNew/(elite_samples) + (1-learning_rate) * distr
    if(verbose)
      print(paste0("Iteration: ", i, "Perf: ", max(modperfs[1,])))
    outV[i] <- max(modperfs[1,])

    if(i > early_stopping + 1){
      if(length(unique(outV[(i-early_stopping):i]))==1)
        break
      if((proc.time() - timeStart)[3]> maximum_time)
        break
    }
  }
  index_best <- which(outV ==max(outV, na.rm = T))
  if(length(index_best) > 1){
    index_best <- sort(index_best,decreasing = T)[1]
  }

  finalMod <- allBest[,index_best]
  modSpecs <- indexToNode(finalMod)

  values <- modSpecs$value
  cix <- modSpecs$cue.ix
  sideix <- modSpecs$side
  values <- sapply(1:length(values),function(x)cutValues[[cix[x]]][values[x]])

  ixdup <- duplicated(cix) # duplicated cues are not allowed
  values <- values[!ixdup]
  sideix <- sideix[!ixdup]
  cix <- cix[!ixdup]

  out <- list()
  values <- values*(maxx[cix]-minx[cix]) + minx[cix]
  split_points <- values
  cueix <- cix
  dirix <- sideix
  values <- smoothThresholds(data_input_not_rescaled, split_points,cueix, dirix)

  # create a split object
  split_points <- apply(data_input_not_rescaled[,-1, drop = F], 2, mean)
  split_points[names(values)] <- values

  unit_weights <- rep(0, nc)
  names(unit_weights) <- colnames(data_input_not_rescaled)[-1]
  unit_weights[names(values)] <- dirix * 2 -1

  splitted_cues <- sapply(names(split_points), function(x) 1 *(data_input_not_rescaled[, x] > split_points[x]))
  data_splitted <- data.frame(data_input_not_rescaled[,1], splitted_cues)
  colnames(data_splitted)[1] <- colnames(data_input_not_rescaled)[1]
  rm(splitted_cues)

  pred <- (unit_weights %*% t(data_splitted[, -1, drop = FALSE]) )[1,]
  pred_unique <- unique(pred)
  pred_unique <- sort(pred_unique)
  intercept_index <- sapply(pred_unique, function(x) performanceAccuracy(data_splitted[,1], pred, threshold = x, weights = weights, random = FALSE))
  intercept <- pred_unique[which.max(intercept_index)]

  split_object <- findSplits(data_splitted)
  rm(data_splitted)


  # new
  tally <- list()
  # adjust intercept because tallying object internally uses (-1, 1) weights and not only positive weights.
  tally$intercept <- -intercept
  tally$weights <- unit_weights
  tally$matrix <- split_object@splits$matrix
  tally$matrix[, "splitPoint"] <- split_points
  tally$categorical <- split_object@splits$categorical

  model <- new("tallyModel", tally = tally)
  model@parameters$algorithm = "cross-entropy"
  model@formula <- stats::formula(data_input_not_rescaled)
  model@class_labels <- as.character(sort(unique(data_input_not_rescaled[,1])))
  return(model)
}

fft_cross_entropy_multiple_starts <- function(data_input,
                                              starts = 10,
                                              maximum_size = 6,
                                              samples = 100,
                                              thresholds = 100,
                                              elite_samples = 10,
                                              iterations = 1000,
                                              costs = c(.5,.5),
                                              learning_rate = 0.05,
                                              maximum_time = 3600,
                                              early_stopping = 10,
                                              threads = 4,
                                              split_percentiles = F,
                                              multiple_splits = TRUE,
                                              verbose = F,
                                              wFrugal = 0,
                                              ...){

  maximum_time <- maximum_time / starts * threads
  `%dopar%` <- foreach::`%dopar%`
  doParallel::registerDoParallel(threads)
  mods <- foreach::foreach(k = 1:starts) %dopar% fft_cross_entropy(
    data_input,
    maximum_size = maximum_size,
    samples = samples,
    thresholds = thresholds,
    elite_samples = elite_samples,
    iterations = iterations,
    costs = costs,
    learning_rate = learning_rate,
    maximum_time = maximum_time,
    early_stopping = early_stopping,
    split_percentiles= split_percentiles,
    multiple_splits = multiple_splits,
    wFrugal = wFrugal,
    inrep = k,
    verbose = verbose)

  doParallel::stopImplicitCluster()



  mod_maximum_size <- sapply(mods, function(x) nrow(x@tree$matrix)-1)
  predictions <- lapply(mods, function(x) as.numeric(as.character(predict(x,data_input,"response"))))
  weights <- getWeightsFromCost(costs,getPrior(data_input))
  model_predictions <- sapply(1:length(mods), function(x)  computePerformance(data_input[,1], predictions[[x]], weights = weights)["Accuracy"]- 0.0001 *(mod_maximum_size[x]) / maximum_size)
  return(mods[[which.max(model_predictions)]])
}

tally_cross_entropy_multiple_starts <- function(
  data_input,
  starts = 10,
  maximum_size = 6,
  samples = 100,
  thresholds = 100,
  elite_samples = 10,
  iterations = 1000,
  costs = c(.5,.5),
  learning_rate = 0.05,
  maximum_time = 3600,
  early_stopping = 10,
  threads = 4,
  split_percentiles = F,
  verbose = F,
  ...){



  maximum_time <- maximum_time / starts * threads


  `%dopar%` <- foreach::`%dopar%`
  doParallel::registerDoParallel(threads)
  mods <- foreach::foreach(k = 1:starts) %dopar% tally_cross_entropy(
    data_input,
    maximum_size = maximum_size,
    samples = samples,
    thresholds = thresholds,
    elite_samples = elite_samples,
    iterations = iterations,
    costs = costs,
    learning_rate = learning_rate,
    maximum_time = maximum_time,
    early_stopping = early_stopping,
    split_percentiles = split_percentiles,
    inrep = k,
    verbose = verbose)
  doParallel::stopImplicitCluster()




  mod_maximum_size <- sapply(mods, function(x) sum(x@tally$weights != 0))
  predictions <- lapply(mods, function(x) as.numeric(as.character(predict(x,data_input,"response"))))
  weights <- getWeightsFromCost(costs, getPrior(data_input))
  models_performance <- sapply(1:length(mods), function(x) computePerformance(data_input[,1], predictions[[x]], weights = weights)["Accuracy"] - 0.0001 * (mod_maximum_size[x])/maximum_size)
  return(mods[[which.max(models_performance)]])
}

fft_cross_entropy <- function(data_input,
                              maximum_size = 6,
                              samples = 100,
                              thresholds = 100,
                              elite_samples = 10,
                              iterations = 1000,
                              costs = c(.5,.5),
                              learning_rate = 0.05,
                              maximum_time = 3600,
                              early_stopping = 25,
                              verbose = F,
                              split_percentiles = F,
                              multiple_splits = F,
                              wFrugal = 0,
                              inrep = NULL){


  if(verbose & !is.null(inrep)){
    cat("#################### \n")
    cat(paste0(inrep ,". tree \n" ))
    cat("#################### \n")
  }
  weights <- getWeightsFromCost(costs, getPrior(data_input))
  data_input_not_rescaled <- data_input
  minx <- apply(data_input[,-1, drop = F],2,min)
  maxx <- apply(data_input[,-1, drop = F],2,max)
  data_input[,-1] <- sapply(data_input[,-1], normalizeCue)
  nc <- ncol(data_input)-1; cues <- data_input[,-1]

  no <- nrow(data_input)
  if(split_percentiles){
    cutValues <- lapply(1:ncol(cues), function(x)unique(quantile(cues[,x],c(0:thresholds)/thresholds)))
  } else {
    cutValues <- lapply(1:ncol(cues), function(x)c(0:thresholds)/thresholds)
  }

  indexToNode <- function(ix){
    cue.ix <- floor((ix-1)/(thresholds*2))+1
    temp <- ix - ((cue.ix-1)*thresholds*2); value <- ceiling(temp/2); side <- temp%%2
    cue.ix <- c(cue.ix,tail(cue.ix,1))
    value <- c(value,tail(value,1))
    side <- c(side,1-tail(side,1))
    out <- list(cue.ix,value,side); names(out) <- c("cue.ix","value","side")
    return(out)
  }
  treePerf <- function(x){

    treespecs <- indexToNode(x)
    tree1@tree$matrix[,"Cue"] <- treespecs$cue.ix
    tree1@tree$matrix[,"splitPoint"] <- sapply(1:length(treespecs$cue.ix),function(x)cutValues[[treespecs$cue.ix[x]]][treespecs$value[x]])
    tree1@tree$matrix[,"side"] <- treespecs$side
    treeOut <- tree1
    if(!multiple_splits & maximum_size > 2){
      index_delete <- duplicated(treeOut@tree$matrix[1:maximum_size,"Cue"])
      if(index_delete[maximum_size])
        index_delete <- c(index_delete,T)
      treeOut@tree$matrix <- treeOut@tree$matrix[!index_delete,,drop = F]
      treeOut@tree$categorical <- treeOut@tree$categorical[!index_delete]
      if(index_delete[maximum_size])
        treeOut <- addLastLeaf(treeOut)
    }

    treeOut <- updateTree(treeOut, data_input, weights = weights,pruneEmpty = T)

    o <- as.numeric(as.character(predict(treeOut,data_input, "numeric")[,2]))
    # this is the normal case

    p1 <- performanceAccuracy(data_input[,1],o, weights = weights) - wFrugal * predict(treeOut,data_input, type = "frugality")
    p2 <- maximum_size-(nrow(treeOut@tree$matrix)-1)
    return(c(p1,p2))
  }

  outV <- rep(NA, iterations)
  tree1 <- fftree(data_input, max_depth = maximum_size, method = "basic") # only for establishing the tree object.
  if(nrow(tree1@tree$matrix)-1 < maximum_size){
    ndif <- maximum_size - (nrow(tree1@tree$matrix)-1)
    tree1@tree$matrix <- rbind(tree1@tree$matrix,tree1@tree$matrix[rep(1,ndif),])
    tree1@tree$categorical <- c(tree1@tree$categorical,tree1@tree$categorical[rep(1,ndif)])
  }
  allBest <- array(NA, dim = c(maximum_size,iterations))
  cues <- data_input[,-1]
  vv <- lapply(1:ncol(cues),function(x)table(cut(cues[,x],cutValues[[x]],include.lowest = T)))
  vv <- sapply(vv,function(x)c(x,rep(0,thresholds-length(x))))
  vv <- as.vector(vv)
  vv <- rep(vv, each = 2)

  distr <- array(0, dim = c(maximum_size,nc*thresholds*2))   # cue1, value1, side1 .... cue1, value1, side2 .... cue1, value2, side1 .... cue2, value1, side1
  ix <- vv>0
  #print(paste(sum(ix),nc*thresholds*2))
  value <- 1/sum(ix)
  for(d in 1:maximum_size)
    distr[d,ix] <- value
  no <- nrow(data_input); ncol <- nc*thresholds*2
  timeStart <- proc.time()
  for(i in 1:iterations){
    population <- replicate(samples, sapply(1:maximum_size, function(x) sample(1:ncol,size = 1,prob = distr[x,])))
    if(maximum_size == 1)
      population <- t(as.matrix(population))

    treesperfs <- apply(population,2,function(x) treePerf(x))



    index_best <- order(treesperfs[1,], treesperfs[2,], decreasing = T)[1:elite_samples]
    bestTrees <- population[,index_best, drop = F]
    distrNew <- distr*0
    for(treeIx in 1:elite_samples){
      for(node in 1:maximum_size){
        distrNew[node,bestTrees[node,treeIx]] = distrNew[node,bestTrees[node,treeIx]] + 1
      }
    }
    allBest[,i] <- bestTrees[,1]
    distr = learning_rate * distrNew/(elite_samples) + (1-learning_rate) * distr
    if(verbose){
      if(i%%10==0 | i == 1)
        cat(paste0("iterationsation: ", i, ", Accuracy: ", format(round(max(treesperfs[1,]),4),nsmall = 4)), "\n")

    }
    outV[i] <- max(treesperfs[1,])
    if(i > early_stopping + 1){
      if(length(unique(outV[(i-early_stopping):i]))==1)
        break
      if((proc.time() - timeStart)[3]> maximum_time)
        break
    }

  }

  index_best <- which(outV ==max(outV, na.rm = T))
  if(length(index_best) > 1){
    index_best <- sort(index_best,decreasing = T)[1]
  }

  finalTree <- allBest[, index_best]
  treespecs <- indexToNode(finalTree)
  treeOut <- tree1
  treeOut@tree$matrix[,"Cue"] <- treespecs$cue.ix
  values <- treespecs$value
  treeOut@tree$matrix[,"splitPoint"] <- sapply(1:length(values),function(x)cutValues[[treespecs$cue.ix[x]]][values[x]])
  treeOut@tree$matrix[,"side"] <- treespecs$side
  rownames(treeOut@tree$matrix) <- colnames(data_input)[-1][treespecs$cue.ix]

  if(!multiple_splits & maximum_size > 2){
    index_delete <- duplicated(treeOut@tree$matrix[1:maximum_size,"Cue"])
    if(index_delete[maximum_size])
      index_delete <- c(index_delete,T)
    treeOut@tree$matrix <- treeOut@tree$matrix[!index_delete,]
    treeOut@tree$categorical <- treeOut@tree$categorical[!index_delete]
    if(index_delete[maximum_size])
      treeOut <- addLastLeaf(treeOut)
  }


  treeOut <- updateTree(treeOut, data_input, weights = weights, pruneEmpty = T)

  attr(treeOut,"convergence") <- ifelse(i<iterations & ((proc.time() - timeStart)[3] < maximum_time),1,0)

  if(verbose){
    if(attr(treeOut,"convergence") == 1)
      cat("Model converged \n")
  }
  cix <- treeOut@tree$matrix[,"Cue"]
  treeOut@tree$matrix[,"splitPoint"] <- treeOut@tree$matrix[,"splitPoint"] * (maxx[cix]-minx[cix]) + minx[cix]
  treeOut@training_data <- data_input_not_rescaled
  split_points <- treeOut@tree$matrix[,"splitPoint"]
  cueix <- treeOut@tree$matrix[,"Cue"]
  dirix <- treeOut@tree$matrix[,"side"]
  treeOut@tree$matrix[,"splitPoint"] <- smoothThresholds(data_input_not_rescaled, split_points,cueix,dirix)
  treeOut@parameters$algorithm = "cross-entropy"
  return(treeOut)
}

smoothThresholds <- function(data_input,split_points,cueix,dirix){
  temp <- split_points
  for(i in 1:length(split_points)){
    th <- round(split_points[i],10)
    cuei <- round(sort(unique(data_input[,cueix[i]+1])),10)
    diri <- dirix[i]
    if(th %in% cuei){
      ix <- which(th == cuei)
      if(ix < length(cuei)){
        split_points[i] <- (th + cuei[ix+1])/2
      }
    } else {
      # threshold value is not observed in data
      x <- cuei-th
      x <- min(x[x>0])
      x <- x+th
      ix <- which(x==cuei)
      split_points[i] <- (cuei[ix-1] + cuei[ix])/2
    }
  }
  split_points[is.na(split_points)] <- temp[is.na(split_points)]
  return(split_points)
}







