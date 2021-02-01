getWeightsFromCost <- function(costs,prior){
  weights <- costs/(prior * costs[1] + (1-prior) * costs[2])
  return(weights)
}

getCostsFromWeights <- function(weights,prior){
  costs <- c(weights[1]*prior, weights[2]*(1-prior))/c(prior,1-prior)
  costs <-  costs/sum(costs)
  return(costs)
}

getCueDirection <- function(splits4){
  dir <- (splits4[,">+"] + splits4[,"<=-"])/rowSums(splits4)
  dir <- ifelse(dir >= .5,1,0)
  return(dir)
}

makePuritySides <- function(splits_proportion){
  left <- splits_proportion[,1,drop=F]*(1-splits_proportion[,1,drop=F])
  right <- splits_proportion[,2,drop=F]*(1-splits_proportion[,2,drop=F])
  out <- cbind(left,right)
  colnames(out) <- c("Gini.left","Gini.right")
  return(out)
}

makePuritySidesGini<-function(splits.proportion){
  left <- splits.proportion[,1,drop=F]*(1-splits.proportion[,1,drop=F])
  right <- splits.proportion[,2,drop=F]*(1-splits.proportion[,2,drop=F])
  out <- cbind(left,right)
  colnames(out) <- c("Gini.left","Gini.right")
  return(out)
}


findOrder <- function(purities, splits_input, orderFunction){

  purer_side <- findPurerSide(purities,splits_input)
  object_count <- countObjectsLeftorRight(splits_input)
  cue.purity <- orderFunction(splits_input, object_count)
  object.number <- ifelse(purer_side == 1,object_count[,1,drop=F], object_count[,2,drop=F])
  finalOrder <- order(cue.purity, -object.number,stats::runif(length(cue.purity)))
  return(finalOrder)
}

findPurerSide <- function(purities, splits_input){

  more_left <- moreObjectsLeftorRight(splits_input)
  purer_side <- ifelse(purities[,1,drop=F] < purities[,2,drop=F],1,0)
  purer_side <- ifelse(round(purities[,1,drop=F], 15) == round(purities[,2,drop=F], 15), more_left,purer_side)
  return(purer_side)
}
findExit <- function(purer_side,left.right.proportion){
  exit <- ifelse(purer_side == 1, left.right.proportion[,1,drop=F], left.right.proportion[,2,drop=F])
  return(exit)
}

countObjectsLeftorRight<-function(splits_input){
  left_number <- rowSums(splits_input[,1:2,drop=F])
  right_number <- rowSums(splits_input[,3:4,drop=F])
  return(cbind(left_number,right_number))
}

moreObjectsLeftorRight <- function(splits_input){
  random.row <- round(stats::runif(nrow(splits_input)))
  more_left <- ifelse(rowSums(splits_input[,1:2,drop=F]) > rowSums(splits_input[,3:4,drop=F]),1,0)
  more_left <- ifelse(round(rowSums(splits_input[,1:2,drop=F]),10) == round(rowSums(splits_input[,3:4,drop=F]),10),random.row,more_left)
  return(more_left)
}

orderGini <- function(splits4, cue_order = NULL,...){
  splits_proportion <- makeProportionSplits(splits4)
  splits_purity <- makePuritySides(splits_proportion)
  purer_side <- findPurerSide(splits_purity,splits4)
  left_gini <- splits_purity[,1]
  right_gini <- splits_purity[,2]
  proportion.left <- splits_proportion[,3]
  if(is.null(cue_order)){
    cue_order <- left_gini * proportion.left + right_gini * (1 - proportion.left)
    cue_order <- order(cue_order, decreasing = F)
  }
  side <- purer_side
  exit <- ifelse(side == 1,
                 splits_proportion[,1],
                 splits_proportion[,2])
  output <- list(cue_order,side,exit)
  names(output) <- c("cue_order","side","exit")
  return(output)
}

getCueDirectionBook <- function(splits4){
  tpr <- splits4[,1]/(splits4[,1] + splits4[,3])
  tnr <- splits4[,4]/(splits4[,4] + splits4[,2])
  dir <- ifelse((tpr + tnr) / 2 >= .5,1,0)
  return(dir)
}

orderGiniBook <- function(splits4,...){
  splits_proportion <- makeProportionSplits(splits4)
  splits_purity <- makePuritySidesGini(splits_proportion)
  cue.direction <- getCueDirectionBook(splits4)
  purer.side <- findPurerSide(splits_purity, splits4)
  left_gini <- splits_purity[,1]
  right_gini <- splits_purity[,2]
  proportion.left <- splits_proportion[,3]
  cue_order <- left_gini * proportion.left + right_gini * (1 - proportion.left)
  cue_order <- order(cue_order, decreasing = F)
  side <- (purer.side)[,1]
  exit <- ifelse(side == 1, cue.direction, 1 - cue.direction)
  output <- list(cue_order,side,exit)
  names(output) <- c("cue_order","side","exit")
  return(output)
}

orderGiniL <- function(splits4,...){
  #L means literature
  splits_proportion <- makeProportionSplits(splits4)
  splits_purity <- makePuritySides(splits_proportion)
  cue.direction <- getCueDirection(splits4)
  purer_side <- findPurerSide(splits_purity,splits4)
  left_gini <- splits_purity[,1]
  right_gini <- splits_purity[,2]
  proportion.left <- splits_proportion[,3]
  cue_order <- left_gini * proportion.left + right_gini * (1 - proportion.left)
  cue_order <- order(cue_order, decreasing = F)
  valP <- splits_proportion[,1]
  valN <- 1 - splits_proportion[,2]

  valP <- ifelse(cue.direction == 1,valP,1-valP)
  valN <- ifelse(cue.direction == 1,valN,1-valN)
  side <- ifelse(valP>valN,1,0)
  exit <- ifelse(side == 1, splits_proportion[,1],splits_proportion[,2])
  output <- list(cue_order,side,exit)
  names(output) <- c("cue_order","side","exit")
  return(output)
}


orderDPrime <- function(splits4,cue_order = NULL,laplace = F,...){



  positives <- splits4[,1]/(splits4[,1,drop=F]+splits4[,3,drop=F])
  negatives <- splits4[,2]/(splits4[,2,drop=F]+splits4[,4,drop=F])
  #adjusting extreme values sp that qnorm can be computed
  positives[positives == 1] <- ((splits4[,1]-.5)/splits4[,1])[positives==1]
  positives[positives == 0] <- ((.5)/splits4[,3])[positives==0]
  negatives[negatives == 1] <-  ((splits4[,2]-.5)/splits4[,2])[negatives==1]
  negatives[negatives == 0] <- ((.5)/splits4[,4])[negatives==0]

  if(is.null(cue_order)){
    cue_order <- abs(stats::qnorm(positives)-stats::qnorm(negatives))
    cue_order <- order(cue_order, decreasing = T)
  }
  splits_proportion <- makeProportionSplits(splits4)
  splits_purity <- makePuritySides(splits_proportion)
  purer_side <- findPurerSide(splits_purity,splits4)
  left_gini <- splits_purity[,1]
  right_gini <- splits_purity[,2]
  proportion.left <- splits_proportion[,3]

  side <- purer_side
  exit <- ifelse(side == 1,
                 splits_proportion[,1],
                 splits_proportion[,2])
  output <- list(cue_order,side,exit)
  names(output) <- c("cue_order","side","exit")
  return(output)
}

orderEqualWeight <- function(splits4,cue_order = NULL,laplace = F,...){
  positives <- splits4[,1]/(splits4[,1,drop=F]+splits4[,3,drop=F])
  negatives <- splits4[,2]/(splits4[,2,drop=F]+splits4[,4,drop=F])
  if(is.null(cue_order)){
    cue_order <- abs(positives-negatives)
    cue_order <- order(cue_order, decreasing = T)
  }

  splits_proportion <- makeProportionSplits(splits4)
  splits_purity <- makePuritySides(splits_proportion)
  purer_side <- findPurerSide(splits_purity,splits4)
  left_gini <- splits_purity[,1]
  right_gini <- splits_purity[,2]
  proportion.left <- splits_proportion[,3]


  side <- purer_side
  exit <- ifelse(side == 1,
                 splits_proportion[,1],
                 splits_proportion[,2])
  output <- list(cue_order,side,exit)
  names(output) <- c("cue_order","side","exit")
  return(output)
}

addLastLeaf <- function(model_input, opposite_class = F){
  m <- model_input
  model <- model_input@tree$matrix
  category_information <-  model_input@tree$categorical
  lastRow <- nrow(model)
  side <- model[lastRow,7]
  if(side == 1)
    exit.new <- model[lastRow,5]/(model[lastRow,5] + model[lastRow,6])
  if(side == 0)
    exit.new <- model[lastRow,3]/(model[lastRow,3] + model[lastRow,4])
  if(opposite_class){
    exit.new <- 1 - round(model[lastRow,"exit"])
  }
  add.last.line <- c(model[lastRow,1:6],1 - side,exit.new,lastRow+1)
  model <- rbind(model,add.last.line)
  rownames(model)[nrow(model)] <- rownames(model)[nrow(model)-1]
  category_information[[length(category_information) + 1]] <- category_information[[length(category_information)]]
  model_input@tree$matrix <- model
  model_input@tree$categorical <- category_information
  return(model_input)
}

deleteLastLeaf <-  function(model_input){
  n <- nrow(model_input@tree$matrix)
  model_input@tree$matrix <- model_input@tree$matrix[1:(n-1),,drop = F]
  model_input@tree$categorical <- model_input@tree$categorical[1:(n-1)]
  return(model_input)
}

#' @noRd
FFTtest <- function(model_input, test.cues, return.frugality = F){
  if(nrow(model_input@tree$matrix) == 0){
    if(return.frugality)
      return(0)

    return(rep(model_input@prior,nrow(test.cues)))
  }

  fft.model <- model_input@tree$matrix
  category.assignments <- model_input@tree$categorical
  order.of.cues <- order(fft.model[,9])
  fft.model <- fft.model[order.of.cues,,drop=F]
  category.assignments <- category.assignments[order.of.cues]
  decided.per.cue <- rep(0,nrow(fft.model))

  predict.criterion <- rep(NA,nrow(test.cues))
  for(i in 1:nrow(fft.model)){
    cue.pointer <- fft.model[i,1]
    current_cue <- test.cues[,cue.pointer]
    if(i >= nrow(fft.model) - 1 & any(is.na(current_cue))){
      #if here are still NAS in last cue, we impute the modal value of the training set
      cue.train <- model_input@training_data[,cue.pointer + 1]
      current_cue[is.na(current_cue)] <- findMode(cue.train)
    }
    current_cue <- transformCategoricalCues(current_cue,category.assignments[[i]])
    current.threshold <- fft.model[i,2]
    current.side <- fft.model[i,7]
    current.prediction <- fft.model[i,8]
    predict.criterion <- classifyObjects(predict.criterion, current_cue, current.threshold, current.side, current.prediction)
    decided.per.cue[i] <- nrow(test.cues) - sum(is.na(predict.criterion))-sum(decided.per.cue)
    if(sum(is.na(predict.criterion))==0)
      break
  }
  if(return.frugality){
    length.ix <- length(decided.per.cue)
    decided.per.cue[length.ix-1] <- decided.per.cue[length.ix-1] + decided.per.cue[length.ix]
    decided.per.cue <- decided.per.cue[1:(length.ix-1)]
    frugality <- sum(decided.per.cue*(1:length(decided.per.cue)))/sum(decided.per.cue)
    return(frugality)
  }
  return(predict.criterion)
}

transformCategoricalCues<-function(cue,category.assignments){
  #numeric cues are not transformed
  if(is.na(category.assignments[1]))
    return(cue)
  #categorical cues are transformed
  cue <- as.character(cue)
  cue.ix <- cue%in%category.assignments
  cue[cue.ix] <- 0
  cue[!cue.ix] <- 1
  cue<-as.numeric(cue)
  return(cue)
}

classifyObjects <- function(predict.criterion,current_cue,threshold,side,current.prediction){
  if(side==1)
    predict.criterion[current_cue>threshold&is.na(predict.criterion)] <- current.prediction
  if(side==0)
    predict.criterion[current_cue<=threshold&is.na(predict.criterion)] <- current.prediction

  return(predict.criterion)
}



performanceAccuracy <- function(criterion,predicted,threshold = .5,weights = c(1,1),...){
  # we input weights not costs here !!!
  predicted[is.na(predicted)] <- stats::runif(sum(is.na(predicted)))
  predicted.t <- ifelse(predicted > threshold,1,0)
  tp <- as.numeric(sum(predicted.t==1&criterion==1)) * weights[1]
  fp <- as.numeric(sum(predicted.t==1&criterion==0)) * weights[2]
  tn <- as.numeric(sum(predicted.t==0&criterion==0)) * weights[2]
  fn <- as.numeric(sum(predicted.t==0&criterion==1)) * weights[1]
  accuracy <- (tp+tn)/(tp+fp+tn+fn)
  return(accuracy)
}



estimateProbabilty <- function(criterion,predict.criterion,current_cue,threshold,side){
  if(side==1){
    new.predictions = current_cue>threshold & is.na(predict.criterion)
  }
  if(side==0){
    new.predictions = current_cue<=threshold & is.na(predict.criterion)
  }
  probability.estimate<-mean(criterion[new.predictions])

  return(probability.estimate)
}


invertSidesModel <- function(model){
  side <- model[7]
  if(side==1)
    exit.new<-model[5]/(model[5]+model[6])
  if(side==0)
    exit.new<-model[3]/(model[3]+model[4])

  inverted.line <- c(model[1:6],1-side,exit.new,NA)
  return(inverted.line)
}


cutTree <- function(model_input, depth = NULL, opposite_class = FALSE){
  if(is.null(depth) || depth >= nrow(model_input@tree$matrix))
    return(model_input)
  category_information <- model_input@tree$categorical
  category_information <- category_information[1:depth]

  model.matrix <- model_input@tree$matrix
  model.matrix <- model.matrix[1:depth, , drop = F]
  model_input@tree$matrix <- model.matrix
  model_input@tree$categorical <- category_information

  return(addLastLeaf(model_input, opposite_class = opposite_class))
}





updateTree <- function(model_input, data.input, changeSide = F, changePrediction = T, weights = c(1,1), pruneEmpty = F){

  model<- model_input@tree$matrix
  model.output <- model
  category_information <- model_input@tree$categorical

  test.criterion <- getCriterion(data.input)
  test.cues <- getCues(data.input)
  predict.criterion <- rep(NA,nrow(data.input))
  for(i in 1:nrow(model)){
    cue.pointer <- model[i,1]
    current_cue <- test.cues[,cue.pointer]
    current_cue <- transformCategoricalCues(current_cue,category_information[[i]])
    current.threshold <- model[i,2]
    current.side <- model[i,7]
    current.prediction <- model[i,8]
    newABCD <- getNewSplits(predict.criterion, current_cue, current.threshold, test.criterion, weights = weights)

    if(changeSide){ # should side be adjusted?
      sum.left <- sum(newABCD[1:2])
      left <- binaryGini(newABCD[1]/sum.left)
      sum.right <- sum(newABCD[3:4])
      right <- binaryGini(newABCD[3]/sum.right)
      left[is.na(left)] <- 0
      right[is.na(right)] <- 0
      current.side <- 1*(left < right |
                           (left == right & sum.left > sum.right)|
                           (left == right & sum.left == sum.right & stats::runif(1) > .5))
    }
    if(changePrediction){ # can the prediction change?
      if(current.side == 1){
        current.prediction <- newABCD[1]/(newABCD[1] + newABCD[2])
      }
      else{
        current.prediction <- newABCD[3]/(newABCD[3] + newABCD[4])
      }
    }
    model.output[i,3:6] <- newABCD
    model.output[i,"exit"] <- current.prediction
    model.output[i,"side"] <- current.side
    predict.criterion <- classifyObjects(predict.criterion,current_cue,current.threshold,current.side,current.prediction)
  }
  model.output[,8][is.na(model.output[,8])] <- model[,8][is.na(model.output[,8])]
  model_input@tree$matrix <- model.output


  if(pruneEmpty){# should empty cues be discarded?
    model_input <- deleteLastLeaf(model_input)
    ix.keep <- rowSums(model_input@tree$matrix[, c(">+",">-", "<=+", "<=-"), drop = F])>3 &
      rowSums(model_input@tree$matrix[,c(">+",">-"), drop = F]) > 0 & rowSums(model_input@tree$matrix[,c("<=+", "<=-"), drop = F]) > 0
    if(all(!ix.keep))
      ix.keep[1] <- T

    model_input@tree$matrix <- model_input@tree$matrix[ix.keep,,drop = F]
    model_input@tree$categorical <- model_input@tree$categorical[ix.keep]
    model_input <- addLastLeaf(model_input)
  }
  return(model_input)
}

getNewSplits <- function(predict.criterion, current_cue, threshold, criterion, weights = c(1,1)){
  a <- sum(current_cue > threshold & is.na (predict.criterion) & criterion == 1, na.rm = T) * weights[1]
  b <- sum(current_cue > threshold & is.na (predict.criterion) & criterion == 0, na.rm = T) * weights[2]
  c <- sum(current_cue <= threshold & is.na (predict.criterion) & criterion == 1, na.rm = T) * weights[1]
  d <- sum(current_cue <= threshold & is.na (predict.criterion) & criterion == 0, na.rm = T) * weights[2]
  out <- c(">+" = a, ">-" = b, "<=+" = c, "<=-" = d)
  return(out)
}

iterateThroughAllStructures <- function(model_input, costs, train, omit_cues = FALSE){

  ncues <- nrow(model_input@tree$matrix) - 1
  if(ncues == 1)
    return(model_input)

  if(omit_cues)
    ncues <- ncues + 1
  binary_patterns <- expand.grid(lapply(1:ncues, function(x) c(0,1)))
  ordr <- order(((2 ^ (ncues:1)) %*% t(binary_patterns))[1,])
  binary_patterns <- as.matrix(binary_patterns[ordr,])

  if(!omit_cues)
    binary_patterns <- cbind(binary_patterns, 1 - binary_patterns[,ncues])

  weights <- getWeightsFromCost(costs,prior = getPrior(train))
  models <- list()
  performance <- rep(NA, ncues)
  for(b in 1:nrow(binary_patterns)){
    model <- model_input
    model@tree$matrix[,"exit"] <- ifelse(binary_patterns[b,] == model@tree$matrix[,"side"], model@tree$matrix[,"exit"], 1 - model@tree$matrix[,"exit"])
    model@tree$matrix[,"side"] <- c(binary_patterns[b,])

    if(binary_patterns[b,ncol(binary_patterns)] == binary_patterns[b,ncol(binary_patterns) - 1]){ # neglect last cue by making identical predictions in both branches (can only happen if omit_cues is True)
      # these predictions are opposite to previous cue
      model@tree$matrix[ncol(binary_patterns), "side"] <- 1 - model@tree$matrix[ncol(binary_patterns), "side"]
      model@tree$matrix[c(ncol(binary_patterns),ncol(binary_patterns)-1), "exit"] <- 1 - model@tree$matrix[ncol(binary_patterns) - 2, "exit"]
    }

    models[[b]] <- model
    performance[b] <- predict(model, train, weights = weights, type = "metric")["Accuracy"]
  }
  return(models[[which.max(performance)]])
}



