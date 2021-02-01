Tallytest <- function(model.input, test.cues){
  if(all(model.input@tally$weights == 0)){
    return(rep(model.input@prior,nrow(test.cues)))
  }
  # dichotomize test data
  cues.out <- test.cues
  mod <- model.input@tally
  for(m in 1:nrow(mod$matrix)){
    cue.ix <- mod$matrix[m,1]
    if(is.numeric(test.cues[,cue.ix])){
      cues.out[,cue.ix] <- ifelse(test.cues[,cue.ix] > mod$matrix[m,2],1,0)
    } else {
      cues.out[,cue.ix] <- ifelse(!test.cues[,cue.ix]%in%mod$categorical[[m]],1,0)
    }
  }


  predicted <- (model.input@tally$weights%*%t(cues.out[,mod$matrix[,1]]))[1,] + model.input@tally$intercept
  return(sigmoid(predicted))
}


simMatrix <- function(x,y){
  n <- length(x)
  cor.coef <- cor(x,y)
  validity <- NA
  suppressWarnings(rho <- cor.test(x,y, method = "spearman"))
  rho <- rho$estimate
  hitr <- sum(y==1 & x==1) / sum(y==1)
  negr <- sum(y==0 & x==0) / sum(y==0)
  balanced <- (hitr + negr)/2
  balanced <- balanced * 2 - 1
  output <- c(cor.coef,validity,rho, balanced)
  names(output) <- c("correlation", "validity", "rho", "balanced")
  return(output)
}


tallyfromCorrelation <- function(data_input, importance_metric = "correlation", transform = F, prune = NULL,costs = c(.5,.5), ncues = 10^10, selectFeatures =F){
  n.objects <- nrow(data_input)
  criterion <- getCriterion(data_input)
  cues <- getCues(data_input)
  m <- ncol(cues)
  perf_cue <- apply(cues, 2, function(x) simMatrix(criterion, x)[importance_metric])
  weights <- perf_cue
  weights[is.na(weights)] <- 0
  weights <- round(weights,5)


  # if weights are equal, random order
  sw <- sort(weights)
  sw <- sw[-1] - sw[-length(sw)]
  sw <- sw[sw != 0]
  weights[weights != 0] <- weights[weights!=0] + runif(sum(weights!=0))*min(sw)*.5
  if(sum(weights != 0) > ncues) # set weakest cues to zero
    weights[rank(-abs(weights)) > ncues] <- 0

  weights <- sign(weights)

  predicted_values <- (weights%*%t(cues))[1,]
  pv.unique <- unique(predicted_values)
  intercept <- -findOptimalThresholdCost(data_input,predicted = predicted_values, costs = costs)
  output <- c(intercept,weights)
  class(output) <- "tallyBinary"
  return(output)
}



findOptimalThresholdCost <- function(data_input, model, costs, predicted = NULL, integer_threshold = F){
  weights <- getWeightsFromCost(costs,getPrior(data_input))
  criterion <- getCriterion(data_input)

  if(is.null(predicted)){ # if no predictings are given we optimize threshold using the (training) data inputt
    predicted <- predicting(model, data_input, return.metric = F, doSigmoid = F)
  }

  unique_predicted <- sort(unique(predicted))
  if(length(unique_predicted) == 2)
    return(mean(unique_predicted))
  if(hasNoVariance(predicted))
    return(.5)
  all.splits <- makeCueSplits(criterion, predicted)
  n.objects <- sum(all.splits[1,-1])
  split.values.middle <- (all.splits[,1] + c(all.splits[-1,1], max(all.splits[-1, 1]))) / 2
  split.values.middle <- c(0, .5, split.values.middle)
  split.performance <- sapply(split.values.middle, function(x) computePerformance(criterion, predicted, threshold = x, random = F, weights = weights)["Accuracy"])
  ix <- which(split.performance == max(split.performance))
  n.splits <- length(split.values.middle)
  threshold <- split.values.middle[ix]
  threshold <- unique(threshold)

  if(length(threshold) > 1)
    threshold <- threshold[which.min(abs(threshold-.1))]
  if(integer_threshold)
    threshold <- as.integer(floor(threshold))
  return(threshold)
}


getWeights <- function(object)
  UseMethod("getWeights",object)

getWeights.cv.glmnet <- function(object){
  coefs <- glmnet:::coef.cv.glmnet(object,s = "lambda.min")
  intercept <- coefs@x[1]
  weights <- as.matrix(coefs)[-1,1]
  weights[is.na(weights)] <- 0
  output <- list(intercept,weights)
  names(output) <- c("intercept","weights")
  return(output)
}



predicting <- function(object,...)
  UseMethod("predicting",object)

predicting.cv.glmnet <- function(object, test.data, return.metric = T, optimize.threshold = NULL,weights, ...){
  cues <- getCues(test.data)
  criterion <- getCriterion(test.data)
  size.set <- length(criterion)

  dec.thresh <- .5
  if("regressor"%in%class(object))
    dec.thresh <- 0

  predicted <- predict(object = object, newx = as.matrix(cues), s = "lambda.min", type = "response")[,1]
  if(return.metric)
    return(computePerformance(criterion,predicted, threshold = dec.thresh, weights = weights))
  return(predicted)
}

predicting.tallyBinary <- function(model.input, data_input, weights = c(1,1), return.metric = TRUE, random = TRUE, doSigmoid = TRUE,...){
  #input is any linear classifier that is represented as a vector
  # with the intercept as the first entry and the weights in the remaining entries
  pred <- model.input[1] + (model.input[-1]%*%t(data_input[,-1]))[1,]
  if(return.metric)
    return(computePerformance(data_input[,1], pred, threshold = 0, weights = weights, random = random))
  if(doSigmoid)
    return(sigmoid(pred))
  return(pred)
}


selCues <- function(model, ncues,data_input){
  err <- model$cvm
  nz <- model$nzero
  ix <- which(abs(ncues - nz) == min(abs(ncues - nz)))
  ixl <- which.min(err[ix])
  lmin <- model$lambda[ix][ixl]
  model$lambda.min <- ifelse(lmin > model$lambda.min, lmin,
                             model$lambda.min)
  ixcues <- getWeights(model)$weights != 0
  if(sum(ixcues)> ncues){ # too many cues selected
    nextix <- order(abs(cor(data_input)[1,])[-1], decreasing = F)
    nreplace <- sum(ixcues) - ncues
    ixcues[nextix][ixcues[nextix]][1:nreplace] <- F
  }
  if(sum(ixcues)<ncues & ncol(data_input)-1 > sum(ixcues)){ # not enough cues selected
    nextix <- order(abs(cor(data_input)[1,])[-1], decreasing = T)
    nreplace <- ncues - sum(ixcues)
    ixcues[nextix][!ixcues[nextix]][1:nreplace] <- T
  }
  if(sum(ixcues)!=ncues)
    stop("wrong cue selection")
  return(ixcues)
}

binaryTallyFromGlmnet <- function(model, data_input, costs= c(.5,.5), ncues){

  # get sign of weights from glmnet model like lasso
  # resulting model is a ModelSimpleNew model
  model <- fixLambda(model, ncues = ncues)
  cues <- as.matrix(getCues(data_input))
  class_labels <- getCriterion(data_input)
  weights <- sign(getWeights(model)$weights)

  predicted.values <- (weights%*%t(cues))[1,]
  pv.unique <- unique(predicted.values)

  d.frame <- data.frame(class_labels, predicted.values)
  d.frame[,1] <- as.factor(d.frame[,1])
  colnames(d.frame) <- c("class_labels","predicted.values")

  intercept <- -findOptimalThresholdCost(data_input, predicted = predicted.values, costs = costs, integer_threshold = TRUE)

  output <- c(intercept, weights)
  class(output) <- "tallyBinary"
  return(output)
}


buildLogisticLassoFullLambdaSequence <- function(data_input, costs = c(.5,.5),maximum_size = NULL){
  weights <- getWeightsFromCost(costs, getPrior(data_input))
  case.weights <- ifelse(data_input[,1] == 1,weights[1],weights[2])

  cues <- getCues(data_input)
  nvar <- ncol(cues)
  pmax <- min((nvar+1) * 2+20, nvar)
  if(!is.null(maximum_size))
    pmax <- maximum_size

  criterion <- getCriterion(data_input,is.factor = T)
  err <- "no error default"
  if(nrow(data_input)>5)#CV only possible wiht at least 4 training objects
    err <- try(model.output <- glmnet::cv.glmnet(y = criterion, x = as.matrix(cues), family="binomial", type.measure = "deviance", alpha = 1,weights = case.weights, lambda =(10^seq(-10,0,length=500)),pmax = pmax),silent = T) #alpha =1 is lasso, alpha = 0 is ridge

  if(nrow(data_input)<=5||class(err) =="try-error"){
    if(nrow(data_input < 4)){
      criterion <- c(criterion, criterion)
      cues <- rbind(cues,cues)
      case.weights <- c(case.weights,case.weights)
    }
    model.output <- glmnet::glmnet(y = criterion, x = as.matrix(cues), family="binomial", alpha = 1,weights = case.weights) #alpha =1 is lasso, alpha = 0 is ridge
  }
  model.output$fitted <- predicting(model.output, data_input, return.metric = F)
  model.output$data <- data_input
  model.output$caseWeights <- case.weights[1:nrow(data_input)]
  model.output$costs <- costs
  model.output$nzero
  return(model.output)
}


fixLambda <- function(model, ncues){

  data <- model$data
  ixcues <- selCues(model,ncues,data_input = data)

  criterion <- getCriterion(data,is.factor = T)
  cues <- getCues(data)
  err <- "noerror"
  err <- try(model <- glmnet::cv.glmnet(y = criterion, x = as.matrix(cues), family="binomial", type.measure = "deviance", alpha = 0,weights = model$caseWeights,exclude = which(!ixcues)),silent = T) #alpha =1 is lasso, alpha = 0 is ridge

  if(class(err) =="try-error"){
    criterion <- c(criterion, criterion)
    cues <- rbind(cues,cues)
    case.weights <- c(model$caseWeights,model$caseWeights)

    model <- glmnet::glmnet(y = criterion, x = as.matrix(cues), family="binomial", alpha = 1,weights = case.weights,exclude = which(!ixcues)) #alpha =1 is lasso, alpha = 0 is ridge
  }


  model$fitted <- predicting(model, data, return.metric = F)
  model$data <- data
  return(model)
}
