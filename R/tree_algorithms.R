createUnconditionalFFT <- function(splits.input, orderFunction, opposite_class = F, fold = F, cue_order = NULL, laplacePurity = F){
  splits <- splits.input@splits$matrix
  category_information <- splits.input@splits$categorical
  splits.4.confusion <-splits[,3:6,drop=F]

  out <- orderFunction(splits.4.confusion, opposite_class = opposite_class, fold = fold, cue_order = cue_order, laplace = laplacePurity)
  cue_order <- out$cue_order
  side <- out$side
  exit <- out$exit

  model <- cbind(splits,side,exit)
  model <- model[cue_order,,drop=F]
  category_information <- category_information[cue_order]
  model <- cbind(model,1:nrow(model))
  colnames(model) <- c("Cue","splitPoint",">+",">-","<=+","<=-","side","exit","order")


  model.output <- new("fftreeModel", tree = list(matrix = model, categorical = category_information))
  model.output@type$algorithm = "naive"

  model.output <- addLastLeaf(model.output, opposite_class = opposite_class)

  return(model.output)
}




createRecursiveFFT <- function(data.input, orderFunction, splittingFunction="gini", stump = NULL, multiple_splits = T, costs = c(.5,.5),laplacePurity = F){
  weights <- getWeightsFromCost(costs,getPrior(data.input))
  stump = ifelse(is.null(stump),0,stump)
  model.output <- NULL
  category_information <- list()
  data.use <- data.frame(data.input)
  counter=0
  while(T){
    if(nrow(data.use)<2 | all(sapply(data.use[,-1,drop=F],hasNoVariance)) | hasNoVariance(data.use[,1]))
      break
    counter <- counter+1
    current.splits <- findSplits(data.use,splittingFunction, weights = weights)
    fft.uncondtional <- createUnconditionalFFT(current.splits, orderFunction, laplacePurity = laplacePurity)

    add.model <- fft.uncondtional@tree$matrix[1, ,drop = F]
    add.category_information <- fft.uncondtional@tree$categorical[1]
    model.output <- rbind(model.output,add.model)
    category_information <- c(category_information,add.category_information)
    model.test <- new("fftreeModel",tree =  list(matrix = add.model, categorical = add.category_information))
    model.test@type$algorithm <- "modelPart"
    model.test@training_data <- data.input
    objects.decided <- FFTtest(model.test, data.use[, -1,drop = F])
    data.use <- data.use[is.na(objects.decided),,drop=F]
    if(counter==stump)
      break

    if(!multiple_splits){
      current.cue<-add.model[1,1]
      if(nrow(data.use)>0)
        data.use[,current.cue+1]<-0
    }
  }
  model.output[,9]<-1:nrow(model.output)

  model <- new("fftreeModel", tree = list(matrix = model.output, categorical = category_information))
  model@type$algorithm = "recursive"
  model <- addLastLeaf(model)
  return(model)
}
