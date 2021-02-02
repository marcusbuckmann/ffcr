#' class for fast-and-frugal-tree models
#'
#' An S4 class to represent a fast-and-frugal tree model
#'@slot call an image of the call that produced the object.
#'@slot type induction algorithm used to train the fast-and-frugal tree.
#'@slot performance list showing fitting and cross-validated performance.
#'@slot formula \link[stats]{formula} object of the model.
#'@slot weights a numeric vector of length 2. The first entry denotes the weight of instances in the positive class, the second entry the weight of instances in the negative class.
#'@slot training_data data that was used to train the model
#'@slot tree representation of the tree
#'@slot prior the proportion of objects in the positive class in the training set.
#'@slot class_labels a vector of length 2 containing the class labels. The second entry is referred to as the positive class.
#'@export
setClass("fftreeModel",representation(type = "list",tree = "list",
                                      performance = "list",
                                      call = "call", formula = "formula", class_labels = "character",
                                      training_data = "data.frame", weights = "numeric",
                                      prior = "numeric"),
         prototype(formula = formula(NULL), type = list(
           method = "empty",
           use_features_once = "empty",
           split_function = "empty",
           use_features_once = "empty",
           n_nodes = "empty",
           max_depth = "empty",
           cross_entropy_parameters = "empty"
         ))
)




#'Fitting fast-and-frugal trees
#'
#'\code{fftree} is used to fit fast-and-frugal trees.

#'@param data an object of class \code{\link[base]{data.frame}} or \code{\link[base]{matrix}}. The criterion can either be a factor with two levels or an integer \code{(0,1)}. The \emph{positive class} is the second factor level (\code{levels(data$criterion)[2]}), or \code{1} if the criterion is numeric.
#'@param formula \code{\link[stats]{formula}} (optional). If \code{formula} is not provided, the first column of the data argument is used as the response variable and all other columns as predictors.
#'@param method type of induction method for the fast-and-frugal tree:
#' \itemize{
#' \item{greedy (default and recommended)}
#' \item{basic}
#' \item{cross-entropy}
#' }
#'@param split_function Which function should be used to determine the splitting values on numeric features. This only applies to fast-and-frugal trees with the 'basic' or 'greedy' method.
#'
#'By default Gini entropy ('gini') is used. Other options are Shannon entropy ('entropy') and 'median'.
#'@param max_depth set maximum number of nodes of the fast-and-frugal tree (default: 6)
#'@param weights a numeric vector of length 2 (default: \code{c(1,1)}). The first entry specifies the weight of instances in the positive class, the second entry the weight of instances in the negative class.
#'(\emph{see examples}).
#'@param pruning by default pruning is not employed (\code{FALSE}). If the argument is set to \code{TRUE} the tree is pruned using cross-validation.
#'@param cv If \code{TRUE} 10-fold cross validation is used to estimate the predictive performance of the model.
#'@param use_features_once if \code{TRUE} an attribute is used only once in a tree. If \code{FALSE}, a feature may be split several times. Note that, by construction, the basic method can only use each feature once.
#'@param cross_entropy_parameters hyperparameters for the cross-entropy method. By default the output of the function \code{\link{cross_entropy_control}} is passed.

#'@return A \linkS4class{fftreeModel} object.

#'@examples
#' data(liver)
#' model <- fftree(data = liver, formula = diagnosis~., method = "greedy")
#' plot(model)
#' model
#'
#' # weight instances by the inverse of the prior
#' # in this way both classes contribute equally when training the model
#' prior <- mean(ifelse(liver$diagnosis == "Liver disease", 1, 0))
#' weights <- c(1/prior, 1/(1-prior))
#' mod <- fftree(data = liver, formula = diagnosis~., weights = weights, method = "greedy")
#'

#' @export
setGeneric("fftree", function(data,
                              formula = stats::as.formula(data),
                              method = "greedy",
                              split_function = "gini",
                              max_depth = 6,
                              weights = c(1,1),
                              pruning = FALSE,
                              cv = FALSE,
                              use_features_once = TRUE,
                              cross_entropy_parameters = cross_entropy_control()
) standardGeneric("fftree"))
#' @rdname fftree
setMethod("fftree", signature(data = "data.frame"),
          function(data,
                   formula = stats::as.formula(data.frame(data)),
                   method = "greedy",
                   split_function = "gini",
                   max_depth = 6,
                   weights = c(1,1),
                   pruning = FALSE,
                   cv = FALSE,
                   use_features_once = TRUE,
                   cross_entropy_parameters = cross_entropy_control())
          {
            data <- model.frame(formula = formula, data = data, na.action = NULL)
            if(cv){
              folds <- createFolds(nrow(data))
              nfolds <- length(unique(folds[,1]))
              pred <- rep(NA,nrow(data))
              structure.output <- array(NA, dim = c(nfolds,3))
              colnames(structure.output) <- c("Depth","Features","Frugality")
              for(i in 1:nfolds){
                test.ix <- folds[,1] == i
                model <- buildTree(data = data[!test.ix,],
                                   method = method,
                                   order = "gini",
                                   split_function = split_function,
                                   weights = weights,
                                   pruning = pruning,
                                   multiple_splits = !use_features_once,
                                   maximum_size = max_depth,
                                   cross_entropy_parameters = cross_entropy_parameters)
                pred[test.ix] <- predict(model, data[test.ix,], type = "probability")[,2]
                structure.output[i,] <- computeStructure(model, data[test.ix,])
              }

              criterion <- data[,1]
              class_labels <- as.character(sort(unique(criterion)))
              criterion <- ifelse(as.character(criterion) == class_labels[2], 1,0)
              prediction.clean <- computePerformance(criterion, pred)
              structure.clean <- apply(structure.output,2,mean)

            }

            model <- buildTree(data = data,
                               method = method,
                               order = "gini",
                               split_function = split_function,
                               weights = weights,
                               pruning = pruning,
                               multiple_splits = !use_features_once,
                               maximum_size = max_depth,
                               cross_entropy_parameters = cross_entropy_parameters)

            cl <- match.call(expand.dots = TRUE)
            cl$data <- substitute(data, parent.frame())
            cl[[1]] <- as.name("fftree")
            cl[["formula"]] <- substitute(formula, parent.frame())

            model@call <- cl
            if(cv){
              model@performance$cv.performance <- prediction.clean
              model@performance$cv.structure <- structure.clean
            }

            return(model)
          })

#' @rdname fftree
setMethod("fftree", signature(data = "matrix"),
          function(data,
                   formula = stats::as.formula(data.frame(data)),
                   method = "greedy",
                   split_function = "gini",
                   max_depth = 6,
                   weights = c(1,1),
                   pruning = FALSE,
                   cv = FALSE,
                   use_features_once = TRUE,
                   cross_entropy_parameters = cross_entropy_control()
          )
          {
            data <- data.frame(data)
            model <- fftree(data, formula = formula,
                            method = method,
                            split_function = split_function,
                            weights = weights,
                            pruning = pruning,
                            cv = cv,
                            use_features_once = use_features_once,
                            max_depth = max_depth,
                            cross_entropy_parameters = cross_entropy_parameters
            )


            cl <- match.call(expand.dots = TRUE)
            cl$data <- substitute(data, parent.frame())
            cl[[1]] <- as.name("fftree")
            cl[["formula"]] <- substitute(formula, parent.frame())
            model@call <- cl
            return(model)
          })



buildTree <-  function(data,
                       method = "greedy",
                       multiple_splits = FALSE,
                       splits.input = NULL,
                       order = "gini",
                       split_function = "gini",
                       maximum_size = NULL,
                       opposite_class = FALSE,
                       costs = c(.5, .5),
                       weights = c(1,1),
                       pruning = "no",
                       cross_entropy_parameters = cross_entropy_control()){
  data_original <- data
  criterion <- getCriterion(data)
  class_labels <- as.character(sort(unique(criterion)))
  prior <- getPrior(data)
  if(length(class_labels)>2 || length(class_labels) == 1)
    stop("The criterion variable has to have exactly 2 distinct values")
  if(any(is.na(data)))
    stop("The data contains missing values. This method does not support that.")


  criterionName <- colnames(data)[1]
  data[,1] <- ifelse(as.character(data[,1]) == class_labels[2], 1,0)

  # if costs are defined use them to overwrite weights
  if(!all(costs == c(.5,.5))){
    weights <- getWeightsFromCost(costs, prior)
  }

  # if weights are defined use them to overwrite costs
  if(!all(weights == c(1,1))){
    weights <- weights/((weights*c(prior,1 - prior))[1]*2) #scale weights correctly
    costs <- getCostsFromWeights(weights, prior)
  }

  method <- match.arg(method, c("basic", "greedy", "cross-entropy"))



  orderFunction <- switch(order,
                          "gini" = orderGiniBook,
                          stop("no valid order function")
  )
  if(method %in% c("basic")){
    if(maximum_size > 6 & (ncol(data) -1) > 6)
      message("The basic algorithm iteratues through all possible exits. This can be slow when the tree is deep. The greedy method will be more efficient in this case.")


    splits.input <- findSplits(data, splittingFunction = split_function, weights = weights)
    model <- createUnconditionalFFT(splits.input = splits.input, orderFunction = orderFunction, opposite_class = TRUE)
    model <- pruneLeaves(model, depth = maximum_size) # reduce model to maximum size
    model <- iterateThroughAllStructures(model, costs, data, omit_cues = TRUE) # try all possible exits
  }
  if(method == "greedy"){
    model <- createRecursiveFFT(data, orderFunction = orderFunction, splittingFunction = split_function, multiple_splits = multiple_splits, stump = maximum_size, costs = costs)
    model@training_data <- data
    model <- pruneFit(model, data, weights = weights) # prune nodes that do not improve accuracy. This is what CART does as well (no information gain)
  }

  if(method == "cross-entropy"){
    model <- do.call(fft_cross_entropy_multiple_starts, c(
      list(data),
      maximum_size = ifelse(is.null(maximum_size), 6, maximum_size),
      multiple_splits = multiple_splits,
      costs = list(costs),
      cross_entropy_parameters)
    )
  }

  if(method %in% c("naive")){
    model <- updateTree(model, data, changeSide = TRUE, weights = weights, pruneEmpty = TRUE)
    model@training_data <- data
    model <- pruneFit(model, data, weights = weights) # prune nodes that do not imporve accuracy. This is what CART does as well (no information gain)
  }

  model@prior <- prior
  if(!is.null(maximum_size))
    model <- cutTree(model, maximum_size, opposite_class = TRUE)

  model@formula <- stats::formula(data)
  model@class_labels <- as.character(class_labels)
  model@training_data <- data # this was x before
  model@weights <- weights
  model@type$method = method
  model@type$split_function = split_function
  model@type$use_features_once = !multiple_splits
  model@type$n_nodes <- nrow(model@tree$matrix) - 1
  model@type$max_depth <- maximum_size
  model@type$cross_entropy_parameters <- cross_entropy_parameters

  if(pruning)
    model <- pruneCV(model, data, include_empty = TRUE, costs = costs)

    model@performance$fit <- predict(model, data_original, "metric")
    model@performance$fit.structure <- computeStructure(model, data_original)

  return(model)
}






