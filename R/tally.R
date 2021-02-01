#' class for tallying models
#'
#' An S4 class to represent a fast-and-frugal tree model
#'@slot call an image of the call that produced the object.
#'@slot type induction algorithm used to train the fast-and-frugal tree.
#'@slot performance list showing fitting and cross-validated performance.
#'@slot formula \link[stats]{formula} object of the model.
#'@slot weights a numeric vector of length 2. The first entry denotes the weight of instances in the positive class, the second entry the weight of instances in the negative class.
#'@slot training_data data that was used to train the model
#'@slot tally representation of the tallying model
#'@slot prior the proportion of objects in the positive class in the training set.
#'@slot class_labels a vector of length 2 containing the class labels. The second entry is referred to as the positive class.
#'@slot split_function how numeric features are split when 'basic' method is used.
#'@export
setClass("tallyModel", representation(
  call = "call",
  type = "list",
  performance = "list",
  formula = "formula",
  weights = "numeric",
  training_data = "data.frame",
  tally = "list",
  prior = "numeric",
  class_labels = "character",
  split_function = "character"),
  prototype(formula = formula(NULL), type = list(algorithm = "empty")
  )
)


#'\code{tally} is used to fit a tallying model.

#'@param data an object of class \code{\link[base]{data.frame}} or \code{\link[base]{matrix}}. The criterion can either be a factor with two levels or an integer \code{(0,1)}. The \emph{positive class} is the second factor level (\code{levels(data$criterion)[2]}), or \code{1} if the criterion is numeric.
#'@param formula \code{\link[stats]{formula}} (optional). If \code{formula} is not provided, the first column of the data argument is used as the response variable and all other columns as predictors.
#'@param method type of induction method for the fast-and-frugal tree:
#' \itemize{
#' \item{regression}
#' \item{cross-entropy}
#' }
#'@param split_function Which function should be used to determine the splitting values on numeric features. This only applies to tallying models trained with the 'regression' method.
#' to the By default Gini entropy ('gini') is used. Other options are Shannono entropy ('entropy') and 'median'.
#'@param max_size set maximum number of features that contribute to the tallying model (default:  6)
#'@param weights a numeric vector of length 2 (default: \code{c(1,1)}). The first entry specifies the weight of instances in the positive class, the second entry the weight of instances in the negative class.
#'(\emph{see examples}).
#'@param cv If \code{TRUE} 10-fold cross validation is used to estimate the predictive performance of the model.
#'@param cross_entropy_parameters hyperparameters for the cross-entropy method. By default the output of the function \code{\link{cross_entropy_control}} is passed.

#'@return A \linkS4class{tallyModel} object.

#'@examples
#' data(liver)
#' model <- tally(data = liver, formula = diagnosis~.)
#' model
#'
#' # weight instances by the inverse of the prior
#' # in this way both classes contribute equally when training the model

#' prior <- mean(ifelse(liver$diagnosis == "Liver disease", 1, 0))
#' weights <- c(1/prior, 1/(1-prior))
#' mod <- tally(data = liver, formula = diagnosis~., weights = weights)
#'


#' @export
setGeneric("tally", function(data,
                             formula = stats::as.formula(data.frame(data)),
                             method = "basic",
                             split_function = "gini",
                             weights = c(1,1),
                             cv = FALSE,
                             max_size = 6,
                             cross_entropy_parameters = cross_entropy_control()
) standardGeneric("tally"))
#' @rdname tally
setMethod("tally", signature(data = "data.frame"),
          function(data,
                   formula = stats::as.formula(data.frame(data)),
                   method = "basic",
                   split_function = "gini",
                   weights = c(1,1),
                   cv = FALSE,
                   max_size = 6,
                   cross_entropy_parameters = cross_entropy_control())
          {

            data <- model.frame(formula = formula, data = data, na.action = NULL)

            if(cv){

              folds <- createFolds(nrow(data))
              nfolds <- length(unique(folds[,1]))
              pred <- rep(NA,nrow(data))

              for(i in 1:nfolds){
                test.ix <- folds[,1] == i

                model <- buildTallying(data = data[!test.ix,],
                                       method = method,
                                       split_function = split_function,
                                       weights = weights,
                                       max_size = max_size,
                                       cross_entropy_parameters = cross_entropy_parameters
                )
                pred[test.ix] <- predict(model,data[test.ix,],type = "probability")[,2]
              }

              criterion <- data[,1]
              class_labels <- as.character(sort(unique(criterion)))
              criterion <- ifelse(as.character(criterion) == class_labels[2], 1,0)
              prediction.clean <- computePerformance(criterion, pred, threshold = .5, random = F)
            }

            model <- buildTallying(data = data,
                                   method = method,
                                   split_function = split_function,
                                   weights = weights,
                                   max_size = max_size,
                                   cross_entropy_parameters = cross_entropy_parameters
            )

            cl <- match.call(expand.dots = TRUE)
            cl$data <- substitute(data, parent.frame())
            cl[[1]] <- as.name("tally")
            cl[["formula"]] <- substitute(formula, parent.frame())

            model@call <- cl
            if(cv){
              model@performance$cv.performance <- prediction.clean
            }
            return(model)
          })

#' @rdname tally
setMethod("tally", signature(data = "matrix"),
          function(data,
                   formula = stats::as.formula(data.frame(data)),
                   method = "basic",
                   split_function = "gini",
                   weights = c(1,1),
                   cv = FALSE,
                   max_size = 6,
                   cross_entropy_parameters = cross_entropy_control()
          )
          {
            data <- data.frame(data)
            model <- buildTallying(data = data,
                                   method = method,
                                   split_function = split_function,
                                   weights = weights,
                                   max_size = max_size,
                                   cross_entropy_parameters = cross_entropy_parameters
            )


            cl <- match.call(expand.dots = TRUE)
            cl$data <- substitute(data, parent.frame())
            cl[[1]] <- as.name("tally")
            cl[["formula"]] <- substitute(formula, parent.frame())

            model@call <- cl
            return(model)


          })

buildTallying <- function(data,
                          method = "regression",
                          split_function = "gini",
                          max_size = 6,
                          costs = c(.5,.5),
                          weights = c(1,1),
                          cross_entropy_parameters = cross_entropy_control()
){
  data.original <- data
  criterion <- getCriterion(data)
  class_labels <- as.character(sort(unique(criterion)))
  prior <- getPrior(data)
  if(length(class_labels)>2 || length(class_labels) == 1)
    stop("The class label variable has to have exactly 2 distinct values")

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
    weights <- weights / ((weights * c(prior, 1 - prior))[1] * 2) #scale weights correctly
    costs <- getCostsFromWeights(weights, prior)
  }


  method <- match.arg(method, c("basic", "regression", "cross-entropy"))

  if(method %in% c("basic", "regression")){
    splitted <- dichotomize_data(data, splittingFunction = split_function, weights = c(.5,.5))
    data_bin <- splitted$data
  }
  if( method == "basic"){
    model_list <- lapply(1:min(c(max_size, ncol(data_bin)-1)), function(x) tallyfromCorrelation(data_bin, transform = "tally", importance_metric = "balanced", costs = costs, ncues = x)) # approach of the book
  }
  if(method == "regression"){
    lasso_model_binary <- buildLogisticLassoFullLambdaSequence(data_bin, costs = costs)
    model_list <- lapply(1:min(c(max_size, ncol(data_bin) - 1)), function(x) binaryTallyFromGlmnet(lasso_model_binary, data_bin, costs = costs, ncues = x))
  }
  if(method %in% c("basic", "regression")){
    p1 <- sapply(model_list, function(x) predicting(x,data_bin, weights)["Accuracy"])
    model_list <- model_list[[which.max(p1)]]

    tally <- list()
    tally$intercept <- model_list[1]
    tally$weights <- model_list[-1]
    tally$matrix <- splitted$split_matrix
    tally$categorical <- splitted$categorical

    model <- new("tallyModel", tally = tally)
    model@type$algorithm = method

    model@prior <- prior
    model@formula <- stats::formula(data)

  }

  if(method == "cross-entropy"){
    model <- do.call(tally_cross_entropy_multiple_starts, c(
      list(data),
      maximum_size = ifelse(is.null(max_size), 6, max_size),
      costs = list(costs),
      cross_entropy_parameters
    ))
  }

  model@class_labels <- as.character(class_labels)
  model@training_data <- data
  model@weights <- weights
  model@type$algorithm = method
  model@performance$fit <- predict(model, data, "metric")
  return(model)
}

