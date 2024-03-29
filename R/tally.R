#' class for tallying models
#'
#' An S4 class to represent a tallying tree model
#'@slot call An image of the call that produced the object.
#'@slot formula \link[stats]{formula} object of the model.
#'@slot parameters Parameters used to train the tallying model.
#'@slot performance List showing fitting and cross-validated performance.
#'@slot class_labels A vector of length 2 containing the class labels. The first entry is referred to as the negative class, the second entry is referred to as the positive class.
#'@slot weights A numeric vector of length 2. The first entry denotes the weight of instances in the negative class, the second entry the weight of instances in the positive class.
#'@slot prior The proportion of objects in the positive class in the training set.
#'@slot tally Representation of the tallying model
#'@slot split_function How numeric features are split when 'basic' method is used.
#'@slot training_data Data that was used to train the model
#'@export
setClass("tallyModel", representation(
  call = "call",
  formula = "formula",
  parameters = "list",
  performance = "list",
  class_labels = "vector",
  weights = "numeric",
  prior = "numeric",
  tally = "list",
  split_function = "character",
  training_data = "data.frame"),
  prototype(formula = formula(NULL), parameters = list(algorithm = "empty")
  )
)

#'\code{tally} is used to fit a tallying model.

#'@param data An object of class \code{\link[base]{data.frame}} or \code{\link[base]{matrix}}. The response variable can either be a factor with two levels or an integer vector with values \code{0,1}.
#'@param formula \code{\link[stats]{formula}} (optional). If \code{formula} is not provided, the first column of the data argument is used as the response variable and all other columns as predictors.
#'@param method Type of induction method for the fast-and-frugal tree:
#' \itemize{
#' \item{basic} (default)
#' \item{cross-entropy}
#' }
#'@param max_size Maximum number of features that contribute to the tallying model (default: 6)
#'@param split_function Function should be used to determine the splitting values on numeric features. This only applies to tallying models trained with the 'basic' method. By default Gini entropy ('gini') is used. Other options are Shannono entropy ('entropy') and 'median'.
#'@param weights A numeric vector of length 2 (default: \code{c(1,1)}) with weights assigned to instances in the two classes. The vector entries should be named by the class labels. If they are not, the first entry refers to the negative class, the second entry to the positive class.
#'(\emph{see examples}).
#'@param cv If \code{TRUE} 10-fold cross validation is used to estimate the predictive performance of the model. By default, pruning is not used.
#'@param cross_entropy_parameters Hyperparameters for the cross-entropy method. By default the output of the function \code{\link{cross_entropy_control}} is passed.

#'@return A \linkS4class{tallyModel} object.

#'@examples
#' data(liver)
#' model <- tally(data = liver, formula = diagnosis~.)
#' model
#'
#' # weight instances by the inverse of the prior
#' # in this way both classes contribute equally when training the model

#' prior <- mean(ifelse(liver$diagnosis == "Liver disease", 1, 0))
#' weights <- c("No liver disease" = prior, "Liver disease" = 1-prior)
#' mod <- tally(data = liver, formula = diagnosis~., weights = weights)
#'


#' @export
setGeneric("tally", function(data,
                             formula = stats::as.formula(data.frame(data)),
                             method = "basic",
                             max_size = 6,
                             split_function = "gini",
                             weights = c(1,1),
                             cv = FALSE,
                             cross_entropy_parameters = cross_entropy_control()
) standardGeneric("tally"))
#' @rdname tally
setMethod("tally", signature(data = "data.frame"),
          function(data,
                   formula = stats::as.formula(data.frame(data)),
                   method = "basic",
                   max_size = 6,
                   split_function = "gini",
                   weights = c(1,1),
                   cv = FALSE,
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
                pred[test.ix] <- predict(model, data[test.ix,],type = "numeric")[,2]
              }

              criterion <- getCriterion(data)
              class_labels <- sort(unique(criterion))
              if(!all(is.numeric(criterion))){
                class_labels <- as.character(class_labels)
                criterion <- ifelse(as.character(criterion) == class_labels[2], 1,0)
              } else {
                criterion <- ifelse(criterion == max(criterion), 1,0)
              }

              prediction.clean <- computePerformance(criterion, pred, threshold = .5)
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
                   max_size = 6,
                   split_function = "gini",
                   weights = c(1,1),
                   cv = FALSE,
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
                          weights = c(1,1),
                          cross_entropy_parameters = cross_entropy_control()
){
  data.original <- data
  criterion <- getCriterion(data)
  class_labels <- sort(unique(criterion))
  if(!all(is.numeric(criterion))){
    class_labels <- as.character(class_labels)
    criterion <- ifelse(as.character(criterion) == class_labels[2], 1,0)
  } else {
    criterion <- ifelse(criterion == max(criterion), 1,0)
  }
  data[,1] <- criterion




  prior <- getPrior(data)
  if(length(class_labels)>2 || length(class_labels) == 1)
    stop("The class label variable has to have exactly 2 distinct values")

  if(any(is.na(data)))
    stop("The data contains missing values. This method does not support that.")



  if(all(is.null(names(weights)))){
    names(weights) <- class_labels
  }
  costs <- c(weights[as.character(class_labels[2])], weights[as.character(class_labels[1])]) # user input: first negative, then positive class; code works with the revered order. We should not change that as class_labels are ordered (negative, positive)
  rescaled_weights <- getWeightsFromCost(costs, prior)

  method <- match.arg(method, c("basic", "regression", "cross-entropy"))

  if(method %in% c("basic", "regression")){
    splitted <- dichotomize_data(data, splittingFunction = split_function)
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
    p1 <- sapply(model_list, function(x) predicting(x, data_bin, rescaled_weights)["Accuracy"])
    model_list <- model_list[[which.max(p1)]]

    tally <- list()
    tally$intercept <- model_list[1]
    tally$weights <- model_list[-1]
    tally$matrix <- splitted$split_matrix
    tally$categorical <- splitted$categorical
    model <- new("tallyModel", tally = tally)
  }

  if(method == "cross-entropy"){
    if (!all(sapply(data, class) %in% c("numeric", "integer"))){
      stop("The cross-entropy method only works with numeric features. Please recode categorical variables to 0/1 binary indicators.")
    }
    model <- do.call(tally_cross_entropy_multiple_starts, c(
      list(data),
      maximum_size = ifelse(is.null(max_size), 6, max_size),
      costs = list(costs),
      cross_entropy_parameters
    ))
  }
  model@prior <- prior
  model@formula <- stats::formula(data)
  model@class_labels <- class_labels
  model@training_data <- data
  model@weights <- weights
  model@parameters$algorithm = method
  model@performance$fit <- predict(model, data, "metric")
  return(model)
}

