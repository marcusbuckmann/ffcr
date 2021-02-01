NULL
#' class for split object
#'
#' An S4 class to represent a split object
#'@slot call an image of the call that produced the object.
#'@slot type splitting function employed to split cues.
#'@slot formula \link[stats]{formula} object of the model.
#'@slot weights a numeric vector of length 2. The first entry denotes the weight of instances in the positive class, the second entry the weight of instances in the negative class.
#'@slot training_data data that was used to train the model
#'@slot splits representation of the splits
#'@slot prior the proportion of objects in the positive class in the training set.
#'@slot class_labels a vector of length 2 containing the class labels. The second entry is referred to as the positive class.

#' @export
setClass("splits", representation(type = "character",
                                 splits = "list",
                                 prior = "numeric",
                                 training_data = "data.frame",
                                 class_labels = "character",
                                 weights = "numeric",
                                 call = "call",
                                 formula = "formula"),
         prototype(formula = formula(NULL),weights = c(1,1))
)

#'Finding split points for cues
#'
#'\code{splitCues} is used to find split points for numeric and categorical cues.

#' @param data an object of class \code{\link[base]{data.frame}} or \code{\link[base]{matrix}}. The criterion can either be a factor with two levels or an integer \code{(0,1)}. The \emph{positive class} is the second factor level (\code{levels(data$criterion)[2]}), or \code{1} if the criterion is numeric.
#' @param formula \code{\link[stats]{formula}} (optional). If \code{formula} is not provided, the first column of the data argument is used as the response variable and all other columns as predictors.
#' @param splits specifies the method used to find a splitting point on numeric and binary cues.
#' \itemize{
#' \item{gini (default)}
#' \item{entropy}
#' \item{median}
#' }
#'@param weights a numeric vector of length 2 (default: \code{c(1,1)}). The first entry specifies the weight of instances in the positive class, the second entry the weight of instances in the negative class.
#'@param ... optional parameters passed to low level function
#'@examples
#' data(liver)
#' splits <- splitCues(data = liver, formula = diagnosis~., splits = "median")
#'@return A \linkS4class{splits} object.
#' @export
setGeneric("splitCues", function(data,formula = stats::as.formula(data), ...) standardGeneric("splitCues"))
#' @rdname splitCues
setMethod("splitCues", signature(data = "data.frame"),
          function(data,
                   formula = as.formula(data.frame(data)),
                   splits = "gini",
                   weights = c(1,1),
                   ...)
          {

            data <- model.frame(formula = formula, data = data, na.action = NULL)
            split_profile <- splitCuesInternal(data = data,
                                               formula = formula,
                                               splits = splits,
                                               weights = weights,
                                               ...)
            cl <- match.call(expand.dots = TRUE)
            cl$data <- substitute(data, parent.frame())
            cl[[1]] <- as.name("splitCues")
            cl[["formula"]] <- substitute(formula, parent.frame())
            split_profile@call <- cl
            return(split_profile)

          })


splitCuesInternal <- function(data,
                              formula = as.formula(data.frame(data)),
                              splits = "gini",
                              weights = c(1,1),
                              costs = c(.5,.5), ...){

  prior <- getPrior(data)
  if(!all(costs == c(.5,.5))){
    weights <- getWeightsFromCost(costs, prior)
  }
  # if weights are defined use them to overwrite costs
  if(!all(weights == c(1,1))){
    weights <- weights/((weights*c(prior,1 - prior))[1]*2) #scale weights correctly
    costs <- getCostsFromWeights(weights, prior)
  }

  class_labels <- as.character(sort(unique(data[,1])))
  data[,1] <- ifelse(as.character(data[,1]) == class_labels[2], 1,0)
  split_profile <- findSplits(data.input = data,
                              splittingFunction = splits,
                              weights = weights,
                              ...)
  split_profile@type <- splits
  split_profile@formula <- formula(data)
  split_profile@class_labels <- class_labels
  split_profile@weights <- weights
  return(split_profile)


}

#' @rdname splitCues
setMethod("splitCues", signature(data = "matrix"),
          function(data,
                   formula = stats::as.formula(data.frame(data)),
                   splits = "gini",
                   weights = c(1,1),
                   ...)
          {
            data <- data.frame(data)
            split_profile <- splitCues(data.input = data, formula = formula,
                                       splits = splits,
                                       weights = weights,
                                       ...)


            cl <- match.call(expand.dots = TRUE)
            cl$data <- substitute(data, parent.frame())
            cl[[1]] <- as.name("fftr")
            cl[["formula"]] <- substitute(formula, parent.frame())

            split_profile@call <- cl
            return(split_profile)
          })


#' prints splits
#'
#'@param object An object of type \linkS4class{splits-splits}
setMethod("show", signature("splits"),
          function(object) {
            cat("Cues split by ")
            cat(dQuote(object@type),"\n")
            cat("\nCall: \n")
            print(object@call)
            cat("\nFormula: \n")
            print(object@formula, showEnv = FALSE)

            cat("\n")
            cat("\n")
            m <- object@splits$matrix
            category_information <- object@splits$categorical
            cue.names <- rownames(m)
            cue.thresholds <- m[,"splitPoint"]
            n <- nrow(m)
            for(i in 1:n){
              r <- m[i,]
              if(is.na(category_information[[i]][1])){
                node <- paste(cue.names[i], ">", round(cue.thresholds[i],3))
              } else{
                levels <- levels(object@training_data[,cue.names[i]])
                levels.out <- levels[!levels %in% category_information[[i]]]
                node <- paste(cue.names[i],"=", paste(levels.out, collapse = ", "))
              }

              confusion.matrix <- array(NA, dim=c(2,2))
              confusion.matrix[1,] <- c(r[">+"],r[">-"])
              confusion.matrix[2,] <- c(r["<=+"], r["<=-"])
              colnames(confusion.matrix) <- c("yes","no")
              rownames(confusion.matrix) <- c(paste("Class",object@class_labels[2]),
                                              paste("     ",object@class_labels[1]))

              cat("      ",node,"?","\n", sep = "")
              print(confusion.matrix)
              cat("\n")
            }
          }
)

findSplits <- function(data.input, splittingFunction = "gini", weights = c(1,1)){

  splittingFunctionName <- splittingFunction

  splittingFunction <- switch(splittingFunction,
                              "median" = medianFromConfusionMatrix,
                              "gini" = giniFromConfusionMatrix,
                              "accuracy" = accuracyFromConfusionMatrix,
                              "equal" = equalFromConfusionMatrix,
                              "d-prime" = dPrimeFromConfusionMatrix,
                              "entropy" = entropyFromConfusionMatrix,
                              "balErr"  = balErrFromConfusionMatrix,
                              stop("no valid splitting function")
  )

  prior <- getPrior(data.input)
  criterion <- getCriterion(data.input)
  class_labels <- as.character(sort(unique(criterion)))
  criterion <- ifelse(as.character(criterion) == class_labels[2], 1,0)

  cues <- getCues(data.input)

  best_splits <- t(sapply(cues,function(x) splitCue(criterion, x, splittingFunction, weights = weights)))
  best_splits <- cbind(1:(ncol(data.input)-1), best_splits) # add cue ID
  non_empty_cues <- stats::complete.cases(best_splits)
  best_splits <- best_splits[non_empty_cues,,drop=F]

  # save information on factor levels for categorical cues
  category_information <- lapply(cues, function(x) {
    splitCategoricalCue(criterion, x, splittingFunction, return.categories.split = T, weights = weights)
  })
  category_information <- category_information[non_empty_cues]

  colnames(best_splits) <- c("Cue","splitPoint",">+",">-","<=+","<=-")
  splits <- new("splits", type = splittingFunctionName,
                splits = list(matrix = best_splits,
                              categorical = category_information, n.cues = nrow(best_splits)),
                prior = prior,
                training_data = data.input,
                class_labels = class_labels)
  return(splits)
}


