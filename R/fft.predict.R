###### #' @rdname fftreeModel
#' Predictions from a fftree objects
#'
#' Return predicted responses or measures of performance from a fitted \code{\link{fftreeModel-class}} object
#' @param object an S4 object of class \code{\link{fftreeModel-class}} created by \code{\link{fftree}} function
#' @param newdata a data frame or matrix containing new data
#' @param type character string denoting the type of output returned. \code{response} (default) returns the predicted class label, \code{metric} returns the classification performance and \code{frugality} returns the average number of nodes visisted until a decision was made.
#' @param ... additional arguments that are only used if the function is called internally
#' @export
setMethod("predict", signature("fftreeModel"),
          function(object, newdata, type = "response", ...){

            type <- match.arg(type,c("response", "metric","frugality", "numeric"))

            if(!isTRUE(all.equal(object@formula, formula(NULL)))){
              formula_terms <- terms(object@formula)
              train_names <- attr(formula_terms,"term.labels")
              newcues <- newdata[,train_names, drop = F]
              if(type == "metric"){
                criterion <- model.frame(formula = object@formula, data = newdata, na.action = NULL)[,1]
                criterion <- ifelse(as.character(criterion) == object@class_labels[2], 1,0)
              }
            } else { # this functionality is used internally
                criterion <- newdata[,1]
                newcues <- newdata[,-1, drop = F]
            }

            output <- FFTtest(object, newcues)

            if(type == "numeric"){
              out_final <- cbind(1 - output,output)
              colnames(out_final) <- object@class_labels
            }
            if(type == "response"){
              out_final <- ifelse(output >= 0.5, object@class_labels[2],object@class_labels[1])
            }
            if(type == "metric"){
              out_final <- computePerformance(criterion, output, ...)
            }
            if(type == "frugality"){
              out_final <- FFTtest(object, newcues, return.frugality = T)
            }
            return(out_final)
          }
)


auroc <- function(observed, score) {
  observed <- observed == 1
  n1 <- sum(!observed)
  n2 <- sum(observed)
  U  <- sum(rank(score)[!observed]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}


computePerformance <- function(criterion, predicted, threshold = .5, random = FALSE, weights = c(1,1)){

  predicted[is.na(predicted)] <- stats::runif(sum(is.na(predicted)))
  predicted.t <- ifelse(predicted > threshold,1,0)
  if(random & threshold %=% .5)
    predicted.t[predicted == threshold] <- round(stats::runif(sum(predicted==threshold)))

  tp <- as.numeric(sum(predicted.t == 1 & criterion == 1)) * weights[1]
  fp <- as.numeric(sum(predicted.t == 1 & criterion == 0)) * weights[2]
  tn <- as.numeric(sum(predicted.t == 0 & criterion == 0)) * weights[2]
  fn <- as.numeric(sum(predicted.t == 0 & criterion == 1)) * weights[1]
  accuracy <- (tp + tn) / (tp + fp + tn + fn)
  tp.rate <- tp/ (tp + fn)
  fp.rate <- fp/ (tn + fp)
  balanced <- (tp.rate+(1-fp.rate))/2
  f1 <- 2 * tp/(2 * tp + fp + fn)
  performance <- c(accuracy, tp.rate, 1-fp.rate, balanced, f1, tp, fp, tn, fn)
  names(performance) <- c("Accuracy", "Sensitivity", "Specificity", "Balanced accuracy", "F1 score", "True positives", "False positives", "True negatives", "False negatives")
  return(performance)
}


computeStructure <- function(object, test.data){
  depth <- nrow(object@tree$matrix) - 1
  cues <- length(unique(object@tree$matrix[,1]))
  frugality <- predict(object,test.data, type = "frugality")
  output <- c(depth,cues,frugality)
  names(output) <- c("Depth", "Features", "Frugality")
  return(output)
}
