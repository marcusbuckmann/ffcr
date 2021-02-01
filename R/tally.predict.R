setGeneric("prediction", function(object, data.input, ...) standardGeneric("prediction"))
setMethod("prediction", signature("tallyModel"),
          function(object, data.input, return.metric = T, random = T, lazy = F, weights = c(1,1)){
            criterion <- getCriterion(data.input)
            cues <- getCues(data.input)
            output <- Tallytest(object, cues)
            if(return.metric)
              return(computePerformance(criterion,output, random = random, weights = weights))
            else
              return(output)
          }
)


#### #' @rdname tallyModel
#' Predictions from a fftr objects
#'
#' Return predicted responses or measures of performance from a fitted \linkS4class{tallyModel-class} object
#' @param object an S4 object of class \linkS4class{tallyModel-class} created by \code{\link{tally}} function
#' @param newdata a data frame or matrix containing new data
#' @param type character string denoting the type of output returned. \code{response} (default) returns the predicted class label, \code{probability} returns a matrix of class probabilities, \code{metric} returns the classification performance.
#' @export
setMethod("predict", signature("tallyModel"),
          function(object, newdata, type = "response"){

            type <- match.arg(type,c("response","probability", "metric"))
            formula.terms <- terms(object@formula)
            train.names <- attr(formula.terms,"term.labels")
            newcues <- newdata[,train.names, drop = F]
            output <- Tallytest(object, newcues)
            if(type == "probability"){
              out.final <- cbind(1 - output,output)
              colnames(out.final) <- object@class_labels
            }
            if(type == "response"){
              out.final <- ifelse(output >= 0.5, object@class_labels[2],object@class_labels[1])
              out.final <- as.factor(out.final)
            }
            if(type == "metric"){
              criterion <- model.frame(formula = object@formula, data = newdata, na.action = NULL)[,1]
              class_labels <- as.character(sort(unique(criterion)))
              criterion <- ifelse(as.character(criterion) == class_labels[2], 1,0)

              out_final <- computePerformance(criterion, output, threshold = .5, random = F)
            }
            return(out_final)
          }
)
