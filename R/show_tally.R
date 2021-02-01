

#' prints tallying model
#'
#'@param object An object of type \linkS4class{tallyModel-class}
setMethod("show", signature("tallyModel"),
          function(object) {

            cat("Tallying object\n")
            cat("  type:", dQuote(object@type$algorithm),"\n")

            cat("\nCall: \n")
            print(object@call)
            cat("\nFormula: \n")
            print(object@formula, showEnv = FALSE)
            cat("\nReasons:")
            showTally(object)

            if(length(object@performance$fit)>0){
              cat("\n")
              cat("\nFitted values:\n")
              counts <- c(object@performance$fit["True positives"],
                          object@performance$fit["False positives"],
                          object@performance$fit["False negatives"],
                          object@performance$fit["True negatives"])
              tab <- data.frame("   Observed" = paste0("   ",rep(rev(object@class_labels),2)),
                         Predicted = rep(rev(object@class_labels),each = 2),
                         N=counts, check.names = FALSE)
              # center column names
              name_width <- max(sapply(names(tab)[1:2], nchar))
              names(tab)[1:2] <- format(names(tab)[1:2], width = name_width, justify = "centre")
              print(tab, row.names = FALSE)

              performance_train <- object@performance$fit
              cat("\nFitting:")
              tab <- data.frame(" " = paste0("   ", names(performance_train)), "  " = format(round(performance_train,2)))
              colnames(tab) <- c(" ", "  ")
              print(tab[1:6, ], row.names = FALSE, right = FALSE)
            }

            if(length(object@performance$cv.performance)>0){
              cat("\n")
              cat("Cross-validation:")
              performance_cv <- object@performance$cv.performance
              tab <- data.frame(" " = paste0("   ", names(performance_cv)), "  " = format(round(performance_cv,2)))
              colnames(tab) <- c(" ", "  ")
              print(tab[1:6, ], row.names = FALSE, right = FALSE)
            }
          }
)


showTally <- function(model,...){
  weights <- model@tally$weights
  intercept <- model@tally$intercept
  # we show tallying as a strictly positive sum, therefore we have to adjust intercept
  transformed_intercept <- intercept - sum(weights == -1)
  model_matrix <- model@tally$matrix
  category.information <- model@tally$categorical
  out.spaces <- "  "
  ix <- weights!=0
  n.cues <- sum(ix)
  cue_names <- rownames(model_matrix)

  if(nrow(model@tally$matrix) == 0){
    cat("Empty model:\n")
    cat("Prediction:",round(model@prior,4), "\n")
  } else {

    adder <- rep("  + ",length(ix))
    adder[1] <-  "    "
    thresholds <- c()
    comparators <- c()


    for(i in 1:length(ix)){
      if(!ix[i])
        next


      if(is.na(category.information[[i]][1])){
        comparators <- c(comparators, ifelse(weights[i] < 0," <= ", " > "))
        thresholds <- c(thresholds, round(model_matrix[i,"splitPoint"],3))
      }
      else{
        levels <- levels(model@training_data[,cue_names[i]])
        if(weights[i]==1){
          levels.out <- levels[!levels %in% category.information[[i]]]
        } else {
          levels.out <- levels[levels %in% category.information[[i]]]
        }
        comparators <- c(comparators, " = ")
        thresholds = c(thresholds, paste(levels.out, collapse = ", "))
      }
    }


    tab <- data.frame(paste0("  + ", cue_names[ix]),
                      comparators,
                      thresholds)
    colnames(tab) <- c(" ", "  ", "   ")
    print(tab, row.names = FALSE, right = FALSE)




    cat("   ____________________________________ \n")
    cat(paste0("Predict ",model@class_labels[2], " if at least ",(-transformed_intercept), " reasons hold. \n"))
  }
}
