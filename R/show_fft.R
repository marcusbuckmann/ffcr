
#' prints fast-and-frugal tree
#'
#'@param object An object of type \linkS4class{fftreeModel}
setMethod("show", signature("fftreeModel"),
          function(object) {

            cat("Fast-and-frugal Tree object\n")
            cat("Trained with :", dQuote(object@parameters$algorithm), "method. \n")

            cat("\nCall: \n")
            print(object@call)
            cat("\nFormula: \n")
            print(object@formula, showEnv = FALSE)
            cat("\nTree: \n")
            showTree(object, probabilities = F)


            performance_train <- object@performance$fit
            cat("\n")
            cat("\nFitted values:\n")
            counts <- c(performance_train["True positives"],
                        performance_train["False positives"],
                        performance_train["False negatives"],
                        performance_train["True negatives"])
            tab <- data.frame("   Observed" = paste0("   ",rep(rev(object@class_labels),2)),
                              Predicted = rep(rev(object@class_labels),each = 2),
                              N=counts, check.names = FALSE)
            # center column names
            name_width <- max(sapply(names(tab)[1:2], nchar))
            # names(tab)[1:2] <- format(names(tab)[1:2], width = name_width, justify = "centre")
            print(tab, row.names = FALSE, right = FALSE)


            cat("\nFitting performance:")
            tab <- data.frame(" " = paste0("   ", names(performance_train)), "  " = format(round(performance_train,2)))
            colnames(tab) <- c(" ", "  ")
            print(tab[1:5, ], row.names = FALSE, right = FALSE) # we do not show all metrics

            structure_train <- object@performance$fit.structure
            tab <- data.frame(" " = paste0("   ", names(structure_train)), "  " = format(round(structure_train,2)))
            colnames(tab) <- c(" ", "  ")
            print(tab, row.names = FALSE, right = FALSE)


            if(length(object@performance$cv.performance) > 0){
              cat("\n")
              cat("Cross-validation performance:")
              performance_cv <- object@performance$cv.performance
              tab <- data.frame(" " = paste0("   ", names(performance_cv)), "  " = format(round(performance_cv,2)))
              colnames(tab) <- c(" ", "  ")
              print(tab[1:5, ], row.names = FALSE, right = FALSE)

              structure_cv <- object@performance$cv.structure
              tab <- data.frame(" " = paste0("   ", names(structure_cv)), "  " = format(round(structure_cv,2)))
              colnames(tab) <- c(" ", "  ")
              print(tab, row.names = FALSE, right = FALSE)
            }
          }


)


showTree <- function(model, probabilities = F, weights = c(1,1),...){

  model <- updateTree(model, data.input = model@training_data, changeSide = F, changePrediction = F, weights = weights)
  model.matrix <- model@tree$matrix
  category_information <- model@tree$categorical
  out.spaces <- "  "
  n.cues <- nrow(model.matrix)
  cue.names <- rownames(model.matrix)
  cue.names[n.cues] <- cue.names[n.cues-1]
  if(nrow(model@tree$matrix) == 0){
    cat("Empty tree:\n")
    cat("Prediction:", round(model@prior,4), "\n")
  } else {

    cat("Reason / Prediction / (Proportion of class '",model@class_labels[2],"') / (Number of objects classified)\n\n", sep = "")
    for(i in 1:n.cues){

      cue.name <- cue.names[i]
      threshold <- round(model.matrix[i,"splitPoint"],3)
      direction <- ifelse(model.matrix[i,"side"] == 1," > ", " <= ")

      out.pred <- model.matrix[i,"exit"]

      out.prob <- ifelse(model.matrix[i,"side"] == 1,
                         model.matrix[i,">+"] / sum(model.matrix[i,c(">+", ">-")]),
                         model.matrix[i,"<=+"] / sum(model.matrix[i,c("<=+", "<=-")])
      )
      out.prob <- format(round(out.prob, 2), nsmall = 2)

      out.label <- ifelse(out.pred >= 0.5 ,1,0)
      out.label <- model@class_labels[out.label+1]

      n.objects <- ifelse(direction == " > ",sum(model.matrix[i,3:4]),sum(model.matrix[i,5:6]))
      out.prob <- paste(" (",out.prob,")",sep = "")
      n.objects <- paste(" (",round(n.objects,2), ")", sep="")

      if(is.na(category_information[[i]][1])){
        string.out <- paste(out.spaces, cue.name, direction, threshold, ": ", out.label,out.prob,n.objects, "\n", sep = "")
      }
      else{
        levels <- levels(model@training_data[,cue.name])
        if(model.matrix[i,"side"] == 1){
          levels.out <- levels[!levels %in% category_information[[i]]]
        } else{
          levels.out <- category_information[[i]]
        }
        string.out <- paste(out.spaces, cue.name, " = ", paste(levels.out, collapse = ", "), ": ",out.prob, n.objects, "\n", sep = "")
      }
      out.spaces <- paste(out.spaces," ")
      cat(string.out)
    }
  }
}

###### ' @rdname fftreeModel
#' plots fast-and-frugal tree
#'
#'@param x An object of type \linkS4class{fftreeModel-class}
#'@param legend If \code{TRUE} legend is shown.
#'@param probabilities If \code{TRUE} probability estimates are shown for each leaf. If \code{FALSE}, predicted class labels are shown.
#'@param class_labels If \code{TRUE} class labels are shown for each leaf.
#'@param colors Vector of length 2 to set the colors of the two classes. Default: \code{c("cornflowerblue""brown3")}.
#'@param ... optional parameters passed to low level function
#'@export
setMethod("plot", signature("fftreeModel"), function(x, probabilities = FALSE, legend = TRUE, class_labels = FALSE, colors  = c("brown3", "cornflowerblue"),...){
  plotFFT(x, probabilities = probabilities, showLegend = legend, show_label = class_labels, showBox = probabilities,  colPos = colors[1], colNeg = colors[2], ...)
}
)

plotFFT <- function(model, weights = c(1,1), probabilities = F, showLegend = TRUE, showBox = FALSE, branchlab = TRUE, colPos = "brown3", colNeg = "cornflowerblue", show_label = FALSE, show_observations = FALSE){

  colPos = colPos
  colNeg = colNeg

  mar.old <- graphics::par()$mar
  graphics::par(mar = c(0,0,0,0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0,1),ylim = c(0,1))

  m <- model@tree$matrix
  depth <- nrow(m)
  category_information <- model@tree$categorical
  cue.names <- rownames(m)

  cue.thresholds <- m[,"splitPoint"]
  cue.sides <- m[,"side"]
  cue.exits <- m[,"exit"]
  if(!probabilities)
    cue.exits <- ifelse(cue.exits>=.5,1,0)

  tx <- 1
  if(depth>7)
    ix <- 1 - (depth-7)*.1

  y.delta <- - 1/(1.35*depth+2)
  laby.delta <- min(c(abs(y.delta / 5), .02))
  fracy.delta <- min(c(abs(y.delta / 5), .02))

  y.space <- abs(1 / depth / 3)
  y.space <- min(c(0.1, y.space))

  x.range <- cumsum(m[,"exit"]*2-1)
  x.min <- min(x.range)
  x.max <- max(x.range)

  x.delta <- 1 / (3 + 2 * (x.max - x.min))
  x.delta <- min(c(x.delta,1.3*abs(y.delta))) # set maximum ratio of y and x delta.
  current.x <- .5 + ((x.min + x.max)/3)*x.delta

  if(showBox){
    bWidth <- x.delta*.5
    bHeight <- abs(y.delta * .7)
  } else {
    bWidth <- x.delta*.5
    bHeight <- abs(x.delta * .35)
  }

  current.y <- 1 - .5 * y.space

  class_labels <- model@class_labels
  if(showLegend){
    leg.x <- ifelse(current.x>.5, .05, .6)
    graphics::legend(x = leg.x, y = 1,  legend = rev(class_labels), col = c("black","black"),bty ="n", pch = c(22,22), pt.bg = c(colPos, colNeg), cex = 1, pt.cex = 1.5)
  }

  for(i in 1:(depth)){
    side <- cue.sides[i]
    exit <- ifelse(cue.exits[i]>=.5,1,0)

    if(is.na(category_information[[i]][1])){

      gs <- ifelse(xor(exit,side),"<=",">")
      node <- paste(shortName(cue.names[i]), gs, round(cue.thresholds[i],3))
    } else{
      levels <- levels(model@training_data[,cue.names[i]])
      levels.out <- levels[!levels %in% category_information[[i]]]
      gs <- ifelse(xor(exit,side),"!=","=")

      node <- paste(shortName(cue.names[i]),gs, shortName(paste(levels.out, collapse = ", "), factors = T))
    }


    if(i < depth){
      graphics::text(x = current.x, y = current.y+.5*y.space, labels =  node, font = 2, cex = .9 * tx)
      graphics::segments(x0 = current.x,x1 = current.x - x.delta, y0= current.y, y1 = current.y + y.delta)
      graphics::segments(x0 = current.x,x1 = current.x+x.delta, y0= current.y, y1 = current.y + y.delta)
      if(i < 2 | branchlab){
        graphics::text(x = current.x+ .5*x.delta+.03, y = current.y + .5 * y.delta, label = "yes", cex = .7*tx)
        graphics::text(x = current.x- .5*x.delta-.03, y = current.y + .5 * y.delta, label = "no", cex = .7*tx)
      }
    }

    if(side == 1){
      out.frac <- c(m[i,">+"],m[i,">-"])
    } else {
      out.frac <- c(m[i,"<=+"],m[i,"<=-"])
    }

    out.prop <- out.frac[1]/sum(out.frac)
    if(exit  == 1){

      if(showBox){
        graphics::rect(xleft = current.x + x.delta, xright = current.x + x.delta + bWidth, ybottom = current.y + y.delta, ytop = current.y + y.delta - bHeight, col = colPos, border = NA)
        graphics::rect(xleft = current.x + x.delta, xright = current.x + x.delta + bWidth, ytop = current.y + y.delta, ybottom = current.y + y.delta - bHeight * (1 - out.prop), col = colNeg, border = NA)
        graphics::rect(xleft = current.x + x.delta, xright = current.x + x.delta + bWidth, ytop = current.y + y.delta, ybottom = current.y + y.delta - bHeight, col = NULL) # border only
        if(show_observations)
        graphics::text(x = current.x + x.delta + .5 * bWidth, y = current.y + y.delta - bHeight - fracy.delta, label = paste(round(out.frac[1],2),"/",round(sum(out.frac),2)), cex = .8*tx)
      } else {
        graphics::rect(xleft = current.x + x.delta, xright = current.x + x.delta + bWidth, ybottom = current.y + y.delta, ytop = current.y+y.delta - bHeight, col = colPos, border = NA)

      }

      if(probabilities){
        prob.out <- format(round((out.frac[1])/(sum(out.frac)),2),nsmall = 2)
        graphics::text(x = current.x + x.delta + .5 * bWidth, y = current.y+y.delta + laby.delta, label = prob.out, font = 4, cex = .9 * tx) # plot class label
      }
      else if (show_label){
        graphics::text(x = current.x + x.delta + .5 * bWidth, y = current.y+y.delta + laby.delta, label = class_labels[2], font = 4, cex = .9 * tx) # plot class label
      }

    }
    if(exit == 0){
      if(showBox){
        graphics::rect(xleft = current.x - x.delta, xright = current.x - x.delta - bWidth,ytop = current.y + y.delta, ybottom = current.y + y.delta - bHeight, col = colPos, border = NA)
        graphics::rect(xleft = current.x - x.delta, xright = current.x - x.delta - bWidth,ytop = current.y + y.delta, ybottom = current.y + y.delta - bHeight*(1-out.prop), col = colNeg, border = NA)
        graphics::rect(xleft = current.x - x.delta, xright = current.x - x.delta - bWidth,ytop = current.y + y.delta, ybottom = current.y + y.delta - bHeight, col = NULL)
        if(show_observations)
          graphics::text(x = current.x - x.delta - .5 * bWidth, y = current.y+y.delta - bHeight - fracy.delta, label = paste(round(out.frac[1],2),"/",round(sum(out.frac),2)), cex = .8 *tx)

      } else {
        graphics::rect(xleft = current.x - x.delta, xright = current.x - x.delta - bWidth,ytop = current.y + y.delta, ybottom = current.y + y.delta - bHeight, col = colNeg, border = NA)
      }

      prob.out <- format(round((out.frac[1])/(sum(out.frac)),2),nsmall = 2)
      if (probabilities){
        graphics::text(x = current.x - x.delta - .5 * bWidth, y = current.y+y.delta + laby.delta, label = prob.out, font = 4, cex = .9 * tx) # plot class label
      }
      else if(show_label){
        graphics::text(x = current.x - x.delta - .5 * bWidth, y = current.y+y.delta + laby.delta, label = class_labels[1], font = 4, cex = .9 * tx) # plot class label
      }


    }
    if(i < depth - 1){

      current.x <- current.x - (exit * 2 -1) *  x.delta
      current.y <- current.y + y.delta - y.space
    }


  }
  # if(depth == i){
  #   label.last = class_labels[2]
  #   if(side == 1){
  #     out.frac <- c(m[i,">+"], m[i,">-"])
  #   } else {
  #     out.frac <- c(m[i,"<=+"], m[i,"<=-"])
  #   }
  #   out.prop <- out.frac[1]/sum(out.frac)
  #
  #   if(showBox){
  #     graphics::rect(xleft = current.x + x.delta, xright = current.x + x.delta + bWidth, ybottom = current.y+y.delta, ytop = current.y + y.delta - bHeight, col = colPos, border = NA)
  #     graphics::rect(xleft = current.x + x.delta, xright = current.x + x.delta + bWidth, ytop = current.y+y.delta, ybottom = current.y + y.delta - bHeight*(1 - out.prop), col = colNeg, border = NA)
  #     graphics::rect(xleft = current.x + x.delta, xright = current.x + x.delta + bWidth, ytop = current.y+y.delta, ybottom = current.y + y.delta - bHeight, col = NULL) # border only
  #     if(show_observations)
  #       graphics::text(x = current.x + x.delta + .5 * bWidth, y = current.y+y.delta - bHeight - fracy.delta, label = paste(round(out.frac[1],2),"/",round(sum(out.frac),2)), cex = .8*tx)
  #   } else {
  #     graphics::rect(xleft = current.x + x.delta, xright = current.x + x.delta + bWidth, ybottom = current.y+y.delta, ytop = current.y + y.delta - bHeight, col = colPos, border = NA)
  #   }
  #   if(probabilities){
  #
  #     prob.out <- format(round((out.frac[1])/(sum(out.frac)),2),nsmall = 2)
  #     graphics::text(x = current.x + x.delta + .5 * bWidth, y = current.y+y.delta + laby.delta, label = prob.out, font = 4, cex = .9 * tx) # plot class label
  #   } else if (show_label){
  #     graphics::text(x = current.x + x.delta + .5 * bWidth, y = current.y+y.delta + laby.delta, label = label.last, font = 4, cex = .9 * tx) # plot class label
  #   }
  #
  #
  #   mlast <- m[depth :(depth+1),]
  #   ix.last <- which(side == mlast[,"side"])
  #   # label.last <- ifelse(mlast[ix.last,"exit"]>=.5, class_labels[1], class_labels[2])
  #   label.last <- class_labels[1]
  #
  #   if(showBox){
  #     graphics::rect(xleft = current.x - x.delta, xright = current.x - x.delta - bWidth,ytop = current.y + y.delta, ybottom = current.y + y.delta - bHeight, col = colPos, border = NA)
  #     graphics::rect(xleft = current.x - x.delta, xright = current.x - x.delta - bWidth,ytop = current.y + y.delta, ybottom = current.y + y.delta - bHeight*(1-out.prop), col = colNeg, border = NA)
  #     graphics::rect(xleft = current.x - x.delta, xright = current.x - x.delta - bWidth,ytop = current.y + y.delta, ybottom = current.y + y.delta - bHeight, col = NULL)
  #     if(show_observations)
  #       graphics::text(x = current.x - x.delta - .5 * bWidth, y = current.y+y.delta - bHeight - fracy.delta, label = paste(round(out.frac[1],2),"/",round(sum(out.frac),2)), cex = .8 *tx)
  #   } else {
  #     graphics::rect(xleft = current.x - x.delta, xright = current.x - x.delta - bWidth,ytop = current.y + y.delta, ybottom = current.y + y.delta - bHeight, col = colNeg, border = NA)
  #   }
  #   if(probabilities){
  #     prob.out <- format(round((out.frac[1])/(sum(out.frac)),2),nsmall = 2)
  #     graphics::text(x = current.x - x.delta - .5 * bWidth, y = current.y + y.delta + laby.delta, label = prob.out, font = 4, cex = .9 * tx) # plot class label
  #   } else if (show_label) {
  #
  #     graphics::text(x = current.x - x.delta - .5 * bWidth, y = current.y + y.delta + laby.delta, label = label.last, font = 4, cex = .9 * tx) # plot class label
  #   }
  # }




  graphics::par(mar = mar.old)
}

shortName <- function(x, factors = F){
  if(nchar(x) <= 17)
    return(x)

  if(factors){
    if(substr(x, nchar(x), nchar(x)) == " ") # remove space
      x <- substr(x, 1, nchar(x)-1)
    if(substr(x, nchar(x), nchar(x)) == ",") # remove comma
      x <- substr(x, 1, nchar(x)-1)

    xcom <- as.vector(gregexpr(",",x)[[1]])
    if(max(xcom)>15){
      if(min(xcom) <=15){
        xcom <- max(xcom[xcom<=15])
        out <- paste0(strtrim(x,xcom-1),", ...")
      } else {
        out <- paste0(strtrim(x,15-1),", ...")

      }
    } else {
      out <- paste0(strtrim(x,15),"..")

    }

  } else { # no factor
    out <- paste0(strtrim(x,15),"..")
  }
  return(out)
}


