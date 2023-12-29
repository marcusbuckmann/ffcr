findMode <- function(x){
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  max.ix <- which(tab == max(tab))
  if(length(max.ix)>1)
    max.ix <- sample(max.ix)[1]

  return(ux[max.ix])
}


splitCue <- function(criterion, cue, splittingFunction, weights = c(1,1), randomTie = T) {
  if (hasNoVariance(cue))
    return(rep(NA,5))

  if (!is.numeric(cue))
    return(splitCategoricalCue(criterion,cue,splittingFunction, weights = weights))

  splits <- makeCueSplits(criterion,cue)
  splits[,c(2,4)] <- splits[,c(2,4)] * weights[1]
  splits[,c(3,5)] <- splits[,c(3,5)] * weights[2]

  splits.proportion <- makeProportionSplits(splits[,-1,drop = F])
  splits.splittingcriterion <- splittingFunction(splits.proportion = splits.proportion, splits[,-1,drop = F])

  split_select <- which(splits.splittingcriterion == min(splits.splittingcriterion, na.rm = T))
  if (length(split_select) > 1){
    if(randomTie){
      split_select <- sample(split_select)[1]
    } else {
      split_select <- split_select[1] # we cannot allow random tie here because otherwise the category information might be different from the split as both are requested at a differnt poitn of time
    }

  }

  best_split <- splits[split_select,]

  if (split_select < nrow(splits))
    best_split[1] <- (splits[split_select,1] + splits[split_select + 1,1]) / 2
  if (split_select == nrow(splits) & nrow(splits) > 1){
    best_split[1] <- (best_split[1] + max(cue, na.rm = T)) / 2
  }

  if (nrow(splits) == 1)
    best_split[1] <- (min(cue, na.rm = T) + max(cue, na.rm = T)) / 2
  names(best_split) <- c("threshold", ">+", ">-", "<=+", "<=-")
  return(best_split)
}

splitCategoricalCue <- function(criterion, cue,
                                    splittingFunction,
                                    return.categories.split = FALSE,
                                    weights = c(1,1)){
  if(is.numeric(cue) | hasNoVariance(cue))
    return(NA)

  unique_factor_values <- as.character(unique(cue))
  dummy.confusion.table <- t(sapply(unique_factor_values, function(x)splitCue(criterion,ifelse(cue==x,1,0),splittingFunction, weights = weights)))
  proportion_class_pos <- dummy.confusion.table[,2]/(dummy.confusion.table[,2]+dummy.confusion.table[,3])
  order.factor <- order(proportion_class_pos)
  names.factor.order <- rownames(dummy.confusion.table)[order.factor]
  cue <- as.character(cue)
  cue.out<-rep(0,length(cue))
  for(i in 1:length(names.factor.order))
    cue.out[cue==names.factor.order[i]] <- i

  splitting.point <- splitCue(criterion,cue.out,splittingFunction, weights = weights, randomTie = F)
  splitting.point.category <- splitting.point[1]
  splitting.point[1] <- 0


  ix.smaller.categories <- 1:floor(splitting.point.category)
  smaller.categories <- names.factor.order[ix.smaller.categories]
  if(return.categories.split)
    return(smaller.categories)

  return(splitting.point)
}


dPrimeFromConfusionMatrix <- function(splits.proportion,splits.input,...){
  positives <- splits.input[,1]/(splits.input[,1,drop=F]+splits.input[,3,drop=F])
  negatives <- splits.input[,2]/(splits.input[,2,drop=F]+splits.input[,4,drop=F])
  #adjusting extreme values so that qnorm can be computed
  positives[positives == 1] <- ((splits.input[,1]-.5)/splits.input[,1])[positives==1]
  positives[positives == 0] <- ((.5)/splits.input[,3])[positives==0]
  negatives[negatives == 1] <-  ((splits.input[,2]-.5)/splits.input[,2])[negatives==1]
  negatives[negatives == 0] <- ((.5)/splits.input[,4])[negatives==0]
  total <- 1/abs(stats::qnorm(positives)-stats::qnorm(negatives))
  return(total)
}

balErrFromConfusionMatrix <- function(splits.proportion,splits.input,...){
  positives <- splits.input[,1]/(splits.input[,1,drop=F]+splits.input[,3,drop=F])
  negatives <- splits.input[,2]/(splits.input[,2,drop=F]+splits.input[,4,drop=F])
  #adjusting extreme values so that qnorm can be computed
  total <- 1/abs((positives)-(negatives))
  return(total)
}

giniFromConfusionMatrix <- function(splits.proportion, ...){
  left.gini <- binaryGini(splits.proportion[,1])
  right.gini <- binaryGini(splits.proportion[,2])
  gini.total <- left.gini * splits.proportion[,3] + right.gini * (1 - splits.proportion[,3])
  return(gini.total)
}


binaryMB <- function(p, prm = 4){
  p <- ifelse(p<.5,1-p,p)
  p1 <- -((2^prm)/2)*(p-.5)^prm + .5
  maxp <- .5
  p2<- ifelse(p1>maxp,maxp -p1 + maxp ,p1)
  return(p2)
}


equalFromConfusionMatrix <- function(splits.proportion,...){
  left <- splits.proportion[,1]
  right <- splits.proportion[,2]

  total <- 1/abs(left-right)
  return(total)
}

entropyFromConfusionMatrix <- function(splits.proportion,...){
  left.entropy <- binaryEntropy(splits.proportion[,1])
  right.entropy <- binaryEntropy(splits.proportion[,2])
  entropy.total <- left.entropy*splits.proportion[,3]+right.entropy * (1 - splits.proportion[,3])

  return(entropy.total)

}

accuracyFromConfusionMatrix <- function(splits.proportion,...){
  left <- splits.proportion[,1]
  right <- splits.proportion[,2]
  left <- pmax(left,1-left)
  right <- pmax(right,1-right)
  validity <- left * splits.proportion[,3] + right * (1 - splits.proportion[,3])
  validity <- 1/validity
  return(validity)
}

hellingerFromConfusionMatrix <- function(splits.proportion,splits.input){

  n1 <- splits.input[,1]+ splits.input[,3]
  n1L <- splits.input[,1]
  n1R <- splits.input[,3]
  n0 <- splits.input[,2]+ splits.input[,4]
  n0L <- splits.input[,2]
  n0R <- splits.input[,4]

  sum1 <- sqrt(n0L/n0)-sqrt(n1L/n1)
  sum2 <- sqrt(n0R/n0)-sqrt(n1R/n1)
  hellinger <- sqrt(sum1^2+sum2^2)
  hellinger <- 1-(hellinger/sqrt(2))
  return(hellinger)
}

medianFromConfusionMatrix<-function(splits.proportion,...){
  close.to.median <- abs(.5-splits.proportion[,3])
  return(close.to.median)
}

makeProportionSplits <- function(cue.splits){
  cue.splits.proportion.one.left <- cue.splits[,1,drop=F] / (cue.splits[,1,drop=F] + cue.splits[,2,drop=F])
  cue.splits.proportion.one.right <- cue.splits[,3,drop=F] / (cue.splits[,3,drop=F] + cue.splits[,4,drop=F])
  proportion.left.to.right <- (cue.splits[,1,drop=F]+cue.splits[,2,drop=F]) / rowSums(cue.splits)
  splits.1left.1right.lefttoright<-cbind(cue.splits.proportion.one.left,
                                         cue.splits.proportion.one.right,
                                         proportion.left.to.right)

  colnames(splits.1left.1right.lefttoright) <- c("prop.class1.left","prop.class1.right", "proportion.objects.left")
  return(splits.1left.1right.lefttoright)
}

makeCueSplitsNoC <- function(criterion,cue,laplace=F){
  # just for debug purposes, not used
  confusion.List <- NULL
  unique.cue.values <- sort(unique(cue))
  unique.cue.values <- unique.cue.values[-length(unique.cue.values)]
  confusion.matrix.cue <- t(sapply(unique.cue.values,function(x) as.numeric(laplace) + makeConfusionMatrix(criterion,cue,x))) #here we can also use the C version
  return(cbind(unique.cue.values,confusion.matrix.cue))
}

makeCueSplits<-function(criterion,cue,laplace=F){
  #makeCueSplitsC only works with ordered cue

  ix.na <- is.na(cue)
  criterion <- criterion[!ix.na]
  cue <- cue[!ix.na]
  ox <- order(cue)
  cue <- cue[ox]
  criterion <- criterion[ox]
  return(makeCueSplitsC(criterion,cue))
}



makeConfusionMatrix <- function(criterion,cue.input,threshold){
  bigger.threshold <- cue.input > threshold
  criterion.bigger.threshold <- criterion[bigger.threshold]
  criterion.smaller.threshold <- criterion[!bigger.threshold]
  a <- sum(criterion.bigger.threshold == 1)
  b <- length(criterion.bigger.threshold) - a
  c <- sum(criterion.smaller.threshold == 1)
  d <- length(criterion.smaller.threshold) - c
  return(c(a,b,c,d))
}

binaryGini <- function(p){
  return(p * (1-p) * 2)
}

binaryEntropy <- function(p){
  out <- - p * log2(p) - (1 - p) * log2(1 - p)
  out[is.na(out)] <- 0
  return(out)
}




mbFromConfusionMatrix <- function(splits.proportion, prm = 4, ...){
  left.gini <- binaryMB(splits.proportion[,1], prm = prm)
  right.gini <- binaryMB(splits.proportion[,2], prm = prm)
  gini.total <- left.gini * splits.proportion[,3] + right.gini * (1 - splits.proportion[,3])
  return(gini.total)
}


charleyFromConfusionMatrix <- function(splits.proportion, prm = c(1,1), ...){
  # fill in charleys function

  left.out <- SMentropy(splits.proportion[,1],prm[1],prm[2])
  right.out <- SMentropy(splits.proportion[,2],prm[1],prm[2])
  out.total <- left.out * splits.proportion[,3] + right.out * (1 - splits.proportion[,3])
  return(out.total)
}



