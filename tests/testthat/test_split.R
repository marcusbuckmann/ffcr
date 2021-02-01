test_that("findSplit", {
  criterion <- c(0,0,0,1,1,1)
  cue <- c(1,2,3,4,5,6)
  output <- c(3.5,3,0,0,3)
  names(output) <- c("threshold", ">+", ">-", "<=+", "<=-")
  expect_equal(splitCue(criterion,cue,giniFromConfusionMatrix), output)
  expect_equal(splitCue(criterion,cue,medianFromConfusionMatrix), output)
  expect_equal(splitCue(criterion,cue,accuracyFromConfusionMatrix), output)
}
)

test_that("findCategoricalSplit", {
  criterion <- c(0,0,0,1,1,1)
  cue <- as.factor(c(1,2,3,4,5,6))
  output <- c(0,3,0,0,3)
  names(output) <- c("threshold", ">+", ">-", "<=+", "<=-")
  expect_equal(splitCue(criterion,cue,giniFromConfusionMatrix),output )
  expect_equal(splitCue(criterion,cue,medianFromConfusionMatrix), output)
  expect_equal(splitCue(criterion,cue,accuracyFromConfusionMatrix), output)
  expect_equal(splitCategoricalCue(criterion,cue,giniFromConfusionMatrix,return.categories.split=T),c("1","2","3"))
  expect_equal(splitCategoricalCue(criterion,cue,medianFromConfusionMatrix,return.categories.split=T),c("1","2","3"))
  expect_equal(splitCategoricalCue(criterion,cue,accuracyFromConfusionMatrix,return.categories.split=T),c("1","2","3"))
}
)

test_that("transformCategorical", {
  criterion <- c(0,0,0,1,1,1)
  cue <- as.factor(c(1,2,3,4,5,6))
  splits <- findSplits(data.frame(criterion,cue),"gini")
  expect_equal(transformCategoricalCues(cue,splits@splits$categorical[[1]]),c(0,0,0,1,1,1))
}
)





