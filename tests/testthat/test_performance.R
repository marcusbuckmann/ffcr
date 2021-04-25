# context("test evaluation of performance")

test_that("givenModelTestPredictions", {
  set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")
  data.set <- replicate(3,stats::runif(100))
  data.set[ ,1] <- round(data.set[ ,1])
  model <- fftree(data.frame(data.set), method = "basic")
  test_object1 <- c(1, .4, .2)
  test_object2 <- c(0, .2, .3)
  test_object3 <- c(1, .4, .3)
  test_object4 <- c(1, .2, .2)
  test_set <- data.frame(rbind(test_object1,test_object2,test_object3,test_object4))
  correct_prediction <- c(model@tree$matrix[1,"exit"],
                          model@tree$matrix[2,"exit"],
                          model@tree$matrix[3,"exit"],
                          model@tree$matrix[1,"exit"])
  expect_equal(predict(model, test_set, type = "numeric")[,2], correct_prediction)
}
)


test_that("performanceMetrics", {
  set.seed(10, kind = "Mersenne-Twister", normal.kind = "Inversion")
  tolerance <- 10 ^ -3
  criterion <- round(stats::runif(100))
  cue <- round(stats::runif(100))
  totest <- computePerformance(criterion, cue)

  # accuracy
  expect_lt(abs(totest["Accuracy"] - sum(cue == criterion)/length(criterion)), tolerance)
  # true positive rate
  expect_lt(abs(totest["Sensitivity"]- sum(criterion==1&cue==1)/sum(criterion==1)), tolerance)
  # false positive rate
  expect_lt(abs(totest["Specificity"] - (sum(criterion==0&cue==0)/sum(criterion==0))), tolerance)

  # balanced accuracy
  expect_lt(abs(totest["Balanced accuracy"] - (sum(criterion==1&cue==1)/sum(criterion==1)+
                                        sum(criterion==0&cue==0)/sum(criterion==0))/2),tolerance)
  #cross table metrics
  expect_equal(as.integer(totest["True positives"]), as.integer(sum(criterion==1 & cue==1)))
  expect_equal(as.integer(totest["False positives"]), as.integer(sum(criterion==0 & cue==1)))
  expect_equal(as.integer(totest["True negatives"]), as.integer(sum(criterion==0 & cue==0)))
  expect_equal(as.integer(totest["False negatives"]), as.integer(sum(criterion==1 & cue==0)))
}
)





