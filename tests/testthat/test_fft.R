test_that("consistent_calls", {
  # we can call the fftree function in differnt ways but they sohuld all give the same results
  data(liver)
  model1 <- fftree(liver, max_depth = 4)
  data_matrix <- liver
  data_matrix$diagnosis <- ifelse(data_matrix$diagnosis == "Liver disease", 1, 0)
  data_matrix$sex <- ifelse(data_matrix$sex == "Female", 1, 0)

  model2 <- fftree(as.matrix(data_matrix), max_depth = 4)
  model3 <- fftree(diagnosis ~., data = liver, max_depth = 4)
  expect_equal(model1@tree$matrix,model2@tree$matrix)
  expect_equal(model1@tree$matrix,model3@tree$matrix)
}
)

test_that("randomExit", {
  #random order of equally good cues
  criterion <- c(1,1,1,1,0,0,0,0)
  cueA <- c(0,0,0,1,1,1,1,0)
  expect_true(stats::sd(replicate(100,fftree(data.frame(cbind(criterion,cueA)), method = "basic")@tree$matrix[1,"side"]))>0)
  expect_true(stats::sd(replicate(100,fftree(data.frame(cbind(criterion,cueA)), method = "greedy")@tree$matrix[1,"side"]))>0)
}
)


test_that("categoricalVSBinary", {
  #test that categorical and binary coded features yield exactly the same models
  set.seed(3, kind = "Mersenne-Twister", normal.kind = "Inversion") # with this seed, cues are not equivalent in their predictive power
  criterion <- round(stats::runif(111))
  cueA <- round(stats::runif(111))
  cueB <- round(stats::runif(111))
  data_set <- data.frame(cbind(criterion,cueA,cueB))
  data_set_cat <- data.frame(criterion,cueA = as.factor(cueA), cueB = as.factor(cueB))
  #expect equal exit structure
  model_bin <- fftree(data_set, method = "greedy")
  model_cat <- fftree(data_set_cat, method = "greedy")
  expect_equal(model_bin@tree$matrix[,"exit"], model_cat@tree$matrix[,"exit"])
  #expect equal fitting accuracy
  expect_equal(model_bin@performance$fit,model_cat@performance$fit)

  #same procedure but of higher complexity
  set.seed(3, kind = "Mersenne-Twister", normal.kind = "Inversion") # with this seed, cues are not equivalent in their predictive power
  cueA <- round(stats::runif(111)*3)
  cueB <- round(stats::runif(111)*3)
  criterion <- stats::runif(111) + cueA - cueB
  criterion <- ifelse(criterion > median(criterion),0,1)
  data_set <- data.frame(cbind(criterion, cueA, cueB))
  data_set_cat <- data.frame(criterion, cueA = as.factor(cueA), cueB = as.factor(cueB))
  model_bin <- fftree(data_set, method = "greedy")
  model_cat <- fftree(data_set_cat, method = "greedy")
  # expect same exists
  expect_equal(model_bin@tree$matrix[,"exit"], model_cat@tree$matrix[,"exit"])
  #expect equal fitting accuracy
  expect_equal(model_bin@performance$fit, model_cat@performance$fit)
}
)


test_that("run_methods_without_error", {
  expect_error(fftree(liver), NA)
  expect_error(fftree(liver, method = "basic"), NA)
  expect_error(fftree(liver, method = "greedy"), NA)
  df <- liver
  df$sex <- ifelse(df$sex == "Female", 1,0)
  expect_error(fftree(df, method = "cross-entropy", cross_entropy_parameters = cross_entropy_control(iterations = 10, starts = 2, threads = 2)), NA)
}
)


