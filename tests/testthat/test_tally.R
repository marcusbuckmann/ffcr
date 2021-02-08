test_that("consistent_calls", {
  # we can call the tallying function in different ways but they should all give the same results
  model1 <- tally(liver, method = "basic", max_size = 4)
  data_matrix <- liver
  data_matrix$diagnosis <- ifelse(data_matrix$diagnosis == "Liver disease", 1, 0)
  data_matrix$sex <- ifelse(data_matrix$sex == "Female", 1, 0)

  model2 <- tally(as.matrix(data_matrix), method = "basic", max_size = 4)
  model3 <- tally(diagnosis ~., data = liver, method = "basic", max_size = 4)
  expect_equal(model1@tally$weights, model1@tally$weights)
  expect_equal(model1@tally$weights,model3@tally$weights)

  expect_equal(model1@tally$matrix, model1@tally$matrix)
  expect_equal(model1@tally$matrix,model3@tally$matrix)
}
)



test_that("run_methods_without_error", {
  expect_error(tally(liver), NA)
  expect_error(tally(liver, method = "basic"), NA)
  expect_error(tally(liver, method = "regression"), NA)
  df <- liver
  df$sex <- ifelse(df$sex == "Female", 1,0)
  expect_error(tally(df, method = "cross-entropy", cross_entropy_parameters = cross_entropy_control(iterations = 10, starts = 2, threads = 2)), NA)

  }
)
