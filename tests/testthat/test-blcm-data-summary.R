test_that("prepared data validates labels and excludes held-out truth from JAGS", {
  data <- blcm_two_class_fixture(); data$A[25] <- 1
  prepared <- prepare_blcm_data(data, c("A", "B"), feature_columns = c("f1", "f2", "f3", "f4"))
  expect_equal(prepared$jags_data$N, 26)
  expect_equal(prepared$jags_data$P, 4)
  expect_true(is.na(prepared$jags_data$z[25]))
  expect_equal(prepared$observed_class[25], "A")
  bad <- data; bad$A[1] <- 0
  expect_error(prepare_blcm_data(bad, c("A", "B"), feature_columns = c("f1", "f2")), "exactly one")
  bad <- data; bad$f1[1] <- 2
  bad <- data; bad$A[1:12] <- 0; bad$B[1:12] <- 1
  expect_error(prepare_blcm_data(bad, c("A", "B"), feature_columns = c("f1", "f2")), "every configured")
})

test_that("posterior summaries calculate probabilities, entropy, ties and FZEC", {
  data <- blcm_two_class_fixture(); data$A[25] <- 1
  prepared <- prepare_blcm_data(data, c("A", "B"), feature_columns = c("f1", "f2"))
  fit <- structure(list(draws = blcm_synthetic_draws()), class = "blcm_fit")
  result <- summarize_blcm_predictions(fit, prepared, groups = list(human = "A", animal = "B"))
  expect_equal(result$predictions$A_probability, c(0.5, 0.25))
  expect_equal(result$predictions$predicted_class, c("A", "B"))
  expect_equal(result$predictions$normalized_entropy[1], 1)
  expect_false(result$predictions$FZEC[1])
  expect_equal(result$validation$accuracy, 1)
  expect_equal(unname(result$validation$confusion_matrix["A", "A"]), 1)
  boundary_draws <- coda::mcmc(matrix(c(2, 1, 2, 2, 2), ncol = 1, dimnames = list(NULL, "z[25]")))
  boundary_fit <- structure(list(draws = boundary_draws), class = "blcm_fit")
  boundary_data <- prepared; boundary_data$prediction_rows <- 25L
  boundary <- summarize_blcm_predictions(boundary_fit, boundary_data, groups = list(human = "A", animal = "B"))
  expect_true(boundary$predictions$FZEC)
})
