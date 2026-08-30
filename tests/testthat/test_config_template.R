

test_that("mcmc_configuration correctly stores params", {
  expect_type(config$mcmc_jags_configuration, "list")
  expect_length(config$mcmc_jags_configuration, 6)
  expect_equal(config$mcmc_jags_configuration$save_params, c("theta", "pi", "z"))
  expect_equal(config$mcmc_jags_configuration$chains, 4L)
})

test_that("data_column_configuration correctly stores params", {
  expect_type(config$data_column_configuration, "list")
  expect_equal(config$data_column_configuration$id_column, "Sample_Name")
  expect_equal(config$data_column_configuration$training_column, "training")
  expect_equal(config$data_column_configuration$class_columns_prefix, "^CLA_")
  expect_equal(config$data_column_configuration$feature_columns_prefix, "^FEA_")
})

test_that("plot_configuration correctly stores params", {
  expect_type(config$plot_configuration, "list")
})
