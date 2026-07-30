test_that("blcm_input_parser passes the input option through", {
  input_path <- tempfile(fileext = ".csv")
  file.create(input_path)
  output_path <- tempdir()

  options <- blcm_input_parser(c("--input", input_path, "--output", output_path))

  expect_identical(options$input, input_path)
})

test_that("blcm_input_parser passes the output option through", {
  input_path <- tempfile(fileext = ".csv")
  file.create(input_path)
  output_path <- tempfile()
  dir.create(output_path)

  options <- blcm_input_parser(c("--input", input_path, "--output", output_path))

  expect_identical(options$output, output_path)
})
