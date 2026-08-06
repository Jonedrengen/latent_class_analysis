test_that("CLI parses sampling arguments and validates them", {
  input <- tempfile(fileext = ".csv"); file.create(input)
  parsed <- blcm_input_parser(c("--input", input, "--output", tempdir(), "--chains", "2", "--iterations", "400", "--burn-in", "200"))
  expect_equal(parsed$chains, 2L)
  expect_error(blcm_sampling_options(iterations = 10, burn_in = 10), "greater")
})

test_that("seeded runner writes prediction CSV and self-contained report", {
  skip_if_not_installed("rjags"); skip_if_not_installed("rmarkdown")
  data <- blcm_two_class_fixture(); input <- tempfile(fileext = ".csv"); utils::write.csv(data, input, row.names = FALSE)
  output <- tempfile(); dir.create(output)
  options <- list(input = input, output = output, seed = 67L, chains = 2L, iterations = 400L, burn_in = 200L, thin = 1L)
  result <- run_blcm(options, class_columns = c("A", "B"), metadata_columns = character())
  scores <- utils::read.csv(file.path(output, "pred_scores.csv"), check.names = FALSE)
  expect_true(file.exists(file.path(output, "blcm_report.html")))
  expect_equal(nrow(scores), 2)
  expect_true(all(c("A_probability", "B_probability", "predicted_class", "normalized_entropy") %in% names(scores)))
  expect_equal(rowSums(scores[, c("A_probability", "B_probability")]), c(1, 1))
  expect_equal(stats::setNames(scores$predicted_class, scores$Sample_Name), blcm_expected_held_out())
  html <- paste(readLines(file.path(output, "blcm_report.html"), warn = FALSE), collapse = "")
  expect_match(html, "Bayesian Latent Class Model Report")
  expect_match(html, "Input summary")
  expect_match(html, "Predicted-class counts")
})

test_that("report data preserves added sections and identifies unavailable truth", {
  data <- blcm_two_class_fixture()
  prepared <- prepare_blcm_data(data, c("A", "B"), feature_columns = c("f1", "f2"))
  fit <- structure(list(sampling = blcm_sampling_options()), class = "blcm_fit")
  summary <- structure(list(predictions = data.frame(predicted_class = "A", normalized_entropy = 0.2, FZEC = NA), validation = list(accuracy = NA_real_, confusion_matrix = NULL, n_truth = 0L)), class = "blcm_summary")
  report <- build_blcm_report_data(prepared, fit, summary, validations = list("Extra diagnostic" = data.frame(value = 1)), plots = list("Extra plot" = ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()))
  expect_equal(report$validations[["Extra diagnostic"]]$value, 1)
  expect_true(is.na(summary$validation$accuracy))
  expect_true("Extra plot" %in% names(report$plots))
})

test_that("R Markdown template renders an HTML BLCM report", {
  skip_if_not_installed("rmarkdown")
  output <- tempfile(fileext = ".html")
  report <- list(
    input = list(samples = 1L, training = 0L, prediction = 1L, features = "f1", classes = c("A", "B")),
    sampling = blcm_sampling_options(),
    predictions = data.frame(predicted_class = "A", normalized_entropy = 0.2, FZEC = NA),
    validations = list("Accuracy" = list(accuracy = NA_real_, confusion_matrix = NULL, n_truth = 0L)),
    plots = list()
  )
  render_blcm_report(report, output, file.path(project_root, "src", "blcm_report.Rmd"))
  expect_true(file.exists(output))
  expect_match(paste(readLines(output, warn = FALSE), collapse = ""), "Bayesian Latent Class Model Report")
})
