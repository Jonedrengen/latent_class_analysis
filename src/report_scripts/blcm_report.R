build_blcm_report_data <- function(
  blcm_data,
  fit,
  summary,
  validations = list(),
  plots = list()
) {
  defaults <- list("Accuracy" = summary$validation)

  list(
    input = list(
      samples = nrow(blcm_data$raw_data),
      training = sum(blcm_data$training),
      prediction = length(blcm_data$prediction_rows),
      features = blcm_data$feature_columns,
      classes = blcm_data$class_columns
    ),
    sampling = fit$sampling,
    predictions = summary$predictions,
    validations = c(defaults, validations),
    plots = plots
  )
}

render_blcm_report <- function(report_data, output_file, template_file) {
  rmarkdown::render(
    template_file,
    output_file = basename(output_file),
    output_dir = dirname(output_file),
    params = list(report = report_data),
    envir = new.env(parent = globalenv()),
    quiet = TRUE,
    output_options = list(self_contained = TRUE)
  )

  invisible(output_file)
}
