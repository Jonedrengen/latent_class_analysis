blcm_script_dir <- function() {
  candidates <- c(file.path(getwd(), "src"), getwd(), file.path(getwd(), "..", "src"), file.path(getwd(), "..", "..", "src"))
  candidates <- candidates[file.exists(file.path(candidates, "blcm_data.R"))]
  if (length(candidates)) return(normalizePath(candidates[[1L]]))
  file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(file)) return(dirname(normalizePath(file)))
  file_arg <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[grepl("^--file=", commandArgs(trailingOnly = FALSE))])
  if (length(file_arg)) return(dirname(normalizePath(file_arg)))
  normalizePath("src")
}

.blcm_dir <- blcm_script_dir()
source(file.path(.blcm_dir, "blcm_data.R"))
source(file.path(.blcm_dir, "blcm_fit.R"))
source(file.path(.blcm_dir, "blcm_summary.R"))
source(file.path(.blcm_dir, "blcm_report.R"))
source(file.path(.blcm_dir, "plots", "generate_histogram.R"))
source(file.path(.blcm_dir, "plots", "blcm_report_plots.R"))

blcm_input_parser <- function(argv = commandArgs(trailingOnly = TRUE)) {
  if (!requireNamespace("optparse", quietly = TRUE)) blcm_abort("optparse is required to parse BLCM options")
  options <- list(
    optparse::make_option(c("-i", "--input"), type = "character", required = TRUE),
    optparse::make_option(c("-o", "--output"), type = "character", required = TRUE),
    optparse::make_option("--seed", type = "integer", default = 67L),
    optparse::make_option("--chains", type = "integer", default = 4L),
    optparse::make_option("--iterations", type = "integer", default = 2000L),
    optparse::make_option("--burn-in", type = "integer", default = 1000L),
    optparse::make_option("--thin", type = "integer", default = 1L)
  )
  parsed <- optparse::parse_args(optparse::OptionParser(option_list = options), args = argv)
  parsed$burn_in <- parsed[["burn-in"]]
  if (!file.exists(parsed$input)) blcm_abort("can't parse --input, Input file does not exist or can't be accessed")
  if (!dir.exists(parsed$output)) blcm_abort("can't parse --input, output dir does not exist")
  blcm_sampling_options(parsed$seed, parsed$chains, parsed$iterations, parsed$burn_in, parsed$thin)
  parsed
}

run_blcm <- function(options, class_columns = default_blcm_classes(), id_column = "Sample_Name", training_column = "training", metadata_columns = "MLST") {
  data <- utils::read.csv(options$input, check.names = FALSE, stringsAsFactors = FALSE)
  prepared <- prepare_blcm_data(data, class_columns, id_column, training_column, metadata_columns = intersect(metadata_columns, names(data)))
  sampling <- blcm_sampling_options(options$seed, options$chains, options$iterations, options$burn_in, options$thin)
  fit <- fit_blcm_model(prepared, sampling, file.path(.blcm_dir, "models", "blcm.jags"))
  summary <- summarize_blcm_predictions(fit, prepared)
  report <- build_blcm_report_data(prepared, fit, summary, plots = blcm_report_plots(summary$predictions))
  csv_final <- file.path(options$output, "pred_scores.csv"); html_final <- file.path(options$output, "blcm_report.html")
  csv_temp <- tempfile("pred_scores-", tmpdir = options$output, fileext = ".csv"); html_temp <- tempfile("blcm_report-", tmpdir = options$output, fileext = ".html")
  on.exit(unlink(c(csv_temp, html_temp)), add = TRUE)
  utils::write.csv(summary$predictions, csv_temp, row.names = FALSE, na = "")
  render_blcm_report(report, html_temp, file.path(.blcm_dir, "blcm_report.Rmd"))
  if (!file.rename(csv_temp, csv_final) || !file.rename(html_temp, html_final)) blcm_abort("could not replace BLCM output files")
  invisible(list(predictions = summary$predictions, report = html_final, fit = fit))
}

if (sys.nframe() == 0L) run_blcm(blcm_input_parser())
