# This is the main script for running and generating reports for the BLCM model.

# author: Jon Sztuk Slotved
# maintainer: Jon Sztuk Slotved 
# model design: Daniel Park


############ get current loc and source #############


# normalizng the path, so we can find the other scripts
blcm_script_dir <- function() {
  arguments <- commandArgs(trailingOnly = FALSE)
  file_arg <- arguments[grepl("^--file=", arguments)]
  raw_path <- sub("^--file=", "", file_arg)
  dirname(normalizePath(raw_path))
}

# do not touch! this is source dir
.blcm_dir <- blcm_script_dir()

#sourcing other scripts
source(file.path(.blcm_dir, "data_input_validator.R"))
source(file.path(.blcm_dir, "blcm_data.R"))
source(file.path(.blcm_dir, "blcm_fit.R"))
source(file.path(.blcm_dir, "blcm_summary.R"))
source(file.path(.blcm_dir, "blcm_report.R"))
source(file.path(.blcm_dir, "config.R"))
source(file.path(.blcm_dir, "plots", "generate_histogram.R"))
source(file.path(.blcm_dir, "plots", "blcm_report_plots.R"))


############ input parsing func ############


blcm_input_parser <- function(argv = commandArgs(trailingOnly = TRUE)) {
  options <- list(
    optparse::make_option(
      c("-i", "--input"),
      type = "character",
      required = TRUE
    ),
    optparse::make_option(
      c("-o", "--output"),
      type = "character",
      required = TRUE
    )
  )

  parsed <- optparse::parse_args(
    optparse::OptionParser(option_list = options),
    args = argv
  )
  parsed
}




run_blcm <- function(options) {
  data <- read_validated_blcm_input(options)

  prepared <- prepare_blcm_data(data)
  sampling <- blcm_sampling_options()
  fit <- fit_blcm_model(
    prepared,
    sampling,
    file.path(.blcm_dir, "models", "blcm.jags")
  )
  summary <- summarize_blcm_predictions(fit, prepared)
  report <- build_blcm_report_data(
    prepared,
    fit,
    summary,
    plots = blcm_report_plots(summary$predictions)
  )

  csv_final <- file.path(options$output, "pred_scores.csv")
  html_final <- file.path(options$output, "blcm_report.html")
  csv_temp <- tempfile(
    "pred_scores-",
    tmpdir = options$output,
    fileext = ".csv"
  )
  html_temp <- tempfile(
    "blcm_report-",
    tmpdir = options$output,
    fileext = ".html"
  )

  on.exit(unlink(c(csv_temp, html_temp)), add = TRUE)

  utils::write.csv(summary$predictions, csv_temp, row.names = FALSE, na = "")
  render_blcm_report(
    report,
    html_temp,
    file.path(.blcm_dir, "blcm_report.Rmd")
  )
  file.rename(csv_temp, csv_final)
  file.rename(html_temp, html_final)

  invisible(list(
    predictions = summary$predictions,
    report = html_final,
    fit = fit
  ))
}

if (sys.nframe() == 0L) {
  run_blcm(blcm_input_parser())
}


############ main ############


#get input options
options <- blcm_input_parser()

data <- read.csv(
  options$input,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

validate_input(options, data)

validate_config(fitting_configuration)