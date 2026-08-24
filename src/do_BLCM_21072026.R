# This is the main script for running and generating reports for the BLCM model.

# author: Jon Sztuk Slotved
# maintainer: Jon Sztuk Slotved 
# model design: Daniel Park


############ get current loc and source #############


# get "--file" default argument from commandArgs, replace it with "" so the path is not --file=path/to/script.R. Then normalize. 
blcm_script_dir <- function() {
  arguments <- commandArgs(trailingOnly = FALSE)
  file_arg <- arguments[grepl("^--file=", arguments)] #returns --file=path/to/script.R
  raw_path <- sub("^--file=", "", file_arg)
  dirname(normalizePath(raw_path))
}

# do not touch! this is source dir
.blcm_dir <- blcm_script_dir()

#sourcing other scripts
source(file.path(.blcm_dir, "logging.R"))
source(file.path(.blcm_dir, "prepare_blcm_input_data.R"))
source(file.path(.blcm_dir, "data_input_validator.R"))
source(file.path(.blcm_dir, "blcm_fit.R"))
source(file.path(.blcm_dir, "blcm_summary.R"))
source(file.path(.blcm_dir, "blcm_report.R"))
source(file.path(.blcm_dir, "config.R"))
source(file.path(.blcm_dir, "plots", "generate_histogram.R"))
source(file.path(.blcm_dir, "plots", "blcm_report_plots.R"))



#input parser for command line arguments
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

# orchestration function for running the BLCM model
run_blcm_main <- function(input_options, config) {

  #read data (move this to a func, when more input formats are supported)
  data <- read.csv(
    input_options$input,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  output_dir <- input_options$output
  logger("input data read",
         data = data, 
         log_file_path = file.path(output_dir, "blcm_log.txt"))

  #TODO: should split options data validation and config into separate funcs
  valdiated_input <- validate_input(input_options, data, config)
  logger("input data validated", 
         data = valdiated_input,
         log_file_path = file.path(output_dir, "blcm_log.txt"))

  #preparing input data for BLCM
  prepared <- prepare_blcm_data(data)
  logger("input data prepared",
         data = prepared,
         log_file_path = file.path(output_dir, "blcm_log.txt"))

}


############ parse input ############

input_options <- blcm_input_parser()



############ main function ############

run_blcm_main(input_options)
