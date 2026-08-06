#please add new validation functions here
#remember to add your func to the main validator func at the bottom of this file
# - Jon


blcm_abort <- function(message) {
  stop(message, call. = FALSE)
}

get_blcm_class_columns <- function(data) {
  grep("^CLASS_", names(data), value = TRUE)
}

get_blcm_feature_columns <- function(data) {
  grep("^FEATURE_", names(data), value = TRUE)
}

validate_blcm_cli_options <- function(options) {
  if (!file.exists(options$input)) {
    blcm_abort("can't find --input, file does not exist")
  }

  if (!dir.exists(options$output)) {
    blcm_abort("can't find --output, output dir not exist")
  }
}

validate_blcm_data_input <- function(
  data,
  id_column = "Sample_Name",
  training_column = "training"
) {

  #validate CLASS_ cols
  class_columns <- get_blcm_class_columns(data)

  if (length(class_columns) < 2L) {
    blcm_abort("at least two CLASS_ columns are required")
  }

  #validate FEATURE_ cols
  feature_columns <- get_blcm_feature_columns(data)

  if (!length(feature_columns)) {
    blcm_abort("at least one FEATURE_ column is required")
  }
}

#TODO: write condig validation function
validate_config <- function(config) {
  #validate config file
  # - Jon
}

########## validation function ##########

validate_input <- function(options, data, config = NULL) {
  validate_blcm_cli_options(options)
  validate_blcm_data_input(data)
  validate_config(config)
}

