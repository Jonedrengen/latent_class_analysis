#please add new validation functions here
#remember to add your func to the main validator func at the bottom of this file
# - Jon

#blcm utils
source(file.path(.blcm_dir, "blcm_utilities.R"))

validate_blcm_cli_options <- function(options) {
  if (!file.exists(options$input)) {
    blcm_abort("can't find --input, file does not exist")
  }

  if (!dir.exists(options$output)) {
    blcm_abort("can't find --output, output dir not exist")
  }
}

#id_column and training_column, not currently used. 11/08/2026
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

  #TODO: validate that the class columns are binary (0/1) and that each row has at most one class label. 11/08/2026

  #validate FEATURE_ cols
  feature_columns <- get_blcm_feature_columns(data)

  if (!length(feature_columns) < 2L) {
    blcm_abort("at least two FEATURE_ column is required")
  }

  #TODO: validate that the feature columns are binary (0/1). 11/08/2026

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

  data
}
