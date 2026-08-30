#please add new validation functions here
#remember to add your func to the main validator func at the bottom of this file
# - Jon


validate_options <- function(options) {
  if (!file.exists(options$input)) {
    failed_validation("can't find --input, file does not exist")
  }

  if (!dir.exists(options$output)) {
    failed_validation("can't find --output, output dir not exist")
  }
}

#TODO: write condig validation function
validate_config <- function(config) {
  #validate config file
  # - Jon
  

}

#validate that columns binary (0/1)
validate_binary_column <- function(data, columns) {
  for (col in columns) {
    values <- data[, col]
    for (val in values) {
      if (!val %in% c(0, 1)) {
        failed_validation(sprintf("Invalid values found in %s on row: %s",
                                  col,
                                  which(values == val)))
      }
    }
  }
  data
}

#id_column and training_column, not currently used. 11/08/2026
validate_blcm_data_input <- function(
  data,
  config
) {

  #validate class "CLA_" cols
  #__________________________
  class_column_names <- get_blcm_class_column_names(data, config$class_columns_prefix)

  if (length(class_column_names) < 2L) {
    failed_validation("at least two CLA_ columns are required")
  }

  validate_binary_column(data, class_column_names)


  #validate feature "FEA_" cols
  #__________________________
  feature_column_names <- get_blcm_feature_column_names(data, config$feature_columns_prefix)

  if (length(feature_column_names) < 2L) {
    failed_validation("at least two FEA_ column is required")
  }

  validate_binary_column(data, feature_column_names)

  data
}


########## validation function ##########

validate_input <- function(options, data, config = NULL) {
  validate_options(options)
  validate_blcm_data_input(data, config$data_column_configuration)
  validate_config(config)

  data
}
