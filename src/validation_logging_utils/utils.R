

#get blcm class and feature columns
get_blcm_class_column_names <- function(data, prefix) {
  class_columns <- grep(prefix, names(data), value = TRUE)
  class_columns
}
get_blcm_feature_column_names <- function(data, prefix) {
  feature_columns <- grep(prefix, names(data), value = TRUE)
  feature_columns
}

#blcm utils
failed_validation <- function(message) {
  stop(message, call. = FALSE)
}