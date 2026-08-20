

#abort with message (maybe delete, since this is already in logging.R)
blcm_abort <- function(message) {
  stop(message, call. = FALSE)
}

#get blcm class and feature columns
get_blcm_class_columns <- function(data) {
  grep("^CLA_", names(data), value = TRUE)
}

get_blcm_feature_columns <- function(data) {
  grep("^FEA_", names(data), value = TRUE)
}
