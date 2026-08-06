blcm_abort <- function(message) stop(message, call. = FALSE)

default_blcm_classes <- function() {
  c("Human_CL1", "Human_CL2", "Chicken_CL1", "Chicken_CL2", "Pork")
}
#takes in 
validate_binary_columns <- function(data, columns, label) {
  for (column in columns) if
  (any(is.na(data[[column]])) || any(!(data[[column]] %in% c(0, 1)))) {
    blcm_abort(sprintf("%s column '%s' must be 0 or 1", label, column))
  }
}

prepare_blcm_data <- function(data,
                              class_columns,
                              id_column = "Sample_Name",
                              training_column = "training",
                              feature_columns = NULL,
                              metadata_columns = NULL) {
  if (!is.data.frame(data)) blcm_abort("data must be a data frame")
  if (length(class_columns) < 2L) blcm_abort("at least two class columns are required")
  missing <- setdiff(unique(c(id_column, training_column, class_columns)), names(data))
  if (length(missing)) blcm_abort(sprintf("missing required columns: %s", paste(missing, collapse = ", ")))
  ids <- as.character(data[[id_column]])
  if (any(is.na(ids) | !nzchar(ids)) || anyDuplicated(ids)) blcm_abort("sample IDs must be unique and non-empty")
  validate_binary_columns(data, training_column, "training"); validate_binary_columns(data, class_columns, "class")
  if (is.null(metadata_columns)) metadata_columns <- character()
  if (is.null(feature_columns)) feature_columns <- setdiff(names(data), c(id_column, training_column, class_columns, metadata_columns))
  if (!length(feature_columns)) blcm_abort("at least one feature column is required")
  if (length(setdiff(feature_columns, names(data)))) blcm_abort("feature columns are missing from data")
  validate_binary_columns(data, feature_columns, "feature")
  training <- data[[training_column]] == 1
  class_counts <- rowSums(as.matrix(data[, class_columns, drop = FALSE]))
  if (any(class_counts[training] != 1L)) blcm_abort("training rows must have exactly one class label")
  if (any(class_counts[!training] > 1L)) blcm_abort("held-out rows may have at most one class label")
  if (!any(!training)) blcm_abort("at least one prediction row is required")
  observed <- rep(NA_character_, nrow(data)); has_label <- class_counts == 1L
  observed[has_label] <- class_columns[max.col(as.matrix(data[has_label, class_columns, drop = FALSE]), ties.method = "first")]
  if (any(table(factor(observed[training], levels = class_columns)) == 0L)) blcm_abort("every configured class must have at least one training observation")
  z <- rep(NA_integer_, nrow(data)); z[training] <- match(observed[training], class_columns)
  structure(list(raw_data = data, id_column = id_column, training_column = training_column, class_columns = class_columns, feature_columns = feature_columns, metadata_columns = metadata_columns, sample_ids = ids, training = training, observed_class = observed, prediction_rows = which(!training), jags_data = list(N = nrow(data), P = length(feature_columns), K = length(class_columns), x = as.matrix(data[, feature_columns, drop = FALSE]), z = z)), class = "blcm_data")
}
