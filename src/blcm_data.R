prepare_blcm_data <- function(
  data,
  id_column = "Sample_Name",
  training_column = "training"
) {
  ids <- as.character(data[[id_column]])
  class_columns <- get_blcm_class_columns(data)
  feature_columns <- get_blcm_feature_columns(data)
  training <- data[[training_column]] == 1
  class_counts <- rowSums(as.matrix(data[, class_columns, drop = FALSE]))

  observed <- rep(NA_character_, nrow(data))
  has_label <- class_counts == 1L
  observed[has_label] <- class_columns[max.col(
    as.matrix(data[has_label, class_columns, drop = FALSE]),
    ties.method = "first"
  )]

  eta <- rep(NA_integer_, nrow(data))
  eta[training] <- match(observed[training], class_columns)

  structure(
    list(
      raw_data = data,
      id_column = id_column,
      training_column = training_column,
      class_columns = class_columns,
      feature_columns = feature_columns,
      sample_ids = ids,
      training = training,
      observed_class = observed,
      prediction_rows = which(!training),
      jags_data = list(
        N = nrow(data),
        P = length(feature_columns),
        K = length(class_columns),
        x = as.matrix(data[, feature_columns, drop = FALSE]),
        eta = eta
      )
    ),
    class = "blcm_data"
  )
}
