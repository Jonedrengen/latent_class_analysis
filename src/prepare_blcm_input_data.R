


#get blcm class and feature columns
get_blcm_class_columns <- function(data) {
  grep("^CLA_", names(data), value = TRUE)
}
get_blcm_feature_columns <- function(data) {
  grep("^FEA_", names(data), value = TRUE)
}

# create eta vector, which is the classes, represented as integers. Because blcm must have that 
create_eta_vector <- function(data) {
  # init eta with NA for all samples
  eta <- rep(NA_integer_, nrow(data))

  # class cols
  class_columns <- grep("^CLA_", names(data), value = TRUE)

  # vector of TRUE/FALSE for training samples
  training <- data[, "training"] == 1L
  
  # get the matrix of classes for the training samples
  matrix_of_classes <- as.matrix(data[training, class_columns, drop = FALSE])

  # 1 if the first class, 2 if the second class, etc. NA if no class label
  eta[training] <- max.col(matrix_of_classes, ties.method = "first")
  eta
}

# create Y matrix for jags, which is the features, represented as a matrix of 0/1 values
create_Y_matrix <- function(data) {
  # feature cols
  feature_columns <- grep("^FEA_", names(data), value = TRUE)

  # get the matrix of features for all samples
  Y <- as.matrix(data[, feature_columns, drop = FALSE])

  # ensure that the matrix is numeric
  storage.mode(Y) <- "numeric"

  Y
}


# create a jags_data list, which is a list of data to be passed to jags
create_jags_data <- function(data) {
  #eta
  eta <- create_eta_vector(data)

  # y matrix of features
  Y <- create_Y_matrix(data)

  # n classes
  M_fit <- length(grep("^CLA_", names(data)))

  # number of samples
  N <- nrow(data)

  # number of features
  K <- ncol(Y)

  # create a named list for the data
  jags_data <- list(Y = Y,
                    M_fit = M_fit,
                    N = N,
                    K = K,
                    eta = eta)
  
  jags_data
}

prepare_blcm_data <- function(
  data
) {
  ids <- data[, "Sample_Name"]
  class_columns <- get_blcm_class_columns(data)
  feature_columns <- get_blcm_feature_columns(data)
  jags_data <- create_jags_data(data)

  list(
    ids = ids,
    class_columns = class_columns,
    feature_columns = feature_columns,
    jags_data = jags_data
    )
}
