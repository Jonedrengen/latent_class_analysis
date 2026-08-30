

# create eta vector, which is the classes, represented as integers. Because blcm must have that 
create_eta_vector <- function(data, data_column_configuration) {
  # init eta with NA for all samples
  eta <- rep(NA_integer_, nrow(data))

  # class cols
  class_columns <- get_blcm_class_column_names(data, data_column_configuration$class_columns_prefix)

  # vector of TRUE/FALSE for training samples
  training <- data[, data_column_configuration$training_column] == 1L
  
  # get the matrix of classes for the training samples
  matrix_of_classes <- as.matrix(data[training, class_columns, drop = FALSE])

  # 1 if the first class, 2 if the second class, etc. NA if no class label
  eta[training] <- max.col(matrix_of_classes, ties.method = "first")
  eta
}

# create Y matrix for jags, which is the features, represented as a matrix of 0/1 values
create_Y_matrix <- function(data, data_column_configuration) {
  # feature cols
  feature_columns <- get_blcm_feature_column_names(data, data_column_configuration$feature_columns_prefix)

  # get the matrix of features for all samples
  Y <- as.matrix(data[, feature_columns, drop = FALSE])

  # ensure that the matrix is numeric
  storage.mode(Y) <- "numeric"

  Y
}


# create a jags_data list, which is a list of data to be passed to jags
create_jags_data <- function(data, config) {
  #eta
  eta <- create_eta_vector(data, config$data_column_configuration)

  # y matrix of features
  Y <- create_Y_matrix(data, config$data_column_configuration)

  # n classes
  M_fit <- length(get_blcm_class_column_names(data, config$data_column_configuration$class_columns_prefix))

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

#inital values for chains in jags (will create the mc chain initial values, for each chain separately
#maybe change this to create random initial values for the chains
inits_function_factory <- function(M_fit) {
    inits_function <- function() {
      list(a = rep(0, M_fit))
  }
  inits_function
}

prepare_blcm_data <- function(
  data,
  config
) {
  ids <- data[, config$data_column_configuration$id_column]
  class_column_names <- get_blcm_class_column_names(data, config$data_column_configuration$class_columns_prefix)
  feature_column_names <- get_blcm_feature_column_names(data, config$data_column_configuration$feature_columns_prefix)
  jags_data <- create_jags_data(data, config)

  blcm_prepared <- list(
        ids = ids,
        class_columns = class_column_names,
        feature_columns = feature_column_names,
        jags_data = jags_data,
        inits = inits_function_factory(jags_data$M_fit) #factory needed because each chain needs separate initial values
      )
  blcm_prepared
}
