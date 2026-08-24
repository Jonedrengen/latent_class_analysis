
#tests for blcm input data preparation functions

###### fixtures ######

#fixure for for 2 classes, 4 features, 16 samples (8 training and 8 testing)
# training: 4 humans, 4 animals
# testing: 4 humans, 4 animals
# Features: 4 total, 2 human, 2 animal.
# Features: no animal features in humans, no human features in animals.
four_features_fixture <- data.frame(
    Sample_Name = c(
      "Human_1", "Human_2", "Human_3", "Human_4",
      "Animal_1", "Animal_2", "Animal_3", "Animal_4",
      "Human_5", "Human_6", "Human_7", "Human_8",
      "Animal_5", "Animal_6", "Animal_7", "Animal_8"
    ),
    training = c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0),
    CLA_Human = c(1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0),
    CLA_Animal = c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1),
    FEA_Animal_1 = c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1),
    FEA_Animal_2 = c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1),
    FEA_Human_1 = c(1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0),
    FEA_Human_2 = c(1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
  )

# 1 class, 1 feature, 8 samples (4 training and 4 testing)
one_class_fixture <- data.frame(
    Sample_Name = c(
      "Human_1", "Human_2", "Human_3", "Human_4",
      "Human_5", "Human_6", "Human_7", "Human_8"
    ),
    training = c(1, 1, 1, 1, 1, 1, 0, 0),
    CLA_Human = c(1, 1, 1, 1, 1, 1, 1, 1),
    FEA_Human_1 = c(1, 1, 1, 1, 1, 1, 1, 1)
)

# 2 classes, 2 features, 4 samples (2 training and 2 testing)
two_classes_fixture <- data.frame(
    Sample_Name = c(
      "Human_1", "Human_2", "Animal_1", "Animal_2"
    ),
    training = c(1, 0, 1, 0),
    CLA_Human = c(1, 1, 0, 0),
    CLA_Animal = c(0, 0, 1, 1),
    FEA_Animal_1 = c(0, 0, 1, 1),
    FEA_Human_1 = c(1, 1, 0, 0)
)


###### tests ######

#eta
test_that("generate eta vector works for 2 classes, 4 features, 16 samples", {
  eta <- create_eta_vector(four_features_fixture)
  eta_four_features_fixture <- c(1L, 1L, 1L, 1L,
                                2L, 2L, 2L, 2L,
                                NA_integer_, NA_integer_,
                                NA_integer_, NA_integer_,
                                NA_integer_, NA_integer_,
                                NA_integer_, NA_integer_)

  expect_equal(eta, eta_four_features_fixture)
})

#eta 
test_that("eta vector generates ", {
  eta <- create_eta_vector(four_features_fixture)
  expect_true(length(eta) == nrow(four_features_fixture))

  
  expect_true(eta[1] == 1L)
  expect_true(eta[5] == 2L)
  expect_type(eta, "integer")
})

#Y matrix
test_that("generate Y matrix works for 2 classes, 2 features, 4 samples", {
  Y <- create_Y_matrix(two_classes_fixture)
  Y_two_classes_fixture <- matrix(c(0, 0, 1, 1, 1, 1, 0, 0), nrow = 4)
  expect_equal(nrow(Y), nrow(Y_two_classes_fixture))
  expect_equal(ncol(Y), ncol(Y_two_classes_fixture))
})

#jags_data list (Y, M_fit, N, K, eta)
test_that("jags_data list is created correctly for four_features_fixture", {
  jags_data <- create_jags_data(four_features_fixture)
  expect_equal(dim(jags_data$Y), c(nrow(four_features_fixture), 4))
  expect_equal(jags_data$M_fit, 2)
  expect_equal(jags_data$N, nrow(four_features_fixture))
  expect_equal(jags_data$K, 4)
  expect_equal(length(jags_data$eta), nrow(four_features_fixture))
})

#blcm_data list is prepared correctly
test_that("blcm_data list is prepared correctly for four_features_fixture", {
  blcm_data <- prepare_blcm_data(four_features_fixture)
  expect_equal(dim(blcm_data$jags_data$Y), c(nrow(four_features_fixture), 4))
  expect_equal(blcm_data$jags_data$M_fit, 2)
  expect_equal(blcm_data$jags_data$N, nrow(four_features_fixture))
  expect_equal(blcm_data$jags_data$K, 4)
  expect_equal(length(blcm_data$jags_data$eta), nrow(four_features_fixture))
  expect_equal(blcm_data$class_columns, c("CLA_Human", "CLA_Animal"))
  expect_equal(blcm_data$feature_columns, c("FEA_Animal_1", "FEA_Animal_2",
                                            "FEA_Human_1", "FEA_Human_2"))
  expect_type(blcm_data, "list")
})

# blcm_data has correct structure and class
test_that("blcm_data has correct structure and class", {
  blcm_data <- prepare_blcm_data(four_features_fixture)
  expect_true(is.list(blcm_data))
  expect_true(length(blcm_data) == 4)
  expect_named(blcm_data, c("ids", "class_columns", "feature_columns", "jags_data"))

  expect_type(blcm_data$ids, "character")
  expect_type(blcm_data$class_columns, "character")
  expect_type(blcm_data$feature_columns, "character")
  expect_type(blcm_data$jags_data, "list")
})