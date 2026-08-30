#testing input validation functions for BLCM



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

# 2 class, 1 features, 8 samples (4 training and 4 testing)
two_classes_one_feature_fixture <- data.frame(
    Sample_Name = c(
      "Human_1", "Human_2", "Animal_1", "Animal_2",
      "Human_3", "Human_4", "Animal_3", "Animal_4"
    ),
    training = c(1, 1, 1, 1, 0, 0, 0, 0),
    CLA_Human = c(1, 1, 0, 0, 1, 1, 0, 0),
    CLA_Animal = c(0, 0, 1, 1, 0, 0, 1, 1),
    FEA_Animal_1 = c(0, 0, 1, 1, 0, 0, 1, 1),
    FEA_Human_1 = c(1, 1, 0, 0, 1, 1, 0, 0)
)

faulty_data_fixture_CLA <- data.frame(
    Sample_Name = c(
      "Human_1", "Human_2", "Animal_1", "Animal_2"
    ),
    training = c(1, 0, 1, NA),
    CLA_Human = c(1, 2, 0, 0),
    CLA_Animal = c(0, 0, 1, 1),
    FEA_Animal_1 = c(0, 0, 1, 1),
    FEA_Human_1 = c(1, 1, 0, 0)
)
faulty_data_fixture_FEA <- data.frame(
    Sample_Name = c(
      "Human_1", "Human_2", "Animal_1", "Animal_2"
    ),
    training = c(1, 0, 1, 0),
    CLA_Human = c(1, 1, 0, 0),
    CLA_Animal = c(0, 0, 1, 1),
    FEA_Animal_1 = c(0, 0, "?", 1),
    FEA_Human_1 = c(1, 1, 0, 0)
)

test_that("get_blcm_class_columns returns correct class columns", {
  expect_equal(get_blcm_class_column_names(four_features_fixture, data_column_configuration$class_columns_prefix), c("CLA_Human", "CLA_Animal"))
})

test_that("get_blcm_feature_columns returns correct feature columns", {
  expect_equal(get_blcm_feature_column_names(four_features_fixture, data_column_configuration$feature_columns_prefix), c("FEA_Animal_1", "FEA_Animal_2", "FEA_Human_1", "FEA_Human_2"))
})

test_that("failed_validation stops execution with an error in CLA columns", {
    expect_error(validate_blcm_data_input(faulty_data_fixture_CLA), "Invalid values found in CLA_Human on row: 2")
})

test_that("failed_validation stops execution with an error in FEA columns", {
    expect_error(validate_blcm_data_input(faulty_data_fixture_FEA), "Invalid values found in FEA_Animal_1 on row: 3")
})
