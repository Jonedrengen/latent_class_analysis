




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


