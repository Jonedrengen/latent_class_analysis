project_root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(project_root, "src", "plots", "generate_histogram.R"))
source(file.path(project_root, "src", "plots", "generate_treemap.R"))
source(file.path(project_root, "src", "plots", "generate_fzecComparison.R"))
source(file.path(project_root, "src", "do_BLCM_21072026.R"))

histogram_input <- function() {
  data.frame(Meat_pred = c(0, 0.2, 0.21, 0.79, 0.8, 1))
}

sequence_type_input <- function() {
  data.frame(
    MLST = c("ST1", "ST1", "ST2", "ST2", "ST3", "ST4"),
    Meat_pred = c(0.1, 0.8, 0.2, 0.7, 0.9, 0.3),
    phylotype = c("A", "A", "B1", "B1", "B2", "C"),
    stringsAsFactors = FALSE
  )
}

blcm_two_class_fixture <- function() {
  train_a <- data.frame(Sample_Name = sprintf("A%02d", 1:12), training = 1, A = 1, B = 0)
  train_b <- data.frame(Sample_Name = sprintf("B%02d", 1:12), training = 1, A = 0, B = 1)
  held <- data.frame(Sample_Name = c("held_A", "held_B"), training = 0, A = c(1, 0), B = c(0, 1))
  data <- rbind(train_a, train_b, held)
  data$f1 <- c(rep(1, 12), rep(0, 12), 1, 0); data$f2 <- 1 - data$f1
  data$f3 <- data$f1; data$f4 <- data$f2
  data
}

blcm_expected_held_out <- function() c(held_A = "A", held_B = "B")

blcm_synthetic_draws <- function() {
  coda::mcmc(matrix(c(1, 1, 2, 2, 2, 1, 2, 2), ncol = 2, dimnames = list(NULL, c("z[25]", "z[26]"))))
}
