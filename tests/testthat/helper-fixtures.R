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
