blcm_posterior_probabilities <- function(draws, rows, class_columns) {
  matrix_draws <- as.matrix(draws)
  probabilities <- t(vapply(rows, function(row) tabulate(matrix_draws[, sprintf("z[%d]", row)], nbins = length(class_columns)) / nrow(matrix_draws), numeric(length(class_columns))))
  colnames(probabilities) <- class_columns
  probabilities
}

blcm_validation_values <- function(observed, predicted) {
  known <- !is.na(observed)
  if (!any(known)) return(list(accuracy = NA_real_, confusion_matrix = NULL, n_truth = 0L))
  list(accuracy = mean(observed[known] == predicted[known]), confusion_matrix = table(observed = observed[known], predicted = predicted[known]), n_truth = sum(known))
}

summarize_blcm_predictions <- function(fit, blcm_data, groups = list(human = c("Human_CL1", "Human_CL2"), animal = c("Chicken_CL1", "Chicken_CL2", "Pork"))) {
  probabilities <- blcm_posterior_probabilities(fit$draws, blcm_data$prediction_rows, blcm_data$class_columns)
  predicted_index <- max.col(probabilities, ties.method = "first")
  rows <- blcm_data$prediction_rows; observed <- blcm_data$observed_class[rows]
  result <- blcm_data$raw_data[rows, unique(c(blcm_data$id_column, blcm_data$metadata_columns)), drop = FALSE]
  result$observed_class <- observed
  for (class_name in blcm_data$class_columns) result[[paste0(class_name, "_probability")]] <- probabilities[, class_name]
  result$predicted_class <- blcm_data$class_columns[predicted_index]
  result$max_posterior_probability <- probabilities[cbind(seq_len(nrow(probabilities)), predicted_index)]
  result$normalized_entropy <- -rowSums(ifelse(probabilities == 0, 0, probabilities * log(probabilities))) / log(ncol(probabilities))
  human <- intersect(groups$human, blcm_data$class_columns); animal <- intersect(groups$animal, blcm_data$class_columns)
  result$human_probability <- if (length(human)) rowSums(probabilities[, human, drop = FALSE]) else NA_real_
  result$animal_probability <- if (length(animal)) rowSums(probabilities[, animal, drop = FALSE]) else NA_real_
  eligible <- !is.na(observed) & observed %in% human
  result$FZEC <- ifelse(eligible, result$animal_probability >= 0.8, NA)
  structure(list(predictions = result, probabilities = probabilities, validation = blcm_validation_values(observed, result$predicted_class), groups = groups), class = "blcm_summary")
}
