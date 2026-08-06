blcm_posterior_probabilities <- function(draws, rows, class_columns) {
  matrix_draws <- as.matrix(draws)

  probabilities <- t(vapply(
    rows,
    function(row) {
      tabulate(
        matrix_draws[, sprintf("eta[%d]", row)],
        nbins = length(class_columns)
      ) / nrow(matrix_draws)
    },
    numeric(length(class_columns))
  ))

  colnames(probabilities) <- class_columns
  probabilities
}

blcm_validation_values <- function(observed, predicted) {
  known <- !is.na(observed)

  if (!any(known)) {
    return(list(
      accuracy = NA_real_,
      confusion_matrix = NULL,
      n_truth = 0L
    ))
  }

  list(
    accuracy = mean(observed[known] == predicted[known]),
    confusion_matrix = table(
      observed = observed[known],
      predicted = predicted[known]
    ),
    n_truth = sum(known)
  )
}

default_blcm_groups <- function(class_columns) {
  labels <- sub("^CLASS_", "", class_columns)

  list(
    human = class_columns[labels %in% c("Human_CL1", "Human_CL2", "Human")],
    animal = class_columns[labels %in% c(
      "Chicken_CL1",
      "Chicken_CL2",
      "Pork",
      "Animal"
    )]
  )
}

summarize_blcm_predictions <- function(
  fit,
  blcm_data,
  groups = default_blcm_groups(blcm_data$class_columns)
) {
  probabilities <- blcm_posterior_probabilities(
    fit$draws,
    blcm_data$prediction_rows,
    blcm_data$class_columns
  )
  predicted_index <- max.col(probabilities, ties.method = "first")
  rows <- blcm_data$prediction_rows
  observed <- blcm_data$observed_class[rows]
  result <- blcm_data$raw_data[rows, blcm_data$id_column, drop = FALSE]

  result$observed_class <- observed

  for (class_name in blcm_data$class_columns) {
    result[[paste0(class_name, "_probability")]] <- probabilities[, class_name]
  }

  result$predicted_class <- blcm_data$class_columns[predicted_index]
  result$max_posterior_probability <- probabilities[cbind(
    seq_len(nrow(probabilities)),
    predicted_index
  )]
  result$normalized_entropy <- -rowSums(
    ifelse(probabilities == 0, 0, probabilities * log(probabilities))
  ) / log(ncol(probabilities))

  human <- intersect(groups$human, blcm_data$class_columns)
  animal <- intersect(groups$animal, blcm_data$class_columns)

  if (length(human)) {
    result$human_probability <- rowSums(probabilities[, human, drop = FALSE])
  } else {
    result$human_probability <- NA_real_
  }

  if (length(animal)) {
    result$animal_probability <- rowSums(
      probabilities[, animal, drop = FALSE]
    )
  } else {
    result$animal_probability <- NA_real_
  }

  eligible <- !is.na(observed) & observed %in% human
  result$FZEC <- ifelse(
    eligible,
    result$animal_probability >= 0.8,
    NA
  )

  structure(
    list(
      predictions = result,
      probabilities = probabilities,
      validation = blcm_validation_values(observed, result$predicted_class),
      groups = groups
    ),
    class = "blcm_summary"
  )
}
