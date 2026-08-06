blcm_report_plots <- function(predictions) {
  human <- predictions[!is.na(predictions$FZEC), , drop = FALSE]

  list(
    "Predicted-class counts" = ggplot2::ggplot(
      predictions,
      ggplot2::aes(x = .data$predicted_class)
    ) +
      ggplot2::geom_bar() +
      ggplot2::labs(x = "Predicted class", y = "Count"),
    "Normalized entropy" = ggplot2::ggplot(
      predictions,
      ggplot2::aes(x = .data$normalized_entropy)
    ) +
      ggplot2::geom_histogram(bins = 20) +
      ggplot2::labs(x = "Normalized entropy", y = "Count"),
    "Animal posterior among held-out human isolates" = generate_hist(
      data.frame(Meat_pred = human$animal_probability),
      title = "Animal posterior among held-out human isolates"
    )
  )
}
