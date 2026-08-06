generate_hist <- function(input, title = "") {
    #--------docstring----------
  # Creates a histogram of predicted food-animal origin. Isolates are grouped
  # as Human (Meat_pred <= 0.2), Indeterminate (> 0.2 and < 0.8), or
  # Food-animal (Meat_pred >= 0.8).
  #
  # @Parameter: input (data frame)
  #   Must contain a numeric Meat_pred column with finite values from 0 to 1.
  # @Parameter: title (character)
  #   Plot title. Defaults to an empty title.
  # @Return: ggplot histogram object
  #
  # save: ggsave(file="DTU_TAIWAN_treemap.svg", plot=big_treemap, width=18, height=14)
  #
  # dependencies: ggplot2, dplyr
  if (!is.data.frame(input) || !"Meat_pred" %in% names(input)) {
    stop("`input` must be a data frame containing `Meat_pred`.", call. = FALSE)
  }

  if (
    !is.numeric(input$Meat_pred) ||
      any(!is.finite(input$Meat_pred) | input$Meat_pred < 0 | input$Meat_pred > 1)
  ) {
    stop("`Meat_pred` must contain finite numeric values between 0 and 1.", call. = FALSE)
  }

  groups <- c("Human", "Indeterminate", "Food-animal")
  colors <- c(
    "Human" = "goldenrod1",
    "Indeterminate" = "grey70",
    "Food-animal" = "red3"
  )
  plot_data <- data.frame(Meat_pred = input$Meat_pred)
  plot_data$group <- factor(
    ifelse(
      plot_data$Meat_pred <= 0.2,
      "Human",
      ifelse(plot_data$Meat_pred >= 0.8, "Food-animal", "Indeterminate")
    ),
    levels = groups
  )

  counts <- as.integer(table(plot_data$group))
  denominator <- sum(counts)
  percentages <- if (denominator) round(100 * counts / denominator, 1) else rep(0, length(counts))
  labels <- paste0(
    groups,
    "\nn=", counts,
    " (", percentages, "%)"
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$Meat_pred, fill = .data$group)) +
    ggplot2::geom_histogram(binwidth = 0.02, boundary = 0) +
    ggplot2::geom_vline(xintercept = c(0.2, 0.8), linetype = "dashed") +
    ggplot2::annotate("text", x = c(0.1, 0.5, 0.9), y = Inf, label = labels, vjust = 1.5) +
    ggplot2::scale_fill_manual(values = colors, drop = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    ggplot2::labs(
      title = title,
      x = "Predicted probability of food-animal origin (meat)",
      y = "Isolate count"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none")
}
