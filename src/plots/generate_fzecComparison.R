library(ggplot2)
library(dplyr)
library(optparse)


generate_ST_dist <- function(input, top_n = 20, title = "ST distribution: FZEC vs Non-FZEC") {
   #--------docstring----------
  # @Parameter: input (data frame)
  #   -MLST: character sequence type
  #   -Meat_Pred or Meat_pred: numeric food-animal probability from 0 to 1
  # @Parameter: top_n (integer)
  #   Number of sequence types with the largest combined count to display.
  # @Parameter: title (character)
  #   Plot title.
  # @Return: ggplot object
  # 
  # save: ggsave(file="DTU_TAIWAN_treemap.svg", plot=big_treemap, width=18, height=14)
  #
  # dependencies: ggplot2, dplyr

  colors <- c("Non-FZEC" = "goldenrod1", "FZEC" = "red3")
  plot_data <- data.frame(MLST = as.character(input$MLST), Meat_pred = input$Meat_pred) %>%
    mutate(FZEC_group = ifelse(Meat_pred >= 0.8, "FZEC", "Non-FZEC"))

  counts <- plot_data %>%
    count(MLST, FZEC_group, name = "Frequency")
  all_pairs <- expand.grid(
    MLST = sort(unique(plot_data$MLST)),
    FZEC_group = names(colors),
    stringsAsFactors = FALSE
  )
  counts <- all_pairs %>%
    left_join(counts, by = c("MLST", "FZEC_group")) %>%
    mutate(Frequency = ifelse(is.na(Frequency), 0L, Frequency))

  selected_st <- counts %>%
    group_by(MLST) %>%
    summarise(total = sum(Frequency), .groups = "drop") %>%
    arrange(desc(total), MLST) %>%
    slice_head(n = top_n) %>%
    pull(MLST)
  counts <- counts %>%
    filter(MLST %in% selected_st) %>%
    mutate(
      MLST = factor(MLST, levels = selected_st),
      FZEC_group = factor(FZEC_group, levels = names(colors))
    )

  ggplot(counts, aes(x = MLST, y = Frequency, fill = FZEC_group)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = colors, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, x = "Sequence type", y = "Isolate count", fill = "Classification") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
