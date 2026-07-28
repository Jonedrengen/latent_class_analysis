library(ggplot2)
library(dplyr)
library(optparse)
library(treemapify)

generate_treemap <- function(
  input,
  is_FZEC = NULL,
  labelling_freq = 1,
  title = "MLST distribution, coloured by phylotype"
) {
  #--------docstring----------
  # Creates an MLST treemap for either FZEC or non-FZEC isolates. Each tile is
  # sized by sequence-type frequency and coloured by phylotype. FZEC is
  # defined as Meat_pred >= 0.8.
  #
  # @Parameter: input (data frame)
  #   Must contain MLST, Meat_pred, and phylotype columns. Meat_pred must have
  #   finite numeric values from 0 to 1.
  # @Parameter: is_FZEC (numeric)
  #   Use 1 for FZEC isolates, 0 for non-FZEC isolates or leave empty for including all.
  # @Parameter: labelling_freq (non-negative integer)
  #   Sequence types with frequency greater than this value receive a label.
  # @Parameter: title (character)
  #   Plot title.
  # @Return: ggplot treemap object
  #
  # save: ggsave(file="histbig_danmap.svg", plot=histogram, width=12, height=8)
  #
  # dependencies: ggplot2, dplyr, treemapify
  required_columns <- c("MLST", "Meat_pred", "phylotype")
  if (!is.data.frame(input) || !all(required_columns %in% names(input))) {
    stop(
      "`input` must contain `MLST`, `Meat_pred`, and `phylotype`.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(input$Meat_pred) ||
      any(
        !is.finite(input$Meat_pred) | input$Meat_pred < 0 | input$Meat_pred > 1
      )
  ) {
    stop(
      "`Meat_pred` must contain finite numeric values between 0 and 1.",
      call. = FALSE
    )
  }
  if (
    any(is.na(input$MLST) | trimws(as.character(input$MLST)) == "") ||
      any(is.na(input$phylotype) | trimws(as.character(input$phylotype)) == "")
  ) {
    stop(
      "`MLST` and `phylotype` must not contain missing or empty values.",
      call. = FALSE
    )
  }
  if (
    !is.null(is_FZEC) &&
      (length(is_FZEC) != 1 ||
        !is.numeric(is_FZEC) ||
        is.na(is_FZEC) ||
        !is_FZEC %in% c(0, 1))
  ) {
    stop(
      "`is_FZEC` must be NULL (all isolates), 0 (non-FZEC), or 1 (FZEC).",
      call. = FALSE
    )
  }
  if (
    length(labelling_freq) != 1 ||
      !is.numeric(labelling_freq) ||
      labelling_freq < 0 ||
      labelling_freq != as.integer(labelling_freq)
  ) {
    stop("`labelling_freq` must be a non-negative integer.", call. = FALSE)
  }

  colors <- c(
    "A" = "#7ea0c5",
    "B1" = "#ade4ff",
    "B2" = "#f5b7c4",
    "C" = "#5e9a66",
    "D" = "#bc7eab",
    "E" = "#aaaaaa",
    "F" = "#ffda6f",
    "G" = "#b8cc7e",
    "U" = "#8c7035",
    "unknown" = "red"
  )
  plot_input <- input %>%
    mutate(FZEC = ifelse(Meat_pred >= 0.8, 1, 0))
  if (!is.null(is_FZEC)) {
    plot_input <- plot_input %>% filter(FZEC == is_FZEC)
  }
  plot_data <- plot_input %>%
    count(MLST, phylotype, name = "Frequency") %>%
    arrange(desc(Frequency), MLST)

  if (nrow(plot_data) == 0) {
    group_name <- if (is.null(is_FZEC)) {
      "all isolates"
    } else if (is_FZEC == 1) {
      "FZEC"
    } else {
      "non-FZEC"
    }
    stop(
      paste("No observations are available for the", group_name, "group."),
      call. = FALSE
    )
  }

  ggplot(
    plot_data,
    aes(area = Frequency, fill = phylotype, label = MLST, subgroup = phylotype)
  ) +
    geom_treemap(color = "white", size = 0.5) +
    geom_treemap_subgroup_border(colour = "black", size = 2) +
    geom_treemap_text(
      aes(label = ifelse(Frequency > labelling_freq, MLST, "")),
      colour = "black",
      place = "centre",
      grow = FALSE
    ) +
    scale_fill_manual(values = colors) +
    labs(title = title, fill = "Phylotype") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}
