test_that("generate_ST_dist selects the most frequent sequence types", {
  plot <- generate_ST_dist(sequence_type_input(), top_n = 2, title = "Example distribution")

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title, "Example distribution")
  expect_setequal(as.character(plot$data$MLST), c("ST1", "ST2"))
  expect_equal(
    plot$data$Frequency[
      as.character(plot$data$MLST) == "ST1" & plot$data$FZEC_group == "FZEC"
    ],
    1L
  )
  expect_equal(
    plot$data$Frequency[
      as.character(plot$data$MLST) == "ST2" & plot$data$FZEC_group == "FZEC"
    ],
    0L
  )
})

test_that("generate_ST_dist rejects invalid input and options", {
  expect_error(generate_ST_dist(data.frame(MLST = "ST1")), "containing `MLST` and `Meat_pred`")
  expect_error(
    generate_ST_dist(data.frame(MLST = "", Meat_pred = 0.5)),
    "must not contain missing or empty values"
  )
  expect_error(generate_ST_dist(sequence_type_input(), top_n = 0), "positive integer")
})
