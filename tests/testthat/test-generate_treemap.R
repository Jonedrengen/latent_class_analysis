test_that("generate_treemap filters all, FZEC, and non-FZEC observations", {
  input <- sequence_type_input()

  all_plot <- generate_treemap(input)
  fzec_plot <- generate_treemap(input, is_FZEC = 1)
  non_fzec_plot <- generate_treemap(input, is_FZEC = 0)

  expect_s3_class(all_plot, "ggplot")
  expect_setequal(as.character(all_plot$data$MLST), c("ST1", "ST2", "ST3", "ST4"))
  expect_equal(as.character(fzec_plot$data$MLST), c("ST1", "ST3"))
  expect_setequal(as.character(non_fzec_plot$data$MLST), c("ST1", "ST2", "ST4"))
})

test_that("generate_treemap validates inputs and empty selections", {
  expect_error(
    generate_treemap(data.frame(MLST = "ST1", Meat_pred = 0.5)),
    "must contain `MLST`, `Meat_pred`, and `phylotype`"
  )
  expect_error(
    generate_treemap(sequence_type_input(), is_FZEC = 2),
    "must be NULL"
  )
  expect_error(
    generate_treemap(sequence_type_input(), labelling_freq = 0.5),
    "non-negative integer"
  )
  expect_error(
    generate_treemap(
      data.frame(MLST = "ST1", Meat_pred = 0.1, phylotype = "A"),
      is_FZEC = 1
    ),
    "No observations are available"
  )
})
