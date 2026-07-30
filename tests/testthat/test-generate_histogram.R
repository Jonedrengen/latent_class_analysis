test_that("generate_hist returns a histogram with threshold classifications", {
  plot <- generate_hist(histogram_input(), title = "Example histogram")

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title, "Example histogram")
  expect_equal(
    ggplot_build(plot)$data[[3]]$label,
    c("Human\nn=2 (33.3%)", "Indeterminate\nn=2 (33.3%)", "Food-animal\nn=2 (33.3%)")
  )
})

test_that("generate_hist rejects invalid input", {
  expect_error(generate_hist(data.frame(other = 0.5)), "containing `Meat_pred`")
  expect_error(generate_hist(data.frame(Meat_pred = "0.5")), "finite numeric values")
  expect_error(generate_hist(data.frame(Meat_pred = c(0.5, 1.1))), "between 0 and 1")
})
