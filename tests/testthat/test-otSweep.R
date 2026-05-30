test_that("otSweep runs and returns valid output", {
  data("sample_data", package = "ThSQCA")
  dat <- sample_data
  
  res <- otSweep(
    dat = dat,
    Yvar = "Y",
    Xvars = c("X1", "X2", "X3"),
    sweep_range = 6:8,
    thrX = c(X1 = 7, X2 = 7, X3 = 7),
    return_details = FALSE
  )
  
  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)
  required_cols <- c("expression", "inclS", "covS")
  expect_true(all(required_cols %in% colnames(res)))
  thr_cols <- grep("thr|threshold", colnames(res), ignore.case = TRUE, value = TRUE)
  expect_true(length(thr_cols) >= 1)
})
