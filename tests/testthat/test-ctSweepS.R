test_that("ctSweepS runs and returns valid output", {
  data("sample_data", package = "ThSQCA")
  dat <- sample_data
  
  res <- ctSweepS(
    dat = dat,
    Yvar = "Y",
    Xvars = c("X1", "X2", "X3"),
    sweep_var = "X3",
    sweep_range = 6:8,
    thrY = 7,
    thrX_default = 7,
    return_details = FALSE
  )
  
  # 1. Output must be a data frame
  expect_s3_class(res, "data.frame")
  
  # 2. Must contain at least one row
  expect_true(nrow(res) > 0)
  
  # 3. Core columns must exist
  required_cols <- c("expression", "inclS", "covS")
  expect_true(all(required_cols %in% colnames(res)))
  
  # 4. Must include at least one threshold-related column
  thr_cols <- grep("thr|threshold", colnames(res), ignore.case = TRUE, value = TRUE)
  expect_true(length(thr_cols) >= 1)
})

