# tests/testthat/test-s3-methods.R

test_that("otSweep returns correct S3 class", {
  data(sample_data)
  thrX <- c(X1 = 7, X2 = 7, X3 = 7)
  
  result <- otSweep(
    dat = sample_data,
    outcome = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_range = 6:8,
    thrX = thrX
  )
  
  expect_s3_class(result, "otSweep_result")
  expect_s3_class(result, "tsqca_result")
  expect_s3_class(result, "list")
})

test_that("dtSweep returns correct S3 class", {
  data(sample_data)
  
  result <- dtSweep(
    dat = sample_data,
    outcome = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_list_X = list(X1 = 7, X2 = 7, X3 = 7),
    sweep_range_Y = 7:8
  )
  
  expect_s3_class(result, "dtSweep_result")
  expect_s3_class(result, "tsqca_result")
  expect_s3_class(result, "list")
})

test_that("ctSweepS returns correct S3 class", {
  data(sample_data)
  
  result <- ctSweepS(
    dat = sample_data,
    outcome = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_var = "X3",
    sweep_range = 6:8,
    thrY = 7,
    thrX_default = 7
  )
  
  expect_s3_class(result, "ctSweepS_result")
  expect_s3_class(result, "tsqca_result")
  expect_s3_class(result, "list")
})

test_that("ctSweepM returns correct S3 class", {
  data(sample_data)
  
  result <- ctSweepM(
    dat = sample_data,
    outcome = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_list = list(X1 = 7, X2 = 7, X3 = 6:7),
    thrY = 7
  )
  
  expect_s3_class(result, "ctSweepM_result")
  expect_s3_class(result, "tsqca_result")
  expect_s3_class(result, "list")
})

test_that("print.otSweep_result works without error", {
  data(sample_data)
  thrX <- c(X1 = 7, X2 = 7, X3 = 7)
  
  result <- otSweep(
    dat = sample_data,
    outcome = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_range = 6:8,
    thrX = thrX
  )
  
  expect_output(print(result), "OTS-QCA")
})

test_that("summary.otSweep_result works without error", {
  data(sample_data)
  thrX <- c(X1 = 7, X2 = 7, X3 = 7)
  
  result <- otSweep(
    dat = sample_data,
    outcome = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_range = 6:8,
    thrX = thrX
  )
  
  expect_output(summary(result), "Summary")
})

test_that("$ accessor still works after S3 class assignment", {
  data(sample_data)
  thrX <- c(X1 = 7, X2 = 7, X3 = 7)
  
  result <- otSweep(
    dat = sample_data,
    outcome = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_range = 6:8,
    thrX = thrX
  )
  
  # Existing access methods should still work

  expect_true(is.data.frame(result$summary))
  expect_true(is.list(result$details))
  expect_true(is.list(result$params))
})

test_that("return_details = FALSE does not add S3 class", {
  data(sample_data)
  thrX <- c(X1 = 7, X2 = 7, X3 = 7)
  
  result <- otSweep(
    dat = sample_data,
    outcome = "Y",
    conditions = c("X1", "X2", "X3"),
    sweep_range = 6:8,
    thrX = thrX,
    return_details = FALSE
  )
  
  # Should be a plain data.frame, not S3 class
  expect_true(is.data.frame(result))
  expect_false(inherits(result, "tsqca_result"))
})

test_that("print works with negated outcome", {
  data(sample_data)
  thrX <- c(X1 = 7, X2 = 7, X3 = 7)
  
  result <- otSweep(
    dat = sample_data,
    outcome = "~Y",
    conditions = c("X1", "X2", "X3"),
    sweep_range = 6:8,
    thrX = thrX
  )
  
  expect_output(print(result), "negated")
})
