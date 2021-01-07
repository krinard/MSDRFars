testthat::test_that("existing file", {
  filename2 <- MSDRFars::make_filename("2013")
  testthat::expect_equal(basename(filename2), "accident_2013.csv.bz2")
})

testthat::test_that("missing file", {
  testthat::expect_error(MSDRFars::make_filename("2020"))
})

