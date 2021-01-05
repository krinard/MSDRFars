testthat::test_that("make filename", {
  filename1 <- MSDRFars::make_filename("2020")
  testthat::expect_error(basename(filename1), "accident_2020.csv.bz2")

  filename2 <- MSDRFars::make_filename(2012)
  testthat::expect_equal(basename(filename2), "accident_2012.csv.bz2")
})
