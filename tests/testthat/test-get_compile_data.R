# library(testthat)
# library(SENDQSAR)
#
# test_that("get_compile_data returns correct data for valid inputs", {
#   # Set up your test inputs
#   studyid <- '511-21060018'
#   path_db <- 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db'
#   fake_study <- FALSE
#
#   # Call the function
#   result <- get_compile_data(studyid, path_db, fake_study)
#
#   # Check the expected output
#   #expect_type(result, "data.frame")
#   expect_type(result, "list")
#   expect_true("STUDYID" %in% colnames(result))
# })
#
# test_that("get_compile_data handles invalid inputs gracefully", {
#   # Test with invalid studyid
#   expect_error(get_compile_data(NULL, path_db, fake_study))
#   expect_error(get_compile_data("invalid_study", "wrong/path", fake_study))
# })
#
#
#
