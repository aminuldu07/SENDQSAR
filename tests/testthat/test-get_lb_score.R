#library(testthat)
#library(SENDQSAR)

# test_that("get_bw_score returns correct data for valid inputs", {
#   # Set up your test inputs
#   studyid <- '511-21060018'
#   path_db <- 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db'
#   fake_study <- FALSE
#   master_CompileData = NULL
#   score_in_list_format = TRUE
#
#   # Call the function
#   result <- get_lb_score(studyid, path_db, fake_study, master_CompileData, score_in_list_format)
#
#   # Check the expected output
#   #expect_type(result, "data.frame")
#   expect_type(result, "list")
#   expect_true("STUDYID" %in% colnames(result))
# })
#
# test_that("get_compile_data handles invalid inputs gracefully", {
#   # Test with invalid studyid
#   expect_error(get_lb_score(NULL, path_db, fake_study))
#   expect_error(get_lb_score("invalid_study", "wrong/path", fake_study))
# })
