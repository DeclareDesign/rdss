
test_that("book runs", {
  test_files <- list.files("tests/book_purl_scripts")
  cmd <- paste0("Rscript tests/book_purl_scripts/", test_files)
  # for(i in seq_along(cmd)){
  #   test_output <- system(cmd[i])
  #   expect_equals(test_output, 0)
  # }

})
