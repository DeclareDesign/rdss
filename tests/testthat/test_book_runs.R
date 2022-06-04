library(DeclareDesign)
library(tidyverse)
library(rdddr)

test_that("book runs", {
  test_files <- list.files("../book_scripts")
  cmd <- paste0("Rscript ../book_scripts/", test_files)
  for(i in seq_along(cmd)){
    test_output <- system(cmd[i])
    expect_equal(test_output, 0)
  }

})
