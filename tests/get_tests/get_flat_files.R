
library(parsermd)
library(purrr)
library(tidyverse)
library(httr)

req <- GET("https://api.github.com/repos/DeclareDesign/book/git/trees/master?recursive=1")
stop_for_status(req)
filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)

r_nms <- filelist[str_detect(filelist, c(".R|.r"))]
r_nms <- r_nms[str_detect(r_nms, "scripts_")]
r_nms <- r_nms[str_detect(r_nms, "scripts_figures", negate = TRUE)]
r_nms <- r_nms[str_detect(r_nms, ".R")]
r_nms <- r_nms[str_detect(r_nms, "purl_R_scripts_tests.R", negate = TRUE)]

urls <- paste0("https://raw.github.com/DeclareDesign/book/master/", r_nms)

r_files <- map(urls, read_delim, col_names = FALSE, delim = "XT$*%@(442555")

r_files %>%
  set_names(nm = r_nms) %>%
  map(~bind_rows(tibble(X1 = "library(DeclareDesign); library(rdddr); library(tidyverse)\n\n"), .x)) %>%
  # map(~bind_rows(tibble(X1 = "context('"), .x)) %>%
  bind_rows(.id = "filename") %>%
  # map_df(as_tibble, .id = "filename") %>%
  separate(filename, into = c("dir", "file"), sep = "/") %>%
  rename(value = X1) %>%
  mutate(file = str_replace(file, "simulation", "a_simulation")) %>%
  mutate(value = str_replace(value, "sims <- 2000", "sims <- 3")) %>%
  mutate(value = str_replace(value, "bootstrap_sims <- 2000", "bootstrap_sims <- 3")) %>%
  mutate(value = if_else(str_detect(value, "write_rds"), "", value)) %>%
  mutate(value = str_replace(value, "scripts_declarations", "..\\/book_scripts")) %>%
  mutate(value = str_replace(value, "scripts_declarations", "..\\/book_scripts")) %>%
  mutate(value = if_else(value == "library(DeclareDesign); library(rdddr); library(tidyverse)\n\n",
                         str_c("print('", file, "'); library(DeclareDesign); library(rdddr); library(tidyverse)\n\n"),
                         value)) %>%
  # mutate(value = str_c("library(DeclareDesign); library(rdddr); library(tidyverse)\n\n", value)) %>%
  group_by(dir, file) %>%
  summarize(
    value = str_c(value, collapse = "\n"),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  group_walk(~ write_lines(x = .x$value, file = paste0("tests/book_scripts/", .x$file)))
