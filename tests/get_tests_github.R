
library(parsermd)
library(purrr)
library(tidyverse)

clean_file_name <- function(str) {
  str %>% tolower %>% str_replace_all(.,  "[^[:alpha:] ]", " ") %>% str_squish %>% str_trim %>% str_replace_all(., " ", "-") %>% str_replace_all(., ".rmd", "")
}

clean_section_name <- function(str) {
  str %>% tolower %>% str_replace_all(.,  "[^[:alpha:] ]", " ") %>% str_squish %>% str_trim %>% str_replace_all(., ".rmd", "") %>% str_to_sentence()
}

# dir.create("_book")

# rmds <- list.files(pattern = c('.Rmd', '.rmd'), recursive = TRUE)
library(httr)
req <- GET("https://api.github.com/repos/DeclareDesign/book/git/trees/master?recursive=1")
stop_for_status(req)
filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
rmds <- filelist[str_detect(filelist, c(".Rmd|.rmd"))]

rmds <- rmds[!rmds %in% c("book.Rmd", "index.Rmd", "999_References/00_references.Rmd") & !str_detect(rmds, "/000_")]

urls <- paste0("https://raw.github.com/DeclareDesign/book/master/", rmds)

do_diagnosis <- TRUE

rmds_files <- map(urls, read_file)

rmd_df <-
  rmds_files %>%
  set_names(nm = rmds) %>%
  map(parse_rmd) %>%
  map_df(as_tibble, .id = "filename") %>%
  separate(filename, into = c("part", "section", "subsection"), sep = "/") %>%
  mutate(options = rmd_node_options(ast)) %>%
  unnest_wider(col = options) %>%
  unite(col = echo, starts_with("echo"), na.rm = TRUE) %>%
  mutate(echo = replace(echo, echo == "", NA_character_)) |>
  filter(!label %in% c("install_package_example", "install_tidyverse_example"))

collapse_ast <- function(x) sapply(lapply(x, as_document), paste0, collapse = "\n")

# output top level R files at the .html level
part_i_ii_chunk_df <-
  rmd_df %>%
  mutate(
    output_sec_name = case_when(
      !part %in% "03_Research_Design_Library" ~ sec_h1,
      part %in% c("03_Research_Design_Library", "04_Research_Design_Lifecycle") ~ section
    ),
    output_file_name = clean_file_name(output_sec_name)
  ) %>%
  filter(!is.na(output_file_name)) %>% # filter out garbage graeme created to make it easy to write single .Rmds
  filter(type == "rmd_chunk") %>%
  filter(((echo == TRUE | is.na(echo)) & is.na(purl)) | purl == TRUE)

# part_i_ii_chunk_df |> filter(section == "04_Software_Primer.Rmd") |> select(ast) |> mutate(code = collapse_ast(ast)) |> summarize(paste0(code, collapse = "\n\n"))

part_i_ii_chunk_df %>%
  mutate(eval = TRUE) %>%
  mutate(sec_name = str_c(str_trim(sec_h2), if_else(!is.na(sec_h3), str_c(" :: ", str_trim(sec_h3)), ""))) %>%
  mutate(sec_name = if_else(sec_name == lag(sec_name) & !is.na(lag(sec_name)), "", sec_name))  %>%
  mutate(
    sec_code = collapse_ast(ast),
    sec_code = str_replace(sec_code, "eval = FALSE", "eval = TRUE"),
    sec_header_text = if_else(sec_name != "", str_c("```{r, echo = TRUE}\n# ", str_dup("-", str_length(sec_name)), "\n# ", sec_name, "\n# ", str_dup("-", str_length(sec_name)), "\n```\n"), ""),
    sec_text = str_c(sec_header_text, sec_code)
  ) %>%
  group_by(output_file_name) %>%
  summarize(
    purl_text = paste0(sec_text, collapse = "\n\n")
  ) %>%
  rowwise() %>%
  group_walk(~ knitr::purl(text = .x$purl_text, output = paste0("tests/book_purl_scripts/", .x$output_file_name, ".R")))

# now output subsections (for Part III and IV)

part_iii_iv_chunk_df <-
  rmd_df %>%
  filter(part %in% c("03_Research_Design_Library", "04_Research_Design_Lifecycle")) %>%
  mutate(
    output_sec_name = subsection,
    output_file_name = clean_file_name(output_sec_name)
  ) %>%
  filter(!is.na(output_file_name)) %>% # filter out garbage graeme created to make it easy to write single .Rmds
  filter(type == "rmd_chunk") %>%
  filter(((echo == TRUE | is.na(echo)) & is.na(purl)) | purl == TRUE)

part_iii_iv_chunk_df %>%
  mutate(eval = TRUE) %>%
  mutate(sec_name = str_c(str_trim(sec_h2), if_else(!is.na(sec_h3), str_c(" :: ", str_trim(sec_h3)), ""))) %>%
  mutate(sec_name = if_else(sec_name == lag(sec_name) & !is.na(lag(sec_name)), "", sec_name))  %>%
  # mutate(sec_code = str_c("```{r, echo = TRUE}\n# ", str_dup("-", str_length(sec_name)), "\n# ", sec_name, "\n# ", str_dup("-", str_length(sec_name)), "\n```\n", collapse_ast(ast))) %>%
  mutate(
    sec_code = collapse_ast(ast),
    sec_code = str_replace(sec_code, "eval = FALSE", "eval = TRUE"),
    sec_header_text = if_else(sec_name != "", str_c("```{r, echo = TRUE}\n# ", str_dup("-", str_length(sec_name)), "\n# ", sec_name, "\n# ", str_dup("-", str_length(sec_name)), "\n```\n"), ""),
    sec_text = str_c(sec_header_text, sec_code)
  ) %>%
  group_by(output_file_name) %>%
  summarize(
    purl_text = paste0(sec_text, collapse = "\n\n")
  ) %>%
  rowwise() %>%
  group_walk(~ knitr::purl(text = .x$purl_text, output = paste0("tests/book_purl_scripts/", .x$output_file_name, ".R")))

