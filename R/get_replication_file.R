
#' Download a replication file from the dataverse archive for Research Design in the Social Sciences: Declaration, Diagnosis, and Redesign
#'
#' See https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HYVPO5 for further details and the code used to create these files.
#'
#' The available names include:
#'
#' Design declaration objects:
#'
#' declaration_9.5 \cr
#' declaration_2.1 \cr
#' declaration_2.2 \cr
#' declaration_4.1 \cr
#' declaration_5.1 \cr
#' declaration_7.1 \cr
#' declaration_9.1 \cr
#' declaration_9.2 \cr
#' declaration_9.3 \cr
#' declaration_9.4 \cr
#' declaration_9.6 \cr
#' declaration_9.7 \cr
#' declaration_10.1 \cr
#' declaration_10.2 \cr
#' declaration_10.3 \cr
#' declaration_10.4 \cr
#' declaration_10a \cr
#' declaration_11.1 \cr
#' declaration_11.2 \cr
#' declaration_11.3 \cr
#' declaration_11.4 \cr
#' declaration_11.5 \cr
#' declaration_12.1a \cr
#' declaration_12.1b \cr
#' declaration_12.1c \cr
#' declaration_12.1d \cr
#' declaration_13.1 \cr
#' declaration_13.2 \cr
#' declaration_15.1 \cr
#' declaration_15.2 \cr
#' declaration_15.3a \cr
#' declaration_15.3b \cr
#' declaration_15.3c \cr
#' declaration_15.4 \cr
#' declaration_15.5 \cr
#' declaration_15.6 \cr
#' declaration_16.1a \cr
#' declaration_16.1b \cr
#' declaration_16.2 \cr
#' declaration_16.3 \cr
#' declaration_16.4 \cr
#' declaration_16.5 \cr
#' declaration_16.6 \cr
#' declaration_17.1 \cr
#' declaration_17.2 \cr
#' declaration_17.3 \cr
#' declaration_17.4 \cr
#' declaration_17.5 \cr
#' declaration_17.6_a \cr
#' declaration_17.6_b \cr
#' declaration_18.1 \cr
#' declaration_18.2 \cr
#' declaration_18.3 \cr
#' declaration_18.4 \cr
#' declaration_18.5 \cr
#' declaration_18.6 \cr
#' declaration_18.7 \cr
#' declaration_18.8 \cr
#' declaration_18.9a \cr
#' declaration_18.9b \cr
#' declaration_18.9c \cr
#' declaration_18.10 \cr
#' declaration_18.11 \cr
#' declaration_18.12 \cr
#' declaration_18.13 \cr
#' declaration_19.1 \cr
#' declaration_19.2 \cr
#' declaration_19.3 \cr
#' declaration_19.4 \cr
#' declaration_23.1a \cr
#' declaration_23.1b \cr
#' declaration_23.1c \cr
#' declaration_23.1d \cr
#'
#' Diagnosis objects:
#'
#' diagnosis_2.1 \cr
#' diagnosis_4.1 \cr
#' diagnosis_9.1 \cr
#' diagnosis_9.2 \cr
#' diagnosis_9.3 \cr
#' diagnosis_9.4 \cr
#' diagnosis_9.5 \cr
#' diagnosis_9.6 \cr
#' diagnosis_9.7 \cr
#' simulation_10.1 \cr
#' diagnosis_10.1 \cr
#' diagnosis_10.2 \cr
#' diagnosis_10.3 \cr
#' diagnosis_10.4 \cr
#' diagnosis_10.5 \cr
#' diagnosis_10a \cr
#' diagnosis_11.1 \cr
#' diagnosis_11.2 \cr
#' diagnosis_11.3 \cr
#' diagnosis_11.4 \cr
#' diagnosis_11.5 \cr
#' diagnosis_12.1 \cr
#' diagnosis_12.2 \cr
#' diagnosis_13.1 \cr
#' diagnosis_15.1 \cr
#' diagnosis_15.2 \cr
#' diagnosis_15.3 \cr
#' diagnosis_15.4 \cr
#' diagnosis_15.5 \cr
#' diagnosis_16.1 \cr
#' diagnosis_16.2 \cr
#' diagnosis_16.3 \cr
#' diagnosis_16.4 \cr
#' diagnosis_16.5 \cr
#' diagnosis_17.1 \cr
#' diagnosis_17.2 \cr
#' diagnosis_17.3 \cr
#' diagnosis_17.4 \cr
#' diagnosis_17.5 \cr
#' diagnosis_18.1 \cr
#' diagnosis_18.10_encouragment \cr
#' diagnosis_18.10_placebo \cr
#' diagnosis_18.11 \cr
#' diagnosis_18.12 \cr
#' diagnosis_18.13 \cr
#' diagnosis_18.2 \cr
#' diagnosis_18.3 \cr
#' diagnosis_18.4 \cr
#' diagnosis_18.5 \cr
#' diagnosis_18.6 \cr
#' diagnosis_18.7 \cr
#' diagnosis_18.8 \cr
#' diagnosis_18.9 \cr
#' diagnosis_19.1 \cr
#' diagnosis_19.2 \cr
#' diagnosis_19.3 \cr
#' diagnosis_19.4 \cr
#' diagnosis_19a \cr
#' diagnosis_21a \cr
#' diagnosis_21b \cr
#' diagnosis_23.1 \cr
#' diagnosis_23a \cr
#'
#' @param name quoted name of the file on the dataverse archive
#' @param verbose print declaration code if requesting a declaration
#'
#' @return an r object
#' @export
#'
#' @importFrom dataverse get_dataframe_by_name get_file_by_name
#' @importFrom readr read_rds
#'
#' @examples
#'
#' \donttest{
#' # Requires internet access
#' if(curl::has_internet()) {
#'   diagnosis_2.1 <- get_rdss_file("diagnosis_2.1")
#'   diagnosis_2.1
#' }
#' }
get_rdss_file <- function(name, verbose = TRUE) {
  if(!"DeclareDesign" %in% loadedNamespaces()){
    stop("Please load DeclareDesign before running this function via library(DeclareDesign).")
  }
  if(!curl::has_internet()) {
    stop("This function requires internet access.")
  }
  if(substr(name, 1, 9) == "diagnosis") {
    return(get_dataframe_by_name(
      filename = paste0(name, ".rds"),
      .f = read_rds,
      dataset = "10.7910/DVN/HYVPO5",
      server = "dataverse.harvard.edu"
    ))
  } else if(substr(name, 1, 11) == "declaration") {
    file_bin <- get_file_by_name(
      filename = paste0(name, ".R"),
      dataset = "10.7910/DVN/HYVPO5",
      server = "dataverse.harvard.edu"
    )
    file_text <- rawToChar(file_bin)
    file_lines <- strsplit(file_text, split = "\n")[[1]]
    if(verbose) cat("\nDesign declaration:", name, "\n\n", file_text, "\n\n")
    return(eval(parse(text = file_lines)))
  } else {
    stop("Only declarations and diagnosis objects are currently supported.")
  }
}

