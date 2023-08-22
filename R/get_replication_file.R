
#' Download a replication file from the dataverse archive for Research Design in the Social Sciences: Declaration, Diagnosis, and Redesign
#'
#' See https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HYVPO5 for further details and the code used to create these files.
#'
#' The available names include:
#'
#' diagnosis_2.1.rds \cr
#' diagnosis_4.1.rds \cr
#' diagnosis_9.1.rds \cr
#' diagnosis_9.2.rds \cr
#' diagnosis_9.3.rds \cr
#' diagnosis_9.4.rds \cr
#' diagnosis_9.5.rds \cr
#' diagnosis_9.6.rds \cr
#' diagnosis_9.7.rds \cr
#' simulation_10.1.rds \cr
#' diagnosis_10.1.rds \cr
#' diagnosis_10.2.rds \cr
#' diagnosis_10.3.rds \cr
#' diagnosis_10.4.rds \cr
#' diagnosis_10.5.rds \cr
#' diagnosis_10a.rds \cr
#' diagnosis_11.1.rds \cr
#' diagnosis_11.2.rds \cr
#' diagnosis_11.3.rds \cr
#' diagnosis_11.4.rds \cr
#' diagnosis_11.5.rds \cr
#' diagnosis_12.1.rds \cr
#' diagnosis_12.2.rds \cr
#' diagnosis_13.1.rds \cr
#' diagnosis_15.1.rds \cr
#' diagnosis_15.2.rds \cr
#' diagnosis_15.3.rds \cr
#' diagnosis_15.4.rds \cr
#' diagnosis_15.5.rds \cr
#' diagnosis_16.1.rds \cr
#' diagnosis_16.2.rds \cr
#' diagnosis_16.3.rds \cr
#' diagnosis_16.4.rds \cr
#' diagnosis_16.5.rds \cr
#' diagnosis_17.1.rds \cr
#' diagnosis_17.2.rds \cr
#' diagnosis_17.3.rds \cr
#' diagnosis_17.4.rds \cr
#' diagnosis_17.5.rds \cr
#' diagnosis_18.1.rds \cr
#' diagnosis_18.10_encouragment.rds \cr
#' diagnosis_18.10_placebo.rds \cr
#' diagnosis_18.11.rds \cr
#' diagnosis_18.12.rds \cr
#' diagnosis_18.13.rds \cr
#' diagnosis_18.2.rds \cr
#' diagnosis_18.3.rds \cr
#' diagnosis_18.4.rds \cr
#' diagnosis_18.5.rds \cr
#' diagnosis_18.6.rds \cr
#' diagnosis_18.7.rds \cr
#' diagnosis_18.8.rds \cr
#' diagnosis_18.9.rds \cr
#' diagnosis_19.1.rds \cr
#' diagnosis_19.2.rds \cr
#' diagnosis_19.3.rds \cr
#' diagnosis_19.4.rds \cr
#' diagnosis_19a.rds \cr
#' diagnosis_21a.rds \cr
#' diagnosis_21b.rds \cr
#' diagnosis_23.1.rds \cr
#' diagnosis_23a.rds \cr
#'
#' @param name quoted name of the file on the dataverse archive
#'
#' @return an r object
#' @export
#'
#' @importFrom dataverse get_dataframe_by_name
#' @importFrom readr read_rds
#'
#' @examples
#'
#' \donttest{
#' diagnosis_2.1 <- get_rdss_file("diagnosis_2.1")
#' diagnosis_2.1
#' }
get_rdss_file <- function(name, verbose = TRUE) {
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
    require(DeclareDesign)
    new_env <- new.env()
    return(eval(parse(text = file_lines), envir = new_env))
  } else {
    stop("Only declarations and diagnosis objects are currently supported.")
  }
}
