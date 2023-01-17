
#' Download a replication file from the dataverse archive for Research Design in the Social Sciences: Declaration, Diagnosis, and Redesign
#'
#' See https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HYVPO5 for a full list of available files
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
get_rdss_file <- function(name) {
  get_dataframe_by_name(
    filename = paste0(name, ".rds"),
    .f = read_rds,
    dataset = "10.7910/DVN/HYVPO5",
    server = "dataverse.harvard.edu"
  )
}
