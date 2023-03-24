#' Voter file sample for Los Angeles County
#'
#' A dataset containing the party registration, age, census tract number, and voter turnout in 2012 for 1,000 randomly-sampled registered voters in Los Angeles County, California.
#'
#' @format A data frame with 1000 rows and 4 variables:
#' \describe{
#'   \item{party}{political party registration}
#'   \item{age}{age of voter in years}
#'   \item{census_tract}{US Census tract number}
#'   \item{voted_2012}{voter turnout in 2012 election}
#' }
#' @source California Secretary of State.
"la_voter_file"

#' Shapefile of Fairfax County, Virginia, voting precincts
#'
#' An sf object containing the boundaries of voting precincts for Fairfax County, Virginia as well as precinct ID, name, district, polling place name, address, city, zip code, area, length, and geometry (polygons)
#'
#' @format An sf object with 236 rows and 10 variables:
"fairfax"

#' Replication data for Bonilla and Tillery (2020), American Political Science Review (obtained from Dataverse 10.7910/DVN/IUZDQI)
#'
#' @format A data.frame
"bonilla_tillery"

#' Replication data for Foos, John, Muller, and Cunningham (2021), Journal of Politics (derived from from Dataverse 10.7910/DVN/NDPXND)
#'
#' @format A data.frame
"foos_etal"

#' Replication data for David Clingingsmith, Asim Ijaz Khwaja, Michael Kremer (2020): Estimating the Impact of The Hajj: Religion and Tolerance in Islam's Global Gathering. The Quarterly Journal of Economics, Volume 124, Issue 3, August 2009, Pages 1133–1170
#'
#' @format A data.frame
"clingingsmith_etal"

#' Data used in student exercises for RDSS based on LAPOP survey of Brazil in 2018
#'
#' These data were resampled with replacement from LAPOP data (to 10,000 rows) for a subset of variables. These data cannot be used for scientific inferences, and are only useful for teaching purposes. ID numbers were scrambled so that individuals and municipalities cannot easily be identified.
#'
#' Download the original data from https://www.vanderbilt.edu/lapop/raw-data.php
#'
#' See https://www.vanderbilt.edu/lapop/core-surveys.php for survey questionnaire
#'
#' @format A data.frame
"lapop_brazil"

