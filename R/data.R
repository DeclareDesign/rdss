#' Voter file sample for Los Angeles County
#'
#' Party registration, age, census tract, and 2012 turnout for 1,000 registered voters sampled at random from the Los Angeles County voter file. One row per voter.
#'
#' @format A data frame with 1000 rows and 4 variables:
#' \describe{
#'   \item{party}{political party registration}
#'   \item{age}{age of voter in years}
#'   \item{census_tract}{US Census tract number}
#'   \item{voted_2012}{1 if the voter voted in the 2012 general election, 0 otherwise}
#' }
#' @source California Secretary of State.
#' @examples
#'
#' table(la_voter_file$party)
#'
"la_voter_file"

#' Shapefile of Fairfax County, Virginia, voting precincts
#'
#' Boundaries of the 238 voting precincts in Fairfax County, Virginia, with each precinct's ID, name, district, and polling place. One row per precinct. The book uses it to build the adjacency matrix for the experiment over networks; see [estimator_AS_tidy()].
#'
#' @format An sf object with 238 rows and 10 columns:
#' @source Fairfax County Government, voting precincts layer (`REGMGR.VOTING_PRECINCTS`).
#' @examples
#'
#' names(fairfax)
#'
"fairfax"

#' Replication data for Bonilla and Tillery (2020), American Political Science Review (obtained from Dataverse 10.7910/DVN/IUZDQI)
#'
#' One row per survey respondent. `Z` is the randomly assigned message (a factor with four levels) and `blm_support` the outcome; the remaining columns are respondent characteristics.
#'
#' @format A data frame with 849 rows and 10 columns: `female`, `lgbtq`, `age`, `religiosity`, `income`, `college`, `blm_familiarity`, `linked_fate`, `Z`, and `blm_support`.
#' @source Bonilla, Tabitha, and Alvin B. Tillery. 2020. "Which Identity Frames Boost Support for and Mobilization in the #BlackLivesMatter Movement? An Experimental Test." *American Political Science Review* 114(4): 947-962. \doi{10.1017/S0003055420000544}. Data from Dataverse, \doi{10.7910/DVN/IUZDQI}.
#' @examples
#'
#' estimatr::lm_robust(blm_support ~ Z, data = bonilla_tillery)
#'
"bonilla_tillery"

#' Replication data for Foos, John, Muller, and Cunningham (2021), Journal of Politics (derived from Dataverse 10.7910/DVN/NDPXND)
#'
#' One row per voter. `treat` is the randomly assigned treatment, `marked_register_2014` the turnout outcome, and `ward` and `street` (a hashed identifier) the levels at which voters are clustered.
#'
#' @format A data frame with 8,375 rows and 5 columns: `marked_register_2014`, `ward`, `street`, `treat`, and `weights`.
#' @source Foos, Florian, Peter John, Christian Müller, and Kevin Cunningham. 2021. "Social Mobilization in Partisan Spaces." *The Journal of Politics* 83(3): 1190-1197. \doi{10.1086/710970}. Data from Dataverse, \doi{10.7910/DVN/NDPXND}.
#' @examples
#'
#' estimatr::lm_robust(marked_register_2014 ~ treat, clusters = street, data = foos_etal)
#'
"foos_etal"

#' Replication data for Clingingsmith, Khwaja, and Kremer (2009), Quarterly Journal of Economics
#'
#' From "Estimating the Impact of the Hajj: Religion and Tolerance in Islam's Global Gathering," The Quarterly Journal of Economics 124(3): 1133-1170. One row per applicant to the Hajj visa lottery. `success` is 1 for lottery winners; the `views_` columns are attitudes toward each group, and `views` is their sum.
#'
#' @format A data frame with 958 rows and 8 columns: `success`, `views_saudi`, `views_indonesian`, `views_turkish`, `views_african`, `views_chinese`, `views_european`, and `views`.
#' @source Clingingsmith, David, Asim Ijaz Khwaja, and Michael Kremer. 2009. "Estimating the Impact of the Hajj: Religion and Tolerance in Islam's Global Gathering." *The Quarterly Journal of Economics* 124(3): 1133-1170. \doi{10.1162/qjec.2009.124.3.1133}.
#' @examples
#'
#' estimatr::difference_in_means(views ~ success, data = clingingsmith_etal)
#'
"clingingsmith_etal"

#' Teaching data based on the 2018/19 LAPOP AmericasBarometer survey of Brazil
#'
#' Used in the book's student exercises. These data were resampled with replacement from the LAPOP data, to 10,000 rows, for a subset of variables. These data cannot be used for scientific inferences, and are only useful for teaching purposes. ID numbers were scrambled so that individuals and municipalities cannot easily be identified.
#'
#' Download the original data from https://www.vanderbilt.edu/cgd/data-access/
#'
#' See https://www.vanderbilt.edu/lapop/core-surveys.php for the survey questionnaire.
#'
#' @format A data frame with 10,000 rows and 10 columns: `ID`, `municipality`, and eight survey items (`trust_police`, `govt_responsive`, `ideology`, `govt_pride`, `self_efficacy_political`, `trust_military`, `trust_supreme_court`, and `support_political_system`).
#' @source LAPOP AmericasBarometer 2018/19 survey of Brazil (fieldwork January to March 2019), Vanderbilt University, <https://www.vanderbilt.edu/cgd/data-access/>.
#' @examples
#'
#' summary(lapop_brazil$trust_police)
#'
"lapop_brazil"

