% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers-conjoint-experiments.R
\name{tidy.amce}
\alias{tidy.amce}
\title{Tidy estimates from the amce estimator}
\usage{
\method{tidy}{amce}(x, alpha = 0.05, ...)
}
\arguments{
\item{x}{an amce fit object from cjoint::amce}

\item{alpha}{Confidence level}

\item{...}{Extra arguments to pass to tidy}
}
\value{
a data.frame of estimates
}
\description{
Runs amce estimation function and returns tidy data frame output
}
\details{
See https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments
}
\examples{

\donttest{
library(cjoint)

data(immigrationconjoint)
data(immigrationdesign)

# Run AMCE estimator using all attributes in the design
results <- amce(Chosen_Immigrant ~  Gender + Education + `Language Skills` +
                  `Country of Origin` + Job + `Job Experience` + `Job Plans` +
                  `Reason for Application` + `Prior Entry`, data = immigrationconjoint,
                cluster = TRUE, respondent.id = "CaseID", design = immigrationdesign)

# Print summary
# tidy(results)
}

}
