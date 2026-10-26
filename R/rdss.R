#' rdss package
#'
#' Companion datasets and functions for the book *Research Design in the Social Sciences: Declaration, Diagnosis, and Redesign* by Graeme Blair, Alexander Coppock, and Macartan Humphreys (Princeton University Press, 2023), free to read at <https://book.declaredesign.org>.
#'
#' Most functions fill one step of a design declaration in the book, and each help page links to the section that uses it. The Get started article on the package website, <https://declaredesign.org/r/rdss/articles/rdss.html>, lists them chapter by chapter.
#'
#' @name rdss
"_PACKAGE"

#' @importFrom generics tidy
#' @export
generics::tidy

utils::globalVariables(c("X", "variance", "estimate", "std.error", "value", "name", "order_by___", "state", "fitted", "PS_weight", "covariates", "pred", "type", "tau.squared", "tau.squared.se", "term", "ci.lb", "ci.ub", "Estimate", "Std. Err", "z value", "Pr(>|z|)", "Attribute", "Level", "profile", "attribute", "level", "partial", "conjoint_utility", "subject", "task", "U", "XY"))

