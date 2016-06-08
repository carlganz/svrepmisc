#' svrepmisc: Miscellaneous Functions for Replicate Weights
#'
#' Wrapper functions for Complex Surveys using replicate weights.
#' Takes advantage of \code{\link[survey]{withReplicates}}.
#' @import survey
#' @docType package
#' @name svrepmisc
#'
NULL

# helper function
wR <- function(FUN, formula, design, subset, ..., scale.weights=FALSE) {
  # stolen from Lumley
  # surveyrep.R line 1311
  subset <- substitute(subset)
  subset <- eval(subset, design$variables, parent.frame())
  if (!is.null(subset)) {
    design <- design[subset, ]
  }

  # per Lumley textbook appendix E, often better to rescale weights
  if (scale.weights) {
    design$pweights <- design$pweights/mean(design$pweights)
  }

  est <- survey::withReplicates(design,
                        function(w, data) {
                          environment(formula) <- environment()
                          out <- match.call()
                          out$formula <- formula
                          out$data <- data
                          out$weight <- w
                          out$... <- list(...)
                          out[[1]] <- FUN
                          coef(eval(out))
                        })

  attr(est, "statistic") <- "Coefficient"
  return(est)

}

#' Wrapper for Multinomial Logistic Regression for Replicate Weights
#'
#' Uses \code{\link[survey]{withReplicates}} and \code{\link[nnet]{multinom}} to generate
#' coefficients, and standards errors for multinomial logistic regressions
#' using replicate weights
#'
#' @note Output is consistent with SAS's proc surveylogistic's multinomial
#' survey output
#'
#' @export
#' @seealso \code{\link[survey]{withReplicates}} \code{\link[nnet]{multinom}}
#' @param formula Model formula
#' @param design Survey design from \code{\link[survey]{svrepdesign}}
#' @param subset Expression to select a subpopulation
#' @param ... Other arugments passed to \code{\link[nnet]{multinom}}
#' @param scale.weights Indicate whether to rescale weights (defaults to false)
#' @importFrom nnet multinom
#'

svymultinom <- function(formula, design, subset, ..., scale.weights=FALSE) {
  wR(nnet::multinom,formula,design,subset,..., scale.weights)

}

#' Wrapper for Quantile Regression
#'
#' Wrapper for \code{\link[quantreg]{rq}} for replicate weights
#'
#' @seealso \code{\link[survey]{withReplicates}} \code{\link[quantreg]{rq}}
#' @param formula Model formula
#' @param design Survey design from \code{\link[survey]{svrepdesign}}
#' @param subset Expression to select a subpopulation
#' @param ... Other arugments passed to \code{\link[quantreg]{rq}}
#' @param scale.weights Indicate whether to rescale weights (defaults to false)
#' @importFrom quantreg rq
#' @export

svyrq <- function(formula, design, subset, ..., scale.weights=FALSE) {
  wR(quantreg::rq, formula, design, subset, ..., scale.weights)

}

#' Wrapper for MASS functions
#'
#' Wrapper for several functions from \pkg{MASS} for replicate weights
#'
#' @seealso \code{\link[survey]{withReplicates}} \code{\link[MASS]{polr}} \code{\link[MASS]{glm.nb}}
#' @param formula Model formula
#' @param design Survey design from \code{\link[survey]{svrepdesign}}
#' @param subset Expression to select a subpopulation
#' @param ... Other arugments passed to \code{\link[MASS]{polr}} or
#' \code{\link[MASS]{glm.nb}} or \code{\link[MASS]{rlm}}
#' @param scale.weights Indicate whether to rescale weights (defaults to false)
#'
#' @importFrom MASS polr
#' @export

svypolr <- function(formula, design, subset, ..., scale.weights=FALSE) {
  wR(MASS::polr, formula, design, subset, ..., scale.weights)

}

#' @rdname svypolr
#' @importFrom MASS glm.nb
#' @export

svynb <- function(formula, design, subset, ..., scale.weights=FALSE) {
  wR(MASS::glm.nb, formula, design, subset, ..., scale.weights)

}

#' @rdname svypolr
#' @importFrom MASS rlm
#' @export

svyrlm <- function(formula, design, subset, ..., scale.weights=FALSE) {
  wR(MASS::rlm, formula, design, subset, ..., scale.weights)

}

#' Wrapper for Truncated Response Model
#'
#' Wrapper for \code{\link[truncreg]{truncreg}} for replicate weights
#'
#' @importFrom truncreg truncreg
#' @param formula Model formula
#' @param design Survey design from \code{\link[survey]{svrepdesign}}
#' @param subset Expression to select a subpopulation
#' @param ... Other arugments passed to \code{\link[truncreg]{truncreg}}
#' @param scale.weights Indicate whether to rescale weights (defaults to false)
#' @export

svytruncreg <- function(formula, design, subset, ..., scale.weights=FALSE) {
  wR(truncreg::truncreg, formula, design, subset, ..., scale.weights)

}
