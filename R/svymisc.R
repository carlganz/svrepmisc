#' svrepmisc: Miscellaneous Functions for Replicate Weights
#'
#' Wrapper functions for Complex Surveys using replicate weights.
#' Takes advantage of \code{\link[survey]{withReplicates}}.
#' @import survey
#' @importFrom stats coef
#' @importFrom stats printCoefmat
#' @importFrom stats pt
#' @docType package
#' @name svrepmisc
#'
NULL

# helper function
wR <- function(FUN, formula, design, subset, ..., scale.weights=FALSE) {
  # stolen from Lumley
  # surveyrep.R line 1311
  if (!missing(subset)) {
  subset <- substitute(subset)
  subset <- eval(subset, design$variables, parent.frame())
  if (!is.null(subset)) {
    design <- design[subset, ]
  }
  }

  est <- survey::withReplicates(design,
                        function(w, data) {
                          environment(formula) <- environment()
                          vals <- stats::coef(FUN(formula=formula,data=data,weights=w,...))
                          if (is.matrix(vals)) {
                            vals <- mat2vec(vals)
                          }
                          return(vals)
                        }, scale.weights=scale.weights)

  attr(est, "statistic") <- "Coefficient"
  class(est) <- c("svrepstatmisc",class(est))
  # from Lumley surveyrep.R line 1404
  df.residual <- degf(design)+1-length(est)
  attr(est, "df.residual") <- df.residual
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
#' @references Lumley, Thomas. Complex Surveys: A Guide to Analisys Using R.
#'  Hoboken, NJ: Wiley, 2010. Print.


svymultinom <- function(formula, design, subset, ..., scale.weights=FALSE) {
  wR(nnet::multinom,formula,design,subset,..., scale.weights=scale.weights)

}

#' Wrapper for Quantile Regression
#'
#' Wrapper for \code{\link[quantreg]{rq}} and \code{\link[quantreg]{rqss}} for replicate weights
#'
#' @seealso \code{\link[survey]{withReplicates}} \code{\link[quantreg]{rq}} \code{\link[quantreg]{rqss}}
#' @param formula Model formula
#' @param design Survey design from \code{\link[survey]{svrepdesign}}
#' @param subset Expression to select a subpopulation
#' @param ... Other arugments passed to \code{\link[quantreg]{rq}} or \code{\link[quantreg]{rqss}}
#' @param scale.weights Indicate whether to rescale weights (defaults to false)
#' @importFrom quantreg rq
#' @importFrom quantreg rqss
#' @export
#' @references Lumley, Thomas. Complex Surveys: A Guide to Analisys Using R.
#'  Hoboken, NJ: Wiley, 2010. Print.

svyrq <- function(formula, design, subset, ..., scale.weights=FALSE) {
  wR(quantreg::rq, formula, design, subset, ..., scale.weights=scale.weights)

}

#' @export
#' @rdname svyrq
svyrqss <- function(formula, design, subset, ..., scale.weights=FALSE) {
  wR(quantreg::rqss,formula,design,subset,...,scale.weights = scale.weights)

}

#' Wrapper for Negative Binomial
#'
#' Wrapper for \code{\link[MASS]{glm.nb}} for replicate weights
#'
#' @seealso \code{\link[survey]{withReplicates}} \code{\link[MASS]{glm.nb}}
#' @param formula Model formula
#' @param design Survey design from \code{\link[survey]{svrepdesign}}
#' @param subset Expression to select a subpopulation
#' @param ... Other arugments passed to \code{\link[MASS]{glm.nb}}
#' @param scale.weights Indicate whether to rescale weights (defaults to false)
#' @importFrom MASS glm.nb
#' @export
#' @references Lumley, Thomas. Complex Surveys: A Guide to Analisys Using R.
#'  Hoboken, NJ: Wiley, 2010. Print.

svynb <- function(formula, design, subset, ..., scale.weights=FALSE) {
  wR(MASS::glm.nb, formula, design, subset, ..., scale.weights=scale.weights)

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
#' @note weights in \code{\link[truncreg]{truncreg}} do not currently work so this function
#' also doesn't work
#' @export
#' @references Lumley, Thomas. Complex Surveys: A Guide to Analisys Using R.
#'  Hoboken, NJ: Wiley, 2010. Print.

svytruncreg <- function(formula, design, subset, ..., scale.weights=FALSE) {
  wR(truncreg::truncreg, formula, design, subset, ..., scale.weights=scale.weights)

}

#' Wrapper for Interval Regression
#'
#' Wrapper for \code{\link[intReg]{intReg}} for replicate weights
#'
#' @importFrom intReg intReg
#' @param formula Model formula
#' @param design Survey design from \code{\link[survey]{svrepdesign}}
#' @param subset Expression to select a subpopulation
#' @param ... Other arugments passed to \code{\link[intReg]{intReg}}
#' @param scale.weights Indicate whether to rescale weights (defaults to false)
#' @export
#' @references Lumley, Thomas. Complex Surveys: A Guide to Analisys Using R.
#'  Hoboken, NJ: Wiley, 2010. Print.

svyintReg <- function(formula, design, subset, ..., scale.weights=FALSE) {
  wR(intReg::intReg,formula,design,subset,...,scale.weights=scale.weights)

}
