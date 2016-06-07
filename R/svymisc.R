#' svrepmisc: Miscellaneous Functions for Replicate Weights
#'
#' Wrapper functions for Complex Surveys using replicate weights.
#' Takes advantage of \code{\link[survey]{withReplicates}}.
#' @docType package
#' @name svrepmisc
#'
NULL

# helper function
wR <- function(FUN, formula, design, subset, ...) {
  # stolen from Lumley
  # surveyrep.R line 1311
  subset<-substitute(subset)
  subset<-eval(subset, design$variables, parent.frame())
  if (!is.null(subset)) {
    design<-design[subset,]
  }

  est <- withReplicates(design,
                        function(w,data) {
                          environment(formula)<-environment()
                          out <- match.call()
                          out$formula <- formula
                          out$data <- data
                          out$weight <- w
                          out$... <- list(...)
                          out[[1]] <- FUN
                          summary(eval(out))$coefficients[,1]
                        }

  )

  attr(est,"statistic") <- "Coefficient"
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
#'
#' @import survey
#' @importFrom nnet multinom
#'

svymultinom <- function(formula, design, subset, ...) {
  # stolen from Lumley
  # surveyrep.R line 1311
  subset<-substitute(subset)
  subset<-eval(subset, design$variables, parent.frame())
  if (!is.null(subset)) {
    design<-design[subset,]
  }
  est <- withReplicates(design,
                 function(w,data){
                   environment(formula)<-environment()
                   summary(nnet::multinom(formula,
                            data,weight=w,...))$coefficients
                 }
  )
  attr(est,"statistic") <- "Coefficient"
  return(est)
}


#' Wrapper for Quantile Regression
#'
#' Wrapper for \code{\link[quantreg]{rq}} for replicate weights
#'
#' @seealso \code{\link[survey]{withReplicates}} \code{\link[quantreg]{rq}}
#' @importFrom quantreg rq
#' @export

svyrq <- function(formula, design, subset, ...) {

  wR(quantreg::rq,formula,design,subset,...)

}

#' Wrapper for MASS functions
#'
#' Wrapper for several functions from \pkg{MASS} for replicate weights
#'
#' @seealso \code{\link[survey]{withReplicates}} \code{\link[MASS]{polr}} \code{\link[MASS]{glm.nb}}
#' @importFrom MASS polr
#' @export

svypolr <- function(formula, design, subset, ...) {

  wR(MASS::polr,formula,design,subset,...)

}
#' @rdname svypolr
#' @importFrom MASS glm.nb
#' @export

svyglm.nb <- function(formula, design, subset, ...) {

  wR(MASS::glm.nb,formula,design,subset,...)

}

