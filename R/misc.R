mat2vec <- function(X) {
  if (is.vector(X)) {
    return(X)
  }
  levs <- colnames(X)
  rows <- rownames(X)
  X <- as.vector(X)
  if (is.null(levs) | is.null(rows)) {
    return(X)
  }
  rows <- as.vector(outer(rows,levs,paste,sep="."))
  names(X) <- rows
  X

}


#' @export
print.svrepstatmisc <- function(x, df.residual=NULL, ...) {
  ### COPY AND PASTED FROM LUMLEY
  if (is.list(x)){
    x<-x[[1]]
  }
  vv<-sqrt(diag(as.matrix(attr(x,"var"))))

  if (is.null(df.residual)) {
    df.residual <- attr(x, "df.residual")
  }

  tvals <- x/vv
  attributes(tvals) <- NULL
  ### probably wrong in some cases
  ### may need pnorm in some cases
  pvals <- stats::pt(tvals,df.residual)
  m <- cbind(x,vv,tvals,pvals)
  colnames(m)<-c(attr(x,"statistic"),"SE", "t value", "Pr(>|t|)")
  stats::printCoefmat(m)

}


#' @export
# adapted from confint.lm
confint.svrepstatmisc <- function (object, parm, level = 0.95, df.residual=NULL, ...)
{
  cf <- object
  pnames <- names(cf)
  if (is.null(df.residual)) {
    df.residual <- attr(object, "df.residual")
  }
  if (df.residual <= 0)
    stop("Not enough duplicates to compute confidence intervals.")
  if (missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qt(a, df.residual)
  pct <- stats:::format.perc(a, 3)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                             pct))
  ses <- sqrt(diag(vcov(object)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}



#' A tidier for svrepstatmisc
#'
#' @param x a svrepstatmisc object
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level of the interval, used only if \code{conf.int=TRUE}
#' @param exponentiate whether to exponentiate the coefficient estimates and confidence intervals
#' @param quick	whether to compute a smaller and faster version, containing only the term and estimate columns.
#' @param ... extra arguments
#' @importFrom stats coef
#' @export

tidy.svrepstatmisc <- function(x, conf.int = FALSE, conf.level = .95, exponentiate = FALSE, quick = FALSE, ...) {
  if (is.list(x)){
    x<-x[[1]]
  }

  if (quick) {
    co <- x
    ret <- data.frame(term = names(co), estimate = unname(co), stringsAsFactors = FALSE)
    return(process_model(ret, x, conf.int = FALSE, exponentiate = exponentiate))
  }


  if (is.null(df.residual)) {
    df.residual <- attr(x, "df.residual")
  }

  vv <- sqrt(diag(as.matrix(attr(x, "var"))))
  tvals <- x / vv
  attributes(tvals) <- NULL
  if (df.residual > 0) {
    pvals <- 2 * stats::pt(-abs(tvals), df.residual)
  } else {
    pvals <- NA
    warning("Not enough replicates to compute p-values and confidence intervals.")
  }

  ret <- data.frame(
    term = names(x),
    estimate = unname(x),
    std.error = vv,
    statistic = tvals,
    p.value = pvals,
    stringsAsFactors = FALSE
  )
  process_model(ret, x, conf.int = conf.int, conf.level = conf.level, exponentiate = exponentiate, df.residual = df.residual)
}


process_model <- function(ret, x, conf.int = FALSE, conf.level = .95, exponentiate = FALSE, df.residual) {
  if (exponentiate) {
    trans <- exp
  } else {
    trans <- identity
  }

  if (conf.int & df.residual > 0) {
    CI <- suppressMessages(trans(stats::confint(x, level = conf.level)))
    colnames(CI) = c("conf.low", "conf.high")
    CI <- as.data.frame(CI)
    CI$term <- rownames(CI)
    ret <- merge(ret, unrowname(CI), by = "term", all.x = TRUE)
  }

  ret$estimate <- trans(ret$estimate)
  rownames(ret) <- NULL
  ret
}

