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
