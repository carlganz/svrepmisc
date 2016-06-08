mat2vec <- function(X) {
  levs <- colnames(X)
  rows <- rownames(X)
  rows <- as.vector(outer(rows,levs,paste,sep="."))

  X <- as.vector(X)

  names(X) <- rows
  X
}
