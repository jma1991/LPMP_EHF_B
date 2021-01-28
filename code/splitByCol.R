splitByCol <- function(x, f, drop = FALSE) {

  by.col <- split(seq_len(ncol(x)), f, drop = drop)

  out <- lapply(by.col, function(i) x[, i])

  return(out)

}
