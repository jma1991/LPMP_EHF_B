weightedkNN <- function(x, class = NULL) {

  x$weight <- 1 / x$distance

  itr <- seq_len(nrow(x$index))

  out <- sapply(itr, function(i) {

    y <- aggregate(x = x$weight[i, ], by = list(class = x[[class]][i, ]), FUN = sum)

    y$class[which.max(y$x)]

  })

  out

}
