coolmap <- function(x) {

  x <- as.matrix(x)

  N <- ncol(x)

  hc <- hclust(dist(t(x), method = "euclidean"), method = "complete")

  M <- rowMeans(x, na.rm = TRUE)

  DF <- N - 1

  isNA <- is.na(x)

  anyNA <- any(isNA)

  if (anyNA) {

    mode(isNA) <- "integer"

    DF <-  DF - rowSums(isNA)

    DF[DF == 0] <- 1

  }

  x <- x - M

  V <- rowSums(x^2, na.rm = TRUE) / DF

  x <- x / sqrt(V + 0.01)

  hr <- hclust(dist(x, method = "euclidean"), method = "complete")

  out <- list(
    mat = x,
    color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 5, name = "RdBu")))(100),
    breaks = seq(-max(abs(x)), max(abs(x)), length.out = 101),
    cluster_rows = hr,
    cluster_cols = hc
  )

}
