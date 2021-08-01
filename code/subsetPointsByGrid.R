subsetPointsByGrid <- function(X, Y, resolution=200, seed = 42) {

  set.seed(seed)

  resolution <- max(resolution, 1L)
  resolution <- min(resolution, sqrt(.Machine$integer.max))
  resolution <- as.integer(resolution)

  rangeX <- range(X)
  rangeY <- range(Y)

  binX <- (rangeX[2] - rangeX[1])/resolution
  xid <- (X - rangeX[1])/binX
  xid <- as.integer(xid)

  binY <- (rangeY[2] - rangeY[1])/resolution
  yid <- (Y - rangeY[1])/binY
  yid <- as.integer(yid)

  id <- xid + yid * resolution
  !duplicated(id, fromLast=TRUE)

}
