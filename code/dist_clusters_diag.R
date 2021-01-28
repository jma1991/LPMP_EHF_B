dist_clusters_diag <- function(X, w1, w2){
  if(length(w1) != nrow(X) | length(w2) != nrow(X)){
    stop("Reduced dimensional matrix and weights vector contain different
             numbers of points.")
  }
  mu1 <- colWeightedMeans(X, w = w1)
  mu2 <- colWeightedMeans(X, w = w2)
  diff <- mu1 - mu2
  if(sum(w1>0)==1){
    s1 <-  diag(ncol(X))
  }else{
    s1 <- diag(diag(cov.wt(X, wt = w1)$cov))
  }
  if(sum(w2>0)==1){
    s2 <-  diag(ncol(X))
  }else{
    s2 <- diag(diag(cov.wt(X, wt = w2)$cov))
  }
  return(as.numeric(t(diff) %*% solve(s1 + s2) %*% diff))
}
