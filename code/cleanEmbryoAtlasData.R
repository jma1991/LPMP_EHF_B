cleanEmbryoAtlasData <- function(x) {

  # Remove cells marked as stripped nuclei
  use <- !x$stripped
  x <- x[, use]

  # Remove cells marked as doublets
  use <- !x$doublet
  x <- x[, use]

  # Clear metadata slot
  metadata(x) <- list()

  # Compute log-normalized expression values
  x <- logNormCounts(x)

  # Make rownames compatible
  rownames(x) <- rowData(x)$ENSEMBL

  # Clear rowData slot
  rowData(x) <- NULL

  # Replace rowRanges slot
  rowRanges(x) <- relist(GRanges(), PartitioningByEnd(integer(length(x)), names = rownames(x)))

  # Clear colnames slot
  colnames(x) <- NULL

  # Clear reducedDims slot
  reducedDims(x) <- NULL

  # Clear altExp slot
  altExp(x) <- NULL

  # Return experiment object
  return(x)

}
