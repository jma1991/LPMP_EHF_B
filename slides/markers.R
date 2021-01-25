# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(magick)

library(pheatmap)

library(scater)

library(scran)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

source("code/coolmap.R")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sce <- readRDS("output/09-cell-annotation.rds")

res <- findMarkers(sce, sce$cluster, test.type = "t", pval.type = "any", direction = "any")

sig <- lapply(res, subset, FDR < 0.05)

sig <- Filter(nrow, sig)

ids <- unique(unlist(lapply(sig, rownames)))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cluster.palette <- c("1" = "#4E79A7",
                     "2" = "#E15759",
                     "3" = "#59A14F")

lineage.palette <- c(
  "Ectoderm" = "#E03531",
  "Intermediate" = "#F0BD27",
  "Mesoderm" = "#51B364"
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

mat <- logcounts(sce)[ids, ]

fit <- coolmap(mat)

fit$breaks <- seq(-3, 3, length.out = 101)

ann <- list(
  col = data.frame(Cluster = sce$cluster, row.names = colnames(sce)),
  colors = list(Cluster = cluster.palette)
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

FILENAME <- "slides/markers.png"

WIDTH <- 15

HEIGHT <- 12

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

pheatmap(
  mat = fit$mat,
  color = fit$color,
  breaks = fit$breaks,
  cluster_rows = fit$cluster_rows,
  cluster_cols = fit$cluster_cols,
  cutree_cols = 3,
  cutree_rows = 3,
  annotation_col = ann$col,
  annotation_colors = ann$colors,
  annotation_legend = FALSE,
  border = NA,
  show_colnames = FALSE,
  filename = FILENAME,
  width = WIDTH,
  height = HEIGHT
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

img <- image_read(FILENAME)

img <- image_trim(img)

img <- image_border(img, color = "#FFFFFF", geometry = "50x50")

img <- image_write(img, path = FILENAME, format = "png")
