# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(magick)

library(pheatmap)

library(scater)

library(scran)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

source("code/coolmap.R")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sce <- readRDS("output/09-cell-annotation.rds")

sce <- sce[, order(sce$cluster)]

ids <- read.csv("slides/lineage.csv")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cluster.palette <- c("1" = "#4E79A7",
                     "2" = "#E15759",
                     "3" = "#59A14F")

marker.palette <- c(
  "Ectoderm" = "#b49041",
  "EMT" = "#b460bd",
  "Endothelia or Haematopoietic" = "#70a845",
  "Extraembryonic mesoderm" = "#6980ce",
  "Lateral mesoderm" = "#cd5d39",
  "Paraxial mesoderm" = "#4aac8d",
  "Primitive streak" = "#c85979"
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

mat <- logcounts(sce)[ids$gene_id, ]

fit <- coolmap(mat)

fit$breaks <- seq(-3, 3, length.out = 101)

ann <- list(
  row = data.frame(Marker = ids$cell_id, row.names = ids$gene_id),
  col = data.frame(Cluster = sce$cluster, row.names = colnames(sce)),
  colors = list(Cluster = cluster.palette, Marker = marker.palette)
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

FILENAME <- "slides/lineage.png"

WIDTH <- 10

HEIGHT <- WIDTH * 0.75

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

pheatmap(
  mat = fit$mat,
  color = fit$color,
  breaks = fit$breaks,
  cluster_rows = FALSE,
  cluster_cols = fit$cluster_cols,
  gaps_row = cumsum(table(ids$cell_id)[unique(ids$cell_id)]),
  gaps_col = cumsum(table(sce$cluster)[unique(sce$cluster)]),
  cutree_rows = 5,
  annotation_row = ann$row,
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
