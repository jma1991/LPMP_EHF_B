# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(magick)

library(pheatmap)

library(scater)

library(scran)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

source("code/coolmap.R")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sce <- readRDS("output/11-atlas-neighbors.rds")

rownames(sce) <- uniquifyFeatureNames(rowData(sce)$gene_id, rowData(sce)$gene_name)

colnames(sce) <- paste("cell", seq_len(ncol(sce)), sep = "-")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sce$batch <- "Atlas"

sce$batch[sce$sample == "38"] <- "LPMP"

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sce$cluster.original <- NA

sce$cluster.original[sce$batch == "38"] <- colData(readRDS("output/05-clustering.rds"))$cluster

sce$cluster.original <- factor(sce$cluster.original)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

snn <- buildSNNGraph(sce, type = "jaccard", use.dimred = "corrected")

com <- igraph::cluster_louvain(snn)$membership

sce$cluster <- factor(com)

sce <- sce[, order(sce$cluster)]

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

ids <- read.csv("slides/lineage.csv")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

celltype.palette <- MouseGastrulationData::EmbryoCelltypeColours

batch.palette <- c("Atlas" = "#D3D3D3", "LPMP" = "#E15759")

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

mat <- assay(sce, "reconstructed")[ids$gene_id, ]

fit <- coolmap(mat)

ann <- list(
  row = data.frame(Marker = ids$cell_id, row.names = ids$gene_id),
  col = data.frame(Celltype = sce$celltype, Batch = sce$batch, row.names = colnames(sce)),
  colors = list(Celltype = celltype.palette, Batch = batch.palette, Marker = marker.palette)
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

FILENAME <- "slides/atlas-lineage.png"

WIDTH <- 7

HEIGHT <- 6

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

pheatmap(
  mat = fit$mat,
  color = fit$color,
  breaks = seq(-0.3, 0.3, length.out = 101),
  cluster_rows = FALSE,
  gaps_row = cumsum(table(ids$cell_id)[unique(ids$cell_id)]),
  cluster_cols = fit$cluster_cols,
  cutree_cols = 8,
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
