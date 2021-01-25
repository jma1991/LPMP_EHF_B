# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

pkg <- c("ggforce", "ggplot2", "magick", "scater")

lib <- lapply(pkg, library, character.only = TRUE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sce <- readRDS("output/11-atlas-neighbors.rds")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sce$cluster.original <- NA

sce$cluster.original[sce$batch == "38"] <- colData(readRDS("output/05-clustering.rds"))$cluster

sce$cluster.original <- factor(sce$cluster.original)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

snn <- buildSNNGraph(sce, type = "jaccard", use.dimred = "corrected")

com <- igraph::cluster_louvain(snn)$membership

sce$cluster <- factor(com)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

dat <- makePerCellDF(sce)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

plt <- ggplot(dat, aes(UMAP.1, UMAP.2, colour = celltype)) +
  geom_point() +
  scale_colour_manual(name = "Celltype", values = MouseGastrulationData::EmbryoCelltypeColours) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  theme_no_axes() +
  theme(aspect.ratio = 1,
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.text = element_text(size = 12))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

FILENAME <- "slides/atlas-neighbors-1.png"

WIDTH <- 8

HEIGHT <- WIDTH * 0.75

ggsave(filename = FILENAME, plot = plt, width = WIDTH, height = HEIGHT)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

img <- image_read(FILENAME)

img <- image_trim(img)

img <- image_border(img, color = "#FFFFFF", geometry = "50x50")

img <- image_write(img, path = FILENAME, format = "png")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
