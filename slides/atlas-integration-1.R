# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

FILENAME <- "slides/atlas-integration-1.png"

WIDTH <- 8

HEIGHT <- WIDTH * 0.75

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

pkg <- c("ggforce", "ggplot2", "magick", "scater")

lib <- lapply(pkg, library, character.only = TRUE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sce <- readRDS("output/10-atlas-integration.rds")

dat <- makePerCellDF(sce)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

tmp <- subset(dat, batch != "38")

tmp <- tmp[, c("UMAP.1", "UMAP.2")]

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

dat <- subset(dat, batch != "38")

dat <- subset(dat, celltype %in% c("Caudal epiblast",
                                   "Caudal mesoderm",
                                   "Caudal neurectoderm",
                                   "ExE mesoderm",
                                   "Haematoendothelial progenitors",
                                   "Intermediate mesoderm",
                                   "Mesenchyme",
                                   "Mixed mesoderm",
                                   "Nascent mesoderm",
                                   "PGC",
                                   "Primitive Streak",
                                   "Rostral neurectoderm",
                                   "Somitic mesoderm",
                                   "Surface ectoderm"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

col <- MouseGastrulationData::EmbryoCelltypeColours

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

plt <- ggplot(dat, aes(UMAP.1, UMAP.2, colour = celltype)) +
  geom_point(data = tmp, aes(UMAP.1, UMAP.2), size = 1, colour = "gainsboro") +
  geom_point(size = 1, show.legend = TRUE) +
  scale_colour_manual(name = "Celltype", values = col) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  theme_no_axes() +
  theme(aspect.ratio = 1,
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.text = element_text(size = 12))

ggsave(FILENAME, plot = plt, width = WIDTH, height = HEIGHT)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

img <- image_read(FILENAME)

img <- image_trim(img)

img <- image_border(img, color = "#FFFFFF", geometry = "50x50")

img <- image_write(img, path = FILENAME, format = "png")
