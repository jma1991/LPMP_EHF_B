# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(ggforce)

library(ggplot2)

library(magick)

library(scater)

library(scran)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sce <- readRDS("output/09-cell-annotation.rds")

dat <- makePerCellDF(sce)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cluster.palette <- c("1" = "#4E79A7",
                     "2" = "#E15759",
                     "3" = "#59A14F")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

plt <- ggplot(dat, aes(UMAP.1, UMAP.2, colour = cluster)) +
  geom_point() +
  scale_colour_manual(name = "Cluster", values = cluster.palette) +
  theme_no_axes() +
  theme(
    aspect.ratio = 1,
    legend.position = c(0.01, 0.01),
    legend.justification = c(0, 0),
    legend.background = element_blank()
  )

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

FILENAME <- "cluster.png"

WIDTH <- 7

HEIGHT <- WIDTH * 0.75

ggsave(filename = FILENAME, width = WIDTH, height = HEIGHT, scale = 0.8)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

img <- image_read(FILENAME)

img <- image_trim(img)

img <- image_border(img, color = "#FFFFFF", geometry = "50x50")

img <- image_write(img, path = FILENAME, format = "png")
