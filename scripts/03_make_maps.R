# ...............................................
# ...............................................
# Process maps 
# ...............................................
# ...............................................
library("ggplot2")
library("patchwork")
library("raster")
library("sp")
library("sf")
library("ggspatial")

capture.output(sessioninfo::session_info(),
               file = "scripts/sessioninfo/03_makemaps_session_info.txt")

# read the data of geographical extrapolation of sweetpotato
spext <- read.csv("processing/sweetpotato_geo_extrapolation.csv")

head(spext)

gh <- getData("GADM", country = "GHA", level=1)

gh <- st_as_sf(gh)

names(gh)


# make the plot for sweetpotato
r <- spext[,c("x", "y", "best3")]
best3 <- spext[!duplicated(spext$node),c("node", "rules", "best3")]
r$best3 <- factor(r$best3, levels = best3$best3)

coordinates(r) <- ~ x + y
gridded(r) <- TRUE
r <- raster(r)
crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
r <- mask(r, gh)

r <- as.data.frame(r, xy = TRUE)
names(r)[3] <- "layer"
r <- r[!is.na(r$layer), ]
r$layer <- factor(r$layer, levels = best3$best3)

colors <- c("#2c7bb6", "#fee090", "#d73027")

ggplot() +
  geom_tile(r, mapping = aes(x = x, y = y, fill = layer)) +
  geom_sf(gh$geometry, mapping = aes(), colour = "#4d4d4d", fill = NA) +
  geom_point(gen, mapping = aes(x = lon, y = lat), size = 1.2, col = "red", pch = 18) +
  scale_fill_manual(values = colors,
                    name = NULL,
                    labels = best3$best3) +
  labs(title = "", x = "", y = "") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        panel.background = element_blank(),
        legend.text = element_text(size = 10),
        plot.margin = unit(c(1,1,1,1), "mm"))


file.remove("gadm36_GHA_1_sp.rds")
