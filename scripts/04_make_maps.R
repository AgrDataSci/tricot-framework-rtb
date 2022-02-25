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
               file = "scripts/sessioninfo/04_makemaps_session_info.txt")

# read the data of geographical extrapolation of sweetpotato
spext <- read.csv("processing/sweetpotato_geo_extrapolation.csv")
# read the potato
poext <- read.csv("processing/potato_geo_extrapolation.csv")

head(spext)

gh <- getData("GADM", country = "GHA", level=1)
gh <- st_as_sf(gh)

rw <- getData("GADM", country = "RWA", level=1)
rw <- st_as_sf(rw)

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

spmap <- 
ggplot() +
  geom_tile(r, mapping = aes(x = x, y = y, fill = layer)) +
  geom_sf(gh$geometry, mapping = aes(), colour = "#4d4d4d", fill = NA) +
  scale_fill_manual(values = colors,
                    name = NULL,
                    labels = best3$best3) +
  labs(title = "", x = "", y = "") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  annotation_scale(aes(location = "br")) +
  annotate(geom = "text", 
           x = -0.83,
           y = 9.39,
           label = "Tamale",
           color = "grey30", 
           size = 3.5)  +
  annotate(geom = "text", 
           x = -1.61,
           y = 6.68,
           label = "Kumasi",
           color = "grey30", 
           size = 3.5) +
   annotate(geom = "text", 
            x = -0.18,
            y = 5.61,
           label = "Accra",
           color = "grey30", 
           size = 3.5) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        panel.background = element_blank(),
        legend.text = element_text(size = 10),
        plot.margin = unit(c(1,1,1,1), "mm"))


ggsave("output/sweetpotato_map.png",
       plot = spmap,
       width = 20,
       height = 20,
       dpi = 900,
       units = "cm")

# ..............................................
# ..............................................
# ..............................................
# now make the map for potato
r <- poext[,c("x", "y", "best3")]
best3 <- poext[!duplicated(poext$node),c("node", "rules", "best3")]
r$best3 <- factor(r$best3, levels = best3$best3)

coordinates(r) <- ~ x + y
gridded(r) <- TRUE
r <- raster(r)
crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
r <- mask(r, rw)

r <- as.data.frame(r, xy = TRUE)
names(r)[3] <- "layer"
r <- r[!is.na(r$layer), ]
r$layer <- factor(r$layer, levels = best3$best3)

colors <- c("#2c7bb6", "#fee090", "#d73027","#80cdc1")

potmap <- 
ggplot() +
  geom_tile(r, mapping = aes(x = x, y = y, fill = layer)) +
  geom_sf(rw$geometry, mapping = aes(), colour = "#4d4d4d", fill = NA) +
  scale_fill_manual(values = colors,
                    name = NULL,
                    labels = best3$best3) +
  labs(title = "", x = "", y = "") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  annotation_scale(aes(location = "br")) +
  annotate(geom = "text", 
           x = 30.05,
           y = -1.95,
           label = "Kigali",
           color = "grey30", 
           size = 3.5)  +
  annotate(geom = "text", 
           x = 29.73,
           y = -2.6,
           label = "Huye",
           color = "grey30", 
           size = 3.5) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        panel.background = element_blank(),
        legend.text = element_text(size = 10),
        plot.margin = unit(c(1,1,1,1), "mm"))

potmap

ggsave("output/potato_map.png",
       plot = potmap,
       width = 20,
       height = 20,
       dpi = 900,
       units = "cm")



rtbmap <-
  spmap + 
  potmap +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = "bold"))

ggsave("output/rtb_map.png",
       plot = rtbmap,
       width = 30,
       height = 20,
       dpi = 900,
       units = "cm")

ggsave("output/rtb_map.svg",
       plot = rtbmap,
       width = 30,
       height = 20,
       dpi = 900,
       units = "cm")

file.remove("gadm36_GHA_1_sp.rds")
file.remove("gadm36_RWA_1_sp.rds")

