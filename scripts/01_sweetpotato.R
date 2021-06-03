library("gosset")
library("PlackettLuce")
library("psychotools")
library("climatrends")
library("nasapower")
library("caret")
library("qvcalc")
library("ggparty")
library("patchwork")
library("raster")
library("dismo")
library("rgeos")
library("ggplot2")
library("sf")

capture.output(sessioninfo::session_info(),
               file = "scripts/sessioninfo/01_sweetpotato_session_info.txt")

load("data/spotato_ghana.rda")

dt <- rowbind(lt)

names(dt)

n <- nrow(dt)

# find the lon lat with more complete data
lon <- which(grepl("_longitude", names(dt)))

unlist(lapply(dt[lon], function(x){
  sum(is.na(x))
}))

# use dataatharvest_farm_geo_longitude

names(dt)

as.Date(dt$dataatharvest_survey_start) - as.Date(dt$registration_survey_start)

sel <- c("package_item_A", "package_item_B", "package_item_C",
         "dataatharvest_farm_geo_longitude", "dataatharvest_farm_geo_latitude",
         "registration_survey_start", "dataatharvest_survey_start",
         "dataatharvest_performance_pos", "dataatharvest_performance_neg")


dt <- dt[sel]

names(dt) <- c(paste0("item_", LETTERS[1:3]), "lon", "lat", "start", 
               "end", "overall_pos", "overall_neg")

head(dt)


# filter data
keep <- apply(dt[c("lon", "lat", "start")], 1, function(x) {
  sum(is.na(x))
})

keep <- keep == 0

dt <- dt[keep, ]

# start and end as Date
dt$start <- as.Date(dt$start)
dt$end   <- as.Date(dt$end)

dt$span <- as.integer(dt$end - dt$start)

summary(dt$span)
sum(is.na(dt$span))

# get rainfall data from chirps
firstd <- min(dt$start)
lastd <- max(dt$end)

dates <- as.character(c(firstd, lastd))

dt$lat <- as.numeric(dt$lat)
dt$lon <- as.numeric(dt$lon)

rain <- rainfall(dt[,c("lon", "lat")], 
                 day.one = dt$start, 
                 span = 100)


temp <- temperature(dt[,c("lon", "lat")], 
                    day.one = dt$start, 
                    span = 100)

cs <- crop_sensitive(dt[,c("lon", "lat")], 
               day.one = dt$start, 
               span = 100)

covar <- cbind(rain, temp, cs)

drop <- nearZeroVar(covar)

names(covar)[drop]

covar <- covar[-drop]

head(covar)


# PlackettLuce model 

G <- rank_tricot(dt,
                 paste0("item_", LETTERS[1:3]),
                 c("overall_pos", "overall_neg"),
                 group = TRUE)


pld <- cbind(G, covar)


names(pld)

tree <- pltree(G ~ MLDS + SU, 
               data = pld, 
               minsize = 50,
               gamma = TRUE,
               alpha = 0.1,
               verbose = TRUE)


tree

AIC(tree)

plot(tree)

summary(tree)

gosset:::plot_tree(tree, qve = FALSE)

ggsave(filename = "output/sweetpotato.png",
       plot = last_plot(),
       height = 20,
       width = 25,
       units = "cm",
       dpi = 500)

coef(tree, log = FALSE)

# predict is not working so I will make a turn around
rules <- partykit:::.list.rules.party(tree)

dt$node <- with(pld,
                ifelse(MLDS <= 7, 2,
                       ifelse(MLDS > 7 & SU <= 58, 4,
                              ifelse(MLDS > 7 & SU > 58, 5, NA))))

coeftree <- coef(tree, log = FALSE)

best3 <- apply(coeftree, 1, function(x){
  paste(names(rev(sort(x)))[1:3], collapse = ", ")
})

best3

best3 <- as.data.frame(best3)

best3$node <- rownames(best3)

best3$rules <- rules

dt <- merge(dt, best3, by = "node", all.x = TRUE)

summary(as.factor(dt$node))

plot(dt[,c("lon", "lat")], col = dt$node)



# make a raster and try to build a map
#Get coordinates in a separated object
coord <- dt[,c("lon","lat")]
names(coord) <- c("x","y")

#Make a convex hull using trials coordinates
largedist <- max(pointDistance(coord, longlat = FALSE), na.rm = TRUE)

hull <- convHull(coord, lonlat=TRUE)

hull <- gBuffer(hull@polygons, width=0.1*largedist)

#set projection
crs(hull) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#define extention for interpolation
myext <- extent(hull)

#Add best to the main dataset
coord$node <- dt$node

# define raster
r <- raster(hull)
# set the resolution of the cells to 2-5 minutes
res(r) <- c(0.125, 0.125) #c(0.25, 0.04166667)

plot(rasterToPoints(r))
plot(hull, add = TRUE)

coords <- rasterToPoints(r)

coords <- as.data.frame(coords)

names(coords)


do <- "2020-08-15"

tempext <- temperature(coords[,c("x", "y")],
                       day.one = do,
                       span = 100)

rainext <- rainfall(coords[,c("x", "y")],
                    day.one = do,
                    span = 100)


geoext <- cbind(coords, rainext, tempext)


geoext$node <- with(geoext,
             ifelse(MLDS <= 7, 2,
                    ifelse(MLDS > 7 & SU <= 58, 4,
                           ifelse(MLDS > 7 & SU > 58, 5, NA))))


summary(as.factor(geoext$node))

plot(geoext[,c("x", "y")], col = geoext$node)


geoext <- merge(geoext, best3, by = "node", all.y = TRUE)


write.csv(geoext, "processing/sweetpotato_geo_extrapolation.csv", row.names = FALSE)

# # create spatial points data frame
# r <- geoext[,c("x", "y", "node")]
# head(r)
# coordinates(r) <- ~ x + y
# gridded(r) <- TRUE
# # coerce to raster
# r <- raster(r)
# r
# 
# 
# s <- rasterToPolygons(r, dissolve = TRUE)
# s <- st_as_sf(s)
# 
# 
# colors <- c("#2c7bb6", "#fee090", "#d73027")
# 
# ggplot(s) + 
#   geom_sf(aes(fill = factor(node, levels = c("2","4","5"))),
#           lwd = 0) +
#   scale_fill_manual(values= colors,
#                     name = NULL,
#                     labels = best3$rules) +
#   theme_void() +
#   theme(legend.position = "right",
#         legend.title = element_blank(),
#         legend.text= element_text(size = 8, colour="black"))
# 
# 
# 
