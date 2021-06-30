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
               file = "scripts/sessioninfo/03_potato_session_info.txt")

dt <- read.csv("data/potato.csv")

# start and end as Date
dt$start <- as.Date(dt$start)
dt$end   <- as.Date(dt$end)
dt$lat <- as.numeric(dt$lat)
dt$lon <- as.numeric(dt$lon)


plot(dt[,c("lon", "lat")])

# get rainfall and temperature indices using 
# data from NASA POWER
# compute the indices for the first 100 days 
# after the distribution of seeds
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
                 c("yield_pos", "yield_neg"),
                 group = TRUE)

pld <- cbind(G, covar)

names(pld)

# fit the pl tree
tree <- pltree(G ~ MLWS + DTR,
               data = pld, 
               minsize = 5,
               gamma = TRUE,
               alpha = 0.2,
               verbose = TRUE)

tree

AIC(tree) #565

plot(tree)

summary(tree)

gosset:::plot_tree(tree, ci.level = .9)

ggsave(filename = "output/potato_tree.png",
       plot = last_plot(),
       height = 20,
       width = 25,
       units = "cm",
       dpi = 500)

coef(tree, log = FALSE)

# as predict is not working I will make a turn around
# using the coefficients and assigning each of the win probs 
# to the ndes based on the partykit rules
# rules <- partykit:::.list.rules.party(tree)
# 
# dt$node <- with(pld,
#                 ifelse(DTR <= 8 & DTR <= 7 & MLWS <= 32, 4,
#                        ifelse(DTR <= 8 & DTR <= 7 & MLWS > 32, 5,
#                               ifelse(DTR <= 8 & DTR > 7, 6, 
#                                      ifelse(DTR > 8, 7, NA)))))

dt$node <- predict(tree, newdata = geoext, type = "node")

table(dt$node)

# get the coefficients as win probs
coeftree <- coef(tree, log = FALSE)

# and select the best three varieties in each node
best3 <- apply(coeftree, 1, function(x){
  paste(names(rev(sort(x)))[1:3], collapse = ", ")
})

best3

best3 <- as.data.frame(best3)

best3$node <- rownames(best3)

best3$rules <- rules

# merge it with the main data
dt <- merge(dt, best3, by = "node", all.x = TRUE)

summary(as.factor(dt$node))

plot(dt[,c("lon", "lat")], col = dt$node)


# Now work with these coefficients to generate a 
# geographical extrapolation across the research area

# get coordinates in a separated object
coord <- dt[,c("lon","lat")]
names(coord) <- c("x","y")

# # make a convex hull using trials coordinates
# largedist <- max(pointDistance(coord, longlat = FALSE), na.rm = TRUE)
# 
# hull <- convHull(coord, lonlat=TRUE)
# 
# hull <- gBuffer(hull@polygons, width=0.1*largedist)
# 
# #set projection
# crs(hull) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# #define extention for interpolation
# myext <- extent(hull)
# 
# #Add best to the main dataset
# coord$node <- dt$node

rw <- getData("GADM", country = "RWA", level=0)

# define raster
r <- raster(rw)
# set the resolution of the cells to 2-5 minutes
res(r) <- rep(0.08333334, 2) #c(0.125, 0.125) #c(0.25, 0.04166667)
r <- setValues(r, 1)
plot(rasterToPoints(r))
plot(rw, add = TRUE)

plot(r)

coords <- rasterToPoints(r)

coords <- as.data.frame(coords)

names(coords)

mean(dt$start)
median(dt$start)

do <- "2020-09-15"

tempext <- temperature(coords[,c("x", "y")],
                       day.one = do,
                       span = 100)

rainext <- rainfall(coords[,c("x", "y")],
                    day.one = do,
                    span = 100)


geoext <- cbind(coords, rainext, tempext)

# THIS SHOULD BE DONE BY HAND!!! based in the rules 
# geoext$node <- with(geoext,
#                     ifelse(DTR <= 8 & DTR <= 7 & MLWS <= 32, 4,
#                            ifelse(DTR <= 8 & DTR <= 7 & MLWS > 32, 5,
#                                   ifelse(DTR <= 8 & DTR > 7, 6, 
#                                          ifelse(DTR > 8, 7, NA)))))

geoext$node <- predict(tree, newdata = geoext, type = "node")

summary(as.factor(geoext$node))

plot(geoext[,c("x", "y")], col = geoext$node)


geoext <- merge(geoext, best3, by = "node", all.y = TRUE)


write.csv(geoext, "processing/potato_geo_extrapolation.csv", row.names = FALSE)

file.remove("gadm36_RWA_0_sp.rds")

