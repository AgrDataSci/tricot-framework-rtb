library("gosset")
library("jsonlite")
library("ClimMobTools")

capture.output(sessioninfo::session_info(),
               file = "scripts/sessioninfo/01_select_data_session_info.txt")

# select fields in sweetpotato data to use only what is needed for this 
# analysis and to ensure that participant's information remains confidential
load("data/raw/spotato_ghana.rda")

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

sel <- c("package_item_A", "package_item_B", "package_item_C",
         "dataatharvest_farm_geo_longitude", "dataatharvest_farm_geo_latitude",
         "registration_survey_start", "dataatharvest_survey_start",
         "dataatharvest_performance_pos", "dataatharvest_performance_neg")


dt <- dt[sel]

names(dt) <- c(paste0("item_", LETTERS[1:3]), "lon", "lat", "start", 
               "end", "overall_pos", "overall_neg")

head(dt)

# filter data and keep only those with complete data in lon lat and start
keep <- apply(dt[c("lon", "lat", "start")], 1, function(x) {
  sum(is.na(x))
})

keep <- keep == 0

dt <- dt[keep, ]

write.csv(dt, "data/sweetpotato.csv", row.names = FALSE)


# now read the potato data
# two projects from Rwanda
l <- list.dirs("data/raw")[-1]

# read the json files
f <- list()

for(i in seq_along(l)) {
  f_i <- fromJSON(paste0(l[i], "/info.json"))
  class(f_i) <- "CM_list"
  f_i <- as.data.frame(f_i, tidynames = FALSE, pivot.wider = TRUE)
  f[[i]] <- f_i
}

# select yield as I don't know which overall performance to select

names(f[[1]])
names(f[[2]])

dt1 <- f[[1]]

# select the variables in this file
names(dt1)

sel <- c("id", "package_item_A", "package_item_B", "package_item_C", 
         "registration_REG_clm_start", "post-harvest1(5daysafterharvest)_ASS2e47a9554c06_clm_start",
         "post-harvest1(5daysafterharvest)_ASS2e47a9554c06_char_yield_pos",
         "post-harvest1(5daysafterharvest)_ASS2e47a9554c06_char_yield_neg")


dt1 <- dt1[,sel]

names(dt1) <- c("id", paste0("item_", LETTERS[1:3]), 
                "start", "end", "yield_pos", "yield_neg")

head(dt1)

# long lat comes from another file
lonlat1 <- read.csv("data/raw/21AIP/Pregistration.csv")

# select package number and longlat
names(lonlat1)

lonlat1 <- lonlat1[,c("Package_no", "location.geopoint")]

names(lonlat1) <- c("id", "lonlat")

ll <- lapply(strsplit(lonlat1$lonlat, " "), function(x) {
  data.frame(lon = x[2], lat = x[1])
})

ll <- do.call("rbind", ll)

lonlat1 <- cbind(lonlat1, ll)

# the numbers are placed between a : and a - 
# I'll split this
id <- gsub("-", ":", lonlat1$id)

id <- unlist(lapply(strsplit(id, ":"), function (x) {x[2]}))

lonlat1$id <- id


# merge the two files
dt1 <- merge(dt1, lonlat1[,c("id", "lon", "lat")], by = "id", all.x = TRUE)


# go to the other file
dt2 <- f[[2]]

names(dt2)

sel <- c("id", "package_item_A", "package_item_B", "package_item_C", 
         "registration_REG_clm_start", "post-harvest1(5daysafterharvest)_ASS9d462bb9245b_clm_start",
         "post-harvest1(5daysafterharvest)_ASS9d462bb9245b_char_yield_pos",
         "post-harvest1(5daysafterharvest)_ASS9d462bb9245b_char_yield_neg")


dt2 <- dt2[,sel]

names(dt2) <- c("id", paste0("item_", LETTERS[1:3]), 
                "start", "end", "yield_pos", "yield_neg")

head(dt2)

# long lat comes from another file
lonlat2 <- read.csv("data/raw/Pot21A/Pot21Ageopoints.csv")

names(lonlat2)

lonlat2 <- lonlat2[,c("X_longitude", "X_latitude", "householdid")]

names(lonlat2) <- c("lon", "lat", "id")


dt2 <- merge(dt2, lonlat2, by = "id", all.x = TRUE)

head(dt2)

dt <- rbind(dt1, dt2)

# filter data to keep only valid rankings
keep <- apply(dt[c("yield_pos", "yield_neg", "lon", "lat")], 1, function(x) {
  sum(is.na(x))
})

keep <- keep == 0

dt <- dt[keep, ]

write.csv(dt, "data/potato.csv", row.names = FALSE)

