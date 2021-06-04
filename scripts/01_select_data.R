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

names(dt1)

# find the lon lat with more complete data
lon <- which(grepl("_longitude", names(dt1)))

unlist(lapply(dt1[lon], function(x){
  sum(is.na(x))
}))
# this file does not have lon lat data thus cannot be used

# go to the other file
dt1 <- f[[2]]

names(dt1)

sel <- c("id", "package_item_A", "package_item_B", "package_item_C", 
         "registration_REG_clm_start", "post-harvest1(5daysafterharvest)_ASS9d462bb9245b_clm_start",
         "post-harvest1(5daysafterharvest)_ASS9d462bb9245b_char_yield_pos",
         "post-harvest1(5daysafterharvest)_ASS9d462bb9245b_char_yield_neg")


dt1 <- dt1[,sel]

names(dt1) <- c("id", paste0("item_", LETTERS[1:3]), 
                "start", "end", "yield_pos", "yield_neg")

head(dt1)

# long lat comes from another file
lonlat <- read.csv("data/raw/Pot21A/Pot21Ageopoints.csv")

names(lonlat)

lonlat <- lonlat[,c("X_longitude", "X_latitude", "householdid")]

names(lonlat) <- c("lon", "lat", "id")


dt <- merge(dt1, lonlat, by = "id", all.x = TRUE)

head(dt)

# filter data to keep only valid rankings
keep <- apply(dt[c("yield_pos", "yield_neg", "lon", "lat")], 1, function(x) {
  sum(is.na(x))
})

keep <- keep == 0

dt <- dt[keep, ]


write.csv(dt, "data/potato.csv", row.names = FALSE)

