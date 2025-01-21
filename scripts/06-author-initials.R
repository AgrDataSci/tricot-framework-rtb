
dat = read.csv("data/authors.csv")

dat

initials = gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', dat$names, perl = TRUE)

paste(initials, collapse = ", ")
