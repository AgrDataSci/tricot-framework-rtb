library("ClimMobTools")
library("PlackettLuce")
library("gosset")
library("patchwork")
library("ggparty")
library("ggplot2")

key = ""

projs = getProjectsCM(key, server = "climmob3")

projs$project_id

keep = projs$project_id %in% c("groundnut2", "OPP1114827")

projs = projs[keep, ]

dat = list()

for (i in seq_along(projs$project_id)) {
  
  x = getDataCM(key,
                project = projs$project_id[i],
                userowner = projs$user_owner[i],
                server = projs$server[i],
                as.data.frame = TRUE,
                tidynames = TRUE,
                pivot.wider = TRUE)
  
  dat[[i]] = x
  
}

dat = lapply(dat, function(x){
  names(x)[names(x) == "intervalindays_districtname"] = "district"
  names(x)[names(x) == "vegetative30days_districtname"] = "district"
  names(x)[names(x) == "intervalindays_regionname"] = "region"
  names(x)[names(x) == "vegetative30days_regionname"] = "region"
  names(x) = gsub("intervalindays_", "vegetative30days_", names(x))
  x
})

dat = rowbind(dat)

names(dat)

keep = c("package|region|district|_pos|_neg|gender")

keep = grepl(keep, names(dat))

dat = dat[, keep]

trait_names = c("Germination", "Flowering",
                "Resistance to Diseases", "Tolerance to Drought",
                "Days to Maturity", "Harvesting Labor", 
                "Seed Size", "Seed Color",
                "Taste", "Yield", "Overall")

# get trait list
traits = getTraitList(dat, c("_pos", "_neg"), trait_names)


pack_index = paste0("package_item_", LETTERS[1:3])

overall = 11

# filter data using the overall indices
dat = dat[traits[[overall]]$keep, ]

names(dat)[names(dat)=="region"] = "Region"

dat$Region = ClimMobTools:::.title_case(dat$Region)

table(dat$Region)

dat$Region = factor(dat$Region)

# refresh indices in trait list
traits = getTraitList(dat, c("_pos", "_neg"), trait_names)

# fit PL model
G = rank_tricot(data = dat[traits[[overall]]$keep, ],
                items = pack_index,
                input = traits[[overall]]$string,
                group = TRUE)

pld = cbind(G, dat[,c("district", "Region")])

str(pld)

tree = pltree(G ~ Region, 
              data = pld, 
              gamma = TRUE, 
              alpha = 0.2, 
              minsize = 50,
              verbose = TRUE)

plot(tree)

# predict nodes using the main data as newdata
# !!!!!! some issue in getting the same length, input 2 new elements and 
# try to fix it later
nodes = c(predict(tree, newdata = dat, type = "node"),
          c(2, 2))

nodes = as.vector(nodes)

unique_nodes = unique(nodes)

# now use this nodes to see difference in Kendall tau
length(nodes) == nrow(dat)

# run over traits and nodes
kendall = data.frame()

for (i in seq_along(traits)) {
  
  k = data.frame()
  
  for (j in seq_along(unique_nodes)) {
    
    keep = nodes == unique_nodes[j] & traits[[i]]$keep
    
    R1 = rank_tricot(dat[keep, ],
                     items = pack_index,
                     input = traits[[overall]]$string)
    
    R2 = rank_tricot(dat[keep, ],
                     items = pack_index,
                     input = traits[[i]]$string)
    
    kj = kendallTau(R1, R2)
    
    kj$Node = paste0("Node = ", unique_nodes[j])
    
    kj$Trait = traits[[i]]$trait_label
    
    k = rbind(k, kj)
    
  }
  
  kendall = rbind(kendall, k)
  
}

kendall

# remove overall and set negative values to 0
rmv = !kendall$Trait %in% c("Overall", "Flowering")

kendall = kendall[rmv, ]

kendall$kendallTau = ifelse(kendall$kendallTau < 0, 0, kendall$kendallTau)

# set traits as factors 
kendall$Trait = factor(kendall$Trait, levels = rev(trait_names))

# now plot it
tree_node = ggplot(data = kendall, 
         aes(x = kendallTau,
             y = Trait, 
             fill = Trait)) +
  geom_bar(stat = "identity", 
                    position = "dodge",
                    show.legend = FALSE,
                    width = 1, 
                    color = "#ffffff") + 
  facet_grid(~ Node) + 
  scale_fill_brewer(palette = "YlOrRd", direction = -1) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white"),
        text = element_text(color = "grey20"),
        legend.position = "bottom",
        legend.title = element_blank(),
        strip.background.x = element_blank(),
        strip.placement = "outside") +
  labs(y = "Trait",
       x = "Kendall tau")

tree_branch = gosset:::build_tree_branches(tree)

plottree = tree_branch / tree_node + plot_layout(heights =  c(1, 2))

plottree












