library(igraph)
library(qgraph)
library(tidyverse)

data <- read_csv('BFI_data_clean.csv')

### multiplex network example 

# split the data into male and female groups 
table(data$Gender)

male_data <- data %>% filter(Gender == 1) %>% select(-Age, -`Year-of-Study`, -Gender)
female_data <- data %>% filter(Gender == 2) %>% select(-Age, -`Year-of-Study`, -Gender)

# use qgraph to create partial correlation networks 
male_network <- qgraph(cor_auto(male_data), graph = 'pcor', threshold = 'sig', sampleSize = nrow(male_data), 
                layout = 'spring') 
female_network <- qgraph(cor_auto(female_data), graph = 'pcor', threshold = 'sig', 
                         sampleSize = nrow(female_data), 
                       layout = 'spring') 

# clean male network 
male_network <- as.igraph(male_network)
summary(male_network)

labels_to_remove <- vertex_attr_names(male_network) # remove unwanted node attributes
for(i in 1:length(labels_to_remove)) {
  male_network <- delete_vertex_attr(male_network, labels_to_remove[i])
}

labels_to_remove <- edge_attr_names(male_network) # remove unwanted edge attributes 
for(i in 1:length(labels_to_remove)) {
  male_network <- delete_edge_attr(male_network, labels_to_remove[i])
}

V(male_network)$name <- colnames(male_data)[1:9]
male_network <- male_network %>% add_vertices(1, name = 'active-imagination') # this node is missing in the male network (no sig correlations)
summary(male_network)

# clean female network 
female_network <- as.igraph(female_network)
summary(female_network)

labels_to_remove <- vertex_attr_names(female_network) # remove unwanted node attributes
for(i in 1:length(labels_to_remove)) {
  female_network <- delete_vertex_attr(female_network, labels_to_remove[i])
}

labels_to_remove <- edge_attr_names(female_network) # remove unwanted edge attributes
for(i in 1:length(labels_to_remove)) {
  female_network <- delete_edge_attr(female_network, labels_to_remove[i])
}

V(female_network)$name <- colnames(female_data)
summary(female_network)

library(multiplex)
library(multigraph)

## combine each network to be layers in a multiplex 
z <- simplify2array(list(as_adjacency_matrix(female_network, sparse = F), 
                         as_adjacency_matrix(male_network, sparse = F)))

# visualization: dashed = male, solid = female 
multigraph(z, directed = F, layout = "force", seed = 1)



### bipartite networks 

bp_data <- data %>% select(-Age, -`Year-of-Study`, -Gender)

# only retain responses which were strongly agreed (5)
bp_data <- as.matrix(bp_data)
bp_data[bp_data < 5] <- 0
bp_data[bp_data > 4] <- 1
rownames(bp_data) <- 1:82

# bipartite network construction - rows = Ps, cols = items  
bp_net <- graph_from_incidence_matrix(bp_data)
summary(bp_net)
table(V(bp_net)$type)

# viz 1: bipartite network 
V(bp_net)$color <- c("steel blue", "orange")[V(bp_net)$type+1]
V(bp_net)$shape <- c("square", "circle")[V(bp_net)$type+1]

par(mfrow=c(1,1), mar=c(0,0,0,0)+.1)
plot(bp_net, vertex.size = 3, vertex.label = NA, layout = layout.bipartite,
     vertex.frame.color = 'white', vertex.label.dist = -1)

# viz 2: projection plots 

# separate the bipartite into their projections 
bp_net_proj <- bipartite.projection(bp_net) 
summary(bp_net_proj$proj1)
summary(bp_net_proj$proj2)

par(mfrow=c(1,2), mar=c(0,0,0,0)+.1)
set.seed(3)

# remove hermits from student project to improve viz 
isolated <- which(degree(bp_net_proj$proj1)==0) 
no_hermits <- delete.vertices(bp_net_proj$proj1, isolated)

plot(no_hermits, vertex.label.color="black", vertex.label.dist=1, vertex.size = 3,
     edge.color = 'lightgrey',
     layout = layout_nicely)

plot(bp_net_proj$proj2, vertex.label.color="black", vertex.label.dist=0, 
     vertex.size = degree(bp_net_proj$proj2)^1.2,
     edge.color = 'lightgrey',
     layout = layout_nicely)
