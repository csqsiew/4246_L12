# simple multiplex example  

m1 <- matrix(sample(0:1, 16, replace = T), nrow = 4, ncol = 4)
m2 <- matrix(sample(0:1, 16, replace = T), nrow = 4, ncol = 4)

library(multiplex)
z2 <- simplify2array(list(m1,m2))

library(multigraph)
multigraph(z2, directed = F)

# simple bipartite example 

library(igraph)

simple_b_data <- matrix(c(1,0,1,
                          1,0,0,
                          1,0,1), nrow = 3, ncol = 3, byrow = T) 
rownames(simple_b_data) <- 1:3
colnames(simple_b_data) <- c('a', 'b', 'c')
simple_b_data

simple_b_g <- graph_from_incidence_matrix(simple_b_data)
plot(simple_b_g, layout = layout.bipartite)

simple_b_proj <- bipartite.projection(simple_b_g) 

par(mfrow=c(1,2), mar=c(0,0,0,0)+.1)

plot(simple_b_proj$proj1, vertex.label.color="black", vertex.label.dist=1, vertex.size = 3,
     layout = layout_nicely, edge.width = E(simple_b_proj$proj1)$weight*2)

plot(simple_b_proj$proj2, vertex.label.color="black", vertex.label.dist=1, 
     vertex.size = 5,
     layout = layout_nicely)
