#One way to visualize many variables in a lower dimensional space is with t-distributed stochastic neighborhood embedding, 
#or t-SNE. This method is a dimension reduction technique that tries to preserve local structure so as to make clusters visible in a 2D or 3D visualization. 
#pam 2

library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)


tsne_obj <- Rtsne(gower_dist,  pca = FALSE, verbose = TRUE)

#The k -medoids or partitioning around medoids (PAM) algorithm is a clustering algorithm reminiscent of the k -means algorithm.
#Partitioning (clustering) of the data into k clusters "around medoids", a more robust version of K-means.


tsne_data <- 
  tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = df$personId)



ggplot(tsne_data, aes(x = X, y = Y)) +
  geom_point(aes(color = cluster))
