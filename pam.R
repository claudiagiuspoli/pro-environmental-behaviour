#####clustering-on-mixed-type-data

setwd ("C:\\Users\\giusp\\Desktop\\pro-environmental-behaviour\\pro-environmental-behaviour")
set.seed(123)
df <-  read.csv("df-climatechange.csv", sep = ",", header=T)

dim(df)
View(df)

df$X <- NULL

#create subset of climate change questions
df1 <- df[,4:8] 
#View(df1)

df1$qb4_3 <- factor(df1$qb4_3, levels = c('1', '2', '3', '4'), ordered = TRUE)
df1$qb4_5 <- factor(df1$qb4_5, levels = c('1', '2', '3', '4'), ordered = TRUE)
df1$qb7 <- factor(df1$qb7, levels = c('1', '2', '3', '4'), ordered = TRUE)
df1$qb8 <- factor(df1$qb8, levels = c('1', '2', '3', '4'), ordered = TRUE)
df1$qb9 <- factor(df1$qb9, levels = c('1', '2', '3', '4'), ordered = TRUE)

library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)


### Compute Gower distance 
#Gower Distance is a distance measure that can be used to calculate distance between 
#two entity whose attribute has a mixed of categorical and numerical values.

gower_dist <- daisy(df1, metric = "gower")
gower_mat <- as.matrix(gower_dist)

#' Print most similar clients
df1[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]

#' Print most dissimilar clients
df1[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

gower_dist
sil_width <- c(NA)
for(i in 2:6){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:6, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:6, sil_width)


#choose 2 
k <- 2
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- df1 %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

df[pam_fit$medoids, ]

#join cluster in df 
pam_fit$silinfo$avg.width
df$cluster <- pam_fit$clustering
write.csv(df, file = "df-climatechange.csv", row.names = FALSE)



#One way to visualize many variables in a lower dimensional space is with t-distributed stochastic neighborhood embedding, 
#or t-SNE. This method is a dimension reduction technique that tries to preserve local structure so as to make clusters visible in a 2D or 3D visualization. 


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




###SOURCE
#https://rpubs.com/sofiandra/477664

