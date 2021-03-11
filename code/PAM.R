#####clustering-on-mixed-type-data

setwd ("C:\\Users\\giusp\\Desktop\\pro-environmental-behaviour\\pro-environmental-behaviour")
set.seed(123)
df <-  read.csv("df-climatechange.csv", sep = ",", header=T)

dim(df)
View(df)

df$X <- NULL

#create subset of climate change questions
df1 <- df[,4:8] 

df1$benefits.for.companies <- factor(df1$benefits.for.companies, levels = c('1', '2', '3', '4'), ordered = TRUE)
df1$benefits.for.citizens <- factor(df1$benefits.for.citizens, levels = c('1', '2', '3', '4'), ordered = TRUE)
df1$renewable.energy <- factor(df1$renewable.energy, levels = c('1', '2', '3', '4'), ordered = TRUE)
df1$energy.efficient <- factor(df1$energy.efficient, levels = c('1', '2', '3', '4'), ordered = TRUE)
df1$greenhouse.gas <- factor(df1$greenhouse, levels = c('1', '2', '3', '4'), ordered = TRUE)

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


df$green-identity[df$green-identity == 1] <- 'moderate'
df$green-identity[df$green-identity == 2] <- 'extreme'


write.csv(df, file = "df-climatechange.csv", row.names = FALSE)




###SOURCE
#https://rpubs.com/sofiandra/477664

