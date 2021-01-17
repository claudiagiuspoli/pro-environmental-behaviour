
#cluester analysis
setwd ("C:\\Users\\giusp\\Desktop\\data science\\tesi\\prova")
set.seed(123)
df <-  read.csv("df-prova10.csv", sep = ",", header=T)
dim(df)

df$X <- NULL

df <- df[,2:7] #

df$qb5 <-NULL

View(df)

df <- df[complete.cases(df), ]

library(corrplot)
library(RColorBrewer)

cor(df)


library(corclass)
cca1 <- cca(df, filter.significance = TRUE, zero.action = 'drop', filter.value = 0.1) #8 schematic classes, 15787 casi
cca2 <- cca(df, filter.significance = TRUE, zero.action = 'ownclass', filter.value = 0.1) #6 schematic classes. 21300 Casi


cca2_az <- cca(df, filter.significance = TRUE, zero.action = 'ownclass', filter.value = 0.1)  #non va con 6 variabili

print(cca2)

length(cca2$membership)

cormat.to.igraph(cca2)
plot(cca2, 1) 
plot(cca2, 2) 
plot(cca2, 3) 
plot(cca2, 4) 
plot(cca2, 5) 


cca2$membership


cca2$modules

df$cca <- cca2$membership

write.csv(df, file = "df-prova10cca.csv", row.names = FALSE)


plot(cca2, module = 1, colorblind = TRUE, 
     heat_labels = TRUE)

plot(cca2, module = 1, heatmap = FALSE, 
     layout = layout.circle, edge_color = "red", 
     vertex_color = "gray", vertex_frame_color = "red", 
     vertex_size = 30, vertex_label_color= "red", 
     vertex_label_cex = 1, margin = 0.2)


plot(cca2, module = 2, heatmap = FALSE, 
     layout = layout.circle, edge_color = "red", 
     vertex_color = "gray", vertex_frame_color = "red", 
     vertex_size = 30, vertex_label_color= "red", 
     vertex_label_cex = 1, margin = 0.2)


plot(rca, module = 3, heatmap = FALSE, 
     layout = layout.circle, edge_color = "red", 
     vertex_color = "gray", vertex_frame_color = "red", 
     vertex_size = 30, vertex_label_color= "red", 
     vertex_label_cex = 1, margin = 0.2)


plot(rca, module = 4, heatmap = FALSE, 
     layout = layout.circle, edge_color = "red", 
     vertex_color = "gray", vertex_frame_color = "red", 
     vertex_size = 30, vertex_label_color= "red", 
     vertex_label_cex = 1, margin = 0.2)

#The membership vector will tell us which group each person belongs to. 
#We can use this information in later analyses to analyze predictors of class membership etc. 



#classi
print(rca$membership)