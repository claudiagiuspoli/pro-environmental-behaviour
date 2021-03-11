
#CCA
setwd ("C:\\Users\\giusp\\Desktop\\pro-environmental-behaviour\\pro-environmental-behaviour")
set.seed(123)
df <-  read.csv("df-climatechange.csv", sep = ",", header=T)
dim(df)

df$X <- NULL

#create subset of climate change questions
df1 <- df[,4:8] 

View(df1)


library(corclass)

#fit correlational class analysis 

corrclass <- cca(df1, filter.significance = TRUE, zero.action = 'ownclass', filter.value = 0.1) 

#       zero.action = "ownclass" : the correlations between 0-var rows and all other rows
#     	is set to 0, and the correlations between all pairs of 0-var rows are set to 1. 
#       This effectively creates a "zero class".


length(corrclass$membership)
print(corrclass)

#plot classes
plot(corrclass, 1) 
plot(corrclass, 2) 
plot(corrclass, 3) 
plot(corrclass, 4) 
plot(corrclass, 5) 



#The membership vector will tell us which group each person belongs to. 
#We can use this information in later analyses to analyze predictors of class membership etc. 

df$cca <- corrclass$membership

write.csv(df, file = "df-climatechange.csv", row.names = FALSE)


#source: https://github.com/cran/corclass/blob/master/R/cca.R