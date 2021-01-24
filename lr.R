
#logistic regression 

setwd ("C:\\Users\\giusp\\Desktop\\pro-environmental-behaviour\\pro-environmental-behaviour")
set.seed(123)
df <-  read.csv("df-climatechange.csv", sep = ",", header=T)

df$X <- NULL
df$Unnamed..0 <- NULL
df$qb4_3 <- NULL
df$qb4_5 <- NULL
df$qb7 <- NULL
df$qb8 <- NULL
df$qb9 <- NULL
df$k_means_5 <- NULL
df$qb1a <- NULL
df$k_means <- NULL

View(df)
df$cca <- as.factor(df$cca)
df$k.means <- as.factor(df$k.means)
df$country <- as.factor(df$country)
df$d10 <- as.factor(df$d10)
df$d1 <- as.factor(df$d1)
df$d25 <- as.factor(df$d25)
df$d63 <- as.factor(df$d63)
df$d8 <- as.factor(df$d8)

df$d7 <- as.factor(df$d7)

require(dplyr)
df <- df %>%
  mutate(qb5 = ifelse(qb5 == "No",0,1))

View(df)
library(randomForest)
require(caTools)
library(Metrics) 
library(caret)
library(ggplot2)
library(scales)
library(ranger)

ggplotConfusionMatrix <- function(m){
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]),
                   "Kappa", percent_format()(m$overall[2]))
  p <-
    ggplot(data = as.data.frame(m$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}



## 75% of the sample size
smp_size <- floor(0.75 * nrow(df))

## set the seed to make your partition reproducible
set.seed(122)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

table(train$d25) 
table(test$d25) 
table(train$d8) 
table(test$d8) 

#metodo 1

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod1 <- train(as.factor(qb5) ~.,  data=train, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)
pred = predict(mod1, newdata=test)

confusionMatrix(data=pred, as.factor(test$qb5))
confusionMatrix(data=pred, as.factor(test$qb5), mode = "prec_recall", positive="1") #0.79
confusionMatrix(pred, as.factor(test$qb5), mode = "prec_recall") #0.4

summary(mod1)
summary(mod1)$coef


# modello 2 se si vuoi cambiare threshold
model <-  glm(qb5 ~. ,family=binomial(link='logit'),data=train )
summary(model)

probabilities = predict(model,test,type="response")
predicted.classes <- ifelse(probabilities > 0.6, "Yes Action", "No Action")
predicted.classes[1:5]

d <- table(predicted.classes,test$qb5)
d
sum(diag(d))/sum(d) #overall accuracy
1-sum(diag(d))/sum(d) #incorrect classification 


#modello con interazioni 
mod2 <-  glm(qb5 ~.+ d8:d63+d10:d8 +qb2:d8,family=binomial(link='logit'),data=train )
summary(mod2)


probabilities = predict(mod2,test,type="response")
predicted.classes <- ifelse(probabilities > 0.6, "1", "0") #aumeto threshold 
predicted.classes[1:5]

d <- table(predicted.classes,test$qb5)
d
sum(diag(d))/sum(d) #overall accuracy #0.68
1-sum(diag(d))/sum(d) #incorrect classification 


#Likelihood Ratio Test: confronto model and mo2: scelgo mod2
anova(model, mod2, test ="Chisq")

library(lmtest)
lrtest(model, mod2) #meglio mod2

#Pseudo R^2
library(pscl)
pR2(mod2) 


varImp(mod2)

library(caret)

library(pROC)
test_prob = predict(mod2, newdata = test, type = "response")
test_roc = roc(test$qb5 ~ test_prob, plot = TRUE, print.auc = TRUE)
as.numeric(test_roc$auc)

varImp(mod2, scale = FALSE)

varImp(mod2,type=2)


View(df)

#modello def
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod_fit <- train(as.factor(qb5) ~. + d8:d63+d10:d8 +qb2:d8,  data=train, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5)
pred = predict(mod_fit, newdata=test)




confusionMatrix(data=pred, as.factor(test$qb5))
confusionMatrix(pred, as.factor(test$qb5), mode = "prec_recall") #0.47
confusionMatrix(table(predict(mod_fit, type="prob")[,"1"] >= 0.6,
                      test$qb5 == "1"))



varImp(mod_fit, scale = F)


#https://www.datacamp.com/community/tutorials/logistic-regression-R
#http://www.sthda.com/english/articles/36-classification-methods-essentials/143-evaluation-of-classification-model-accuracy-essentials/
#https://www.r-bloggers.com/2015/08/evaluating-logistic-regression-models/