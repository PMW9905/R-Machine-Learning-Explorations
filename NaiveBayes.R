start <- proc.time()
df <- read.csv("titanic_project.csv", header=TRUE)
#getting rid of extraneous row.
df <- df[,2:5]

#turning qualitative data into factors
df$pclass <- factor(df$pclass)
df$survived <- factor(df$survived)
df$sex <- factor(df$sex)

#splitting data into test and train
train <- df[1:750,]
test <- df[751:1046,]

#importing naive Bayes
if(!require("e1071"))
{
  install.packages("e1071")
}
library(e1071)

nb <- naiveBayes(survived~., data=train)
nb

pred <- predict(nb, newdata=test, type="class")
tbl <- table(pred,test$survived)
tbl
acc <- mean(pred==test$survived)
sensitivity <- tbl[2,2]/(tbl[2,2]+tbl[1,2])
specificity <- tbl[1,1]/(tbl[1,1]+tbl[2,1])
print(paste("acc:",acc))
print(paste("sensitivity:",sensitivity))
print(paste("specificty:",specificity))
end <- proc.time()
elapsed <- end[[3]] - start[[3]]
print(paste("Elapsed time: ",elapsed))