getwd()
setwd("C:/Users/zrjha/Desktop")

install.packages("tidyr")
install.packages("ggplot2")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("dplyr")
install.packages("rattle")
install.packages("RGtk2")
install.packages("caret")
install.packages("class")
install.packages("randomForest")

library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(RGtk2)
library(caret)
library(class)
library(randomForest)

#Importing Data.

df<-read.csv("selfdrivingcars_images.csv")
View(df)
summary(df)
levels(df)
col(df)
names(df)

#Data cleaning not required.

#Removing Id Column 

df <- subset(df, select = -id )
View(df)
str(df)

#Train Test Split.
set.seed(6)
rows<-sample(nrow(df))
head(df)
df
df<-df[rows,]head(df)

#Where to split(80/20) 

split<-round(nrow(df)*.80)
split

#Create Train Test
train_df<-df[1:split,]
test_df<-df[(split+1):nrow(df),]
View(train_df)
View(test_df)


#Model 1 - Decision Tree

tree<-rpart(train_df$sign_type ~.,
            data=train_df,
            method = "class")
tree
rpart.plot(tree,extra=4)

pred <- predict(tree, test_df, type = "class")
View(pred)
head(pred)
dim(pred)
dim(test_df)
table(test_df$sign_type)
table(pred)

#Confusion matrix
conf_matrix <- table(test_df$sign_type,pred)
conf_matrix

#Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy 

#85.36%<- Model 1 accuracy

  
#Model 2 - KNN classification (using sinlge train test split and not cross validation)

knn<-knn3(train_df$sign_type~.,train_df,k=6)
knn

predKnn<-predict(knn,test_df, type = "class")
predKnn
table(predKnn)

#Accuracy Measures KNN 
KnnCM<-table(test_df$sign_type,predKnn)
KnnCM

accuracy <- sum(diag(KnnCM)) / sum(KnnCM)
accuracy

#90% percent accuracy for KNN


#MODEL 3 - RANDOM FOREST MODEL(using single train test split) 

RF<-randomForest(sign_type ~ . , data = train_df,method="class")
RF
rpart.plot(RF,extra = 4)
plot(RF)
print(RF)

predRF<-predict(RF,test_df,type = "class")
predRF
table(predRF)


#Accuracy measures.
CRF<-table(test_df$sign_type,predRF)
CRF
accuracyRF<-sum(diag(CRF))/sum(CRF)
accuracyRF  
 
#95.121 % accuracy for Random Forest.


#Out of all the models we used Random Forest gave us highest accuracy - 95%

#.............................................(..)................................

# MODEL 4 - CROSS VALIDATION USING CARET - TREE

train_control<-trainControl(method = "cv",number = 10)
model1<- train(sign_type ~., data=train_df, trControl=train_control, method="rpart")
print(model1) #Accuracy and Kappa show us different parameters of accuracy, higher CP lower accuracy. 
plot(model1)

predmodel<-predict(model1,test_df)
table<-table(test_df$sign_type,predmodel)
matrix<-sum(diag(table))/sum(table)
matrix 
# 82 percent accuracy.
  

# MODEL 5 - CROSS VALIDATION  USING CARET - RANDOM FOREST.

train_control1<-trainControl(method = "cv", number = 10)  
model2<-train(sign_type ~ .,data = train_df, method ="rf", trControl=train_control1)
print(model2) # accuracy and kappa 
plot(model2)
predmodel2<-predict(model2,test_df)
table2<-table(test_df$sign_type,predmodel2)
matrix2<-sum(diag(table2))/sum(table2)
matrix2

#97 accuracy


#........................the end .................................