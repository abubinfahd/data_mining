#Installing Packages

# install.packages("tidyverse") # data manipulation & visualization
# 
# install.packages("caret") # training & evaluating
# 
# install.packages("randomForest") #randomforest
# 
# install.packages("e1071") #svm
# 
# install.packages("glm2") #logistic
#  
# install.packages("MASS") #QDA & LDA


#importing libraries
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)  
library(glm2)   
library(MASS)

#Read Data

# getwd()
# help("read.csv")

df_stroke<-read.csv("stroke.csv", header=TRUE)
df_stroke

#to get overall perspective of our data’s statistical distribution.
summary(df_stroke)

#Describing Data
glimpse(df_stroke)

#checking to confirm is there any missing values using "is.na" command
df_stroke$stroke<- factor(df_stroke$stroke, levels = c(0,1), labels = c("No", "Yes"))
df_stroke$gender<-as.factor(df_stroke$gender)
df_stroke$hypertension<- factor(df_stroke$hypertension, levels = c(0,1), labels = c("No", "Yes"))
df_stroke$heart_disease<- factor(df_stroke$heart_disease, levels = c(0,1), labels = c("No", "Yes"))
df_stroke$ever_married<-as.factor(df_stroke$ever_married)
df_stroke$work_type<-as.factor(df_stroke$work_type)
df_stroke$Residence_type<-as.factor(df_stroke$Residence_type)
df_stroke$smoking_status<-as.factor(df_stroke$smoking_status)
df_stroke$bmi<-as.numeric(df_stroke$bmi)

#Describing Data
glimpse(df_stroke)
str(df_stroke)

#check missing values
sum(is.na(df_stroke))
# Count missing values per column
colSums(is.na(df_stroke))


#imputing dataset
df_stroke$bmi[is.na(df_stroke$bmi)]<- mean(df_stroke$bmi,na.rm = TRUE)

#Since the missing values and data types have been properly configured, it is time to generate some graphs from the data to gain insights. We will plot the distribution for features – gender, hypertension,heart_disease and ever_married.
library(gridExtra)

p1 <- ggplot(df_stroke, aes(x="", y=gender, fill=gender)) + geom_bar(stat="identity", width=1)  + coord_polar("y", start=0)
p2 <-ggplot(df_stroke, aes(x="", y=hypertension, fill=hypertension)) + geom_bar(stat="identity", width=1)  + coord_polar("y", start=0)
p3 <-ggplot(df_stroke, aes(x="", y=heart_disease, fill=heart_disease)) + geom_bar(stat="identity", width=1)  + coord_polar("y", start=0)
p4 <-ggplot(df_stroke, aes(x="", y=ever_married, fill=ever_married)) + geom_bar(stat="identity", width=1)  + coord_polar("y", start=0)
grid.arrange(p1,p2,p3,p4 ,ncol= 2)
ggplot(df_stroke, aes(x="", y=Residence_type, fill=Residence_type)) + geom_bar(stat="identity", width=1)+ coord_polar("y", start=0)
ggplot(df_stroke, aes(x="", y=stroke, fill=stroke)) + geom_bar(stat="identity", width=1)+ coord_polar("y", start=0)

#We can create a few additional bar charts to see how each of these variables relates to the target variable, which is the stroke possibility for the individual.

p1 <- ggplot(data = df_stroke) +geom_bar(mapping = aes(x = gender,fill=stroke))
p2 <-ggplot(data = df_stroke) +geom_bar(mapping = aes(x = hypertension,fill=stroke))
p3 <-ggplot(data = df_stroke) +geom_bar(mapping = aes(x = heart_disease,fill=stroke))
p4 <-ggplot(data = df_stroke) +geom_bar(mapping = aes(x = ever_married,fill=stroke))
grid.arrange(p1,p2,p3,p4 ,ncol= 2)
p5 <- ggplot(data = df_stroke) +geom_bar(mapping = aes(x = work_type,fill=stroke))
p6 <-ggplot(data = df_stroke) +geom_bar(mapping = aes(x = Residence_type,fill=stroke))
p7 <-ggplot(data = df_stroke) +geom_bar(mapping = aes(x = smoking_status,fill=stroke))
grid.arrange(p5,p6,p7 ,ncol= 1)

###Model Building and Prediction

#Lets split the final dataset to training and test data
n_obs <- nrow(df_stroke)
split <- round(n_obs * 0.7)
train <- df_stroke[1:split,]

# Create test
test <- df_stroke[(split + 1):nrow(df_stroke),]
dim(train)
dim(test)

#Modeling
#Random Forest(RF)
set.seed(123)
rf_model<-randomForest(formula= stroke~ .,data = train)
rf_model
confusionMatrix(predict(rf_model, test), test$stroke)

#Quadratic Discriminant Analysis (QDA)
table(df_stroke$stroke)
library(MASS)
# qda_model <- qda(stroke ~ ., data = train)
# qda_predictions <- predict(qda_model, test)
# confusionMatrix(qda_predictions$class, test$stroke)

#Linear Discriminant Analysis (LDA)
lda_model <- lda(stroke ~ ., data = train)
lda_predictions <- predict(lda_model, test)
confusionMatrix(lda_predictions$class, test$stroke)

#Logistic Regression
logistic_model <- glm(stroke ~ ., data = train, family = binomial)
summary(logistic_model)
predict_cls <- predict(logistic_model,test, type = "response")
predict_cls
predict_cls <- ifelse(predict_cls >0.5, 1, 0)
conf_matrix <- table(test$stroke, predict_cls)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

#Support Vector Machine (SVM)
library(e1071)
svm_model <- svm(stroke ~ ., data = train, kernel = "linear")
y_pred = predict(svm_model, test)
cm = table(test$stroke, y_pred)
accuracy <- sum(diag(cm)) / sum(cm)
cat("Accuracy:", accuracy, "\n")









