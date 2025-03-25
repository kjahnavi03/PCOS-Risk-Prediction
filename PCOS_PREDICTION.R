data4<-read.csv("C:/Users/devik/Downloads/CLEAN- PCOS SURVEY SPREADSHEET.csv")
data4

View(data4)
str(data4)
summary(data4)

colnames(data4) <- c(
  "Age", "Weight", "Height", "BloodGroup", "PeriodFrequency", 
  "WeightGain", "ExcessiveHair", "SkinDarkening", "HairLoss", 
  "Acne", "FastFood", "Exercise", "PCOS_Diagnosis", 
  "MoodSwings", "RegularPeriods", "PeriodDuration"
)
View(data4)

categorical_columns <- c(
  "BloodGroup", "PeriodFrequency", "WeightGain", "ExcessiveHair", 
  "SkinDarkening", "HairLoss", "Acne", "FastFood", "Exercise", 
  "PCOS_Diagnosis", "MoodSwings", "RegularPeriods"
)
data4[categorical_columns] <- lapply(data1[categorical_columns], as.factor)

data4 <- na.omit(data4)

numerical_columns <- c("Age", "Weight", "Height", "PeriodDuration")
data4[numerical_columns] <- scale(data4[numerical_columns])




#KNN

library(caTools)
set.seed(123)
split <- sample.split(data4$PCOS, SplitRatio = 0.8)
train_data <- subset(data4, split == TRUE)
test_data <- subset(data4, split == FALSE)
train_label<-train_data$PCOS_Diagnosis
test_label<-test_data$PCOS_Diagnosis

library(class)

knn_class <- knn(train = train_data[, -which(names(train_data) == "PCOS_Diagnosis")],
                 test = test_data[, -which(names(test_data) == "PCOS_Diagnosis")],
                 cl = train_label, k = 5)
knn_class

cm_knn <- table(test_label, knn_class)
cm_knn

acc_knn <- sum(diag(cm_knn)) / sum(cm_knn)
paste("Accuracy KNN: ", round(acc_knn * 100, 2), "%")



#Naive Bayes

library(e1071)

model_naive <- naiveBayes(PCOS_Diagnosis ~ ., data = train_data)
model_naive

predict_naive <- predict(model_naive, newdata = test_data)
cm_naive <- table(test_label, predict_naive)
cm_naive
acc_naive <- sum(diag(cm_naive)) / sum(cm_naive)
paste("Accuracy Naive Bayes: ", round(acc_naive * 100, 2), "%")


#Decision Tree

library(rpart)
library(rpart.plot)


dt_model <- rpart(PCOS_Diagnosis~ ., data = train_data, method = "class")
dt_model

rpart.plot(dt_model, main = "Decision Tree Structure")

predict_dt <- predict(dt_model, test_data, type = "class")
cm_dt <- table(test_label, predict_dt)
cm_dt
acc_dt <- sum(diag(cm_dt)) / sum(cm_dt)
paste("Accuracy Decision Tree: ", round(acc_dt * 100, 2), "%")




#ANN

train_data[] <- lapply(train_data, function(x) {
  if (is.factor(x)) {
    as.numeric(as.factor(x))  
  } else {
    x  
  }
})


library(neuralnet)
model_ann <- neuralnet(PCOS_Diagnosis ~ ., data = train_data)
model_ann

plot(model_ann)


cm_ann <- table(test_label, predict_dt)
print(cm_ann)

accuracy_ann <- sum(diag(cm_ann)) / sum(cm_ann)
print(paste("Accuracy ANN:", round(accuracy_ann * 100, 2), "%"))

