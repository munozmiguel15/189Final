install.packages("caret")
library(caret)

confusion_test <- total_sample[1:20000, best_feat[1:5] ]
confusion_train <- total_sample[20001: 60000, best_feat[1:5] ]

confusion_train$target <- as.factor(total_sample[20001: 60000, "target"])

confusion_targets <- as.factor(total_sample[1:20000, "target"])

confusion_knn <-  kknn(formula = target ~ . , 
                       train = confusion_train, 
                       test = confusion_test, 
                       k = 30, kernel = "optimal")

confusionMatrix(data = confusion_knn$fitted.values, reference = confusion_targets, positive = "1" )