iterative_train <- as.data.frame(matrix(nrow = nrow(mod_train), ncol = 0))
iterative_test <- as.data.frame(matrix(nrow = nrow(mod_test), ncol = 0))
iterative_train[["target"]] <- as.factor( mod_train$target )
model_error <- as.data.frame(matrix(ncol = 3))
colnames(model_error) <- c("feat_count", "train_error", "test_error")

number = 1

for(feature in best_feat){
  
    model_error[number, "feat_count"] <- number
    
    iterative_train[[feature]] <- normalize(mod_train[[feature]])
    iterative_test[[feature]] <- normalize(mod_test[[feature]])
    
    iterative_train_knn <- kknn(formula = target ~ . , 
                          train = iterative_train, 
                          test = iterative_train, 
                          k = 20, kernel = "optimal")
    
    iterative_test_knn <- kknn(formula = target ~ . , 
                               train = iterative_train, 
                               test = iterative_test, 
                               k = 20, kernel = "optimal")
    
    
    model_error[number, "train_error"] <-  MissclassError(iterative_train_knn$fitted.values, train_ans)
    model_error[number, "test_error"] <- MissclassError(iterative_test_knn$fitted.values, test_ans)
    
    number <- number + 1
  
}


ggplot(model_error, aes(x = feat_count)) + 
  geom_line(aes(y = train_error, colour = "train_error"), size = 1) + 
  geom_line(aes(y = test_error, colour = "test_error"), size = 1) + 
  labs(title = "Errors by Amount Features Included in Model", 
       x = "Feature Count", y = "Error") + 
  scale_colour_discrete(name = "Error Type",
                        breaks = c("test_error", "train_error"),
                        labels = c("Test Error", "Train Error")
                        )


