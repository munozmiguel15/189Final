# use 20 fold on the data 

total_sample <- cleantrain[sample(nrow(cleantrain), 60000, replace =  FALSE), c("target", best_feat[1:5])]
step = 60000/20

fold_errors <- numeric(20)

for(i in 1:20){
  fold_test <- total_sample[((i-1)*step) : (i*step), best_feat[1:5] ]
  fold_train <- total_sample[-(((i-1)*step) : (i*step)), best_feat[1:5] ]
  fold_train$target <- as.factor(total_sample[-(((i-1)*step) : (i*step)), "target" ])
  
  fold_targets <- as.factor(total_sample[((i-1)*step) : (i*step), "target" ])
  
  final_model <- kknn(formula = target ~ . , 
                      train = fold_train, 
                      test = fold_test, 
                      k = 30, kernel = "optimal")
  
  
  fold_errors[i] <- MissclassError(final_model$fitted.values, fold_targets )
  
}


