#install.packages("kknn")
library(kknn)
library(ggplot2)

overall_sample <- cleantrain[sample(nrow(cleantrain), 15000, replace =  FALSE), ]
mod_train <- overall_sample[1:5000, ]
mod_test <- overall_sample[5001:15000, ]

mod_train_opt <- mod_train[1:nrow(mod_train),c("target",best_feat[1:5])]
mod_train_opt <- as.data.frame(lapply(mod_train_opt, normalize))
mod_train_opt$target <- as.factor(mod_train_opt$target)

mod_test_opt <- mod_test[1:nrow(mod_test), best_feat[1:5]]
mod_test_opt <- as.data.frame(lapply(mod_test_opt, normalize))

train_ans <- mod_train[["target"]]
test_ans <- mod_test[["target"]]

kknn_DF <- as.data.frame(matrix(ncol = 3, nrow = 30))
colnames(kknn_DF) <- c("k", "train_error", "test_error")
count = 1

for(n in seq(1, 60, 2)){
  
  kknn_DF[count,"k"] <- n
  
  weighted_knn_train <-  kknn(formula = target ~ . , 
                              train = mod_train_opt, 
                              test = mod_train_opt, 
                              k = n, kernel = "optimal")
  
  weighted_knn_test <-  kknn(formula = target ~ . , 
                             train = mod_train_opt, 
                             test = mod_test_opt,
                             k =  n, kernel = "optimal")
  
  
  kknn_DF[count, "train_error"] <- MissclassError(weighted_knn_train$fitted.values, train_ans)
  kknn_DF[count, "test_error"] <- MissclassError(weighted_knn_test$fitted.values, test_ans)
  
  count <- count + 1
}


ggplot(kknn_DF, aes(x = k)) + 
  geom_line(aes(y = test_error, colour = "test_error"), size  = 1) +
  geom_line(aes(y = train_error, colour = "train_error"), size = 1) + 
  labs(title = "Errors by Amount of Neighbours", x = "Neighbours (k)", y = "Error") +
  scale_colour_discrete(name = "Error Type",
                        breaks = c("test_error", "train_error"),
                        labels = c("Test Error", "Train Error")
  )

