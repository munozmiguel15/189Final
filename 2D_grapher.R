twoD_train <- mod_train_opt[1:nrow(mod_train_opt),c("v114", "v14")]
twoD_train$target <- as.factor(train_ans)
  
values <- seq(0,1, 0.01)

squaregrid <- as.data.frame(matrix(ncol = 2, nrow = length(values)^2))
count = 1
for(i in values){
  for(j in values){
    squaregrid[count,] <- c(i,j)
    count <- count + 1
  }
}


colnames(squaregrid) <- c("v114", "v14")

twoD_knn <- kknn(formula = target ~ . , 
     train = twoD_train, 
     test = squaregrid, 
     k = 7, kernel = "optimal")

squaregrid$preds <- as.factor(twoD_knn$fitted.values)

ggplot(squaregrid, aes(x = v14, y = v114, colour = preds )) + 
  geom_point(size = 3.75, shape = 15) +
  labs(title = "7-NN based on v14 and v114 (optimal kernal)")
