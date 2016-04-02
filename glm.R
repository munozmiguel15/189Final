# loop over all features and calculate p-value
feature_errors <- as.data.frame(matrix(ncol = 2, nrow = ncol(cleantrain) - 2))
colnames(feature_errors) <- c("feature", "p_val")

# Create Samples
sampletrain <- cleantrain[sample(nrow(cleantrain), 10000, replace =  FALSE), ]
sampletrain_features <- sampletrain[,3:ncol(cleantrain)]
train_targets <- factor(sampletrain$target)


# loop
count = 1
for(feature in names(sampletrain_features)){
  
  feature_errors[count, "feature"] <- feature
  
  if (class(sampletrain_features[[feature]]) == "numeric"){
      
    working_train <- data.frame("target" = train_targets , "one_feat_data" = sampletrain_features[[feature]])

    working_glm <- glm(target ~ one_feat_data, data = working_train, family = "binomial")

    feature_errors[count, "p_val"] <- coef(summary(working_glm))[2,4]
    
  }else{
    feature_errors[count, "p_val"] <- NA
    
  }
  
  count <- count + 1
  
}

ordered_features <- feature_errors[with(feature_errors, order(p_val)), ]
unfiltered_feat <- ordered_features[ordered_features$p_val < 0.05, "feature"]
best_feat <- unfiltered_feat[!is.na(unfiltered_feat)]