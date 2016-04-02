MissclassError <- function(predictions, actuals){
  sum(predictions != actuals) / length(predictions)
}
  