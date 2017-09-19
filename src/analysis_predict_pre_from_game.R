## packages ##

library("e1071") # naiveBayes function

#############

data <- allDataLA # trees can handle missing values

# function to create, run and record model results
# Ref: https://eight2late.wordpress.com/2015/11/06/a-gentle-introduction-to-naive-bayes-classification-using-r/ 
nb_multiple_runs <- function(data, train_fraction,n){
  fraction_correct <- rep(NA,n)
  precision <- rep(NA,n)
  recall <- rep(NA, n)
  for (i in 1:n){
    data[,"train"] <- ifelse(runif(nrow(data))<train_fraction,1,0)
    trainColNum <- grep("train",names(data))
    data_train <- data[data$train==1,-trainColNum]
    data_test <- data[data$train==0,-trainColNum]
    nb_model <- naiveBayes(PREQPASS~.,data = data)
    nb_test_predict <- predict(nb_model,data_test[,-1])
    fraction_correct[i] <- mean(nb_test_predict==data_test$PREQPASS)
    true_positives <- 0
    false_positives <- 0
    false_negatives <- 0
    for (j in 1:length(nb_test_predict)) {
      if (nb_test_predict[j] && data_test$PREQPASS[j]) { # true positives
        true_positives <- true_positives + 1
      }
      if (nb_test_predict[j] && !data_test$PREQPASS[j]) { # false positives
        false_positives <- false_positives + 1
      }
      if (!nb_test_predict[j] && data_test$PREQPASS[j]) { # false negative
        false_negatives <- false_negatives + 1
      }
    }
    
    # positive predictive value: fraction of true positives of all positives
    precision[i] <- (true_positives / (true_positives + false_positives))
    
    # sensitivity: fraction of true positives of all real positives
    recall[i] <- (true_positives / (true_positives + false_negatives)) 
  }
  return(cbind(fraction_correct, precision, recall))
}

naive_bayes <- function() {
  # input outsider value
  data[data$AGE==92 & !is.na(data$AGE),]$AGE <- 16
  
  # delete columns with post answers and POSTQSCORE and variable J6, GAIN
  # leave as first column PASS with binary result to be predicted
  PREQPASS <- data$PREQSCORE > 7
  data <- cbind(PREQPASS, data[,52:71])
  
  # train: 70%, 100 iterations
  results <- nb_multiple_runs(data, 0.7, 100)
  # summary of results
  summary(results)
  apply(results, 2, sd)

}
