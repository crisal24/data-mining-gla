## packages ##

library("rpart") # tree
library("DMwR") # prettyTree function

#############

data <- allDataLA # trees can handle missing values

tree_score_prediction <- function() {
  
  # input outsider value
  data[data$AGE==92 & !is.na(data$AGE),]$AGE <- 16
  
  # delete columns with post answers and POSTQSCORE and variable J6, GAIN
  # leave as first column PASS with binary result to be predicted
  PASS <- data$PASS
  unbalanced_data <- cbind(PASS, data[,2:27], data[,43:50], data[,52:87])
  
  # *******************************
  # deal with imbalanced classes
  # *******************************
  
  passed <- unbalanced_data[unbalanced_data$PASS==T,]
  failed <- unbalanced_data[unbalanced_data$PASS==F,]
  
  nf <- nrow(failed) # take this as 40% of new set
  N = round(nf*100/40)
  np <- N - nf # 60% of pass class
  passed_undersample <- passed[sample(nrow(passed), np), ]
  balanced_data = rbind(passed_undersample, failed)
  
  # construct regression tree
  # to predict variable POSTQSCORE
  mytree <- rpart(PASS ~ ., data=balanced_data, method="class")
  prettyTree(mytree)
  
  # intermediate trees and variables used
  printcp(mytree)
  
  
  # select tree with cp value 0.06
  mytree2 <- prune(mytree, cp=0.005)
  prettyTree(mytree2)
  
  tree_cross_validation(balanced_data)
  
  tree_cross_validation(unbalanced_data)
 
}

tree_cross_validation <- function(data) {
  
  k = 10 # cross validation 10 groups
  prediction <- data.frame() # prediction set
  probabilities <- data.frame()
  testsetCopy <- data.frame()
  data$id <- sample(1:k, nrow(data), replace = TRUE) # observation identifier
  list <- 1:k
  Iter   <- data.frame(iteracion = NULL, error = NULL, precision = NULL, recall = NULL)
  
  #function for k fold
  for(i in 1:k) {
    # remove rows with id i from dataframe to create training set
    # select rows with id i to create test set
    trainingset <- subset(data, id %in% list[-i])
    testset <- subset(data, id %in% c(i))
    
    #run a random forest model
    mytree <- rpart(PASS ~ ., data=trainingset, method="class")
    
    #remove response column 1 PASS
    temp <- as.data.frame(predict(mytree, testset[,-1]))
    
    # append this iteration's probabilities to the end of the prediction data frame
    probabilities <- rbind(probabilities, temp)
    
    prediction <- probabilities$"TRUE" > probabilities$"FALSE"
    
    # append this iteration's test set to the test set copy data frame
    # keep only the PASS column
    testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
    
    # add predictions and actual PASS values
    error <- prediction!=testsetCopy[,1]
    
    true_positives <- 0
    false_positives <- 0
    false_negatives <- 0
    
    for (i in 1:length(error)) {
      if (prediction[i] && testsetCopy[,1][i]) { # true positives
        true_positives <- true_positives + 1
      }
      if (prediction[i] && !testsetCopy[,1][i]) { # false positives
        false_positives <- false_positives + 1
      }
      if (!prediction[i] && testsetCopy[,1][i]) { # false negative
        false_negatives <- false_negatives + 1
      }
    }
    
    # positive predictive value: fraction of true positives of all positives
    precision <- (true_positives / (true_positives + false_positives))
      
    # sensitivity: fraction of true positives of all real positives
    recall <- (true_positives / (true_positives + false_negatives))
    
    Iter  <- rbind(Iter, data.frame(Iter = i,
                                    error = sum(error[(length(error)+1-nrow(testset)):length(error)]),
                                    precision = precision,
                                    recall = recall
                                    ))  
    
  }
  
  par(mfrow=c(1,3))
  
  # GRAFICO ERROR
  # -------------------------------------------------------------------------------- 
  promedio  <- format(mean(Iter$error, na.rm=TRUE),digits = 4)
  plot(Iter$error,type = "b", main = "Numero de errores en cada iteracion",  
       cex.axis = .7,cex.lab = .7,cex.main = .8, 
       xlab ="No. de Iteraciones", ylab="% Error")
  abline(h = mean(Iter$error), col = "blue", lty = 2)
  legend("topright", legend = paste("N?mero de errores medios de Prediccion =", promedio),
         col = "blue", lty = 2, lwd = 1, cex=.6, bg=NULL)
  
  
  # GRAFICO PRECISION
  # -------------------------------------------------------------------------------- 
  promedio  <- format(mean(Iter$precision, na.rm=TRUE),digits = 4)
  plot(Iter$precision,type = "b", main = "Precision en cada iteracion",  
       cex.axis = .7,cex.lab = .7,cex.main = .8, 
       xlab ="No. de Iteraciones", ylab="Precision")
  abline(h = mean(Iter$precision), col = "blue", lty = 2)
  legend("topright", legend = paste("Precision media =", promedio),
         col = "blue", lty = 2, lwd = 1, cex=.6, bg=NULL)
  
  
  # GRAFICO RECALL
  # -------------------------------------------------------------------------------- 
  promedio  <- format(mean(Iter$recall, na.rm=TRUE),digits = 4)
  plot(Iter$recall,type = "b", main = "Recall en cada iteracion",  
       cex.axis = .7,cex.lab = .7,cex.main = .8, 
       xlab ="No. de Iteraciones", ylab="Recall")
  abline(h = mean(Iter$recall), col = "blue", lty = 2)
  legend("topright", legend = paste("Recall media =", promedio),
         col = "blue", lty = 2, lwd = 1, cex=.6, bg=NULL)
}


tree_score_prediction_gameonly <- function() {
  
  # input outsider value
  data[data$AGE==92 & !is.na(data$AGE),]$AGE <- 16
  
  # delete columns with post answers and POSTQSCORE and variable J6, GAIN
  # leave as first column PASS with binary result to be predicted
  unbalanced_data <- data[,52:71]
  unbalanced_data$PASS <- data$PASS
  
  # *******************************
  # deal with imbalanced classes
  # *******************************
  
  passed <- unbalanced_data[unbalanced_data$PASS==T,]
  failed <- unbalanced_data[unbalanced_data$PASS==F,]
  
  nf <- nrow(failed) # take this as 40% of new set
  N = round(nf*100/40)
  np <- N - nf # 60% of pass class
  passed_undersample <- passed[sample(nrow(passed), np), ]
  balanced_data = rbind(passed_undersample, failed)
  
  # construct regression tree
  # to predict variable POSTQSCORE
  mytree <- rpart(PASS ~ ., data=balanced_data, method="class")
  prettyTree(mytree)
  
  # intermediate trees and variables used
  printcp(mytree)

  # cross validation on balanced data
  PASS <- balanced_data$PASS
  id <- 1:nrow(balanced_data)
  balanced_data_cv <- cbind(PASS, id, balanced_data[,1:20])
  tree_cross_validation_gameonly(balanced_data_cv)
  
  
  # cross validation on original unbalanced data
  PASS <- unbalanced_data$PASS
  id <- 1:nrow(unbalanced_data)
  unbalanced_data_cv <- cbind(PASS, id, unbalanced_data[,1:20])
  tree_cross_validation_gameonly(unbalanced_data_cv)
}

tree_cross_validation_gameonly  <- function(data) {
  
  k = 10 # cross validation 10 groups
  prediction <- data.frame() # prediction set
  probabilities <- data.frame()
  testsetCopy <- data.frame()
  data$id <- sample(1:k, nrow(data), replace = TRUE) # observation identifier
  list <- 1:k
  Iter   <- data.frame(iteracion = NULL, error = NULL)
  
  #function for k fold
  for(i in 1:k) {
    # remove rows with id i from dataframe to create training set
    # select rows with id i to create test set
    trainingset <- subset(data, id %in% list[-i])
    testset <- subset(data, id %in% c(i))
    
    #run a random forest model
    mytree <- rpart(PASS ~ ., data=data, method="class")
    
    #remove response column 1 PASS
    temp <- as.data.frame(predict(mytree, testset[,-1]))
    
    # append this iteration's probabilities to the end of the prediction data frame
    probabilities <- rbind(probabilities, temp)
    
    prediction <- probabilities$"TRUE" > probabilities$"FALSE"
    
    # append this iteration's test set to the test set copy data frame
    # keep only the PASS column
    testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
    
    # add predictions and actual PASS values
    # add predictions and actual PASS values
    error <- prediction!=testsetCopy[,1]
    
    true_positives <- 0
    false_positives <- 0
    false_negatives <- 0
    
    for (i in 1:length(error)) {
      if (prediction[i] && testsetCopy[,1][i]) { # true positives
        true_positives <- true_positives + 1
      }
      if (prediction[i] && !testsetCopy[,1][i]) { # false positives
        false_positives <- false_positives + 1
      }
      if (!prediction[i] && testsetCopy[,1][i]) { # false negative
        false_negatives <- false_negatives + 1
      }
    }
    
    # positive predictive value: fraction of true positives of all positives
    precision <- (true_positives / (true_positives + false_positives))
    
    # sensitivity: fraction of true positives of all real positives
    recall <- (true_positives / (true_positives + false_negatives))
    
    Iter  <- rbind(Iter, data.frame(Iter = i,
                                    error = sum(error[(length(error)+1-nrow(testset)):length(error)]),
                                    precision = precision,
                                    recall = recall
    ))  
    
  }

  par(mfrow=c(1,3))
  
  # GRAFICO ERROR
  # -------------------------------------------------------------------------------- 
  promedio  <- format(mean(Iter$error, na.rm=TRUE),digits = 4)
  plot(Iter$error,type = "b", main = "Numero de errores en cada iteracion",  
       cex.axis = .7,cex.lab = .7,cex.main = .8, 
       xlab ="No. de Iteraciones", ylab="% Error")
  abline(h = mean(Iter$error), col = "blue", lty = 2)
  legend("topright", legend = paste("N?mero de errores medios de Prediccion =", promedio),
         col = "blue", lty = 2, lwd = 1, cex=.6, bg=NULL)
  
  
  # GRAFICO PRECISION
  # -------------------------------------------------------------------------------- 
  promedio  <- format(mean(Iter$precision, na.rm=TRUE),digits = 4)
  plot(Iter$precision,type = "b", main = "Precision en cada iteracion",  
       cex.axis = .7,cex.lab = .7,cex.main = .8, 
       xlab ="No. de Iteraciones", ylab="Precision")
  abline(h = mean(Iter$precision), col = "blue", lty = 2)
  legend("topright", legend = paste("Precision media =", promedio),
         col = "blue", lty = 2, lwd = 1, cex=.6, bg=NULL)
  
  
  # GRAFICO RECALL
  # -------------------------------------------------------------------------------- 
  promedio  <- format(mean(Iter$recall, na.rm=TRUE),digits = 4)
  plot(Iter$recall,type = "b", main = "Recall en cada iteracion",  
       cex.axis = .7,cex.lab = .7,cex.main = .8, 
       xlab ="No. de Iteraciones", ylab="Recall")
  abline(h = mean(Iter$recall), col = "blue", lty = 2)
  legend("topright", legend = paste("Recall media =", promedio),
         col = "blue", lty = 2, lwd = 1, cex=.6, bg=NULL)
}
