## packages ##

library("rpart", lib.loc="/usr/lib/R/library") # regression tree
library("DMwR", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3") # prettyTree function

#############

data <- allDataLA # trees can handle missing values

tree_score_prediction <- function() {

  # delete variable
  data$J6 <- NULL
  
  # input outsider value
  data[data$AGE==92 & !is.na(data$AGE),]$AGE <- 16
  
  # delete columns with post answers
  # data with info from pre questionnaire and xapi traces
  # selected_data <- cbind(data[,2:27], data[,43:87])
  # 
  # data only with in-game interacions information
  selected_data <- data[,51:70]
  selected_data$POSTQSCORE <- data$POSTQSCORE # target variable
  
  # construct regression tree
  # to predict variable POSTQSCORE
  mytree <- rpart(POSTQSCORE ~ ., data=selected_data, method="anova")
  
  # more strict versions
  # minsplit  the minimum number of observations that must exist in a node in order for a split to be attempted (default=20). 
  # minbucket  the minimum number of observations in any terminal <leaf> node (default=7).
  # cp complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted. (default=0.01)
  # xval number of cross-validations (default=10)
  # mytree <-rpart(POSTQSCORE ~ ., data=selected_data, method="anova", 
  #                control=rpart.control(minsplit=30, minbucket = 15))
  # mytree <-rpart(POSTQSCORE ~ ., data=selected_data, method="anova", 
  #               control=rpart.control(minsplit=30, minbucket = 15, cp=0.02))
  # mytree <-rpart(POSTQSCORE ~ ., data=selected_data, method="anova", 
  #                control=rpart.control(xval=20))
  
  
  # prettyTree(mytree)
  prettyTree(mytree, cex=0.7, branch=1, fwidth=0.3, fheight=0.3)
  
  # intermediate trees
  printcp(mytree)
  post(mytree)
  
  # plot cross-validation result to construct ree
  plotcp(mytree)
  
  par(mfrow=c(1,2)) # two plots on one page
  rsq.rpart(mytree) # visualize cross-validation results  
  # par(mfrow=c(1,1)) # return one plot per page
  
  # obtain apparent and relative R^2
  tmp <- printcp(mytree)
  rsq.val <- 1-tmp[,c(3,4)] 
  rsq.val
  
  # select tree with cp value 0.06
  mytree2 <- prune(mytree, cp=0.06)
  mytree3 <- prune(mytree, cp=0.04)
  
  prettyTree(mytree2)
  prettyTree(mytree3)
  
  # pruning tree graphically
  # prettyTree(mytree)
  # snip.rpart(mytree)

  tree_cross_validation(selected_data)
}

tree_cross_validation <- function(data) {
  
  k = 10 # cross validation 10 groups
  prediction <- data.frame() # prediction set
  testsetCopy <- data.frame()
  data$id <- sample(1:k, nrow(data), replace = TRUE) # observation identifier
  list <- 1:k
  Iter   <- data.frame(iteracion = NULL, aciertos = NULL)
  
  #function for k fold
  for(i in 1:k) {
    # remove rows with id i from dataframe to create training set
    # select rows with id i to create test set
    trainingset <- subset(data, id %in% list[-i])
    testset <- subset(data, id %in% c(i))
    
    #run a regression tree model
    mytree <- rpart(POSTQSCORE ~ ., data=trainingset, method="anova")

    # print info about tree including variables used
    printcp(mytree)
    
    # plot relative error of each tree for different sizes
    plotcp(mytree)
    
    #remove response column 71 POSTQSCORE
    #temp <- as.data.frame(predict(mytree, testset[,-71]))
    # remove reponse column 21 
    temp <- as.data.frame(predict(mytree, testset[,-21]))
    
    # append this iteration's predictions to the end of the prediction data frame
    prediction <- rbind(prediction, temp)
    
    # append this iteration's test set to the test set copy data frame
    # keep only the POSTQSCORE Column
    # testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,71]))
    testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,21]))
    
    # add predictions and actual Sepal Length values
    result <- cbind(prediction, testsetCopy[,1])
    names(result) <- c("Predicted", "Actual")
    result$Difference <- abs(result$Actual - result$Predicted)
    
    Iter          <- rbind(Iter, data.frame(Iter = i, error = mean(result$Difference)))  
    
  }
  
  write.table(result, "/home/cristina/R/tdata/results_tree_cv10.csv", sep=",")
  
  # As an example use Mean Absolute Error as Evalution 
  summary(result$Difference)
  
  # GRAFICO
  # -------------------------------------------------------------------------------- 
  promedio  <- format(mean(Iter$error, na.rm=TRUE) /10,digits = 4)
  plot(Iter,type = "b", main = "% Error en Cada Iteracion",  
       cex.axis = .7,cex.lab = .7,cex.main = .8, 
       xlab ="No. de Iteraciones", ylab="% Error")
  abline(h = mean(Iter$error), col = "blue", lty = 2)
  legend("topright", legend = paste("Error medio de Prediccion =", promedio, "%"),
         col = "blue", lty = 2, lwd = 1, cex=.7, bg=NULL)
}

# vc <- function() {
#   # ------------------------------------------------------------------------------- 
#   set.seed(1)
#   Folds         <- 10            
#   datos$kfold   <- sample(1:Folds, nrow(datos), replace = T)
#   
#   
#   # MODELOS 
#   # -------------------------------------------------------------------------------- 
#   Iter   <- data.frame(iteracion = NULL, aciertos = NULL)
#   for (i in 1:Folds)
#   {
#     Test          <- subset(datos, kfold  == i)
#     Entrenamiento <- subset(datos, !kfold == i) 
#     Modelo        <- rpart(POSTQSCORE ~ .,data = Entrenamiento)       
#     Prediccion    <- predict(Modelo, Test)  
#     MC            <- table(Test[, "POSTQSCORE"],Prediccion)           
#     Aciertos      <- MC[1, 1] / (MC[1, 1] + MC[2, 1])
#     Iter          <- rbind(Iter, data.frame(Iter = i, acierto = Aciertos))  
#   }
#   
#   
#   # GRAFICO
#   # -------------------------------------------------------------------------------- 
#   promedio  <- format(mean(Iter$acierto, na.rm=TRUE)*100,digits = 4)
#   plot(Iter,type = "b", main = "% Prediccion en Cada Iteracion",  
#        cex.axis = .7,cex.lab = .7,cex.main = .8, 
#        xlab ="No. de Iteraciones", ylab="% Prediccion")
#   abline(h = mean(Iter$acierto), col = "blue", lty = 2)
#   legend("topright", legend = paste("Eficiencia de Prediccion =", promedio, "%"),
#          col = "blue", lty = 2, lwd = 1, cex=.7, bg=NULL)
# }