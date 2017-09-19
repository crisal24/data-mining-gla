## packages 

library("lattice")  # xyplot
library("leaps") # variable selection
library("DAAG") # cv

data <- allDataLA

predict_score_pre <- function() {
  
  # delete columns with post answers, code and J6 variable
  selected_data <- cbind(data[,2:27], data[,43:50], data[,52:88])
  selected_data <- na.exclude(selected_data)
  
  # model 1 all variables
  model1 = lm(POSTQSCORE ~ ., data = selected_data)
  summary(model1)
  print_model(model1)

  # model 2 only with variables of greater significance in previous coefficients
  # ***, ** Signif. codes:  0 '***' 0.001 '**'
  model2 = lm(POSTQSCORE ~ PREQ12 + PREQ13 + maxScoreCP + maxScoreCH + timesCP + 
                PREQ8 + PREQ9 + PREQ15 + J3 + gameCompleted, 
              data = selected_data)
  summary(model2)
  print_model(model2)
  
  # model 3 original important variables
  # ***, ** Signif. codes:  0 '***' 0.001 '**' 0.01 '*'
  model3 = lm(POSTQSCORE ~ PREQ12 + PREQ13 + maxScoreCP + maxScoreCH + timesCP, 
              data = selected_data)
  summary(model3)
  print_model(model3)
  
  # model 4 
  # *** and ** in model2
  model4 = lm(POSTQSCORE ~ PREQ12 + PREQ13 + maxScoreCP + PREQ8, 
              data = selected_data)
  summary(model4)
  print_model(model4)
  
  # model 5
  # variables important in all previous models
  model5 = lm(POSTQSCORE ~ PREQ12 + PREQ13 + maxScoreCP, 
              data = selected_data)
  summary(model5)
  print_model(model5)
 
  # model 6 interactions class-class
  model6 = lm(POSTQSCORE ~ PREQ12 + PREQ13 + maxScoreCP + PREQ12*PREQ13, 
              data = selected_data)
  summary(model6)
  print_model(model6)
  
  # model 7 interactions class-interval
  model7 = lm(POSTQSCORE ~ PREQ12 + PREQ13 + maxScoreCP + PREQ12*maxScoreCP + PREQ13*maxScoreCP, 
              data = selected_data)
  summary(model7)
  print_model(model7)
  
  # model 8 all interactions
  model8 = lm(POSTQSCORE ~ PREQ12 + PREQ13 + maxScoreCP + PREQ12*PREQ13 + PREQ12*maxScoreCP + PREQ13*maxScoreCP, 
              data = selected_data)
  summary(model8)
  print_model(model8)
  
  # model 9 variable selection: forward
  step(lm(POSTQSCORE ~ PREQ12 + PREQ13 + maxScoreCP + PREQ12*maxScoreCP + PREQ13*maxScoreCP, 
          data = selected_data), direction="forward")
  
  # model 10 variable selection: backward
  step(lm(POSTQSCORE ~ PREQ12 + PREQ13 + maxScoreCP + PREQ12*maxScoreCP + PREQ13*maxScoreCP, 
          data = selected_data), direction="backward")
  
  # model 11 variable selection: step by step
  step(lm(POSTQSCORE ~ PREQ12 + PREQ13 + maxScoreCP + PREQ12*maxScoreCP + PREQ13*maxScoreCP, 
          data = selected_data), direction="both")
  
  # variable selection
  leaps<-regsubsets(POSTQSCORE ~ PREQ12 + PREQ13 + maxScoreCP + PREQ12*maxScoreCP + PREQ13*maxScoreCP,data=selected_data,nbest=5)
  # view results
  summary(leaps)
  # plot a table of models showing variables in each model.
  # models are ordered by the selection statistic.
  plot(leaps,scale="r2")
  
  # cross validation
  CVlm(data=selected_data, model8, m=10) # 4 fold cross-validation
  
  # compare models, measures to compare: ASE (Average Square Error), AIC, SBC
  anova(model1, model8)

}

predict_score_only_game <- function() {
  
  # keep only columns with variables coming from in-game interactions
  selected_data <- data[,52:71]
  selected_data$POSTQSCORE <- data$POSTQSCORE
  selected_data <- na.exclude(selected_data)
  
  # model 1 all variables
  model1 = lm(POSTQSCORE ~ ., data = selected_data)
  summary(model1)
  print_model(model1)
  
  # model 2 only with variables of significance in previous coefficients
  model2 = lm(POSTQSCORE ~ int_patient + maxScoreCP + firstScoreCP + mostRepeatedSituation + failedHPosition, 
              data = selected_data)
  summary(model2)
  print_model(model2)
  
  # model 3 original important variables
  model3 = lm(POSTQSCORE ~ int_patient + maxScoreCP + firstScoreCP + failedHPosition, 
              data = selected_data)
  summary(model3)
  print_model(model3)
  
  # model 4 important variables
  model4 = lm(POSTQSCORE ~ int_patient + maxScoreCP + failedHPosition, 
              data = selected_data)
  summary(model4)
  print_model(model4)
  
  # model 5 most important variables
  model5 = lm(POSTQSCORE ~ int_patient + maxScoreCP, 
              data = selected_data)
  summary(model5)
  print_model(model5)
  
  # model 6 interactions class-class
  model6 = lm(POSTQSCORE ~ int_patient + maxScoreCP + firstScoreCP + mostRepeatedSituation + failedHPosition +
                mostRepeatedSituation*failedHPosition, 
              data = selected_data)
  summary(model6)
  print_model(model6)
  
  # model 7 interactions interval-interval
  model7 = lm(POSTQSCORE ~ int_patient + maxScoreCP + firstScoreCP + mostRepeatedSituation + failedHPosition +
                int_patient*maxScoreCP + int_patient*firstScoreCP + maxScoreCP*firstScoreCP, 
              data = selected_data)
  summary(model7)
  print_model(model7)
  
  # model 8 interactions class-interval
  model8 = lm(POSTQSCORE ~ int_patient + maxScoreCP + firstScoreCP + mostRepeatedSituation + failedHPosition +
                int_patient*mostRepeatedSituation + int_patient*failedHPosition +
                maxScoreCP*mostRepeatedSituation + maxScoreCP*failedHPosition +
                firstScoreCP*mostRepeatedSituation + firstScoreCP*failedHPosition,
              data = selected_data)
  summary(model8)
  print_model(model8)
  
  # model 9 all interactions
  model9 = lm(POSTQSCORE ~ int_patient + maxScoreCP + firstScoreCP + mostRepeatedSituation + failedHPosition +
                mostRepeatedSituation*failedHPosition +
                int_patient*maxScoreCP + int_patient*firstScoreCP + maxScoreCP*firstScoreCP +
                int_patient*mostRepeatedSituation + int_patient*failedHPosition +
                maxScoreCP*mostRepeatedSituation + maxScoreCP*failedHPosition +
                firstScoreCP*mostRepeatedSituation + firstScoreCP*failedHPosition,
              data = selected_data)
  summary(model9)
  print_model(model9)
  
  
  # variable selection: forward
  step(lm(POSTQSCORE ~ int_patient + maxScoreCP + firstScoreCP + mostRepeatedSituation + failedHPosition +
            mostRepeatedSituation*failedHPosition +
            int_patient*maxScoreCP + int_patient*firstScoreCP + maxScoreCP*firstScoreCP +
            int_patient*mostRepeatedSituation + int_patient*failedHPosition +
            maxScoreCP*mostRepeatedSituation + maxScoreCP*failedHPosition +
            firstScoreCP*mostRepeatedSituation + firstScoreCP*failedHPosition,
          data = selected_data), direction="forward")
  
  # variable selection: backward
  step(lm(POSTQSCORE ~ int_patient + maxScoreCP + firstScoreCP + mostRepeatedSituation + failedHPosition +
            mostRepeatedSituation*failedHPosition +
            int_patient*maxScoreCP + int_patient*firstScoreCP + maxScoreCP*firstScoreCP +
            int_patient*mostRepeatedSituation + int_patient*failedHPosition +
            maxScoreCP*mostRepeatedSituation + maxScoreCP*failedHPosition +
            firstScoreCP*mostRepeatedSituation + firstScoreCP*failedHPosition,
          data = selected_data), direction="backward")
  
  # variable selection: step by step
  step(lm(POSTQSCORE ~ int_patient + maxScoreCP + firstScoreCP + mostRepeatedSituation + failedHPosition +
            mostRepeatedSituation*failedHPosition +
            int_patient*maxScoreCP + int_patient*firstScoreCP + maxScoreCP*firstScoreCP +
            int_patient*mostRepeatedSituation + int_patient*failedHPosition +
            maxScoreCP*mostRepeatedSituation + maxScoreCP*failedHPosition +
            firstScoreCP*mostRepeatedSituation + firstScoreCP*failedHPosition,
          data = selected_data), direction="both")
  
  # variable selection
  leaps<-regsubsets(POSTQSCORE ~ int_patient + maxScoreCP + firstScoreCP + mostRepeatedSituation + failedHPosition +
                      mostRepeatedSituation*failedHPosition +
                      int_patient*maxScoreCP + int_patient*firstScoreCP + maxScoreCP*firstScoreCP +
                      int_patient*mostRepeatedSituation + int_patient*failedHPosition +
                      maxScoreCP*mostRepeatedSituation + maxScoreCP*failedHPosition +
                      firstScoreCP*mostRepeatedSituation + firstScoreCP*failedHPosition,
                    data=selected_data,nbest=5)
  # view results
  summary(leaps)
  # plot a table of models showing variables in each model.
  # models are ordered by the selection statistic.
  plot(leaps,scale="r2")
  
  # cross validation
  CVlm(data=selected_data, model9, m=10) # 10 fold cross-validation
  CVlm(data=selected_data, model8, m=10) # 10 fold cross-validation
  
  # compare models, measures to compare: ASE (Average Square Error), AIC, SBC
  anova(model8, model9)
  
}

print_model <- function(model) {
  xyplot(resid(model) ~ fitted(model),
         xlab = "Fitted Values",
         ylab = "Residuals",
         main = "Residual Diagnostic Plot",
         panel = function(x, y, ...)
         {
           panel.grid(h = -1, v = -1)
           panel.abline(h = 0)
           panel.xyplot(x, y, ...)
         }
  )
  
  # diagnostic plots
  #layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
  #plot(model)
}
