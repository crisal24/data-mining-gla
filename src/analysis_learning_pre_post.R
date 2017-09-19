# libraries
library("coin") # for wilcoxsign_test

data <- allData

correct_answers <- function(data) {
  # delete observations with NA in questionnaire answers
  data <- data[rowSums(is.na(data[c("PREQ1", "PREQ2", "PREQ3", "PREQ4", "PREQ5", "PREQ6", "PREQ7", "PREQ8", "PREQ9", "PREQ10", "PREQ11", "PREQ12", "PREQ13", "PREQ14", "PREQ15", "POSTQ1", "POSTQ2", "POSTQ3", "POSTQ4", "POSTQ5", "POSTQ6", "POSTQ7", "POSTQ8", "POSTQ9", "POSTQ10", "POSTQ11", "POSTQ12", "POSTQ13", "POSTQ14", "POSTQ15")])) == 0, ]
  # list of correct answers for questionnaires
  correct <- c("A1", "A3", "A1", "A3", "A4", "A1", "A2", "A1", "A1", "A3", "A1", "A2", "A1", "A1", "A3")
  # for each questionnaire question and student, create binary variable with right / wrong answer
  for (questionnaire in c("PREQ", "POSTQ")) {
    data[[paste(questionnaire, "SCORE", sep='')]] <- 0
    for (q in 1:15) {
      data[[paste(questionnaire, q, "RIGHT", sep='')]] <- F
      for (student in 1:nrow(data)) {
        if (correct[[q]]==data[[student, paste(questionnaire, q, sep='')]]) {
          data[[student, paste(questionnaire, q, "RIGHT", sep='')]] <- T
          data[[student, paste(questionnaire, "SCORE", sep='')]] <- data[[student, paste(questionnaire, "SCORE", sep='')]] + 1
        }
      }
    }
  }
  
  # write.table(data, "/home/cristina/R/tdata/dataca.csv", sep=",")
  
  return(data)
}

analysis_learning <- function() {
  data <- correct_answers(allData)
  data[["GAIN"]] <- data$POSTQSCORE - data$PREQSCORE
  data[["PASS"]] <- data$POSTQSCORE>7
  # data[["STDPREQSCORE"]] <- (data$PREQSCORE - mean(data$PREQSCORE)) / sd(data$PREQSCORE)
  # data[["STDPOSTQSCORE"]] <- (data$POSTQSCORE - mean(data$POSTQSCORE)) / sd(data$POSTQSCORE)
  # hist(data$STDPREQSCORE)
  # hist(data$STDPOSTQSCORE)
  
  mean(data$PREQSCORE)
  mean(data$POSTQSCORE)
  mean(data$POSTQSCORE) - mean(data$PREQSCORE)
  
  hist(data$PREQSCORE,  main="Histogram of variable PREQSCORE", xlab = "PREQSCORE value")
  hist(data$POSTQSCORE, main="Histogram of variable POSTQSCORE", xlab = "POSTQSCORE value")
  
  boxplot(data$GAIN, main="Boxplot of variable GAIN")
  qqnorm(data$GAIN, main="QQPlot of variable GAIN")
  qqline(data$GAIN)
  
  boxplot(data$PREQSCORE, data$POSTQSCORE, main="Boxplot of PREQSCORE (left) and POSTQSCORE(right)")
  
  plot(data$PREQSCORE, data$POSTQSCORE, main="Plof of PREQSCORE against POSTQSCORE", xlab="PREQSCORE", ylab="POSQSCORE")
  abline(a=0, b=1)
  
  shapiro.test(data$PREQSCORE)
  shapiro.test(data$POSTQSCORE)
  
  # if they had normal distributions...
  t.test(data$POSTQSCORE, data$PREQSCORE, mu=0, paired=T, alternative="greater")
  
  # but they don't follow normal distributions therefore
  # wilcoxon signed-rank test
  wilcox.test(data$PREQSCORE, data$POSTQSCORE, paired=T)
  
  # result: V=1946 and p-value < 0.01
  # as p-value < 0.05, we conclude the difference is not zero
  # but tghis does not provide the z-score to calculate the effect size so:
  wilcoxsign_test(PREQSCORE ~ POSTQSCORE, data=data)
  
  # returns z=-8.3456 and same p-value
  
}


