dataOrigin <- allDataLA
dataFuture <- dataTVLA
# delete class variable in TV
dataFuture$class <- NULL
# merge data frames
dataRecallers <- merge(dataOrigin, dataFuture, by= c("CODE"))
# 10 observations

analysis_parametric <- function() {
  data <- dataRecallers
  mean(data$POSTQSCORE.x) # post score of first experiment
  mean(data$PREQSCORE.y) # pre score of recall experiment
  mean(data$POSTQSCORE.x) - mean(data$PREQSCORE.y)
  
  hist(data$POSTQSCORE.x,  main="Histogram of variable POSTQSCORE in first experiment", xlab = "POSTQSCORE first exp value")
  hist(data$PREQSCORE.y, main="Histogram of variable PREQSCORE in recall experiment", xlab = "PREQSCORE exp recall value")
  
  recall <- data$POSTQSCORE.x - data$PREQSCORE.y
  boxplot(recall, main="Boxplot of variable recall")
  qqnorm(recall, main="QQPlot of variable recall")
  qqline(recall)
  
  boxplot(data$PREQSCORE.x, data$POSTQSCORE.x, data$PREQSCORE.y, data$POSTQSCORE.y,
          main="Boxplot of PREQSCORE and POSTQSCORE in exp 1 and exp 2", 
          xlab="PREQSCORE in exp 1, POSTQSCORE exp 1, PREQSCORE in exp 2, POSTQSCORE in exp 2")
  
  
  shapiro.test(data$POSTQSCORE.x)
  shapiro.test(data$PREQSCORE.y)
  
  # as they have normal distributions apply t-test
  t.test(data$PREQSCORE.y, data$POSTQSCORE.x, mu=-1.1, paired=T, alternative="greater")
}

### analysis learning

# libraries
library("coin") # for wilcoxsign_test

data <- dataTVLA

analysis_learning <- function() {
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
  
  # they don't follow normal distributions therefore
  # wilcoxon signed-rank test
  wilcox.test(data$PREQSCORE, data$POSTQSCORE, paired=T)
  
  # result: V=23 and p-value > 0.05
  # reject H0, therefore difference is not zero
  # but this does not provide the z-score to calculate the effect size so:
  wilcoxsign_test(PREQSCORE ~ POSTQSCORE, data=data)
  
  # returns z=-1.2613 and same p-value=0.2072
  
}

