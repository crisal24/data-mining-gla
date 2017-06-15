data <- DATAALLSESSIONS

analysis_descriptive <- function() {
  
  sapply(data, mean)
  summary(data)
  fivenum(data$score)
  sapply(data, sd)   # standard deviation
  
  library("moments", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
  skewness(data$GAIN) # skewness
  kurtosis(data$GAIN) # kurtosis
  
  library("pastecs", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
  stat.desc(data)
  
  library("gmodels", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
  with(data, CrossTable(SEX, class))
  with(data, CrossTable(SEX, AGE))
  
  # descriptive
  hist(data$AGE, xlab = "Age", ylab = "Number of students", main =  "Histogram of number of students per age", breaks=20, ylim = c(0, 60))
  hist(data$H1, xlab = "General game play frequency (1 never, 5 daily)", ylab = "Number of students", main =  "Histogram of frequency of game play", breaks=15, ylim = c(0,60))
  
  boxplot(AGE ~ SEX, data = data, main="Boxplot of age per gender (F=Female, M=Male)", ylab="Age", xlab="Sex")
  boxplot(H1 ~ SEX, data = data, main="Boxplot of game play frequency per gender (F=Female, M=Male)", ylab="General game play frequency (1 never, 5 daily)", xlab="Sex")
  boxplot(H1 ~ AGE, data = data)
  
  data[data$class=="1BACH",]$class <- "Bachillerato"
  data$class <- factor(data$class)
  boxplot(H1 ~ class, data = data, main="Boxplot of game play frequency per class", ylab="General game play frequency (1 never, 5 daily)", xlab="Class")
  


}

#####################################

repitenDolorToracico <- nrow(subset(data, situacionMasRepetida=="DolorToracico"))
repitenAtragantamiento <- nrow(subset(data, situacionMasRepetida=="Atragantamiento"))
repitenInconsciente <- nrow(subset(data, situacionMasRepetida=="Inconsciente"))