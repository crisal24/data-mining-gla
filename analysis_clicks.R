library("sjPlot", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")  # sjt.corr

data <- allData

clicks_game_habits <- function() {
  # keep interactions variables
  # and game habits variables
  interactions <- c("int_patient", "int_phone", "int_saed")
  gameHabits <- c("H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11")
  data <- cbind(data[interactions], data[gameHabits])
  data$int_total <- rowSums(data[interactions])
  
  data[] <- lapply(data, as.integer)
  sjt.corr(data)
  
  boxplot(int_total ~ H1, data=data, 
          main="Boxplot of game play frequency per total interactions", 
          xlab="General game play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  
  boxplot(int_total ~ H2, data=data, 
          main="Boxplot of FPS games play frequency per total interactions", 
          xlab="FPS games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  
  boxplot(int_total ~ H4, data=data, 
          main="Boxplot of music games play frequency per total interactions", 
          xlab="Music games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  
  boxplot(int_total ~ H8, data=data, 
          main="Boxplot of sport games play frequency per total interactions", 
          xlab="Sport games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  
  boxplot(int_total ~ H9, data=data, 
          main="Boxplot of Mario games play frequency per total interactions", 
          xlab="Mario games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  
  boxplot(int_total ~ H11, data=data, 
          main="Boxplot of mobile games play frequency per total interactions", 
          xlab="Mobile games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  

}

clicks_game_habits_sex <- function() {
  # keep interactions variables
  # and game habits variables
  interactions <- c("int_patient", "int_phone", "int_saed")
  gameHabits <- c("H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11")
  sex <- c("SEX")
  data <- cbind(data[interactions], data[gameHabits], data[sex])
  data$int_total <- rowSums(data[interactions])
  
  #data[] <- lapply(data, as.integer)
  #sjt.corr(data)
  
  data_female <- subset(data, SEX=='F')
  data_male <- subset(data, SEX=='M')
  
  ## Ref: http://sphaerula.com/legacy/R/multiplePlotFigure.html
  par(mfrow=c(1,2), oma = c( 0, 0, 2, 0 ))
  
  boxplot(int_total ~ H1, data=data_female, 
          main="Female", 
          xlab="General game play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  boxplot(int_total ~ H1, data=data_male, 
          main="Male", 
          xlab="General game play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  title("Boxplots of game play frequency per total interactions", outer = TRUE )
  
  
  boxplot(int_total ~ H2, data=data_female, 
          main="Female", 
          xlab="FPS games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  boxplot(int_total ~ H2, data=data_male, 
          main="Male", 
          xlab="FPS games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  title("Boxplots of FPS games play frequency per total interactions", outer = TRUE )
  
  boxplot(int_total ~ H4, data=data_female, 
          main="Female", 
          xlab="Music games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  boxplot(int_total ~ H4, data=data_male, 
          main="Male", 
          xlab="Music games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  title("Boxplots of music games play frequency per total interactions", outer = TRUE )
  
  boxplot(int_total ~ H8, data=data_female, 
          main="Female", 
          xlab="Sport games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  boxplot(int_total ~ H8, data=data_male, 
          main="Male", 
          xlab="Sport games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  title("Boxplots of sport games play frequency per total interactions", outer = TRUE )
  
  boxplot(int_total ~ H9, data=data_female, 
          main="Female", 
          xlab="Mario games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  boxplot(int_total ~ H9, data=data_male, 
          main="Male", 
          xlab="Mario games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  title("Boxplots of Mario games play frequency per total interactions", outer = TRUE )
  
  boxplot(int_total ~ H11, data=data_female, 
          main="Female", 
          xlab="Mobile games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  boxplot(int_total ~ H11, data=data_male, 
          main="Male", 
          xlab="Mobile games play frequency (1 never, 5 daily)", 
          ylab="Total interactions")
  title("Boxplots of mobile games play frequency per total interactions", outer = TRUE )
  
  
}