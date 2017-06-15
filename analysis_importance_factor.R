## packages

# for factor analysis number of factors
library(nFactors)

## factor analysis: explain covariances or correlations between variables

factor <- function() {
  data <- allDataLA
  
  data <- data[!is.na(data$AGE),]
  data <- data[!is.na(data$SEX),]
  
  # value 92 detected for code=="JUNF" and class=="1BACH"
  # clean outsider value with most repeated value for class=="1BACH"
  data[data$AGE==92,]$AGE <- 16
  
  # delete variable
  data$J6 <- NULL
  
  # 27 numeric variables
  data <- subset(data, select=c("AGE",
                                "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11",
                                "PREQSCORE", "POSTQSCORE","score", 
                                "maxScoreCP", "maxScoreU", "maxScoreCH",
                                "firstScoreCP", "firstScoreU", "firstScoreCH",
                                "timesCP", "timesU", "timesCH",
                                "int_patient", "int_phone", "int_saed"))
  # -> 183 observations
  
  # determine number of factors
  ev <- eigen(cor(data)) # get eigenvalues
  ap <- parallel(subject=nrow(data),var=ncol(data),
                 rep=100,cent=.05)
  nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  plotnScree(nS) 
  
  # results with 6 factors
  fit <- factanal(data, 6, rotation="varimax")
  print(fit, digits=2, cutoff=.3, sort=TRUE)
  # plot factor 1 by factor 2
  load <- fit$loadings[,1:2] 
  plot(load,type="n") # set up plot 
  text(load,labels=names(data),cex=.7) # add variable names
  
  # results with 10 factors
  fit <- factanal(data, 10, rotation="varimax")
  print(fit, digits=2, cutoff=.3, sort=TRUE)
  # plot factor 1 by factor 2
  load <- fit$loadings[,1:2] 
  plot(load,type="n") # set up plot 
  text(load,labels=names(data),cex=.7) # add variable names
  
  # Ref: http://www.statmethods.net/advstats/factor.html
}
