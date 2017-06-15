## packages

# for single value decomposition
library(svd)

svd <- function() {
  data <- allDataLA
  
  data <- data[!is.na(data$AGE),]
  data <- data[!is.na(data$SEX),]
  
  # value 92 detected for code=="JUNF" and class=="1BACH"
  # clean outsider value with most repeated value for class=="1BACH"
  data[data$AGE==92,]$AGE <- 16
  
  # delete variable
  data$J6 <- NULL
  
  # 26 integer variables
  data <- subset(data, select=c("AGE",
                                "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11",
                                "PREQSCORE", "POSTQSCORE",
                                "maxScoreCP", "maxScoreU", "maxScoreCH",
                                "firstScoreCP", "firstScoreU", "firstScoreCH",
                                "timesCP", "timesU", "timesCH",
                                "int_patient", "int_phone", "int_saed"))
  
  s <- svd(data)
  A <- diag(s$d) # diagonal of singular values
  # s$u a matrix whose columns contain the left singular vectors of x, present if nu > 0
  # s$v a matrix whose columns contain the right singular vectors of x, present if nv > 0s
  s$u %*% A %*% t(s$v) #  X = U A V'
  
  # nos quedamos con las dos primeras componentes
  s$u[,1:2]
  A[1:2,1:2]
  t(s$v)[1:2,]
}

# # svd: singular value decomposition
# 
# analysis_svd <- function () {
#   
#   data <- allDataLAC
#   
#   # delete variable
#   data$J6 <- NULL
#   
#   # 27 numeric variables
#   data <- subset(data, select=c("AGE",
#                                 "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11",
#                                 "PREQSCORE", "POSTQSCORE","score", 
#                                 "maxScoreCP", "maxScoreU", "maxScoreCH",
#                                 "firstScoreCP", "firstScoreU", "firstScoreCH",
#                                 "timesCP", "timesU", "timesCH",
#                                 "int_patient", "int_phone", "int_saed"))
#   
#   svd <- svd(data) # matrix decomposition
#   svd$d  # diagonal matrix
#   svd$u  # u matrix
#   svd$v  # v matrix
#   
#   # con la primera ya recogemos el 91.96% (618.706888^2 / (618.706888^2 + ..27términos...+ 7.893765^2) = 0.9196)
#   
# }
# 
# # als: alternating least squares
# 
# analysis_als <- function() {
#   
# }