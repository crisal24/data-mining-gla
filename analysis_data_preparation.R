source("/home/cristina/R/code/read_questionnaires.R")
source("/home/cristina/R/code/read_xapi_traces.R")

# read data from questionnaires
data_q <- read_questionnaires()

# read data from xAPI traces
data_t <- read_xapi_traces()

# merge data by code
data <- merge(data_q, data_t)

write.table(data, "/home/cristina/R/tdata/alldata.csv", sep=",")

###################################

clean_data <- function(data) {
  # filtering data
  cd <- data[!is.na(data$AGE),]
  cd <- cd[!is.na(cd$SEX),]
  
  # value 92 detected for code=="JUNF" and class=="1BACH"
  # clean outsider value with most repeated value for class=="1BACH"
  cd[cd$AGE==92,]$AGE <- 16
  
  return(cd)
}

# first two session: evaluacion formativa
dataEF <- read.csv(file="/home/cristina/R/tdata/dataEvaluacionFormativa.csv", header=TRUE, sep=",")
dataEF$SEX <- dataEF$SEXO
dataEF$AGE <- dataEF$EDAD
dataEF$SEXO <- NULL
dataEF$EDAD <- NULL
dataEF <- subset(dataEF, select=c("class", "SEX", "AGE", "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11"))

# data from all sessions
allData <- read.csv(file="/home/cristina/R/tdata/alldata.csv", header=TRUE, sep=",")
allData[allData==""] <- NA
allDataGH <- subset(allData, select=c("class", "SEX", "AGE", "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11"))

# total data
DATAALLSESSIONS <- rbind(allDataGH, dataEF)

# all data cleaned
allDataClean <- clean_data(allData)

## data from TV

# read data from questionnaires
data_qTV <- questionnairesTV()

# read data from xAPI traces
data_tTV <- traces_tv()

# merge data by code
dataTV <- merge(data_qTV, data_tTV)

# write.table(dataTV, "C:/Users/Cristina/Dropbox/TFM/code/data/alldataTV.csv", sep=",")
