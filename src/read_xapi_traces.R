# read_xapi_traces.R

################# libraries #########################

# auxiliar library for "fromJSON" method
library("jsonlite", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

# auxiliar library for "smartbind" method
library("gtools", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

############# auxiliar functions #####################

# auxiliar function to find max of vector
# NA is returned only if all values are NA
myMax <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), 0)

# auxiliar function to find the first score of the traces passed
# NA is returned only if all scores are NA
firstScore <- function(x) {
  score <- x[which(!is.na(x$score))[1],]$score
  return(ifelse(is.na(score), 0, score))
}

################## functions ##########################

analysis_session <- function(traces) {
  # Reads xaAPI traces from a unique student and obtains the variables to be analyzed
  #
  # Args:
  #   traces: xAPI traces in JSON format
  #
  # Returns:
  # List with analyzed information from the xAPI traces
  
  ## accesed
  
  ## completed
  
  if (nrow(subset(traces, event=="completed" & target=="JuegoCompleto")) == 0)
    completedGame <- F
  else
    completedGame <- T
  
  score <- myMax(subset(traces, event=="completed" & target=="JuegoCompleto")$score) * 10
  
  maxScoreDolorToracico <- myMax(subset(traces, event=="completed" & target=="DolorToracico")$score) * 10
  maxScoreInconsciente <- myMax(subset(traces, event=="completed" & target=="Inconsciente")$score) * 10
  maxScoreAtragantamiento <- myMax(subset(traces, event=="completed" & target=="Atragantamiento")$score) * 10
  
  firstScoreDolorToracico <- firstScore(subset(traces, event=="completed" & target=="DolorToracico")) * 10
  firstScoreInconsciente <- firstScore(subset(traces, event=="completed" & target=="Inconsciente")) * 10
  firstScoreAtragantamiento <- firstScore(subset(traces, event=="completed" & target=="Atragantamiento")) * 10
  
  vecesDolorToracico <- nrow(subset(traces, event=="completed" & target=="DolorToracico"))
  vecesInconsciente <- nrow(subset(traces, event=="completed" & target=="Inconsciente"))
  vecesAtragantamiento <- nrow(subset(traces, event=="completed" & target=="Atragantamiento"))
  
  if (vecesDolorToracico >= vecesInconsciente & vecesDolorToracico >= vecesAtragantamiento)
    situacionMasRepetida="DolorToracico"
  else if (vecesInconsciente >= vecesDolorToracico & vecesInconsciente >= vecesAtragantamiento)
    situacionMasRepetida="Inconsciente"
  else
    situacionMasRepetida="Atragantamiento"
  
  ## initialized
  
  ## interacted
  interactions_victima <- nrow(subset(traces, event=="interacted" & target=="Victima"))
  interactions_persona <- nrow(subset(traces, event=="interacted" & target=="Persona"))
  interactions_personaParada <- nrow(subset(traces, event=="interacted" & target=="PersonaParada"))
  interactions_personaSentada <- nrow(subset(traces, event=="interacted" & target=="PersonaSentada"))
  interactions_paciente <-interactions_victima  + interactions_persona + interactions_personaParada + interactions_personaSentada
  
  interactions_telefono <- nrow(subset(traces, event=="interacted" & target=="Telefono"))
  interactions_desfibriladorCerrado <- nrow(subset(traces, event=="interacted" & target=="DesfibriladorCerrado"))
  
  ## progressed
  
  ## selected
  
  # emergency number
  if (nrow(subset(traces, event=="selected" & target=="NumeroEmergencias" & !success)) == 0)
    failedEmergencyNumber = F
  else
    failedEmergencyNumber = T
  
  # number of compresiones
  if (nrow(subset(traces, event=="selected" & target=="INCCompresiones" & !success)) == 0)
    failedCompresiones = F
  else
    failedCompresiones = T
  
  # heimlich
  if (nrow(subset(traces, event=="selected" & target=="ATNombreManiobra" & !success)) == 0)
    failedHeimlichName = F
  else
    failedHeimlichName = T
  
  # hemilich: position
  if (nrow(subset(traces, event=="selected" & target=="ColocacionHeimlich" & !success)) == 0)
    failedHeimlichPosition = F
  else
    failedHeimlichPosition = T
  
  # heimlich: hands in chest
  if (nrow(subset(traces, event=="selected" & target=="ManosPechoHeimlich" & !success)) == 0)
    failedHeimlichManosPecho = F
  else
    failedHeimlichManosPecho = T
  
  ## skipped
  
  traces_data <- data.frame(unique(traces$name), completedGame, score, 
                            maxScoreDolorToracico, maxScoreInconsciente, maxScoreAtragantamiento,
                            firstScoreDolorToracico, firstScoreInconsciente, firstScoreAtragantamiento,
                            vecesDolorToracico, vecesInconsciente, vecesAtragantamiento, situacionMasRepetida,
                            interactions_paciente, interactions_telefono, interactions_desfibriladorCerrado,
                            failedEmergencyNumber, failedCompresiones,
                            failedHeimlichName, failedHeimlichPosition, failedHeimlichManosPecho)
  
  colnames(traces_data) <- c("CODE", "gameCompleted", "score", 
                             "maxScoreCP", "maxScoreU", "maxScoreCH",
                             "firstScoreCP", "firstScoreU", "firstScoreCH",
                             "timesCP", "timesU", "timesCH", "mostRepeatedSituation",
                             "int_patient", "int_phone", "int_saed",
                             "failedEmergency", "failedThrusts",
                             "failedHName", "failedHPosition", "failedHHands")
  
  return(traces_data)
  
}

analysis_traces <- function(traces, codes) {
  # Reads xaAPI traces from all students of given codes list
  #
  # Args:
  #   traces: xAPI traces in JSON format
  #   codes: list of valid codes
  #
  # Returns:
  # Data frame with analyzed information from the xAPI traces 
  # for all students with given codes
  
  # initialize data frame
  data <- NULL
  
  # for each unique code (i.e. learner)
  for (code in unique(traces$name)) {
    # analysis of the learner traces (4 letters codes)
    if (is.element(code, codes)) {
      data <- rbind(data, analysis_session(traces[traces$name==code,]))
    }
  }
  return(data)
}

read_traces <- function(route, N) {
  # Reads traces file
  #
  # Args:
  #   N: number of traces files
  #   route: route of .csv file to be read
  #
  # Returns:-
  # Data frame with read information for all classes
  # with the added column of className.
  
  traces<- NULL
  
  # open json files
  for (i in 1:N) {
    document <- fromJSON(txt=paste(route, 'traces', i, '.json', sep=''))
    # obtain traces
    traces <- smartbind(traces, document$hits$hits$`_source`)
  }
  
  return(traces)
}

#################### main ############################

read_xapi_traces <- function() {
  codigos <- read.csv(file='/home/cristina/R/data/codigos.csv', header=TRUE, sep=",")$Code
  traces <- read_traces('/home/cristina/R/data/', 5) # 5 traces files
  data <- analysis_traces(traces, codigos)
  write.table(data, "/home/cristina/R/tdata/dataxapi.csv", sep=",")
  
  return(data)
}

traces_tv <- function() {
  dataTV <- NULL
  
  document <- fromJSON(txt='C:/Users/Cristina/Dropbox/TFM/Trazas Experimentos FirstAidGame/dia 6 - tele/traces-tele.json')
  
  # obtain traces
  traces <- document$hits$hits$`_source`
  
  # for each unique code (i.e. learner)
  for (code in unique(traces$name)) {
    # analysis of the learner traces (4 letters codes)
    dataTV <- rbind(dataTV, analysis_session(traces[traces$name==code,]))
  }
  
  return(dataTV)
  
}
