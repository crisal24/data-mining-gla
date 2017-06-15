# read_questionnaires.R

################# libraries #########################

# auxiliar library for "smartbind" method
library("gtools", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

################## functions ##########################

read_questionnaire <- function(route, className) {
  # Reads pre or post questionnaire of a class from csv file.
  #
  # Args:
  #   route: complete route of .csv file to be read.
  #   className: string with name of class to be add to the data.
  #
  # Returns:
  # Data frame with read information from csv file
  # with the added column of className.
  
  # load data from csv file
  data <- read.csv(file=route, header=TRUE, sep=",")
  
  ## cleaning data ##
  
  # remove unnecessary columns
  data$id<-NULL
  data$lastpage<-NULL
  data$startlanguage<-NULL
  data$submitdate<-NULL
  
  # include class column
  data$class <- className
  
  # return data
  return(data)
}

read_data_from_class <- function(route, className, session) {
  # Reads questionnaires from a given class and session.
  #
  # Args:
  #   route: route of .csv file to be read.
  #   className: string with name of class to be add to the data.
  #   session: number of class session
  #
  # Returns:
  # Data frame with read information from both csv files
  # with the added column of className.
  
  # pre questionnaire data
  Pre <- read_questionnaire(paste(route, "1-pre-", tolower(className), "-", session, ".csv", sep=''), className)
  
  # post questionnaire data
  Post <- read_questionnaire(paste(route, "2-post-", tolower(className), "-", session, ".csv", sep=''), className)
  
  # merge data from pre and post questionnaires
  data <- merge(Pre, Post)
  
  return(data)
}

read_data <- function(route, classes) {
  # Reads pre and post questionnaire of a given list of classes.
  #
  # Args:
  #   route: route of .csv files to be read.
  #   classes: array of classes with name : number of sessions.
  #
  # Returns:
  # Data frame with read information for all classes
  # with the added column of className.
  
  # initialize data
  data <- NULL
  
  # for each class
  for (class in names(classes)) {
    # number of sessions for that class
    sessions <- classes[[class]]
    s <- 1
    while (s <= sessions) {
      
      if (is.null(data))
        data <- read_data_from_class(route, class, s)
      else
        data <- smartbind(data, read_data_from_class(route, class, s))
      
      s <- s+1
    }
  }
  
  return(data)
}

list_of_classes <- function() {
  # Auxiliar function to define the array of classes.
  #
  # Returns:
  # List with the classes 
  # and, for each class, the number of sessions completed
  
  l <- list()
  l[["1ESO"]] <- 4
  l[["2ESO"]] <- 4
  l[["3ESO"]] <- 4
  l[["4ESO"]] <- 4
  l[["1BACH"]] <- 1
  return(l)
}

########### evaluacion formativa ###################

evaluacionFormativa <- function() {
  
  # load data from evaluation
  data3ESO <- read_data_from_class("/home/cristina/R/data/", "3ESO", "prueba")
  dataBachillerato <- read_data_from_class("/home/cristina/R/data/", "Bachillerato", "prueba")
  
  # join data from different groups together
  dataEvaluacionFormativa <- rbind(data3ESO, dataBachillerato)
  
  # export data
  write.table(data, "/home/cristina/R/tdata/dataEvaluacionFormativa.csv", sep=",")
  
}


########### TV ###################

questionnairesTV <- function() {
  
  # load data from TVs
  dataTV <- read_data_from_class("C:/Users/Cristina/Dropbox/TFM/Trazas Experimentos FirstAidGame/dia 6 - tele/", "tele", "")
  
  # change columns name
  colnames(dataTV)[which(names(dataTV) == "token")] <- "CODE"
  colnames(dataTV)[which(names(dataTV) == "SEXO")] <- "SEX"
  colnames(dataTV)[which(names(dataTV) == "EDAD")] <- "AGE"
  
  return(dataTV)
  # export data
  # write.table(data, "/home/cristina/R/tdata/dataQuestionnairesTV.csv", sep=",")
  
}

#################### main ############################

read_questionnaires <- function() {
  # define list of classes
  mylist <- list_of_classes()
  
  # read data
  data <- read_data("/home/cristina/R/data/", mylist)
  
  # change columns name
  colnames(data)[which(names(data) == "token")] <- "CODE"
  colnames(data)[which(names(data) == "SEXO")] <- "SEX"
  colnames(data)[which(names(data) == "EDAD")] <- "AGE"
  
  # export data
  write.table(data, "/home/cristina/R/tdata/dataQuestionnaires.csv", sep=",")
  
  return(data)
}