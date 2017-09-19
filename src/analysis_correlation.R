library("sjPlot", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")  # sjt.corr

data <- allDataClean

## subsets of data
interval <- c("AGE", "score", 
             "maxScoreCP", "maxScoreU", "maxScoreCH", 
             "firstScoreCP", "firstScoreU", "firstScoreCH", 
             "timesCP", "timesU", "timesCH",
             "int_patient", "int_phone", "int_saed")
categorical <- c("class", "SEX", 
                 "PREQ1", "PREQ2", "PREQ3", "PREQ4", "PREQ5", "PREQ6", "PREQ7", "PREQ8", "PREQ9", "PREQ10", "PREQ11", "PREQ12", "PREQ13", "PREQ14", "PREQ15",
                 "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11",
                 "POSTQ1", "POSTQ2", "POSTQ3", "POSTQ4", "POSTQ5", "POSTQ6", "POSTQ7", "POSTQ8", "POSTQ9", "POSTQ10", "POSTQ11", "POSTQ12", "POSTQ13", "POSTQ14", "POSTQ15",
                 "J1", "J2", "J3", "J4", "J5", "gameCompleted", "mostRepeatedSituation",
                 "failedEmergency","failedThrusts","failedHName","failedHPosition","failedHHands")

identifier <- c("CODE")
comments <- c("J6")

## correlation  between interval variables
cor(data[interval])

## correlation between categorical variables
# data[categorical]
data[] <- lapply(data, as.integer)
sjt.corr(data[categorical])

## correlation  between interval and categorical variables
data[] <- lapply(data, as.integer)
sjt.corr(data)
