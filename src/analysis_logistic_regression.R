## packages 

library("caret") # stratified cv partition
library("pROC") # ROC curve

data <- allDataLA

predict_score <- function() {
  
  # delete columns with post answers, code and J6 variable
  # leave binary PASS variable to be predicted as first column
  # exclude observations with NA values
  PASS <- data$PASS
  selected_data <- cbind(PASS, data[,2:27], data[,43:50], data[,52:72])
  selected_data <- na.exclude(selected_data)
  
  # model 1 all variables
  model1 = glm(PASS ~ ., data = selected_data)
  summary(model1)
  
  # model 2 only with variables of greater significance in previous coefficients
  # ***, ** Signif. codes:  0 '***' 0.001 '**'
  model2 = glm(PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation, 
              data = selected_data)
  summary(model2)
  
  # model 3 original important variables
  # ***, ** Signif. codes:  0 '***' 0.001 '**' 0.01 '*'
  model3 = glm(PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation + PREQ9, 
              data = selected_data)
  summary(model3)
  
  # model 4 interactions class-class
  model4 = glm(PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation + 
                 PREQ12*PREQ15 + PREQ12*mostRepeatedSituation + PREQ15*mostRepeatedSituation, 
              data = selected_data)
  summary(model4)
  
  # model 5 more interactions class-class
  model5 = glm(PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation + PREQ9 +
                 PREQ12*PREQ15 + PREQ12*mostRepeatedSituation + PREQ15*mostRepeatedSituation +
                 PREQ9*PREQ12 + PREQ9*PREQ15 + PREQ9*mostRepeatedSituation, 
               data = selected_data)
  summary(model5)
  
  # model variable selection: forward
  step(glm(PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation + PREQ9 +
             PREQ12*PREQ15 + PREQ12*mostRepeatedSituation + PREQ15*mostRepeatedSituation +
             PREQ9*PREQ12 + PREQ9*PREQ15 + PREQ9*mostRepeatedSituation, 
          data = selected_data), direction="forward")
  
  # model variable selection: backward
  step(glm(PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation + PREQ9 +
             PREQ12*PREQ15 + PREQ12*mostRepeatedSituation + PREQ15*mostRepeatedSituation +
             PREQ9*PREQ12 + PREQ9*PREQ15 + PREQ9*mostRepeatedSituation, 
           data = selected_data), direction="backward")
  
  # model variable selection: step by step
  step(glm(PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation + PREQ9 +
             PREQ12*PREQ15 + PREQ12*mostRepeatedSituation + PREQ15*mostRepeatedSituation +
             PREQ9*PREQ12 + PREQ9*PREQ15 + PREQ9*mostRepeatedSituation, 
           data = selected_data), direction="both")
  
  ## validacion cruzada repetida para comparar modelos##
  
  vcr1<-validacionCruzadaRepetida(selected_data,
                                 1, # columna variable objetivo
                                 10, # numero de grupos de CV
                                 c("PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation",
                                   "PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation + PREQ9",
                                   "PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation + PREQ9 + PREQ15*mostRepeatedSituation",
                                   "PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation + PREQ9 + PREQ12*PREQ15 + PREQ12*mostRepeatedSituation + PREQ15*mostRepeatedSituation +PREQ9*PREQ12 + PREQ9*PREQ15 + PREQ9*mostRepeatedSituation"),
                                 100, # numero de repeticiones
                                 12345) # semilla aleatoria
  
  # comparamos 4 estadisticos de los modelos
  par(mfrow=c(1,4))
  boxplot(MC~modelo, data=vcr1, main="Tasa de mala clasificación")
  boxplot(AUC~modelo, data=vcr1, main="Área bajo la curva ROC")
  boxplot(AIC~modelo, data=vcr1, main="AIC")
  boxplot(SBC~modelo, data=vcr1, main="SBC")
  
  par(mfrow=c(1,4))
  boxplot(MC~modelo, data=vcr1, main="Misclassification rate")
  boxplot(AUC~modelo, data=vcr1, main="Area under ROC curve")
  boxplot(PREC~modelo, data=vcr1, main="Precision")
  boxplot(REC~modelo, data=vcr1, main="Recall")
  
  v1 <- as.data.frame(vcr1)
  aggregate(v1[,1:6], list(v1$modelo), mean)
  aggregate(v1[,1:6], list(v1$modelo), sd)
  
  # *******************************
  # deal with imbalanced classes
  # *******************************
  
  unbalanced_data <- selected_data
  passed <- unbalanced_data[unbalanced_data$PASS==T,]
  failed <- unbalanced_data[unbalanced_data$PASS==F,]
  
  nf <- nrow(failed) # take this as 40% of new set
  N = round(nf*100/40)
  np <- N - nf # 60% of pass class
  passed_undersample <- passed[sample(nrow(passed), np), ]
  balanced_data = rbind(passed_undersample, failed)
  selected_data = balanced_data
  
  # model 1 all variables
  model1 = glm(PASS ~ class + PREQ1 + PREQ2 + PREQ3 + PREQ4 + PREQ5 + PREQ6 + PREQ7 + PREQ8 + PREQ9 + PREQ10 + PREQ11 + PREQ12 + PREQ13 + PREQ14 # + PREQ15
               + H1 + H2 + H3 + H4 + H5 + H6 + H7 + H8 + H9 + H10 
               + J1 + J2 # + J3 + J4 + J5
               # + gameCompleted + SEX + AGE + H11 
               # + score + maxScoreCP + maxScoreU + maxScoreCH + firstScoreCP + firstScoreU + firstScoreCH
               # + timesCP + timesU + timesCH + mostRepeatedSituation 
               # + int_patient + int_phone + int_saed 
               # + failedEmergency + failedThrusts + failedHName + failedHPosition + failedHHands 
               + PREQSCORE
               , data = selected_data)
  summary(model1) 
  
  # model 2 only with variables of greater significance in previous coefficients
  # ***, ** Signif. codes:  0 '***' 0.001 '**'
  model2 = glm(PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation, 
               data = selected_data)
  summary(model2)
  
  # model 3 original important variables
  # ***, ** Signif. codes:  0 '***' 0.001 '**' 0.01 '*'
  model3 = glm(PASS ~ PREQ12 + PREQ15, 
               data = selected_data)
  summary(model3)
  
  # model 4 variables
  model4 = glm(PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation +  H1 + J1, 
               data = selected_data)
  summary(model4)
  
  
  # model 4 interactions class-class
  model4 = glm(PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation + 
                 PREQ12*PREQ15 + PREQ12*mostRepeatedSituation + PREQ15*mostRepeatedSituation, 
               data = selected_data)
  summary(model4)
  
  # model 5 more interactions class-class
  model5 = glm(PASS ~ PREQ12 + PREQ15 + PREQ12*PREQ15, 
               data = selected_data)
  summary(model5)
  
  # model variable selection: backward
  step(glm(PASS ~ class + PREQ1 + PREQ2 + PREQ3 + PREQ4 + PREQ5 + PREQ6 + PREQ7 + PREQ8 + PREQ9 + PREQ10 + PREQ11 + PREQ12 + PREQ13 + PREQ14
           + H1 + H2 + H3 + H4 + H5 + H6 + H7 + H8 + H9 + H10 
           + J1 + J2 + PREQSCORE,
           data = selected_data), direction="backward")
  
  # model 6 
  model7 = glm(PASS ~ PREQ1 + PREQ2 + PREQ3 + PREQ4 + PREQ5 + PREQ6 + PREQ7 + PREQ8 + PREQ9 + PREQ10 + PREQ11 + PREQ12 + PREQ13 + PREQ14 + PREQ15,
               data = selected_data)
  summary(model7)
  
  ## validacion cruzada repetida para comparar modelos##
  
  vcr2<-validacionCruzadaRepetida(selected_data,
                                 1, # columna variable objetivo
                                 10, # numero de grupos de CV
                                 c("PASS ~ class + PREQ1 + PREQ2 + PREQ3 + PREQ4 + PREQ5 + PREQ6 + PREQ7 + PREQ8 + PREQ9 + PREQ10 + PREQ11 + PREQ12 + PREQ13 + PREQ14
                                    + H1 + H2 + H3 + H4 + H5 + H6 + H7 + H8 + H9 + H10 
                                    + J1 + J2 + PREQSCORE",
                                   "PASS  ~ class + PREQ1 + PREQ2 + PREQ3 + PREQ4 + PREQ5 + PREQ6 + PREQ7 + PREQ8 + PREQ9 + PREQ10 + PREQ11 + PREQ12 + PREQ13 + PREQ14
                                    + H1 + H2 + H3 + H4 + H5 + H6 + H7 + H8 + H9 + H10 
                                    + J1 + J2 + J3 + J4 + J5 + PREQSCORE",
                                   "PASS ~ PREQ12 + PREQ15"),
                                 100, # numero de repeticiones
                                 12345) # semilla aleatoria
  
  # comparamos 4 estadisticos de los modelos
  par(mfrow=c(1,4))
  boxplot(MC~modelo, data=vcr2, main="Tasa de mala clasificación")
  boxplot(AUC~modelo, data=vcr2, main="Área bajo la curva ROC")
  boxplot(AIC~modelo, data=vcr2, main="AIC")
  boxplot(SBC~modelo, data=vcr2, main="SBC")
  
  par(mfrow=c(1,4))
  boxplot(MC~modelo, data=vcr2, main="Misclassification rate")
  boxplot(AUC~modelo, data=vcr2, main="Area under ROC curve")
  boxplot(PREC~modelo, data=vcr2, main="Precision")
  boxplot(REC~modelo, data=vcr2, main="Recall")
  
  v2 <- as.data.frame(vcr2)
  aggregate(v2[,1:6], list(v2$modelo), mean)
  aggregate(v2[,1:6], list(v2$modelo), sd)
  
}

predict_score_gameonly <- function() {
  
  # delete columns with post answers, code and J6 variable
  # leave binary PASS variable to be predicted as first column
  # exclude observations with NA values
  PASS <- data$PASS
  selected_data <- cbind(PASS, data[,52:71])
  selected_data <- na.exclude(selected_data)
  
  # model 1 all variables
  model1 = glm(PASS ~ ., data = selected_data)
  summary(model1)
  
  # model 2 only with variables of greater significance in previous coefficients
  # ***, ** Signif. codes:  0 '***' 0.001 '**'
  model2 = glm(PASS ~ mostRepeatedSituation + int_patient + maxScoreCP, 
               data = selected_data)
  summary(model2)
  
  # model 3 original important variables
  # ***, ** Signif. codes:  0 '***' 0.001 '**' 0.01 '*'
  model3 = glm(PASS ~ int_patient + maxScoreCP, 
               data = selected_data)
  summary(model3)
  
  # model 4 interactions class-class
  model4 = glm(PASS ~ mostRepeatedSituation + int_patient + maxScoreCP + 
                 mostRepeatedSituation*failedHPosition + mostRepeatedSituation*failedThrusts, 
               data = selected_data)
  summary(model4)
  
  # model 5 class-interval
  model5 = glm(PASS ~ mostRepeatedSituation + int_patient + maxScoreCP + 
                 mostRepeatedSituation*int_patient + mostRepeatedSituation*maxScoreCP,
               data = selected_data)
  summary(model5)
  
  # model 6 interval-interval
  model6 = glm(PASS ~ mostRepeatedSituation + int_patient + maxScoreCP + 
                 int_patient*maxScoreCP,
               data = selected_data)
  summary(model6)
  
  # model variable selection: forward
  step(glm(PASS ~ mostRepeatedSituation + int_patient + maxScoreCP, 
           data = selected_data), direction="forward")
  
  # model variable selection: backward
  step(glm(PASS ~ mostRepeatedSituation + int_patient + maxScoreCP, 
           data = selected_data), direction="backward")
  
  # model variable selection: step by step
  step(glm(PASS ~ mostRepeatedSituation + int_patient + maxScoreCP,
           data = selected_data), direction="both")
  
  ## validacion cruzada repetida para comparar modelos##
  
  vcr3<-validacionCruzadaRepetida(selected_data,
                                 1, # columna variable objetivo
                                 10, # numero de grupos de CV
                                 c("PASS ~ mostRepeatedSituation + int_patient + maxScoreCP",
                                   "PASS ~ int_patient + maxScoreCP",
                                   "PASS ~ mostRepeatedSituation + int_patient + maxScoreCP + mostRepeatedSituation*int_patient + mostRepeatedSituation*maxScoreCP",
                                   "PASS ~ mostRepeatedSituation + int_patient + maxScoreCP + int_patient*maxScoreCP"),
                                 100, # numero de repeticiones
                                 12345) # semilla aleatoria
  
  # comparamos 4 estadisticos de los modelos
  par(mfrow=c(1,4))
  boxplot(MC~modelo, data=vcr3, main="Tasa de mala clasificación")
  boxplot(AUC~modelo, data=vcr3, main="Área bajo la curva ROC")
  boxplot(AIC~modelo, data=vcr3, main="AIC")
  boxplot(SBC~modelo, data=vcr3, main="SBC")
  
  par(mfrow=c(1,4))
  boxplot(MC~modelo, data=vcr3, main="Misclassification rate")
  boxplot(AUC~modelo, data=vcr3, main="Area under ROC curve")
  boxplot(PREC~modelo, data=vcr3, main="Precision")
  boxplot(REC~modelo, data=vcr3, main="Recall")
  
  v3 <- as.data.frame(vcr3)
  aggregate(v3[,1:6], list(v3$modelo), mean)
  aggregate(v3[,1:6], list(v3$modelo), sd)
  
  
  # *******************************
  # deal with imbalanced classes
  # *******************************
  
  unbalanced_data <- selected_data
  passed <- unbalanced_data[unbalanced_data$PASS==T,]
  failed <- unbalanced_data[unbalanced_data$PASS==F,]
  
  nf <- nrow(failed) # take this as 40% of new set
  N = round(nf*100/40)
  np <- N - nf # 60% of pass class
  passed_undersample <- passed[sample(nrow(passed), np), ]
  balanced_data = rbind(passed_undersample, failed)
  selected_data <- balanced_data
  
  ## validacion cruzada repetida para comparar modelos##
  
  vcr4<-validacionCruzadaRepetida(selected_data,
                                  1, # columna variable objetivo
                                  10, # numero de grupos de CV
                                  c("PASS ~ mostRepeatedSituation + int_patient + maxScoreCP",
                                    "PASS ~ int_patient + maxScoreCP",
                                    "PASS ~ mostRepeatedSituation + int_patient + maxScoreCP + mostRepeatedSituation*int_patient + mostRepeatedSituation*maxScoreCP",
                                    "PASS ~ mostRepeatedSituation + int_patient + maxScoreCP + int_patient*maxScoreCP"),
                                  100, # numero de repeticiones
                                  12345) # semilla aleatoria
  
  # comparamos 4 estadisticos de los modelos
  par(mfrow=c(1,4))
  boxplot(MC~modelo, data=vcr4, main="Tasa de mala clasificación")
  boxplot(AUC~modelo, data=vcr4, main="Área bajo la curva ROC")
  boxplot(AIC~modelo, data=vcr4, main="AIC")
  boxplot(SBC~modelo, data=vcr4, main="SBC")
  
  par(mfrow=c(1,4))
  boxplot(MC~modelo, data=vcr4, main="Misclassification rate")
  boxplot(AUC~modelo, data=vcr4, main="Area under ROC curve")
  boxplot(PREC~modelo, data=vcr4, main="Precision")
  boxplot(REC~modelo, data=vcr4, main="Recall")
  
  v4 <- as.data.frame(vcr4)
  aggregate(v4[,1:6], list(v4$modelo), mean)
  aggregate(v4[,1:6], list(v4$modelo), sd)
  
  
}

########################################################
## validacion cruzada #
# funciones modificadas sobre las proporcionadas en SEMMA #
# Aida Calviño #
#######################################################


############ llamadas a las funciones ################

## validacion cruzada repetida para comparar modelos##

vcr<-validacionCruzadaRepetida(selected_data,
                               1, # columna variable objetivo
                               10, # numero de grupos de CV
                               c("PASS ~ mostRepeatedSituation + int_patient + maxScoreCP",
                                 "PASS ~ int_patient + maxScoreCP",
                                 "PASS ~ mostRepeatedSituation + int_patient + maxScoreCP + mostRepeatedSituation*int_patient + mostRepeatedSituation*maxScoreCP",
                                 "PASS ~ mostRepeatedSituation + int_patient + maxScoreCP + int_patient*maxScoreCP"),
                               100, # numero de repeticiones
                               2403) # semilla aleatoria

# comparamos 4 estad?sticos de los modelos
par(mfrow=c(1,4))
boxplot(MC~modelo, data=vcr, main="Tasa de mala clasificación")
boxplot(AUC~modelo, data=vcr, main="Área bajo la curva ROC")
boxplot(AIC~modelo, data=vcr, main="AIC")
boxplot(SBC~modelo, data=vcr, main="SBC")

## cambiando puntos corte ##

vcpc<-validacionCruzadaPuntosCorte(selected_data,
                                   1, # columna variable objetivo
                                   10, # numero grupos vc
                                   c("PASS ~ mostRepeatedSituation + int_patient + maxScoreCP",
                                    "PASS ~ int_patient + maxScoreCP",
                                   "PASS ~ mostRepeatedSituation + int_patient + maxScoreCP + mostRepeatedSituation*int_patient + mostRepeatedSituation*maxScoreCP",
                                    "PASS ~ mostRepeatedSituation + int_patient + maxScoreCP + int_patient*maxScoreCP"),
                                   2403, # semilla aleatoria
                                   seq(0,1,0.01)) # ptos corte

# minimizar tasa de mal clasificados
vcpc[which.min(vcpc[,3]+vcpc[,5]),1]

# maximizar ?ndice de Youden
vcpc[which.max(vcpc[,2]/(vcpc[,2]+vcpc[,3])+vcpc[,4]/(vcpc[,4]+vcpc[,5])-1),1]

# obtenemos m?s informaci?n del modelo mejor
#modelo<-glm(PASS ~ PREQ12 + PREQ15 + mostRepeatedSituation, 
#            data=selected_data)
#summary(modelo)

# comprobar contraste de significatividad del modelo
#1-pchisq(modelo$null.deviance-modelo$deviance,modelo$df.null-modelo$df.residual)


############ definicion funciones ####################


validacionCruzada<-function(data,pos,Ngroup,formulas,seed){
  set.seed(seed)
  aux<-sample(1:Ngroup,nrow(data),replace=T) #valores aleatorios para particion
  aux2<-rep(0,length(formulas))
  aux3<-rep(0,length(formulas))
  auc<-rep(0,length(formulas))
  
  true_positives <- rep(0,length(formulas))
  false_positives <- rep(0,length(formulas))
  false_negatives <- rep(0,length(formulas))

  for (j in 1:Ngroup){
    train.index <- createDataPartition(data$PASS, p = .9, list = FALSE) # stratified partition
    datosTrain <- data[ train.index,] #Obtencion datos train
    datosTest  <- data[-train.index,] #Obtencion datos test
    numpar<-c()
    for (l in 1:length(formulas)){
      modeloAux<-glm(formulas[l],datosTrain,family=binomial)
      modeloAux$xlevels[["PREQ1"]] <- union(modeloAux$xlevels[["PREQ1"]], levels(datosTest$PREQ1))
      modeloAux$xlevels[["PREQ2"]] <- union(modeloAux$xlevels[["PREQ2"]], levels(datosTest$PREQ2))
      modeloAux$xlevels[["PREQ3"]] <- union(modeloAux$xlevels[["PREQ3"]], levels(datosTest$PREQ3))
      modeloAux$xlevels[["PREQ4"]] <- union(modeloAux$xlevels[["PREQ4"]], levels(datosTest$PREQ4))
      modeloAux$xlevels[["PREQ5"]] <- union(modeloAux$xlevels[["PREQ5"]], levels(datosTest$PREQ5))
      modeloAux$xlevels[["PREQ6"]] <- union(modeloAux$xlevels[["PREQ6"]], levels(datosTest$PREQ6))
      modeloAux$xlevels[["PREQ7"]] <- union(modeloAux$xlevels[["PREQ7"]], levels(datosTest$PREQ7))
      modeloAux$xlevels[["PREQ8"]] <- union(modeloAux$xlevels[["PREQ8"]], levels(datosTest$PREQ8))
      modeloAux$xlevels[["PREQ9"]] <- union(modeloAux$xlevels[["PREQ9"]], levels(datosTest$PREQ9))
      modeloAux$xlevels[["PREQ10"]] <- union(modeloAux$xlevels[["PREQ10"]], levels(datosTest$PREQ10))
      modeloAux$xlevels[["PREQ11"]] <- union(modeloAux$xlevels[["PREQ11"]], levels(datosTest$PREQ11))
      modeloAux$xlevels[["PREQ12"]] <- union(modeloAux$xlevels[["PREQ12"]], levels(datosTest$PREQ12))
      modeloAux$xlevels[["PREQ13"]] <- union(modeloAux$xlevels[["PREQ13"]], levels(datosTest$PREQ13))
      modeloAux$xlevels[["PREQ14"]] <- union(modeloAux$xlevels[["PREQ14"]], levels(datosTest$PREQ14))
      modeloAux$xlevels[["PREQ15"]] <- union(modeloAux$xlevels[["PREQ15"]], levels(datosTest$PREQ15))

      modeloAux$xlevels[["class"]] <- union(modeloAux$xlevels[["class"]], levels(datosTest$class))
      
      #Verosimilitud
      L<--2*sum(log(predict(modeloAux,datosTest,type = "response")^datosTest[,pos]
                    *(1-predict(modeloAux,datosTest,type = "response"))^(1-datosTest[,pos])))
      aux2[l]<-aux2[l]+L
      numpar<-c(numpar,length(coef(modeloAux)))
      
      #Mal clasificados
      aux3[l]<-aux3[l]+sum((predict(modeloAux,datosTest,type = "response")>0.5)
                           !=datosTest[,pos])
      
      ## sensitivities (recall) VP / (VP + FN)
      prediction <- predict(modeloAux,datosTest,type = "response")>0.5
      
      for (p in 1:length(prediction)) {
        if (prediction[p] && datosTest[,pos][p]) { # true positives
          true_positives[l] <- true_positives[l] + 1
        }
        if (prediction[p] && !datosTest[,pos][p]) { # false positives
          false_positives[l] <- false_positives[l] + 1
        }
        if (!prediction[p] && datosTest[,pos][p]) { # false negative
          false_negatives[l] <- false_negatives[l] + 1
        }
      }
      
      #Area bajo ROC
      auc[l]<-auc[l]+roc(datosTest[,pos], predict(modeloAux,datosTest,
                                                  type = "response"),direction="<")$auc
    }
  }
  
  for (l in 1:length(formulas)) {
    ## precision: VP / (VP + FP)
    prec[l]<-(true_positives[l] / (true_positives[l] + false_positives[l]))
    
    # sensitivity (recall) VP / (VP + FN)
    rec[l]<-(true_positives[l] / (true_positives[l] + false_negatives[l]))
  }
  
  cbind(MC=aux3/nrow(data),AUC=auc/Ngroup,AIC=aux2+2*numpar,
        SBC=aux2+log(nrow(data))*numpar,
        PREC=prec,REC=rec,
        modelo=1:length(formulas))
}

validacionCruzadaRepetida<-function(data,pos,Ngroup,formulas,Nrep,initialSeed){
  semillas<-1:Nrep+initialSeed #Semillas a utilizar
  results<-c()
  for (i in 1:Nrep){
    results<-rbind(results,validacionCruzada(data,pos,Ngroup,formulas,semillas[i]))
  }
  results
}

reducirCortesAmpliarInfo<-function(objRoc,nuevosCortes){
  aux<-c()
  sens<-c()
  esp<-c()
  for (i in 1:length(nuevosCortes)){
    aux<-c(aux,objRoc$thresholds[which.min(objRoc$thresholds<nuevosCortes[i])-1])
    sens<-c(sens,objRoc$sensitivities[which.min(objRoc$thresholds<nuevosCortes[i])-1])
    esp<-c(esp,objRoc$specificities[which.min(objRoc$thresholds<nuevosCortes[i])-1])
  }
  neg<-length(objRoc$controls)
  pos<-length(objRoc$cases)
  VP<-sens*pos
  FN<-pos-VP
  VN<-esp*neg
  FP<-neg-VN
  cbind(VP=VP,FN=FN,VN=VN,FP=FP)
}

validacionCruzadaPuntosCorte<-function(data,pos,Ngroup,formula,seed,Ptoscorte){
  set.seed(seed)
  aux<-sample(1:Ngroup,nrow(data),replace=T)
  results<-matrix(rep(0,4*length(Ptoscorte)),length(Ptoscorte))
  for (j in 1:Ngroup){
    train.index <- createDataPartition(data$PASS, p = .9, list = FALSE) # stratified partition
    datosTrain <- data[ train.index,] #Obtencion datos train
    datosTest  <- data[-train.index,] #Obtencion datos test
    numpar<-c()
    modeloAux<-glm(formula,datosTrain,family=binomial)
    
    modeloAux$xlevels[["PREQ1"]] <- union(modeloAux$xlevels[["PREQ1"]], levels(datosTest$PREQ1))
    modeloAux$xlevels[["PREQ2"]] <- union(modeloAux$xlevels[["PREQ2"]], levels(datosTest$PREQ2))
    modeloAux$xlevels[["PREQ3"]] <- union(modeloAux$xlevels[["PREQ3"]], levels(datosTest$PREQ3))
    modeloAux$xlevels[["PREQ4"]] <- union(modeloAux$xlevels[["PREQ4"]], levels(datosTest$PREQ4))
    modeloAux$xlevels[["PREQ5"]] <- union(modeloAux$xlevels[["PREQ5"]], levels(datosTest$PREQ5))
    modeloAux$xlevels[["PREQ6"]] <- union(modeloAux$xlevels[["PREQ6"]], levels(datosTest$PREQ6))
    modeloAux$xlevels[["PREQ7"]] <- union(modeloAux$xlevels[["PREQ7"]], levels(datosTest$PREQ7))
    modeloAux$xlevels[["PREQ8"]] <- union(modeloAux$xlevels[["PREQ8"]], levels(datosTest$PREQ8))
    modeloAux$xlevels[["PREQ9"]] <- union(modeloAux$xlevels[["PREQ9"]], levels(datosTest$PREQ9))
    modeloAux$xlevels[["PREQ10"]] <- union(modeloAux$xlevels[["PREQ10"]], levels(datosTest$PREQ10))
    modeloAux$xlevels[["PREQ11"]] <- union(modeloAux$xlevels[["PREQ11"]], levels(datosTest$PREQ11))
    modeloAux$xlevels[["PREQ12"]] <- union(modeloAux$xlevels[["PREQ12"]], levels(datosTest$PREQ12))
    modeloAux$xlevels[["PREQ13"]] <- union(modeloAux$xlevels[["PREQ13"]], levels(datosTest$PREQ13))
    modeloAux$xlevels[["PREQ14"]] <- union(modeloAux$xlevels[["PREQ14"]], levels(datosTest$PREQ14))
    modeloAux$xlevels[["PREQ15"]] <- union(modeloAux$xlevels[["PREQ15"]], levels(datosTest$PREQ15))
    results<-results+reducirCortesAmpliarInfo(roc(datosTest[,pos],
                                                  predict(modeloAux,datosTest,type = "response"), direction="<"),
                                              Ptoscorte)
  }
  cbind(cutoffs=Ptoscorte,results)
}
