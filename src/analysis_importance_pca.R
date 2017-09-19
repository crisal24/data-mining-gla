## packages

# for pca principal function
library("psych", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
# for pca representation
library("ggfortify", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

## principal component analysis: example variances

# http://www.statmethods.net/advstats/factor.html
# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/

pca <- function() {
  data <- allDataLA
  
  data <- data[!is.na(data$AGE),]
  data <- data[!is.na(data$SEX),]
  
  # value 92 detected for code=="JUNF" and class=="1BACH"
  # clean outsider value with most repeated value for class=="1BACH"
  data[data$AGE==92,]$AGE <- 16
  
  # delete variable
  data$J6 <- NULL
  
  # 27 numeric variables
  data <- subset(data, select=c("AGE", "class",
                                "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11",
                                "PREQSCORE", "POSTQSCORE","score", 
                                "maxScoreCP", "maxScoreU", "maxScoreCH",
                                "firstScoreCP", "firstScoreU", "firstScoreCH",
                                "timesCP", "timesU", "timesCH",
                                "int_patient", "int_phone", "int_saed"))
  
  
  # apply on correlation matrix as variables are measured in different units
  fit <- princomp(data, cor=TRUE)
  summary(fit)
  loadings(fit)
  
  # select 5 components
  fit <- principal(data, nfactors=5, rotate='none')
  fit
  
  # other option to calculate pca
  # for not same scale variable is to scale them 
  prin_comp <- prcomp(data, scale. = T)
  prin_comp$center # mean of variables
  prin_comp$scale # standard deviation
  
  prin_comp$rotation # The rotation measure provides the principal component loading. Each column of rotation matrix contains the principal component loading vector. This is the most important measure we should be interested in.
  biplot(prin_comp, scale = 0, main="Plot of two first components of PCA")
  
  # variables associations
  # groups of individuals
  # Ref;: https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
  # plot against class
  autoplot(prin_comp, data=data, 
           colour= 'class', 
           scale=0,
           loadings=T, 
           loadings.label=T, 
           loadings.label.size=3, 
           loadings.colour = 'orange',
           main="Plot of two first components of PCA grouped by class")
  
}
