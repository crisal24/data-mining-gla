## packages ##

library(dplyr) # for data cleaning
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

data <- allData

analysis_game_habits <- function(data) {
  # variable selection
  dataGH <- subset(data, select=c("class", "SEX", "AGE", "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11"))
  dataGH <- dataGH[!is.na(dataGH$H11),]
  dataGH$myname <- row.names(dataGH)
  
  # filling in the unknown values by exploring correlations [Ref: LuisTorgo p.56]
  # cor(dataGH[, 4:15], use="complete.obs")
  # symnum(cor(dataGH[, 4:14], use="complete.obs"))
  
  # clusters of variables
  library("ClustOfVar", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
  kmeansvar(X.quanti = dataGH[, 4:15], X.quali = dataGH[, 2:3], init=2)$var # 2 clusters
  kmeansvar(X.quanti = dataGH[, 4:15], X.quali = dataGH[, 2:3], init=3)$var # 3 clusters
  kmeansvar(X.quanti = dataGH[, 4:15], X.quali = dataGH[, 2:3], init=4)$var # 4 clusters
  
  # clusters according to game habits [Ref: https://www.r-bloggers.com/clustering-mixed-data-types-in-r/ ]
  
  dataGHC <- dataGH %>%
    mutate(myname = row.names(.)) %>%
    select(myname, class, SEX, AGE, H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11)
  
  dataGHC <- dataGH[, c(15, 1,2,3,4,5,6,7,8,9, 10, 11, 12, 13, 14)]
  
  gower_dist <- daisy(dataGHC[, -1], metric = "gower") # using gower distance
  sil_width <- c(NA)
  
  # Calculate silhouette width for many k using PAM
  for(i in 2:10){
    pam_fit <- pam(gower_dist,
                   diss = TRUE,
                   k = i)
    sil_width[i] <- pam_fit$silinfo$avg.width
  }
  
  # Plot sihouette width (higher is better)
  plot(1:10, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width")
  lines(1:10, sil_width)
  
  # plot: 2 clusters best
  # results
  pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
  
  pam_results <- dataGHC %>%
    dplyr::select(-myname) %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  
  pam_results$the_summary
  
  # in PAM, medoids provide information about clusters
  dataGHC[pam_fit$medoids, ]
  
  # plot clusters
  tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
  
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering),
           name = dataGHC$myname)
  
  # include gender and print circle=female, triangle=Male
  tsne_data_sex <- merge(dataGHC, tsne_data, by.x="myname", by.y="name")
  tsne_data_sex <- subset(tsne_data_sex, select=c("X", "Y", "cluster", "myname", "SEX"))
  
  ggplot(tsne_data_sex, aes(x = X, y = Y, shape = SEX)) + geom_point(aes(color = cluster), size=4) + ggtitle("Cluster of players based on game habits") +  theme(plot.title = element_text(lineheight=.18, face="bold")) + theme(legend.title = element_text(size=16, face="bold")) + theme(legend.text = element_text(size = 16, face = "bold"))
  
  ## summary per class
  library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
  dataGH %>% group_by(class) %>% summarize(mean(AGE), mean(H1), mean(H2), mean(H3), mean(H4), mean(H5), mean(H6), mean(H7), mean(H8), mean(H9), mean(H10), mean(H11))
  
}
