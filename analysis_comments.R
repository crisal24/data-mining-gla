library("wordcloud", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3") # word-cloud generator
library("RColorBrewer", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3") # color palettes
library("Hmisc")  # %nin% operator

comments <- allData$J6

most_used_words <- function() {
  # keep comments
  comments <- na.exclude(comments)
  strcomments <- paste(comments, collapse= '  ')
  
  # split by words
  words <- strsplit(tolower(strcomments), "[[:space:]]+")[[1]]
  
  # delete stopwords
  stopwords <- c("a", "como", "con", "cosas", "de", "el", "en", "es", "esta", "está", "este", "forma", 
                 "ha", "hay", "he", "la", "las", "lo", "mas", "más", "me", "muy", "no", "que", "para", 
                 "pero", "porque", "se", "si", "sido", "te", "un", "una", "y", "ya")
  '%nin%' <- Negate('%in%')
  words <- words[words %nin% stopwords]
  
  mucho <- list()
  for (i in 1:length(words)){
    if (tail(head(words,i),1)=="mucho") {
      mucho <- c(mucho,tail(head(words,i-1),1))
    }
  }
  
  aprendidomucho <- length(mucho[mucho %in% "aprendido"])
  gustadomucho <- length(mucho[mucho %in% "gustado"])
  
  words <- words[words %nin% "mucho"]
  
  for (i in 1:aprendidomucho) {
    words <- c(words, "aprendido mucho")  
  }
  
  for (i in 1:gustadomucho) {
    words <- c(words, "gustado mucho")
  }
  
  words <- sort(table(words), decreasing=T)
  # top ten used words
  head(words, 20)
  
  # word cloud
  df <- data.frame(word=names(words), freq=(words))
  set.seed(1234)
  wordcloud(words = df$word, freq=df$freq.Freq, min.freq = 1, max.words = 200, random.order = F, rot.per = 0.25,
            colors=brewer.pal(5, "Dark2"))
  
  ## Ref: https://www.r-bloggers.com/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know/ 
}