#CLASS: PREDICT 455 Data Visualization

#Section: 55

#Name: Scott Morgan

#Assignment: Week 4. Individual Assignment 2: Visualizing Text 

#1. INITIAL SETUP - SETTING WORKSPACE AND INSTALL/LOAD PACKAGES----

  #1.1 Clear workspace and set working directory----
  rm(list=ls())
  
  #Set User name for Windows Machine
  windowsUser='smorgan'
  
  #Laptop
  #smm25

  #Set working directory
  setwd(paste("C:/Users/",windowsUser,"/Desktop/455/Week_4",sep=""))
  
  wdfilepathname<-getwd()
  
  fullfilename<-paste(wdfilepathname,"/twitter_word_frequency_company.pdf",sep="")
  
  #1.2 Install needed package----

  #Install packages if necessary
  install.packages(c('twitteR','ROAuth', 'plyr', 'dplyr', 'stringr','ggplot2','syuzhet','wordcloud','tm'))
  
  library(twitteR)
  library(ROAuth)
  library(plyr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(syuzhet)
  library(wordcloud)
  library(tm)

#2. DATA PREPARATION AND PROCESSING----

  #2.1 Twitter Log In Extraction----
  api_key<- "ANKWk2vZMzGhXNGVMPXNvsMjV"
  api_secret <- "DNVlon0EdmsLCdQrdaY3z6KkrLcdsTMvVXhNzf3TGwoDTQUjjf"
  access_token <- "543925317-j0txuAH9y9OjOIoK42Mu3vdp7StCp4asc4uQQjuw"
  access_token_secret <- "274gD6tVSQTJ5SLva7xQAFvi6FBnryExckaxcCNmWJZCG"
  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

  #Number of records to retrieve
  # d=3000
  
  #2.2 Fetch Data----
  # amazon.tweets = searchTwitter('@amazon',n=d)
  # eBay.tweets = searchTwitter('@eBay', n=d)
  # target.tweets = searchTwitter('@Target', n=d)
  # walmart.tweets = searchTwitter('@Walmart',n=d)
  # 
  # save(amazon.tweets, file=file.path(wdfilepathname, 'amazon.tweets.RData'), ascii=T)
  # save(eBay.tweets, file=file.path(wdfilepathname, 'eBay.tweets.RData'), ascii=T)
  # save(target.tweets, file=file.path(wdfilepathname, 'target.tweets.RData'), ascii=T)
  # save(walmart.tweets, file=file.path(wdfilepathname, 'walmart.tweets.RData'), ascii=T)

  # load(paste(wdfilepathname,'/amazon.tweets.RData',sep=''))
  # load(paste(wdfilepathname,'/eBay.tweets.RData',sep=''))
  # load(paste(wdfilepathname,'/target.tweets.RData',sep=''))
  # load(paste(wdfilepathname,'/walmart.tweets.RData',sep=''))

  #2.3 Convert to text from tweet objects----
  # amazon.text = laply(amazon.tweets, function(t) t$getText() )
  # eBay.text = laply(eBay.tweets, function(t) t$getText() )
  # target.text = laply(target.tweets, function(t) t$getText() )
  # walmart.text = laply(walmart.tweets, function(t) t$getText() )
  
   # save(amazon.text, file=file.path(wdfilepathname, 'amazon.tweets.txt'), ascii=T)
   # save(eBay.text, file=file.path(wdfilepathname, 'eBay.tweets.txt'), ascii=T)
   # save(target.text, file=file.path(wdfilepathname, 'target.tweets.txt'), ascii=T)
   # save(walmart.text, file=file.path(wdfilepathname, 'walmart.tweets.txt'), ascii=T)
   
   # Easiest to check work but please feel free to runsection 2.2
   load(paste(wdfilepathname,'amazon.tweets.txt',sep='/'))
   load(paste(wdfilepathname,'eBay.tweets.txt',sep='/'))
   load(paste(wdfilepathname,'target.tweets.txt',sep='/'))
   load(paste(wdfilepathname,'walmart.tweets.txt',sep='/'))
  
#3. SENTIMENT ANALYSIS ----
  
  #3.1. Positive / Negative Sentiment Word reference in text file----
   neg.words = scan("negative-words.txt", what="character", comment.char=";")
   pos.words = scan("positive-words.txt", what="character", comment.char=";")

  #3.2 Sentiment Scoring Function----
   score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
   {
     require(plyr)
     require(stringr)
     
     # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
     # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
     scores = laply(sentences, function(sentence, pos.words, neg.words) {
       
       # clean up sentences with R's regex-driven global substitute, gsub():
       sentence = gsub('[[:punct:]]', '', sentence)
       sentence = gsub('[[:cntrl:]]', '', sentence)
       sentence = gsub('\\d+', '', sentence)
       # and convert to lower case:
       # sentence = tolower(sentence)
       
       # Let's have error handling function when trying tolower
       tryTolower = function(x){
         # create missing value
         y = NA
         # tryCatch error
         try_error = tryCatch(tolower(x), error=function(e) e)
         # if not an error
         if (!inherits(try_error, "error"))
           y = tolower(x)
         # result
         return(y)
       }
       # use tryTolower with sapply
       sentence = sapply(sentence, tryTolower)
       # split sentence into words with str_split function from stringr package
       # split into words. str_split is in the stringr package
       word.list = str_split(sentence, '\\s+')
       # sometimes a list() is one level of hierarchy too much
       words = unlist(word.list)
       
       # compare our words to the dictionaries of positive & negative terms
       pos.matches = match(words, pos.words)
       neg.matches = match(words, neg.words)
       
       # match() returns the position of the matched term or NA
       # we just want a TRUE/FALSE:
       pos.matches = !is.na(pos.matches)
       neg.matches = !is.na(neg.matches)
       
       # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
       score = sum(pos.matches) - sum(neg.matches)
       
       return(score)
     }, pos.words, neg.words, .progress=.progress )
     
     scores.df = data.frame(score=scores, text=sentences)
     return(scores.df)
   }
   

  #3.3 Sentiment Scoring ----
  amazon.scores = score.sentiment(amazon.text, pos.words, neg.words, .progress='text')
  eBay.scores = score.sentiment(eBay.text, pos.words, neg.words, .progress='text')
  target.scores = score.sentiment(target.text, pos.words, neg.words, .progress='text')
  walmart.scores = score.sentiment(walmart.text, pos.words, neg.words, .progress='text')
    
  #Combine into table
  amazon.scores$Company = 'Amazon'
  amazon.scores$code = 'AMZ'
  eBay.scores$Company = 'eBay'
  eBay.scores$code = 'EBAY'
  target.scores$Company = 'Target'
  target.scores$code = 'TGT'
  walmart.scores$Company = 'Walmart'
  walmart.scores$code = 'WMT'

  all.scores = rbind(amazon.scores,
                      eBay.scores, 
                      target.scores,
                      walmart.scores)

   #3.4 Generate Histrogram and Save PDF -----
  
   # ggplot works on data.frames, always
   g.hist = ggplot(data=all.scores, mapping=aes(x=score, fill=Company) )
    
   # add a bar graph layer. Let it bin the data and compute frequencies
   # (set binwidth=1 since scores are integers)
   g.hist = g.hist + 
     geom_histogram(binwidth=1, colour="black")
    
   # make a separate plot for each airline
   g.hist = g.hist + 
     facet_grid(Company~.)
    
   # plain display, nice colors
   g.hist = g.hist +
     theme_bw() +
     scale_fill_brewer() 
   
   g.hist = g.hist + 
     ggtitle(label = "Online and Discount Store Twitter Post Sentiment",
             subtitle ="Distribution of Scores (Number of Scores = 3000)") + 
     labs(caption = "Source:Twitter") +
     xlab("Sentiment Score -5 to +5") +
     ylab("Frequency") + 
     guides(fill=guide_legend(title="Company"))
    
   print(g.hist)
   ggsave(file.path(wdfilepathname, 'twitter_score_histograms_company.pdf'), g.hist, width=6, height=5.5)
   
   # dev.off()

# 4. WORD CLOUD AND FREQUENCIES----
  #4.1 Create a corpus from the collection of text files ----
  amazon_text_corpus <- Corpus(VectorSource(amazon.text))
  eBay_text_corpus <- Corpus(VectorSource(eBay.text))
  target_text_corpus <- Corpus(VectorSource(target.text))
  walmart_text_corpus <- Corpus(VectorSource(walmart.text))

  #4.2 Data Cleaning on the text files----
   cleanCorpus <- function(corpus){
    toASCII <- content_transformer(function(x) iconv(x, from='UTF-8', to='ASCII', sub=''))
    toNull <- content_transformer(function(x, pattern) gsub(pattern, '', x))
    
    x <- corpus
    x <- tm_map(x, toASCII)
    x <- tm_map(x, removeWords, stopwords('english'))
    x <- tm_map(x, removePunctuation)
    x <- tm_map(x, removeNumbers)
    x <- tm_map(x, content_transformer(tolower))
    x <- tm_map(x, stripWhitespace)
    x <- tm_map(x, removeWords, c("also", "article", "Article", 
                                                  "download", "google", "figure",
                                                  "fig", "groups","Google", "however",
                                                  "high", "human", "levels",
                                                  "larger", "may", "number",
                                                  "shown", "study", "studies", "this",
                                                  "using", "two", "the", "Scholar",
                                                  "pubmedncbi", "PubMedNCBI",
                                                  "view", "View", "the", "biol",
                                                  "via", "image", "doi", "one", 
                                                  "analysis","https","bts","amp","just","like","another"))
    
    
    return(x)
    }

    Amazon_Clean<-cleanCorpus(amazon_text_corpus)
    eBay_Clean<-cleanCorpus(eBay_text_corpus)
    Target_Clean<-cleanCorpus(target_text_corpus)
    Walmart_Clean<-cleanCorpus(walmart_text_corpus)
    
  #4.4 Generate Wordclouds and Save PDF -----

  # paste('Online and Discount Store Most Frequently Tweeted Words and Phrases')
    
  # pdf(file =fullfilename,  width = 7.5, height = 7.5, paper = "letter", exhibitDir)

  #Save down wordclouds
   # a<- wordcloud(Amazon_Clean, min.freq = 5,
   #            max.words = 300, 
   #            random.order = FALSE,
   #            random.color = FALSE,
   #            rot.per = 0.0, # all horizontal text
   #            colors = brewer.pal(8,"Dark2"))
   # 
   # b<-wordcloud(eBay_Clean, min.freq = 5,
   #            max.words = 300, 
   #            random.order = FALSE,
   #            random.color = FALSE,
   #            rot.per = 0.0, # all horizontal text
   #            colors = brewer.pal(8,"Dark2"))
   #  
   # c<- wordcloud(Target_Clean, min.freq = 5,
   #            max.words = 300, 
   #            random.order = FALSE,
   #            random.color = FALSE,
   #            rot.per = 0.0, # all horizontal text
   #            colors = brewer.pal(8,"Dark2"))
   #  
   # d<- wordcloud(Walmart_Clean, min.freq = 5,
   #            max.words = 300, 
   #            random.order = FALSE,
   #            random.color = FALSE,
   #            rot.per = 0.0, # all horizontal text
   #            colors = brewer.pal(8,"Dark2"))
   # 
   # 
   # dev.off()
    
  #4.4 Generate Word Frequency Charts and Save PDF -----
   
   Amazon_Clean_dtm <- TermDocumentMatrix(Amazon_Clean)
   Amazon_Clean_dtm <- as.matrix(Amazon_Clean_dtm)
   Amazon_Clean_dtm <- sort(rowSums(Amazon_Clean_dtm),decreasing=TRUE)
   Amazon_Clean_dtm <- data.frame(word = names(Amazon_Clean_dtm),freq=Amazon_Clean_dtm)
   
   eBay_Clean_dtm <- TermDocumentMatrix(eBay_Clean)
   eBay_Clean_dtm <- as.matrix(eBay_Clean_dtm)
   eBay_Clean_dtm <- sort(rowSums(eBay_Clean_dtm),decreasing=TRUE)
   eBay_Clean_dtm <- data.frame(word = names(eBay_Clean_dtm),freq=eBay_Clean_dtm)
   
   Target_Clean_dtm <- TermDocumentMatrix(Target_Clean)
   Target_Clean_dtm <- as.matrix(Target_Clean_dtm)
   Target_Clean_dtm <- sort(rowSums(Target_Clean_dtm),decreasing=TRUE)
   Target_Clean_dtm <- data.frame(word = names(Target_Clean_dtm),freq=Target_Clean_dtm)
   
   Walmart_Clean_dtm <- TermDocumentMatrix(Walmart_Clean)
   Walmart_Clean_dtm <- as.matrix(Walmart_Clean_dtm)
   Walmart_Clean_dtm <- sort(rowSums(Walmart_Clean_dtm),decreasing=TRUE)
   Walmart_Clean_dtm <- data.frame(word = names(Walmart_Clean_dtm),freq=Walmart_Clean_dtm)

   # Create 2 x 2 export
   pdf(file =fullfilename,  width = 10, height = 10, paper = "letter", wdfilepathname)
   
   par(mfrow = c(2,2)) 
   # layout(matrix(c(1, 2), nrow=2), heights=c(1, 10)) 
   # par(mar=rep(0, 4))
    # plot.new() 
    # text(x=0.15, y=0.15, 'Twitter Retail Company Word Count') 
   
   barplot(Amazon_Clean_dtm[1:5,]$freq, names.arg = Amazon_Clean_dtm[1:5,]$word,
           col ='snow2', main ='Word Frequency Bar Charts by Company', ylab = 'Word Frequencies', ylim = c(0,5000), cex.names=0.65)
   
   barplot(eBay_Clean_dtm[1:5,]$freq, names.arg = eBay_Clean_dtm[1:5,]$word,
           col ='lightblue2', main ='eBay Top 5 Most Tweeted Words', ylab = 'Word Frequencies', ylim = c(0,5000), cex.names=0.65)
   
   barplot(Target_Clean_dtm[1:5,]$freq, names.arg = Target_Clean_dtm[1:5,]$word,
           col ='skyblue2', main ='Target Top 5 Most Tweeted Words', ylab = 'Word Frequencies', ylim = c(0,5000), cex.names=0.65)
   
   barplot(Walmart_Clean_dtm[1:5,]$freq, names.arg = Walmart_Clean_dtm[1:5,]$word,
           col ='dodgerblue4', main ='Walmart Top 5 Most Tweeted Words', ylab = 'Word Frequencies', ylim = c(0,5000), cex.names=0.65)
   
   dev.off()
   
   par(mfrow = c(1,1))

   #Clear workspace
    rm(list=ls())
  