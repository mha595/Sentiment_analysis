# twitter analysis..
#setwd("~/Desktop/Pak politics sentiment analysis/")
library(twitteR)
library(RCurl)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(syuzhet)
library(RSentiment)
library(tm)

setup_twitter_oauth(consumerKey, consumerSecret,accessToken, accessSecrat)
tweets_PTI <- searchTwitter("PTI", n = 1000, lang = "en")
tweets_PMLN <- searchTwitter("PMLN", n = 1000, lang = "en")

df_PTI <- ldply (tweets_PTI, function(t) t$toDataFrame())
df_PMLN <- ldply (tweets_PMLN, function(t) t$toDataFrame())



df_text_PTI <- df_PTI$text
df_text_PMLN <- df_PMLN$text

df_text_PTI <- sapply(df_text_PTI,function(row) iconv(row, "latin1", "ASCII", sub=""))
df_text_PMLN <- sapply(df_text_PMLN,function(row) iconv(row, "latin1", "ASCII", sub=""))


df_text_PTI <- gsub("http[^[:blank:]]+","",df_text_PTI)
df_text_PTI <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",df_text_PTI)

df_text_PMLN <- gsub("http[^[:blank:]]+","",df_text_PMLN)
df_text_PMLN <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",df_text_PMLN)

txt_PTI <- Corpus(VectorSource(df_text_PTI))
txt_PTI <- tm_map(txt_PTI, removePunctuation)
txt_PTI <- tm_map(txt_PTI,content_transformer(tolower))
txt_PTI <- tm_map(txt_PTI, removeWords, stopwords("english"))
txt_PTI <- tm_map(txt_PTI, stripWhitespace)

txt_PMLN <- Corpus(VectorSource(df_text_PMLN))
txt_PMLN <- tm_map(txt_PMLN, removePunctuation)
txt_PMLN <- tm_map(txt_PMLN,content_transformer(tolower))
txt_PMLN <- tm_map(txt_PMLN, removeWords, stopwords("english"))
txt_PMLN <- tm_map(txt_PMLN, stripWhitespace)

#pal <- brewer.pal(8,"Dark2")
#wordcloud(txt, min.freq = 5, colors = pal, width = 1000, height =1000)

sentiment_PTI <- get_nrc_sentiment(df_text_PTI)
sentiment_score_PTI <- data.frame(colSums(sentiment_PTI[,]))
names(sentiment_score_PTI) <- "Score"
sentiment_score_PTI <- cbind("sentiment", rownames(sentiment_score_PTI), sentiment_score_PTI)
names(sentiment_score_PTI) <- c("sentiment" , "Sent" ,"Score" )
sentiment_score_PTI <- sentiment_score_PMLN %>% mutate(Party = "PTI")

sentiment_PMLN <- get_nrc_sentiment(df_text_PMLN)
sentiment_score_PMLN <- data.frame(colSums(sentiment_PMLN[,]))
names(sentiment_score_PMLN) <- "Score"
sentiment_score_PMLN <- cbind("sentiment", rownames(sentiment_score_PMLN), sentiment_score_PMLN)
names(sentiment_score_PMLN) <- c("sentiment" , "Sent" ,"Score" )
sentiment_score_PMLN <- sentiment_score_PMLN %>% mutate(Party = "PMLN")

sentiment_score <- rbind(sentiment_score_PMLN,sentiment_score_PTI)

ggplot(data = sentiment_score, aes(x = Sent, y = Score)) + geom_bar(aes(fill = sentiment), stat = "identity")