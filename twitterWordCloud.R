
################# Twitter Analysis ###############################################
##################################################################################
# By_           Pasquinell Urbani
# Mail_         purbanir@gmail.com
# Date_         2017/05/30
# Function_     This code generates mutiples plots by taking twitter publications
###################################################################################
###################################################################################

# Run only if it's the first time
#install.packages(c("httr","devtools","twitteR","tm","wordcloud","plyr","dplyr","lubdidate","plotly","ggplot2","xts","padr","RColorBrewer"))

# Load all the packages and log in to twitter developers account


library(httr)
library(devtools)
library(twitteR)
library(tm)
library(wordcloud)
library(plyr)
library(dplyr)
detach("package:plyr", unload=TRUE) 
library(lubridate)
library(plotly)
library(ggplot2)
library(xts)
library(padr) # Guide to padr https://www.r-bloggers.com/introducing-padr/


# Set API Keys
# How to create an account https://dev.twitter.com/index
api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)



# Example obtained from https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/wordcloud1
#step 1
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
#mach_tweets = searchTwitter("donald trump", n=500, lang="en")


# step 2
# Find tweets

# Choose one of the folowing
# "DelegacionAero","DAMFMN","daetsam","Delegacion_ICCP",
# "deleetsii_upm","daetsime","DelegacionETSIN","dat_etsit",
# "DatopoUpm","DAETSEM","dalumetsisi","DAETSIAAB_UPM","da_etsidi",
# "DelegacionETSIC","dalum_etsist","daetsiinf","DAInefUpm",
# "delegacioncsdmm"


##### Choose delegation
delegation = "DatopoUpm"

#### Analysis whithout Retweets
mach_tweets = userTimeline(delegation, 3500)

# step 3
mach_text = sapply(mach_tweets, function(x) x$getText())
text <- sapply(mach_text,function(row) iconv(row, "latin1", "ASCII", sub=""))


# step 4
# create a corpus
mach_corpus = Corpus(VectorSource(text))

# create document term matrix applying some transformations
tdm = TermDocumentMatrix(mach_corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = c("upm", "delegacion","alumnos",
                                                      "web","https","http",
                                                      "via","web","delegacin","curso",
                                                      "puedes","hoy",
                                                      "etsiae", "quieres",
                                                      "abrimos", delegation, "eui",
                                                      "etsiaab", "etsidi", "daetsidi",
                                                      "etsic",  "dalum",  "euitt","etsist",
                                                      "javirevillas", "tryit", "inef",
                                                      "dainefupm", "anecafyde",  "csdmm",
                                                      "dalumetsisi", "aeroespacialupm",
                                                      "montes","ingdemontes","damfmn",
                                                      "mnatural", "etsam", "iccp",
                                                      "industrialesupm", "caminosupm",
                                                      "deleetsii","etsii", "minasenergiaupm",
                                                      "etsiminas","infominasmadrid", "daminasmadrid",
                                                      "etsin","dacson","ingnaval","delegacionetsin","naval",
                                                      "telecoupm","etsit","dat", "geomtica",
                                                      "maana",stopwords("spanish")),
                                        removeNumbers = TRUE, tolower = TRUE))


# step 5
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)


# step 6
# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
#png("MachineLearningCloud.png", width=12, height=8, units="in", res=300)
#wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
#dev.off()





#### Analysis with Retweets

mach_tweets = userTimeline(delegation, 3500,includeRts=TRUE)


# step 3
mach_text = sapply(mach_tweets, function(x) x$getText())

text <- sapply(mach_text,function(row) iconv(row, "latin1", "ASCII", sub=""))


# step 4
# create a corpus
mach_corpus = Corpus(VectorSource(text))

# create document term matrix applying some transformations
tdm = TermDocumentMatrix(mach_corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = c("upm", "delegacion","alumnos",
                                                      "web","https","http",
                                                      "via","web","delegacin","curso",
                                                      "puedes","hoy",
                                                      "etsiae", "quieres",
                                                      "abrimos", delegation, "eui",
                                                      "etsiaab", "etsidi", "daetsidi",
                                                      "etsic", "dalum", "euitt","etsist",
                                                      "javirevillas", "tryit", "inef",
                                                      "dainefupm", "anecafyde", "csdmm",
                                                      "dalumetsisi", "aeroespacialupm",
                                                      "montes","ingdemontes","damfmn",
                                                      "mnatural", "etsam","iccp",
                                                      "industrialesupm", "caminosupm",
                                                      "deleetsii","etsii", "minasenergiaupm",
                                                      "etsiminas","infominasmadrid", "daminasmadrid",
                                                      "etsin","dacson","ingnaval","delegacionetsin","naval",
                                                      "telecoupm","etsit","dat",  "geomtica",
                                                      "maana",stopwords("spanish")),
                                        removeNumbers = TRUE, tolower = TRUE))


# step 5
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)


# step 6
# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
#png("MachineLearningCloud.png", width=12, height=8, units="in", res=300)
#wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
#dev.off()




