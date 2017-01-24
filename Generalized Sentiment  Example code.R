# Connect the Libraries 
library(twitteR)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
require(twitteR)
require(RCurl)
library(methods)

##Set working directory

setwd("C:/Users/Jeff/Documents")


#setup OAuth - add before running
consumer_key <-''
consumer_secret <-''
access_token <- ''
access_secret <-''

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#the function for extracting and analyzing tweets
search <- function(searchterm)
{

####This code does the appending of new searches to old
list <- searchTwitter(searchterm, n=1500,lang= 'en')
df <- twListToDF(list)
df <- df[, order(names(df))]
df$created <- strftime(df$created, '%Y-%m-%d')
if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack.csv'), row.names=F)
#merge the last extraction with storage file and remove duplicates
stack <- read.csv(file=paste(searchterm, '_stack.csv'))
stack <- rbind(stack, df)
stack <- subset(stack, !duplicated(stack$text))
write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)
#######################################################################
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- iconv(sentence, 'UTF-8', 'ASCII')
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

pos <- scan('positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

Dataset <- stack
Dataset$text <- as.factor(Dataset$text)
scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')
write.csv(scores, file=paste(searchterm, '_scores.csv'), row.names=TRUE) #save evaluation results

stat <- scores
stat$created <- stack$created
#stat$created <- as.Date(stat$created,"%m/%d/%y")

stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
by.tweet <- group_by(stat, tweet, created)
by.tweet <- summarise(by.tweet, number=n())
write.csv(by.tweet, file=paste(searchterm, '_opin.csv'), row.names=TRUE)

#chart
ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
  geom_point(aes(group=tweet, color=tweet), size=4) +
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
  #stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +
  ggtitle(searchterm)
ggsave(file=paste(searchterm, '_plot.jpeg'))

}
search("Trump") #enter keyword
