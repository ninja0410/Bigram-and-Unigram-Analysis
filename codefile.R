#connect all libraries0io
library(openssl)
library(httpuv)
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(base64enc)
#connect to API
download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

consumerKey <- 'F8153JGdHnDDoaIl2wOQJYcyq' #put the Consumer Key from Twitter Application
consumerSecret <- 'HnlcO3h5PUwqW69HvOXBxM7zpypGftPvNk8p7EPxwmI1BK1rAz'  #put the Consumer Secret from Twitter Application
accesstoken <- '802470848409964545-SyiOJGAwltoP5AxK8GeF4EyoXzOMOy2'
accesssecret <- 'RRYXStliIGRR1d2Xee7wjH6gC6JxEB9Ic8vH79iYga764'  
Cred2 <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=reqURL,
                         accessURL=accessURL,
                         authURL=authURL)
Cred2$handshake(cainfo="cacert.pem")
save(Cred2, file='twitter authentication.Rdata')
load('twitter authentication.Rdata') #Once you launch the code first time, you can start from this line in the future (libraries should be connected)
setup_twitter_oauth(consumerKey, consumerSecret, accesstoken, accesssecret)
registerTwitterOAuth(Cred2)
search.string <- "#ArrestCHORasia"
no.of.tweets <- 10000

PNBScam.list <- searchTwitter(search.string, n=no.of.tweets, lang="en")
PNBScam.df <- twListToDF(PNBScam.list)

colnames(PNBScam.df)
PNBScam.df <- PNBScam.df[!duplicated(PNBScam.df[,c("id","text")]),]


  #evaluation tweets function
  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    require(plyr)
    require(stringr)
    scores <- laply(sentences, function(sentence, pos.words, neg.words){
      sentence <- gsub('[[:punct:]]', "", sentence)
      sentence <- gsub('[[:cntrl:]]', "", sentence)
      sentence <- gsub('\\d+', "", sentence)
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
  
  Dataset <- PNBScam.df
  Dataset$text <- as.factor(Dataset$text)
  Scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')
  
Scores$Review <- "null"
#Scores$Review <- ifelse(Scores$score < 0, Scores$Review <- "negative", ifelse(Scores$score > 0, Scores$Review <- "positive", Scores$Review <- "neutral"))
Scores$Review <- ifelse(Scores$score < -2, Scores$Review <- "Anger", ifelse(Scores$score < 0 , Scores$Review <- "Sadness", ifelse(Scores$score < 1 , Scores$Review <- "Surprise", ifelse(Scores$score < 3 , Scores$Review <- "Happy", Scores$Review <- "Pleasant"))))

write.csv(Scores, file= "ArrestChorasis1gram.csv", row.names=TRUE) #save evaluation results into the file
table(Scores$score)
table(Scores$Review)
stat <- Scores
stat$created <- Dataset$created
stat$created <- as.Date(Dataset$created)
by.tweet <- group_by(stat, Review, created)
by.tweet <- summarise(by.tweet, number=n())
write.csv(by.tweet, file= 'SV_opin.csv', row.names=TRUE)
qplot(factor(Review), data=stat, geom="bar", fill=factor(Review))+xlab("Reaction") + ylab("Frequency") + ggtitle("People's Reaction on Chaurasia Arrest")

y <- group_by(stat, Review)
y <- summarise(y, number=n())

X <- y$number
piepercent<- round(100*X/sum(X), 1)

pie(table(Scores$Review), labels= piepercent,radius= 1 , main = "People Reactions pie chart",col = rainbow(length(X)))
legend("topright", c("Anger","Happy","Pleasant","Sadness","Surprise"), cex = 0.8,
       fill = rainbow(length(X)))
#chart
#ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
 # geom_point(aes(group=tweet, color=tweet), size=4) +
  #theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) 
  #stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +
  #ggtitle("SalmanVerdict")

#ggsave(file='SV_plot.jpeg')
#library(RTextTools)
#library(e1071)
#library(naivebayes)
#matrix <- create_matrix(Scores[,2], language="english", 
#                      removeStopwords=FALSE, removeNumbers=TRUE, 
#                      stemWords=FALSE) 
#mat <- as.matrix(matrix)
#classifier <- naiveBayes(mat[1:20000,], as.factor(Scores[1:20000,3]) )
#predicted <- predict(classifier, mat[20001:29437,]) # predicted
#table(Scores[20001:29437, 3], predicted)
#recall_accuracy(Scores[20001:29437, 3], predicted)

testnn <- read.delim(file = "negnegHL.tsv")
testnn
negnegBI <- testnn[,1:2]
testnp <- read.delim(file = "negposHL.tsv")
testpn <- read.delim(file = "posnegHL.tsv")
testpp <- read.delim(file = "posposHL.tsv")
negposBI <- testnp[,1:2]
posnegBI <- testpn[,1:2]
posposBI <- testpp[,1:2]
colnames(negnegBI)
colnames(negposBI)
colnames(posnegBI)
colnames(posposBI)
negnegBI$ln_1 <- sapply(strsplit(as.character(negnegBI$X.Bigram.too_..),"m"), "[", 1)
negnegBI$ln_2 <- sapply(strsplit(as.character(negnegBI$X.Bigram.too_..),"m"), "[", 2)
negposBI$ln_1 <- sapply(strsplit(as.character(negposBI$X.Bigram.why_..),"m"), "[", 1)
negposBI$ln_2 <- sapply(strsplit(as.character(negposBI$X.Bigram.why_..),"m"), "[", 2)
posnegBI$ln_1 <- sapply(strsplit(as.character(posnegBI$X.Bigram.good_..),"m"), "[", 1)
posnegBI$ln_2 <- sapply(strsplit(as.character(posnegBI$X.Bigram.good_..),"m"), "[", 2)
posposBI$ln_1 <- sapply(strsplit(as.character(posposBI$Bigram.._luck.),"m"), "[", 1)
posposBI$ln_2 <- sapply(strsplit(as.character(posposBI$Bigram.._luck.),"m"), "[", 2)

negnegBI <- negnegBI[,c(1,4)]
negposBI <- negposBI[,c(1,4)]
posnegBI <- posnegBI[,c(1,4)]
posposBI <- posposBI[,c(1,4)]

negnegBI$ln_2 <- gsub(pattern="@",replacement="",x= negnegBI$ln_2)
negnegBI$ln_2 <- gsub(pattern="_",replacement="",x= negnegBI$ln_2)
negnegBI$ln_2 <- gsub(pattern=")",replacement="",x= negnegBI$ln_2)
negnegBI$ln_2 <- str_replace_all(negnegBI$ln_2, "[[:punct:]]", "")
#negnegBI$ln_2 <- gsub(pattern="(",replacement="",x= negnegBI$ln_2)
negnegBI

negposBI$ln_2 <- gsub(pattern="@",replacement="",x= negposBI$ln_2)
negposBI$ln_2 <- gsub(pattern="_",replacement="",x= negposBI$ln_2)
negposBI$ln_2 <- gsub(pattern=")",replacement="",x= negposBI$ln_2)
negposBI$ln_2 <- str_replace_all(negposBI$ln_2, "[[:punct:]]", "")

posnegBI$ln_2 <- gsub(pattern="@",replacement="",x= posnegBI$ln_2)
posnegBI$ln_2 <- gsub(pattern="_",replacement="",x= posnegBI$ln_2)
posnegBI$ln_2 <- gsub(pattern=")",replacement="",x= posnegBI$ln_2)
posnegBI$ln_2 <- str_replace_all(posnegBI$ln_2, "[[:punct:]]", "")

posposBI$ln_2 <- gsub(pattern="@",replacement="",x= posposBI$ln_2)
posposBI$ln_2 <- gsub(pattern="_",replacement="",x= posposBI$ln_2)
posposBI$ln_2 <- gsub(pattern=")",replacement="",x= posposBI$ln_2)
posposBI$ln_2 <- str_replace_all(posposBI$ln_2, "[[:punct:]]", "")


trainnn1 <- paste(negnegBI$bad, negnegBI$ln_2)
trainnn2 <- paste(negnegBI$ln_2, negnegBI$bad)
trainnn <- c(trainnn1, trainnn2)

trainnp1 <- paste(negposBI$limit, negposBI$ln_2)
trainnp2 <- paste(negposBI$ln_2, negposBI$limit)
trainnp <- c(trainnp1, trainnp2)

trainpn1 <- paste(posnegBI$luck, posnegBI$ln_2)
trainpn2 <- paste(posnegBI$ln_2, posnegBI$luck)
trainpn <- c(trainpn1, trainpn2)

trainpp1 <- paste(posposBI$good, posposBI$ln_2)
trainpp2 <- paste(posposBI$ln_2, posposBI$good)
trainpp <- c(trainpp1, trainpp2)

negneg <- trainnn
negpos <- trainnp
posneg <- trainpn
pospos <- trainpp


bigrams <- function(text){
  word.vec <- strsplit(text, "\\s+")[[1]]
  word.vec.length <- length(word.vec)
  lapply(1:(word.vec.length-1), function(x)paste(word.vec[x], word.vec[x+1]))
}

bigrams("Helpers for Developing Command Line Interfaces")

negnegword2 <- read.csv("negativenegative.csv")
negposword2 <- read.csv("negativepositive.csv")
posnegword2 <- read.csv("positivenegative.csv")
posposword2 <- read.csv("positivepositive.csv")

negnegword1 <- negnegword2$badbad
negposword1 <- negposword2$badgood
posnegword1 <- posnegword2$goodbad
posposword1 <- posposword2$goodgood

#evaluation tweets function for bigram
bigramScore.sentiment <- function(sentences, negnegword1, negposword1, posnegword1, posposword1, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, negnegword1, negposword1, posnegword1, posposword1){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    bigrams <- function(text){
      word.vec <- strsplit(text, "\\s+")[[1]]
      word.vec.length <- length(word.vec)
      lapply(1:(word.vec.length-1), function(x)paste(word.vec[x], word.vec[x+1]))
    }
    twoWords.list <- bigrams(sentence)
    words <- unlist(twoWords.list)
    pospos.matches <- match(words, posposword1)
    negneg.matches <- match(words, negnegword1)
    posneg.matches <- match(words, posnegword1)
    negpos.matches <- match(words, negposword1)
    
    pospos.matches <- !is.na(pospos.matches)
    posneg.matches <- !is.na(posneg.matches)
    negpos.matches <- !is.na(negpos.matches)
    negneg.matches <- !is.na(negneg.matches)
    
    score <- 2*sum(pospos.matches) + sum(negneg.matches) - 2*sum(negpos.matches) - sum(posneg.matches)
    return(score)
  }, negnegword1, negposword1, posnegword1, posposword1, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

Dataset1 <- PNBScam.df
Dataset1$text <- as.factor(Dataset1$text)
Scores2 <- bigramScore.sentiment(Dataset1$text, negnegword1, negposword1, posnegword1, posposword1, .progress='text')

Scores2$Review <- "null"
#Scores$Review <- ifelse(Scores$score < 0, Scores$Review <- "negative", ifelse(Scores$score > 0, Scores$Review <- "positive", Scores$Review <- "neutral"))
Scores2$Review <- ifelse(Scores2$score < -2, Scores2$Review <- "Anger", ifelse(Scores2$score < 0 , Scores2$Review <- "Sadness", ifelse(Scores2$score < 1 , Scores2$Review <- "Surprise", ifelse(Scores2$score < 3 , Scores2$Review <- "Happy", Scores2$Review <- "Pleasant"))))

write.csv(Scores2, file= "ArrestChorasis2gram.csv", row.names=TRUE) #save evaluation results into the file



stat <- Scores2
stat$created <- Dataset$created
stat$created <- as.Date(Dataset$created)
by.tweet <- group_by(stat, Review, created)
by.tweet <- summarise(by.tweet, number=n())
write.csv(by.tweet, file= 'SV_opin.csv', row.names=TRUE)

qplot(factor(Review), data=stat, geom="bar", fill=factor(Review))+xlab("Reaction") + ylab("Frequency") + ggtitle("People Reactions on Chorasia Arrest Bigram")

y <- group_by(stat, Review)
y <- summarise(y, number=n())

X <- y$number
piepercent<- round(100*X/sum(X), 1)

pie(table(Scores2$Review), labels= piepercent,radius= 1 , main = "People Reactions pie chart",col = rainbow(length(X)))
legend("topright", c("Happy","Sadness","Surprise"), cex = 0.8,
       fill = rainbow(length(X)))

Scores$score2 <- Scores2$score
