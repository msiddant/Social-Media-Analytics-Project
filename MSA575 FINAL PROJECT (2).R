################## Packages ####################################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(httr)
library(stringr)
library(twitteR)
library(magrittr)
library(SentimentAnalysis)
require(gridExtra)
library(DT)
library(quantmod)
library(tidytext)
library(sentimentr)
library(syuzhet)
library(plotly)
library(wordcloud)
library(tm)
library(rtweet)
library(dplyr)
library(ggcorrplot)
library(RedditExtractoR)

################################DAILY R/STOCK THREADS######################################
#PLEASE SKIP BELOW, THE COMMENTED OUT CODE IS THE SCRPAING 

#jan31 = reddit_content("https://www.reddit.com/r/stocks/comments/ewl6wo/rstocks_daily_discussion_fundamentals_friday_jan/")
#feb03 = reddit_content("https://www.reddit.com/r/stocks/comments/ey3ukp/rstocks_daily_discussion_monday_feb_03_2020/")
#feb07 = reddit_content("https://www.reddit.com/r/stocks/comments/f07i2y/rstocks_daily_discussion_fundamentals_friday_feb/")
#feb14 = reddit_content("https://www.reddit.com/r/stocks/comments/f3p4xs/rstocks_daily_discussion_fundamentals_friday_feb/")
#feb21 = reddit_content("https://www.reddit.com/r/stocks/comments/f781cp/rstocks_daily_discussion_fundamentals_friday_feb/")
#feb28 = reddit_content("https://www.reddit.com/r/stocks/comments/farghc/rstocks_daily_discussion_fundamentals_friday_feb/")
#mar06 = reddit_content("https://www.reddit.com/r/stocks/comments/feaepi/rstocks_daily_discussion_fundamentals_friday_mar/")
#mar13 = reddit_content("https://www.reddit.com/r/stocks/comments/fhw8q1/rstocks_daily_discussion_fundamentals_friday_mar/")
#mar20 = reddit_content("https://www.reddit.com/r/stocks/comments/flrbav/rstocks_daily_discussion_fundamentals_friday_mar/")
#mar27 = reddit_content("https://www.reddit.com/r/stocks/comments/fpt5qp/rstocks_daily_discussion_fundamentals_friday_mar/")
#apr03 = reddit_content("https://www.reddit.com/r/stocks/comments/fu4dic/rstocks_daily_discussion_fundamentals_friday_apr/")
#apr10 = reddit_content("https://www.reddit.com/r/stocks/comments/fybi2q/rstocks_daily_discussion_fundamentals_friday_apr/")
#apr17 = reddit_content("https://www.reddit.com/r/stocks/comments/g2xq0m/rstocks_daily_discussion_fundamentals_friday_apr/")
#apr24 = reddit_content("https://www.reddit.com/r/stocks/comments/g74iad/rstocks_daily_discussion_fundamentals_friday_apr/")
#may01 = reddit_content("https://www.reddit.com/r/stocks/comments/gbe5cs/rstocks_daily_discussion_fundamentals_friday_may/")
#may08 = reddit_content("https://www.reddit.com/r/stocks/comments/gfp02m/rstocks_daily_discussion_fundamentals_friday_may/")

#dailythread = rbind(jan31,feb03,feb07,feb14,feb21,feb28,mar06,mar13,mar20,mar27,
#apr03,apr10,apr17,apr24,may01,may08)
#write.csv(dailythread, "dailythread.csv")


##before corona 
#jan24 = reddit_content("https://www.reddit.com/r/stocks/comments/et7fto/rstocks_daily_discussion_fundamentals_friday_jan/")
#jan14 = reddit_content("https://www.reddit.com/r/stocks/comments/epxj5s/rstocks_daily_discussion_fundamentals_friday_jan/")
#jan10 = reddit_content("https://www.reddit.com/r/stocks/comments/emnzli/rstocks_daily_discussion_fundamentals_friday_jan/")
#jan03 = reddit_content("https://www.reddit.com/r/stocks/comments/ejcglj/rstocks_daily_discussion_fundamentals_friday_jan/")
#dec27 = reddit_content("https://www.reddit.com/r/stocks/comments/eg8g3j/rstocks_daily_discussion_fundamentals_friday_dec/")
#dec20 = reddit_content("https://www.reddit.com/r/stocks/comments/ed7831/rstocks_daily_discussion_fundamentals_friday_dec/")
#dec13 = reddit_content("https://www.reddit.com/r/stocks/comments/ea17t0/rstocks_daily_discussion_fundamentals_friday_dec/")
#dec06 = reddit_content("https://www.reddit.com/r/stocks/comments/e6vtgv/rstocks_daily_discussion_fundamentals_friday_dec/")
#nov29 = reddit_content("https://www.reddit.com/r/stocks/comments/e3bvve/rstocks_daily_discussion_fundamentals_friday_nov/")
#nov22 = reddit_content("https://www.reddit.com/r/stocks/comments/dzy0ha/rstocks_daily_discussion_fundamentals_friday_nov/")
#nov15 = reddit_content("https://www.reddit.com/r/stocks/comments/dwnnvz/rstocks_daily_discussion_fundamentals_friday_nov/")
#nov08 = reddit_content("https://www.reddit.com/r/stocks/comments/dtcbop/rstocks_daily_discussion_fundamentals_friday_nov/")
#nov01 = reddit_content("https://www.reddit.com/r/stocks/comments/dq19hn/rstocks_daily_discussion_fundamentals_friday_nov/")
#oct25 = reddit_content("https://www.reddit.com/r/stocks/comments/dmu9ai/rstocks_daily_discussion_fundamentals_friday_oct/")
#oct18 = reddit_content("https://www.reddit.com/r/stocks/comments/djkci1/rstocks_daily_discussion_fundamentals_friday_oct/")
#oct11 = reddit_content("https://www.reddit.com/r/stocks/comments/dgbyy5/rstocks_daily_discussion_fundamentals_friday_oct/")

#beforedailythread = rbind(jan24,jan14,jan10,jan03,dec27,dec20,dec13,dec06,
#nov29,nov22,nov15,nov08,nov01,oct25,oct18,oct11)
#write.csv(beforedailythread,"beforedailythread.csv")
#############################################################################################################################################################################
###############################################################################################################################################################################

################################################################################
#AFTER JAN 31 ANALYSIS
################################################################################
dailythread=read.csv("dailythread.csv", header = T)

dailythread %>%  
  summarize(sum(num_comments)) #305366

mean(dailythread$num_comments) #159.62

#NRC
dailythread$comment=as.character(dailythread$comment)
dailythreadnrc=get_nrc_sentiment(dailythread$comment)

dailythreadnrc2 = dailythreadnrc %>%
  gather(key = "emotions", value = "scores", "anger":"positive")

ggplot(dailythreadnrc2, aes(emotions, scores, fill = emotions)) + geom_col() +
  labs(title = "Sentiment of r/Stocks Daily thread after January 31") + 
  xlab("emotions") + 
  ylab("scores") + theme(legend.position = "none")

par(mfrow=c(1,2))

#SENTIMENT R
dailythreadsentences=get_sentences(dailythread$comment)
dailythreadsentR=sentiment(dailythread$comment)
View(dailythreadsentR)

dailythreadsentR2 = dailythreadsentR %>% 
  group_by(element_id) %>% 
  summarize(sentiment = mean(sentiment)) 

dailythreadsentR3 = cbind(dailythreadsentR2,dailythread)
View(dailythreadsentR3)

dailtythread4 = dailythreadsentR3 %>% 
  group_by(post_date) %>% 
  summarise(sentiment = mean(sentiment)) 

ggplot(dailtythread4,aes(post_date,sentiment,group=1))+geom_line()+geom_point()+
  labs(title = "r/Stocks Sentiment by Week Jan 31 - May 8")+xlab('Days')

#TEXT ANALYSIS/WORD FREQUENCY 
dailythreadtext = dailythread %>% 
  unnest_tokens(word, comment) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) %>% 
  top_n(15)
View(dailythreadtext)

ggplot(dailythreadtext, aes(word,n,fill = word)) + geom_col() + theme(
  legend.position = "bottom") #before adding to the stop words 

text_1 = tibble(word = "www.reddit.com", lexicon = "SMART")
text_2 = tibble(word = "https", lexicon = "SMART")
text_3 = tibble(word = "comments", lexicon = "SMART")
text_4 = tibble(word = "stock", lexicon = "SMART")
text_5 = tibble(word = "stocks", lexicon = "SMART")
text_6 = tibble(word = "5", lexicon = "SMART")
text_7 = tibble(word = "market", lexicon = "SMART")
stop_words = rbind(text_1,text_2,text_3,text_4,text_5,
                   text_6,text_7,stop_words)

dailythreadtext = dailythread %>% 
  unnest_tokens(word, comment) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) %>% 
  top_n(15)
View(dailythreadtext)

ggplot(dailythreadtext, aes(word,n,fill = word)) + geom_col() + theme(
  legend.position = "none") + labs(title = "Word Frequency of r/Stocks comments after")

################################################################################
#AFTER JAN 31 ANALYSIS
################################################################################
beforedaily=read.csv("beforedailythread.csv", header = T)

beforedaily %>% 
  summarize(sum(num_comments))  #2738

mean(beforedaily$num_comments) #17.439

beforedaily$comment=as.character(beforedaily$comment)
beforedailynrc=get_nrc_sentiment(beforedaily$comment)

beforedailynrc2 = beforedailynrc %>%
  gather(key = "emotions", value = "scores", "anger":"positive")

ggplot(beforedailynrc2, aes(emotions, scores, fill = emotions)) + geom_col() +
  labs(title = "Sentiment of r/Stocks Daily thread before Corona Virus") + 
  xlab("emotions") + 
  ylab("scores") + theme(legend.position = "none")

#SENTIMENT R
beforedailysentR=sentiment(beforedaily$comment)
View(beforedailysentR)

beforedailysentR2 = beforedailysentR %>% 
  group_by(element_id) %>% 
  summarize(sentiment = mean(sentiment)) 

beforedailysentR3 = cbind(beforedailysentR2,beforedaily)
View(dailythreadsentR3)

beforedailysentR4 =  beforedailysentR3%>%
  group_by(post_date)%>%
  summarise(sentiment=mean(sentiment))

ggplot(beforedailysentR4,aes(post_date,sentiment,group=1))+geom_line()+geom_point()+
  labs(title = "r/Stocks Sentiment by Week Oct 11 - Jan 31")+xlab('Days')

#TEXT ANALYSIS/WORD FREQUENCY
beforedailytext = beforedaily %>% 
  unnest_tokens(word, comment) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) %>% 
  top_n(15)
View(beforedailytext)

ggplot(beforedailytext, aes(word,n,fill = word)) + geom_col() + theme(
  legend.position = "none") + labs(title = "Word Frequency of r/Stocks comments before")

text_1 = tibble(word = "200", lexicon = "SMART")
text_2 = tibble(word = "2c", lexicon = "SMART")
text_3 = tibble(word = "keywords", lexicon = "SMART")
text_4 = tibble(word = "subreddit", lexicon = "SMART")
text_5 = tibble(word = "subredditsummarybot", lexicon = "SMART")
text_6 = tibble(word = "0a25", lexicon = "SMART")
stop_words = rbind(text_1,text_2,text_3,text_4,text_5,
                   text_6,stop_words)

beforedailytext = beforedaily %>% 
  unnest_tokens(word, comment) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) %>% 
  top_n(15)
View(beforedailytext)

ggplot(beforedailytext, aes(word,n,fill = word)) + geom_col() + theme(
  legend.position = "none") + labs(title = "Word Frequency of r/Stocks comments before")

#####################COMBINING BOTH BEFORE AND AFTER TO MAKE A PLOT 
View(dailtythread4)
View(beforedailysentR4)
threadcombo = rbind(beforedailysentR4,dailtythread4)
View(threadcombo)

ggplot(threadcombo,aes(post_date,sentiment,group=1))+geom_line()+geom_point()+
  labs(title = "r/Stocks Sentiment by Week")+xlab('Days') + theme(axis.text.x = element_text(angle = 90))


######################################################################################################################################

# Twitter DATA EXTRACTION for INDEXES
#PLEASE SKIP TO DATA ANALYSIS PART OF THE CODE BELOW

#Key <- "c5OmPIvjJq56wiX8s1K9knA3I"
#Secret <- "Af1Y0QIeBKL0XLuhjRJxN0xtr2MWONbdPNeorlwye4Dl9NS22G"
#Access <- "1027978669-a9zMaRuVnoofq85PQM2Nv2E74j2y2le7QkOkYQR"
#T_Secret <- "MZ1P0Q0MyDsnsPHai3Kg1zekgRER523DvxIwmHXUpGchs"
#APP <- "SocialMedia575"

#logging into Twitter API
#token <- create_token(app = APP,
#                      consumer_key = Key,
#                      consumer_secret = Secret,
#                      access_token = Access,
#                      access_secret = T_Secret)

#token

#Stocks and Economy 
#StockMarket <- search_tweets("US Stock market", n = 1000, 
#                           lang = "en", token = token,
#                           include_rts = FALSE)

#Recession <- search_tweets("US AND Recession", n = 1000, 
#                                  lang = "en", token = token,
#                                  include_rts = FALSE)

#Economy <- search_tweets("US AND Economy", n = 1000, 
#                                     lang = "en", token = token,
#                                     include_rts = FALSE)

#StocksTwitter<-rbind(StockMarket,Recession,Economy)
#View(StocksTwitter)
#Each Index
#SPX <- search_tweets("SPX OR S&P500", n = 1000, 
#                           lang = "en", token = token,
#                           include_rts = FALSE)

#DJI <- search_tweets("DJI OR Dow Jones", n = 1000, 
#                     lang = "en", token = token,
#                     include_rts = FALSE)

#NDX <- search_tweets("NDX OR NASDAQ", n = 1000, 
#                            lang = "en", token = token,
#                            include_rts = FALSE)


#Save Data
#write_as_csv(StocksTwitter, "StocksTwitter6.csv")
#write_as_csv(SPX, "SPX6.csv")
#write_as_csv(DJI, "DJI6.csv")
#write_as_csv(NDX, "NDX6.csv")

#####################################################################
#SENTIMENT ANALYSIS
#####################################################################

#stocks1<-read.csv("StocksTwitter.csv",header = T)
#stocks2<-read.csv("StocksTwitter2.csv",header = T)
#stocks3<-read.csv("StocksTwitter3.csv",header = T)
#stocks4<-read.csv("StocksTwitter4.csv",header = T)
#stocks5<-read.csv("StocksTwitter5.csv",header = T)
#stocks6<-read.csv("StocksTwitter6.csv",header = T)
#StockTweet<-rbind(stocks1,stocks2,stocks3,stocks4,stocks5,stocks6)
#View(StockTweet)

#dji1<-read.csv("DJI.csv",header = T)
#dji2<-read.csv("DJI2.csv",header = T)
#dji3<-read.csv("DJI3.csv",header = T)
#dji4<-read.csv("DJI4.csv",header = T)
#dji5<-read.csv("DJI5.csv",header = T)
#dji6<-read.csv("DJI6.csv",header = T)
#DJI<-rbind(dji1,dji2,dji3,dji4,dji5,dji6)


#ndx1<-read.csv("NDX.csv",header = T)
#ndx2<-read.csv("NDX2.csv",header = T)
#ndx3<-read.csv("NDX3.csv",header = T)
#ndx4<-read.csv("NDX4.csv",header = T)
#ndx5<-read.csv("NDX5.csv",header = T)
#ndx6<-read.csv("NDX6.csv",header = T)
#NDX<-rbind(ndx1,ndx2,ndx3,ndx4,ndx5,ndx6)

#spx1<-read.csv("SPX.csv",header = T)
#spx2<-read.csv("SPX2.csv",header = T)
#spx3<-read.csv("SPX3.csv",header = T)
#spx4<-read.csv("SPX4.csv",header = T)
#spx5<-read.csv("SPX5.csv",header = T)
#spx6<-read.csv("SPX6.csv",header = T)
#SPX<-rbind(spx1,spx2,spx3,spx4,spx5,spx6)

#write_as_csv(StockTweet, "StocksTwitterFinal.csv")
#write_as_csv(SPX, "SPXFinal.csv")
#write_as_csv(DJI, "DJIFinal.csv")
#write_as_csv(NDX, "NDXFinal.csv")

##########################################################
#DATA ANALYSIS
##########################################################
StockTweet <- read.csv("StocksTwitterFinal.csv",header=T)
SPXTweet<- read.csv("SPXFinal.csv",header = T)
DJITweet<- read.csv("DJIFinal.csv",header = T)
NDXTweet<- read.csv("NDXFinal.csv",header = T)
IndexPrices<- read.csv("indexprices.csv",header = T)

StockTweet$created_at <- as.Date(StockTweet$created_at)
SPXTweet$created_at <- as.Date(SPXTweet$created_at)
DJITweet$created_at <- as.Date(DJITweet$created_at)
NDXTweet$created_at <- as.Date(NDXTweet$created_at)
IndexPrices$created_at <- as.Date(IndexPrices$created_at)
#Filtering the dataset for Weekdays only
StockTweet1 <- StockTweet %>% 
  filter(created_at >= as.Date("2020-04-27") & created_at <= as.Date("2020-05-01"))

StockTweet3 <- StockTweet %>% 
  filter(created_at >= as.Date("2020-05-04")& created_at <= as.Date("2020-05-08"))

StockTwitter <- rbind(StockTweet1, StockTweet3) 
StockTwitter$text<- as.character(StockTwitter$text)
#S&P500
SPXTweet1 <- SPXTweet %>% 
  filter(created_at >= as.Date("2020-04-27") & created_at <= as.Date("2020-05-01"))

SPXTweet3 <- SPXTweet %>% 
  filter(created_at >= as.Date("2020-05-04")& created_at <= as.Date("2020-05-08"))

SPXTwitter <- rbind(SPXTweet1, SPXTweet3)
SPXTwitter$text<- as.character(SPXTwitter$text)
#DJI
DJITweet1 <- DJITweet %>% 
  filter(created_at >= as.Date("2020-04-27") & created_at <= as.Date("2020-05-01"))

DJITweet3 <- DJITweet %>% 
  filter(created_at >= as.Date("2020-05-04")& created_at <= as.Date("2020-05-08"))

DJITwitter <- rbind(DJITweet1, DJITweet3)
DJITwitter$text<- as.character(DJITwitter$text)
#NDX
NDXTweet1 <- NDXTweet %>% 
  filter(created_at >= as.Date("2020-04-27") & created_at <= as.Date("2020-05-01"))

NDXTweet3 <- NDXTweet %>% 
  filter(created_at >= as.Date("2020-05-04")& created_at <= as.Date("2020-05-08"))

NDXTwitter <- rbind(NDXTweet1, NDXTweet3)
NDXTwitter$text<- as.character(NDXTwitter$text)

#Sentiment Analysis
#Twitter Sentiments
#StockTweets

sents <- sentiment(StockTwitter$text)

sents2 <- sents%>% 
  group_by(element_id) %>% 
  summarize(word_count = sum(word_count), sentiments = mean(sentiment))

StockTwitter2 <- StockTwitter %>% 
  select(created_at, text)

finalsents <- cbind(sents2,StockTwitter2)

finalsents1<- finalsents%>%
  group_by(created_at)%>%
  summarise(sentiment=mean(sentiments))
View(finalsents1)
#mean Sentiment Plot
ggplot(finalsents1,aes(created_at,sentiment,group=1))+geom_line()+geom_point()+
  labs(title = "Overall Mean Sentiment")+xlab('Days')

Stock_corr<-merge(finalsents1, IndexPrices, by="created_at", all = T)
Stock_corr<-na.omit(Stock_corr)

corr<-cor(Stock_corr[,-1])

#Correlation Test
cor.test(Stock_corr[,-1]$sentiment,Stock_corr[,-1]$DJI_Close)
cor.test(Stock_corr[,-1]$sentiment,Stock_corr[,-1]$DJI_Change)
cor.test(Stock_corr[,-1]$sentiment,Stock_corr[,-1]$SPX_Close)
cor.test(Stock_corr[,-1]$sentiment,Stock_corr[,-1]$SPX_Change)
cor.test(Stock_corr[,-1]$sentiment,Stock_corr[,-1]$NDX_Close)
cor.test(Stock_corr[,-1]$sentiment,Stock_corr[,-1]$NDX_Change)

?cor.test()
View(corr)
#Correlation
ggcorrplot(corr, lab = TRUE, type = "lower")


#SPX
sents <- sentiment(SPXTwitter$text)

sents2 <- sents%>% 
  group_by(element_id) %>% 
  summarize(word_count = sum(word_count), sentiments = mean(sentiment))

SPXTwitter2 <- SPXTwitter %>% 
  select(created_at, text)

SPXfinalsents <- cbind(sents2,SPXTwitter2)

SPXfinalsents1<- SPXfinalsents%>%
  group_by(created_at)%>%
  summarise(SPXsentiment=mean(sentiments))
View(SPXfinalsents1)
#Mean Sentiment Plot
ggplot(SPXfinalsents1,aes(created_at,SPXsentiment,group=1))+geom_line()+geom_point()+labs(title = " S&P 500 Mean Sentiment")+xlab('Date')
SPX_corr<-merge(SPXfinalsents1, IndexPrices, by="created_at", all = T)
SPX_corr<-na.omit(SPX_corr)

corr1<-cor(SPX_corr[,-1])

#Correlation Test
cor.test(SPX_corr[,-1]$SPXsentiment,SPX_corr[,-1]$DJI_Close)
cor.test(SPX_corr[,-1]$SPXsentiment,SPX_corr[,-1]$DJI_Change)
cor.test(SPX_corr[,-1]$SPXsentiment,SPX_corr[,-1]$SPX_Close)
cor.test(SPX_corr[,-1]$SPXsentiment,SPX_corr[,-1]$SPX_Change)
cor.test(SPX_corr[,-1]$SPXsentiment,SPX_corr[,-1]$NDX_Close)
cor.test(SPX_corr[,-1]$SPXsentiment,SPX_corr[,-1]$NDX_Change)

View(corr1)
#Correlation
ggcorrplot(corr1, lab = TRUE, type = "lower")

#DJI

Dji_sents <- sentiment(DJITwitter$text)

Dji_sents1 <- Dji_sents%>% 
  group_by(element_id) %>% 
  summarize(word_count = sum(word_count), sentiments = mean(sentiment))

DJITwitter2 <- DJITwitter %>% 
  select(created_at, text)

DJIfinalsents <- cbind(Dji_sents1,DJITwitter2)

DJIfinalsents1<- DJIfinalsents%>%
  group_by(created_at)%>%
  summarise(DJIsentiment=mean(sentiments))
View(DJIfinalsents1)
#mean sentiment plot
ggplot(DJIfinalsents1,aes(created_at,DJIsentiment,group=1))+geom_line()+geom_point()+labs(title = "Dow Jones Mean Sentiment")+xlab('Date')

DJI_corr<-merge(DJIfinalsents1, IndexPrices, by="created_at", all = T)
DJI_corr<-na.omit(DJI_corr)

corr2<-cor(DJI_corr[,-1])

#Correlation Test
cor.test(DJI_corr[,-1]$DJIsentiment,DJI_corr[,-1]$DJI_Close)
cor.test(DJI_corr[,-1]$DJIsentiment,DJI_corr[,-1]$DJI_Change)
cor.test(DJI_corr[,-1]$DJIsentiment,DJI_corr[,-1]$SPX_Close)
cor.test(DJI_corr[,-1]$DJIsentiment,DJI_corr[,-1]$SPX_Change)
cor.test(DJI_corr[,-1]$DJIsentiment,DJI_corr[,-1]$NDX_Close)
cor.test(DJI_corr[,-1]$DJIsentiment,DJI_corr[,-1]$NDX_Change)


View(corr2)
#Correlation
ggcorrplot(corr2, lab = TRUE, type = "lower")


#NDX

Ndx_sents <- sentiment(NDXTwitter$text)

Ndx_sents1 <- Ndx_sents%>% 
  group_by(element_id) %>% 
  summarize(word_count = sum(word_count), sentiments = mean(sentiment))

NDXTwitter2 <- NDXTwitter %>% 
  select(created_at, text)

NDXfinalsents <- cbind(Ndx_sents1,NDXTwitter2)

NDXfinalsents1<- NDXfinalsents%>%
  group_by(created_at)%>%
  summarise(NDXsentiment=mean(sentiments))
View(NDXfinalsents1)
#Mean Sentiment
ggplot(NDXfinalsents1,aes(created_at,NDXsentiment,group=1))+geom_line()+geom_point()+labs(title = "NASDAQ Mean Sentiment")+xlab('Days')

NDX_corr<-merge(NDXfinalsents1, IndexPrices, by="created_at", all = T)
NDX_corr<-na.omit(NDX_corr)

corr3<-cor(NDX_corr[,-1])

#Correlation Test
cor.test(NDX_corr[,-1]$NDXsentiment,NDX_corr[,-1]$DJI_Close)
cor.test(NDX_corr[,-1]$NDXsentiment,NDX_corr[,-1]$DJI_Change)
cor.test(NDX_corr[,-1]$NDXsentiment,NDX_corr[,-1]$SPX_Close)
cor.test(NDX_corr[,-1]$NDXsentiment,NDX_corr[,-1]$SPX_Change)
cor.test(NDX_corr[,-1]$NDXsentiment,NDX_corr[,-1]$NDX_Close)
cor.test(NDX_corr[,-1]$NDXsentiment,NDX_corr[,-1]$NDX_Change)


View(corr3)
#Correlation
ggcorrplot(corr3, lab = TRUE, type = "lower")

#NRC Sentiment
#Overall Stock Market
Stocks_nrc_sentiment <- get_nrc_sentiment(StockTwitter$text)
View(Stocks_nrc_sentiment)

Stocks_nrc_sentiment2<-Stocks_nrc_sentiment %>%
  gather(key="Emotions", value = "Scores","anger":"positive")

ggplot(Stocks_nrc_sentiment2,aes(Emotions, Scores, fill = Emotions))+geom_col()+
  labs(title = "NRC Sentiment Analysis of the Stock Market")+xlab('Emotions')+
  ylab('Scores')+theme(legend.position = "none")

#SPX
Spx_nrc_sentiment <- get_nrc_sentiment(SPXTwitter$text)
View(Spx_nrc_sentiment)

Spx_nrc_sentiment2<-Spx_nrc_sentiment %>%
  gather(key="Emotions", value = "Scores","anger":"positive")

ggplot(Spx_nrc_sentiment2,aes(Emotions, Scores,fill = Emotions))+geom_col()+
  labs(title = "NRC Sentiment Analysis of SPX Index")+xlab('Emotions')+
  ylab('Scores')+theme(legend.position = "none")

#DJI
Dji_nrc_sentiment <- get_nrc_sentiment(DJITwitter$text)
View(Dji_nrc_sentiment)

Dji_nrc_sentiment2<-Dji_nrc_sentiment %>%
  gather(key="Emotions", value = "Scores","anger":"positive")

ggplot(Dji_nrc_sentiment2,aes(Emotions, Scores, fill = Emotions))+geom_col()+
  labs(title = "NRC Sentiment Analysis of DJI Index")+xlab('Emotions')+
  ylab('Scores')+theme(legend.position = "none")

#NDX
Ndx_nrc_sentiment <- get_nrc_sentiment(NDXTwitter$text)
View(Ndx_nrc_sentiment)

Ndx_nrc_sentiment2<-Ndx_nrc_sentiment %>%
  gather(key="Emotions", value = "Scores","anger":"positive")

ggplot(Ndx_nrc_sentiment2,aes(Emotions, Scores, fill = Emotions))+geom_col()+
  labs(title = "NRC Sentiment Analysis of NDX Index")+xlab('Emotions')+
  ylab('Scores')+theme(legend.position = "none")

#Frequency Plots
#StockTweet
#Word Frequency
StockTwitter3<- StockTwitter%>% 
  unnest_tokens(word,text) %>%
  anti_join(stop_words)%>%
  mutate(word = iconv(word, from = "latin1", to = "ASCII"))%>%
  filter(is.na(as.numeric(word)))%>% #Removing Numbers
  filter(!is.na(word))%>%#Removing NA's
  count(word, sort = TRUE)
View(StockTwitter3)


StockTwitter4 <- StockTwitter3[-c(1:2,8),]

View(StockTwitter4)
#Frequency Plot
StockTwitter4%>%
  top_n(15)%>%
  ggplot(aes(word,n,fill=rainbow(15)))+geom_col()+
  theme(axis.text.x = element_text(angle = 90))+theme(legend.position = "none")+
  labs(title="Top 15 most frequent words used on Twitter")

#SPX
#Word Frequency
SPXTwitter3<- SPXTwitter%>% 
  unnest_tokens(word,text) %>%
  anti_join(stop_words)%>%
  mutate(word = iconv(word, from = "latin1", to = "ASCII"))%>%
  #filter(is.na(as.numeric(word)))%>% #Removing Numbers
  filter(!is.na(word))%>%#Removing NA's
  count(word, sort = TRUE)
View(SPXTwitter3)


SPXTwitter4 <- SPXTwitter3[-c(1:3,5),]

#Frequency Plot
SPXTwitter4%>%
  top_n(15)%>%
  ggplot(aes(word,n,fill=rainbow(15)))+geom_col()+
  theme(axis.text.x = element_text(angle = 90))+theme(legend.position = "none")+
  labs(title="Top 15 most frequent words used on Twitter - SPX")


#DJI
#Word Frequency
DJITwitter3<- DJITwitter%>% 
  unnest_tokens(word,text) %>%
  anti_join(stop_words)%>%
  mutate(word = iconv(word, from = "latin1", to = "ASCII"))%>%
  filter(!is.na(word))%>%#Removing NA's
  count(word, sort = TRUE)
View(DJITwitter3)


DJITwitter4 <- DJITwitter3[-c(1:7),]

#Frequency Plot
DJITwitter4%>%
  top_n(15)%>%
  ggplot(aes(word,n,fill=rainbow(15)))+geom_col()+
  theme(axis.text.x = element_text(angle = 90))+theme(legend.position = "none")+
  labs(title="Top 15 most frequent words used on Twitter - DJI")


#NDX
#Word Frequency
NDXTwitter3<- NDXTwitter%>% 
  unnest_tokens(word,text) %>%
  anti_join(stop_words)%>%
  mutate(word = iconv(word, from = "latin1", to = "ASCII"))%>%
  filter(!is.na(word))%>%#Removing NA's
  count(word, sort = TRUE)
View(NDXTwitter3)

NDXTwitter4 <- NDXTwitter3[-c(1:4,6),]

#Frequency Plot
NDXTwitter4%>%
  top_n(15)%>%
  ggplot(aes(word,n,fill=rainbow(15)))+geom_col()+
  theme(axis.text.x = element_text(angle = 90))+theme(legend.position = "none")+
  labs(title="Top 15 most frequent words used on Twitter - NDX")


######################################## {Analysis of Primary Stocks Within Their Index} #############################################
#UNDERSTANDING OF DATA EXTRACTION
#PLEASE SKIP TO DATA ANALYSIS PART OF THE CODE BELOW

#amazon <- search_tweets(q = "amazon AND stocks",n = 2000, lang = "en", token = token, include_rts = TRUE) 
#amazon1 <- search_tweets(q = "amazon AND martet", n = 2000, lang = "en", token = token, include_rts = TRUE) 
#amazon2 <- search_tweets(q = "amazon AND virus", n = 2000, lang = "en", token = token, include_rts = TRUE) 
#amazon3 <- search_tweets(q = "jeffBezos", n = 2000, lang = "en", token = token, include_rts = TRUE) 
#amazon4 <- search_tweets(q = "amazon AND shipping", n = 2000, lang = "en", token = token, include_rts = TRUE)

#exxon <- search_tweets(q = "ExxonMobil", n = 2000, lang = "en", token = token, include_rts = TRUE) 
#exxon1 <- search_tweets(q = "ExxonMobil AND coronavirus ", n = 2000, lang = "en", token = token, include_rts = TRUE) 
#exxon2 <- search_tweets(q = "ExxonMobil AND oil ", n = 2000, lang = "en", token = token, include_rts = TRUE) 
#exxon3 <- search_tweets(q = "ExxonMobil AND market ", n = 2000, lang = "en", token = token, include_rts = TRUE) 
#exxon4 <- search_tweets(q = "ExxonMobil AND stocks ", n = 2000, lang = "en", token = token, include_rts = TRUE) 

#disney <- search_tweets(q = "Disney plus ", n = 2000, lang = "en", token = token, include_rts = TRUE) 
#disney1 <- search_tweets(q = "Disney AND coronavirus ", n = 2000, lang = "en", token = token, include_rts = TRUE) 
#disney2 <- search_tweets(q = "Disney AND market ", n = 2000, lang = "en", token = token, include_rts = TRUE) 
#disney <- search_tweets(q = "Disney AND parks ", n = 2000, lang = "en", token = token, include_rts = TRUE) 
#disney <- search_tweets(q = "Disney AND stocks ", n = 2000, lang = "en", token = token, include_rts = TRUE) 


###### Data files #####################################################################

amzn.tweets = read.csv("amazonpro.csv")
View(amzn.tweets)
str(amzn.tweets)

amzn.stock = read.csv("amzn.stock.csv")
View(amzn.stock)
names(amzn.stock)
str(amzn.stock)

######################## Data Cleaning #################################################

#Create subset of data
amzn <- subset(amzn.stock, select = c(Date, Close))

#Convert to dataframe and encode to native
amzn.tweets$text = as.character(amzn.tweets$text)
amzn.tweets$text <- enc2native(amzn.tweets$text)

#Clean text
amzn.tweets$text <- gsub("^[[:space:]]*","",amzn.tweets$text) # Remove leading whitespaces
amzn.tweets$text <- gsub("[[:space:]]*$","",amzn.tweets$text) # Remove trailing whitespaces
amzn.tweets$text <- gsub(" +"," ",amzn.tweets$text) #Remove extra whitespaces
amzn.tweets$text <- gsub("'", "%%", amzn.tweets$text) #Replace apostrophes with %%
amzn.tweets$text <- iconv(amzn.tweets$text, "latin1", "ASCII", sub="") # Remove emojis
amzn.tweets$text <- gsub("<(.*)>", "", amzn.tweets$text) #Remove Unicodes like <U+A>
amzn.tweets$text <- gsub("\\ \\. ", " ", amzn.tweets$text) #Replace orphaned fullstops with space
amzn.tweets$text <- gsub("  ", " ", amzn.tweets$text) #Replace double space with single space
amzn.tweets$text <- gsub("%%", "\'", amzn.tweets$text) #Change %% back to apostrophes
amzn.tweets$text <- gsub("https(.*)*$", "", amzn.tweets$text) #Remove tweet URL
amzn.tweets$text <- gsub("\\n", "-", amzn.tweets$text) #Replace line breaks with "-"
amzn.tweets$text <- gsub("--", "-", amzn.tweets$text) #Remove double "-" from double line breaks
amzn.tweets$text <- gsub("&amp;", "&", amzn.tweets$text) #Fix ampersand &
amzn.tweets$text[amzn.tweets$text == " "] <- "<no text>"
amzn.tweets$text <- str_replace_all(amzn.tweets$text, "[^[:alnum:]]", " ")
amzn.tweets$text <- str_replace_all(amzn.tweets$text, "[[:punct:]]", " ")
amzn.tweets$text <- gsub(pattern = '([[:upper:]])', perl = TRUE, replacement = '\\L\\1', amzn.tweets$text) # convert to lower case
amzn.tweets$text <- gsub('[[:digit:]]+', '', amzn.tweets$text) # Remove digits
amzn.tweets$text <- gsub("\\W*\\b\\w\\b\\W*", " ", amzn.tweets$text) # remove single characters

#Select clean tweets 
cleanTweets <- amzn.tweets %>% 
  select("text")
View(cleanTweets)

#################################### Sentiment Analysis #######################################

#Sentiment using Afinn 
sentiment2 <- get_sentiment(amzn.tweets$text, method = "afinn")
View(sentiment2)

#View sentiment direction (i.e. positive, neutral and negative)
sentiment3 = convertToDirection(sentiment2)
View(sentiment3)

#Extract and convert 'date' column
date <- amzn.tweets$created_at
date <- as.Date(date, format = "%m/%d/%y")
View(date)

#Create new dataframe 
df <- cbind(cleanTweets, sentiment2, sentiment3, date)
#Remove rows with NA
df <- df[complete.cases(df), ]
View(df)

#Calculate the average of daily sentiment score
df2 <- df %>% 
  group_by(date) %>%
  summarize(meanSentiment = mean(sentiment2, na.rm=TRUE))

DT::datatable(df2, editable = TRUE)

#Get frquency of each sentiment i.e. positive, neutral, and negative  
freq <- df %>% 
  group_by(date,sentiment3) %>% 
  summarise(Freq=n())

#Convert data from long to wide
freq2 <- freq %>% 
  spread(key = sentiment3, value = Freq)

DT::datatable(freq2, editable = TRUE)

ggplot() + 
  geom_bar(mapping = aes(x = freq$date, y = freq$Freq, fill = freq$sentiment3), stat = "identity") +
  ylab('Sentiment Frequency') +
  xlab('April 27th to May 1st  ----  May 4th to May 8th')

#Calculate z-Scores of Amazon closing stock prices
mu <- mean(amzn$Close)
sd <- sd(amzn$Close)
amzn2 <- amzn %>% 
  mutate(zScore = (amzn$Close-mu)/sd)

#Plot mean sentiment scores
p1 <- ggplot(data=df2, aes(x=date,y=meanSentiment, group=1)) +
  geom_line()+
  geom_point() +
  ylab("Mean Twitter Sentiment Score")

#plot Amazon z-score prices
p2 <- ggplot(data=amzn2, aes(x=Date,y=zScore, group=1)) +
  geom_line()+
  geom_point() +
  ylab("Z-Score of Closing Stock Price")
scale_x_date(date_breaks = "1 day", 
             limits = as.Date(c('2020-04-27','2020-05-08')))

plot1 <- p1
plot2 <- p2
grid.arrange(plot1, plot2, nrow=2)

#Plot both data on same plot and Shift stock prices back one day
plot(df2$date,df2$meanSentiment, type="l", col="red3",  xlab='Date', ylab='Mean Sentiment Score')
par(new=TRUE)

plot(amzn2$Date,amzn2$zScore, type="1", axes=F, xlab=NA, ylab=NA, col="blue")
axis(side = 4)
mtext(side = 4, line = 3, 'Closing Stock Price z-Score')
legend("topright",
       legend=c("Mean Sentiment Score"),
       lty=c(1,0), col=c("red3"))

# get the emotions using the NRC dictionary
emotions <- get_nrc_sentiment(amzn.tweets$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

# Visualize the emotions from NRC sentiments
plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for Amazon Tweets (04/27 to 05/08)")

##################################### Comparison word cloud #################################
all = c(
  paste(amzn.tweets$text[emotions$anger > 0], collapse=" "),
  paste(amzn.tweets$text[emotions$anticipation > 0], collapse=" "),
  paste(amzn.tweets$text[emotions$disgust > 0], collapse=" "),
  paste(amzn.tweets$text[emotions$fear > 0], collapse=" "),
  paste(amzn.tweets$text[emotions$joy > 0], collapse=" "),
  paste(amzn.tweets$text[emotions$sadness > 0], collapse=" "),
  paste(amzn.tweets$text[emotions$surprise > 0], collapse=" "),
  paste(amzn.tweets$text[emotions$trust > 0], collapse=" ")
)
all <- removeWords(all, stopwords("english"))

# create corpus
corpus = Corpus(VectorSource(all))
View(all)

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdm1 <- tdm[nchar(rownames(tdm)) < 11,]

# add column names
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdm1) <- colnames(tdm)
comparison.cloud(tdm1, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)

dtm <- DocumentTermMatrix(cleanTweets)
dtm

tdm <- TermDocumentMatrix(docs)
tdm

# Frequency
freqq <- sort(colSums(as.matrix(all)), decreasing=TRUE)
wf <- data.frame(word=names(freq), freqq=freq)
# Plot Histogram
subset(wf, freq>250)    %>%
  ggplot(aes(word, freqq)) +
  geom_bar(stat="identity", fill="darkred", colour="darkgreen") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
###################################################################### **WALT DISNEY** #################################################################################
################# Data files #########################################

dis.tweets = read.csv("disneypro.csv")
View(dis.tweets)
str(dis.tweets)

dis.stock = read.csv("dis.stock.csv")
View(dis.stock)
names(dis.stock)
str(dis.stock)

######################## Data Cleaning ###############################

#Create subset of data
dis <- subset(dis.stock, select = c(Date, Close))

#Convert to dataframe and encode to native
dis.tweets$text = as.character(dis.tweets$text)
dis.tweets$text <- enc2native(dis.tweets$text)

#Clean text
dis.tweets$text <- gsub("^[[:space:]]*","",dis.tweets$text) # Remove leading whitespaces
dis.tweets$text <- gsub("[[:space:]]*$","",dis.tweets$text) # Remove trailing whitespaces
dis.tweets$text <- gsub(" +"," ",dis.tweets$text) #Remove extra whitespaces
dis.tweets$text <- gsub("'", "%%", dis.tweets$text) #Replace apostrophes with %%
dis.tweets$text <- iconv(dis.tweets$text, "latin1", "ASCII", sub="") # Remove emojis
dis.tweets$text <- gsub("<(.*)>", "", dis.tweets$text) #Remove Unicodes like <U+A>
dis.tweets$text <- gsub("\\ \\. ", " ", dis.tweets$text) #Replace orphaned fullstops with space
dis.tweets$text <- gsub("  ", " ", dis.tweets$text) #Replace double space with single space
dis.tweets$text <- gsub("%%", "\'", dis.tweets$text) #Change %% back to apostrophes
dis.tweets$text <- gsub("https(.*)*$", "", dis.tweets$text) #Remove tweet URL
dis.tweets$text <- gsub("\\n", "-", dis.tweets$text) #Replace line breaks with "-"
dis.tweets$text <- gsub("--", "-", dis.tweets$text) #Remove double "-" from double line breaks
dis.tweets$text <- gsub("&amp;", "&", dis.tweets$text) #Fix ampersand &
dis.tweets$text[dis.tweets$text == " "] <- "<no text>"
dis.tweets$text <- str_replace_all(dis.tweets$text, "[^[:alnum:]]", " ")
dis.tweets$text <- str_replace_all(dis.tweets$text, "[[:punct:]]", " ")
dis.tweets$text <- gsub(pattern = '([[:upper:]])', perl = TRUE, replacement = '\\L\\1', dis.tweets$text) # convert to lower case
dis.tweets$text <- gsub('[[:digit:]]+', '', dis.tweets$text) # Remove digits
dis.tweets$text <- gsub("\\W*\\b\\w\\b\\W*", " ", dis.tweets$text) # remove single characters

#Select clean tweets 
discleanTweets <- dis.tweets %>% 
  select("text")
View(discleanTweets)

#################################### Sentiment Analysis #######################################

#Sentiment using afinn 
sentiment4 <- get_sentiment(dis.tweets$text,method = "afinn")
View(sentiment4)

#View sentiment direction (i.e. positive, neutral and negative)
sentiment5 = convertToDirection(sentiment4)
View(sentiment5)

#Extract and convert 'date' column
date1 <- dis.tweets$created_at
date1 <- as.Date(date1, format = "%m/%d/%y")
View(date1)

#Create new dataframe 
df5 <- cbind(discleanTweets, sentiment4, sentiment5, date1)
#Remove rows with NA
df5 <- df5[complete.cases(df5), ]
View(df5)

#Calculate the average of daily sentiment score
df6 <- df5 %>% 
  group_by(date1) %>%
  summarize(meanSentiment = mean(sentiment4, na.rm=TRUE))

DT::datatable(df6, editable = TRUE)

#Get frquency of each sentiment i.e. positive, neutral, and negative  
freq5 <- df5 %>% 
  group_by(date1,sentiment5) %>% 
  summarise(Freq=n())

#Convert data from long to wide
freq6 <- freq5 %>% 
  spread(key = sentiment5, value = Freq)

DT::datatable(freq6, editable = TRUE)

ggplot() + 
  geom_bar(mapping = aes(x = freq5$date1, y = freq5$Freq, fill = freq5$sentiment5), stat = "identity") +
  ylab('Sentiment Frequency') +
  xlab('April 27th to May 1st  ----  May 4th to May 8th')

#Calculate z-Scores of Disney's closing stock prices
mu1 <- mean(dis$Close)
sd1 <- sd(dis$Close)
dis2 <- dis %>% 
  mutate(zScore = (dis$Close-mu1)/sd1)

#Plot mean sentiment scores
p3<- ggplot(data=df6, aes(x=date1,y=meanSentiment, group=1)) +
  geom_line()+
  geom_point() +
  ylab("Mean Twitter Sentiment Score")

#plot Disney z-score prices
p4 <- ggplot(data=dis2, aes(x=Date,y=zScore, group=1)) +
  geom_line()+
  geom_point() +
  ylab("Z-Score of closing stock price")
scale_x_date(date_breaks = "1 day", 
             limits = as.Date(c('2020-04-27','2020-05-08')))

plot3 <- p3
plot4 <- p4
grid.arrange(plot3, plot4, nrow=2)

#Plot both data on same plot and Shift stock prices back one day
plot(df6$date1,df6$meanSentiment, type="l", col="red3",  xlab='Date', ylab='Mean Sentiment Score')
par(new=TRUE)

plot(dis2$Date,dis2$zScore, type="1", axes=F, xlab=NA, ylab=NA, col="blue")
axis(side = 4)
mtext(side = 4, line = 3, 'Closing Stock Price z-Score')
legend("topright",
       legend=c("Mean Sentiment Score"),
       lty=c(1,0), col=c("red3"))

# get the emotions using the NRC dictionary
emotions1 <- get_nrc_sentiment(dis.tweets$text)
emo_bar1 = colSums(emotions1)
emo_sum1 = data.frame(count=emo_bar1, emotion=names(emo_bar1))
emo_sum1$emotion = factor(emo_sum1$emotion, levels=emo_sum1$emotion[order(emo_sum1$count, decreasing = TRUE)])

# Visualize the emotions from NRC sentiments
plot_ly(emo_sum1, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of Emotion Categories for Disney Tweets (04/27 to 05/08)")

##################################### Comparison word cloud #################################
all1 = c(
  paste(dis.tweets$text[emotions1$anger > 0], collapse=" "),
  paste(dis.tweets$text[emotions1$anticipation > 0], collapse=" "),
  paste(dis.tweets$text[emotions1$disgust > 0], collapse=" "),
  paste(dis.tweets$text[emotions1$fear > 0], collapse=" "),
  paste(dis.tweets$text[emotions1$joy > 0], collapse=" "),
  paste(dis.tweets$text[emotions1$sadness > 0], collapse=" "),
  paste(dis.tweets$text[emotions1$surprise > 0], collapse=" "),
  paste(dis.tweets$text[emotions1$trust > 0], collapse=" ")
)
all1 <- removeWords(all1, stopwords("english"))

# create corpus
corpus1 = Corpus(VectorSource(all1))

# create term-document matrix
tdm4 = TermDocumentMatrix(corpus1)

# convert as matrix
tdm4 = as.matrix(tdm4)
tdm5 <- tdm4[nchar(rownames(tdm4)) < 11,]

# add column names
colnames(tdm4) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdm5) <- colnames(tdm4)
comparison.cloud(tdm5, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)

####################################################### **EXXON MOBIL** #############################################################################
################# Data files ##########################

ex.tweets = read.csv("exxonpro.csv")
View(ex.tweets)
str(ex.tweets)

ex.stock = read.csv("ex.stock.csv")
View(ex.stock)
names(ex.stock)
str(ex.stock)

######################## Data Cleaning ################

#Create subset of data
ex <- subset(ex.stock, select = c(Date, Close))

#Convert to dataframe and encode to native
ex.tweets$text = as.character(ex.tweets$text)
ex.tweets$text <- enc2native(ex.tweets$text)

#Clean text
ex.tweets$text <- gsub("^[[:space:]]*","",ex.tweets$text) # Remove leading whitespaces
ex.tweets$text <- gsub("[[:space:]]*$","",ex.tweets$text) # Remove trailing whitespaces
ex.tweets$text <- gsub(" +"," ",ex.tweets$text) #Remove extra whitespaces
ex.tweets$text <- gsub("'", "%%", ex.tweets$text) #Replace apostrophes with %%
ex.tweets$text <- iconv(ex.tweets$text, "latin1", "ASCII", sub="") # Remove emojis
ex.tweets$text <- gsub("<(.*)>", "", ex.tweets$text) #Remove Unicodes like <U+A>
ex.tweets$text <- gsub("\\ \\. ", " ", ex.tweets$text) #Replace orphaned fullstops with space
ex.tweets$text <- gsub("  ", " ", ex.tweets$text) #Replace double space with single space
ex.tweets$text <- gsub("%%", "\'", ex.tweets$text) #Change %% back to apostrophes
ex.tweets$text <- gsub("https(.*)*$", "", ex.tweets$text) #Remove tweet URL
ex.tweets$text <- gsub("\\n", "-", ex.tweets$text) #Replace line breaks with "-"
ex.tweets$text <- gsub("--", "-", ex.tweets$text) #Remove double "-" from double line breaks
ex.tweets$text <- gsub("&amp;", "&", ex.tweets$text) #Fix ampersand &
ex.tweets$text[ex.tweets$text == " "] <- "<no text>"
ex.tweets$text <- str_replace_all(ex.tweets$text, "[^[:alnum:]]", " ")
ex.tweets$text <- str_replace_all(ex.tweets$text, "[[:punct:]]", " ")
ex.tweets$text <- gsub(pattern = '([[:upper:]])', perl = TRUE, replacement = '\\L\\1', ex.tweets$text) # convert to lower case
ex.tweets$text <- gsub('[[:digit:]]+', '', ex.tweets$text) # Remove digits
ex.tweets$text <- gsub("\\W*\\b\\w\\b\\W*", " ", ex.tweets$text) # remove single characters

#Select clean tweets 
excleanTweets <- ex.tweets %>% 
  select("text")
View(excleanTweets)

#################################### Sentiment Analysis #######################################

#Sentiment using Bing 
sentiment8 <- get_sentiment(ex.tweets$text,method = "afinn")
View(sentiment8)

#View sentiment direction (i.e. positive, neutral and negative)
sentiment9 = convertToDirection(sentiment8)
View(sentiment9)

#Extract and convert 'date' column
date2 <- ex.tweets$created_at
date2 <- as.Date(date2, format = "%m/%d/%y")
View(date2)

# get the emotions using the NRC dictionary
emotions2 <- get_nrc_sentiment(ex.tweets$text)
emo_bar2 = colSums(emotions2)
emo_sum2 = data.frame(count=emo_bar2, emotion=names(emo_bar2))
emo_sum2$emotion = factor(emo_sum2$emotion, levels=emo_sum2$emotion[order(emo_sum2$count, decreasing = TRUE)])

# Visualize the emotions from NRC sentiments
plot_ly(emo_sum2, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for Exxon Mobile Tweets (04/27 to 05/08)")

#Create new dataframe 
df7 <- cbind(excleanTweets, sentiment8, sentiment9, date2)
#Remove rows with NA
df7 <- df7[complete.cases(df7), ]
View(df7)

#Calculate the average of daily sentiment score
df8 <- df7 %>% 
  group_by(date2) %>%
  summarize(meanSentiment = mean(sentiment8, na.rm=TRUE))

DT::datatable(df8, editable = TRUE)

#Get frquency of each sentiment i.e. positive, neutral, and negative  
freq7 <- df7 %>% 
  group_by(date2,sentiment9) %>% 
  summarise(Freq=n())

#Convert data from long to wide
freq8 <- freq7 %>% 
  spread(key = sentiment9, value = Freq)

DT::datatable(freq8, editable = TRUE)

ggplot() + 
  geom_bar(mapping = aes(x = freq7$date2, y = freq7$Freq, fill = freq7$sentiment9), stat = "identity") +
  ylab('Sentiment Frequency') +
  xlab('April 27th to May 1st  ----  May 4th to May 8th')

#Calculate z-Scores of EXXON MOBIL's closing stock prices
mu2 <- mean(ex$Close)
sd2 <- sd(ex$Close)
ex2 <- ex %>% 
  mutate(zScore = (ex$Close-mu2)/sd2)

#Plot mean sentiment scores
p7<- ggplot(data=df8, aes(x=date2,y=meanSentiment, group=1)) +
  geom_line()+
  geom_point() +
  ylab("Mean Twitter Sentiment Score")

#plot Exxon z-score prices
p8 <- ggplot(data=ex2, aes(x=Date,y=zScore, group=1)) +
  geom_line()+
  geom_point() +
  ylab("Z-Score of closing stock price")
scale_x_date(date_breaks = "1 day", 
             limits = as.Date(c('2020-04-27','2020-05-08')))

plot7 <- p7
plot8 <- p8
grid.arrange(plot7, plot8, nrow=2)

#Plot both data on same plot and Shift stock prices back one day
plot(df8$date2,df8$meanSentiment, type="l", col="red3",  xlab='Date', ylab='Mean Sentiment Score')
par(new=TRUE)

plot(ex2$Date,ex2$zScore, type="1", axes=F, xlab=NA, ylab=NA, col="blue")
axis(side = 4)
mtext(side = 4, line = 3, 'Closing Stock Price z-Score')
legend("topright",
       legend=c("Mean Sentiment Score"),
       lty=c(1,0), col=c("red3"))

##################################### Comparison word cloud #################################
all2 = c(
  paste(ex.tweets$text[emotions2$anger > 0], collapse=" "),
  paste(ex.tweets$text[emotions2$anticipation > 0], collapse=" "),
  paste(ex.tweets$text[emotions2$disgust > 0], collapse=" "),
  paste(ex.tweets$text[emotions2$fear > 0], collapse=" "),
  paste(ex.tweets$text[emotions2$joy > 0], collapse=" "),
  paste(ex.tweets$text[emotions2$sadness > 0], collapse=" "),
  paste(ex.tweets$text[emotions2$surprise > 0], collapse=" "),
  paste(ex.tweets$text[emotions2$trust > 0], collapse=" ")
)
all2 <- removeWords(all2, stopwords("english"))

# create corpus
corpus2 = Corpus(VectorSource(all2))

# create term-document matrix
tdm7 = TermDocumentMatrix(corpus2)

# convert as matrix
tdm7 = as.matrix(tdm7)
tdm8 <- tdm7[nchar(rownames(tdm7)) < 11,]

# add column names
colnames(tdm7) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdm8) <- colnames(tdm7)
comparison.cloud(tdm8, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)
