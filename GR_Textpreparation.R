### GR_EDA_SentimentAnalysis.R

### Setup and data preparation

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tm)
library(magrittr)
library(textcat)
library(tidytext)
library(RTextTools)

data = read.csv("GoodReadsData.csv")
  
# Selecting only the reviews in english
data$language=as.factor(textcat(data$review))
data=data[language=="english"]

# Removing non-standard ratings
data=data[rating %in% c('did not like it','it was ok','liked it','really liked it','it was amazing')]

# Filtering by review length
data=data[nchar(data$review)>=5]

#Recoding the ratings
data$rating[data$rating=='did not like it']=1
data$rating[data$rating=='it was ok']=2
data$rating[data$rating=='liked it']=3
data$rating[data$rating=='really liked it']=4
data$rating[data$rating=='it was amazing']=5
data$rating=as.integer(data$rating)

### Exploratory data analysis ### 

#Looking at the distribution of ratings
barplot(table(as.factor(data$rating)),ylim = c(0,5000), main = "Distribution of ratings")

#Looking at the distribution of review lengths, and removing the longest reviews
hist(nchar(data$review), ylim = c(0,5000), main = "Distribution of review length" )
n=nrow(data[nchar(data$review)>=8000])
#Removing the reviews with more than 8000 characters
data=data[nchar(data$review)<=8000]
hist(nchar(data$review), ylim = c(0,3000), main = "Distribution of review length" )

# Boxplot of review length by rating
l=nchar(data$review)
boxplot(l~data$rating, main = "Distribution of review length by rating")


### Sentiment analysis with tidytext ###

# Loading the sentiment analysis lexicon
AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)
head(AFINN)

# "tidying" up the data (1 word per row)
review_words <- data %>%
  select(-c(language, author)) %>%
  unnest_tokens(word, review) %>%
  filter(!word %in% stop_words$word)

# "lookup" the words in the sentiment lexicon and grouping by mean for observation 
review_mean_sentiment <- review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(obs, rating) %>%
  summarize(sentiment = mean(afinn_score))
# Plotting the result
theme_set(theme_bw())
ggplot(review_mean_sentiment, aes(rating, sentiment, group = rating)) +
  geom_boxplot() +
  ylab("Average sentiment score")

# Same as previous, but with the median
review_median_sentiment <- review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(obs, rating) %>%
  summarize(sentiment = median(afinn_score))
theme_set(theme_bw())
ggplot(review_median_sentiment, aes(rating, sentiment, group = rating)) +
  geom_boxplot() +
  ylab("Median sentiment score")


# Summarizing words by number of uses and average rating of review
word_mean_summaries <- review_words %>%
  count(obs, rating, word) %>%
  group_by(word) %>%
  summarize(reviews = n(),
            uses = sum(n),
            average_rating = mean(rating)) %>%
  filter(reviews >= 3) %>%
  arrange(average_rating)

# comparing AFINN score with average rating of reviews, per word
words_mean_afinn <- word_mean_summaries %>%
  inner_join(AFINN)
ggplot(words_mean_afinn, aes(afinn_score, average_rating, group = afinn_score)) +
  geom_boxplot() +
  xlab("AFINN score of word") +
  ylab("Mean rating of reviews with this word")