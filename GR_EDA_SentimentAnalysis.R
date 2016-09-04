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

data <- read.csv("GoodReadsData.csv", stringsAsFactors = FALSE)
data <- data.table(data)
  
# Selecting only the reviews in english
data$language <- as.factor(textcat(data$review))
data <- data[language == "english"]

# Removing non-standard ratings
data <- data[rating %in% c('did not like it',
                           'it was ok',
                           'liked it',
                           'really liked it',
                           'it was amazing')]

# Filtering by review length
data <- data[nchar(data$review) >= 5]

#Recoding the ratings
data$rating[data$rating == 'did not like it'] <- 1
data$rating[data$rating == 'it was ok'      ] <- 2
data$rating[data$rating == 'liked it'       ] <- 3
data$rating[data$rating == 'really liked it'] <- 4
data$rating[data$rating == 'it was amazing' ] <- 5
data$rating <- as.integer(data$rating)

# Removing the language and author variables and adding a review_id column
data$language <- NULL
data$reviewer <- NULL
data$review.id <- 1:nrow(data)

### Exploratory data analysis ### 

#Looking at the distribution of ratings
barplot(table(as.factor(data$rating)),
        ylim = c(0,5000),
        main = "Distribution of ratings")

#Looking at the distribution of review lengths, and removing the longest reviews
data$review.length <- nchar(data$review)
hist(data$review.length, 
     ylim = c(0,5000), 
     main = "Distribution of review length" )
n <- nrow(data[data$review.length>=8000])
#Removing the reviews with more than 8000 characters
data <- data[data$review.length <= 8000]
hist(data$review.length, 
     ylim = c(0,3000), 
     main = "Distribution of review length" )

# Boxplot of review length by rating
with(data, boxplot(review.length~rating, 
                   main = "Distribution of review length by rating"))


### Sentiment analysis with tidytext ###

# Loading the first sentiment score lexicon
AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn.score = score)
head(AFINN)

# Loading the second sentiment score lexicon
Bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(word, bing.sentiment = sentiment)
head(Bing)


# "tidying" up the data (1 word per row) and adding the sentiment scores for each word
review.words <- data %>%
  unnest_tokens(word, review) %>%
  select(-c(book, review.length)) %>%
  left_join(AFINN, by = "word") %>%
  left_join(Bing, by = "word")

# Grouping by mean for observation 
review.mean.sentiment <- review.words %>%
  group_by(review.id, rating) %>%
  summarize(mean.sentiment = mean(afinn.score, na.rm = TRUE))
# Plotting the result
theme_set(theme_bw())
ggplot(review.mean.sentiment, aes(rating, mean.sentiment, group = rating)) + 
  geom_boxplot() +
  ylab("Average sentiment score")
# Transferring the results to our dataset
review.mean.sentiment <- review.mean.sentiment %>%
  select(-rating) %>%
  data.table()
clean.data <- data %>%
  left_join(review.mean.sentiment, by = "review.id")


# Same as previous, but with the median
review.median.sentiment <- review.words %>%
  group_by(review.id, rating) %>%
  summarize(median.sentiment = median(afinn.score, na.rm = TRUE))
theme_set(theme_bw())
ggplot(review.median.sentiment, aes(rating, median.sentiment, group = rating)) +
  geom_boxplot() +
  ylab("Median sentiment score")
# Transferring the results to our dataset
review.median.sentiment <- review.median.sentiment %>%
  select(-rating) %>%
  data.table()
clean.data <- clean.data %>%
  left_join(review.median.sentiment, by = "review.id")


# Counting the number of negative words per review according to AFINN lexicon
review.count.afinn.negative <- review.words %>%
  filter(afinn.score < 0) %>%
  group_by(review.id, rating) %>%
  summarize(count.afinn.negative = n())
# Transferring the results to our dataset
review.count.afinn.negative <- review.count.afinn.negative %>%
  select(-rating) %>%
  data.table()
clean.data <- clean.data %>%
  left_join(review.count.afinn.negative, by = "review.id")

# Counting the number of positive words per review according to AFINN lexicon
review.count.afinn.positive <- review.words %>%
  filter(afinn.score > 0) %>%
  group_by(review.id, rating) %>%
  summarize(count.afinn.positive = n())
# Transferring the results to our dataset
review.count.afinn.positive <- review.count.afinn.positive %>%
  select(-rating) %>%
  data.table()
clean.data <- clean.data %>%
  left_join(review.count.afinn.positive, by = "review.id")

# Counting the number of negative words per review according to Bing lexicon
review.count.bing.negative <- review.words %>%
  filter(bing.sentiment == "negative") %>%
  group_by(review.id, rating) %>%
  summarize(count.bing.negative = n())
# Transferring the results to our dataset
review.count.bing.negative <- review.count.bing.negative %>%
  select(-rating) %>%
  data.table()
clean.data <- clean.data %>%
  left_join(review.count.bing.negative, by = "review.id")

# Counting the number of positive words per review according to Bing lexicon
review.count.bing.positive <- review.words %>%
  filter(bing.sentiment == "positive") %>%
  group_by(review.id, rating) %>%
  summarize(count.bing.positive = n())
# Transferring the results to our dataset
review.count.bing.positive <- review.count.bing.positive %>%
  select(-rating) %>%
  data.table()
clean.data <- clean.data %>%
  left_join(review.count.bing.positive, by = "review.id")

# Writing the data to file for future analyses
write.csv(clean.data, "GoodReadsCleanData.csv", row.names = FALSE)

# Summarizing words by number of uses and average rating of review
word.mean.summaries <- review.words %>%
  count(review.id, rating, word) %>%
  group_by(word) %>%
  summarize(reviews = n(),
            uses = sum(n),
            average.rating = mean(rating)) %>%
  filter(reviews >= 3) %>%
  arrange(average.rating)

# comparing AFINN score with average rating of reviews, per word
word.mean.afinn <- word.mean.summaries %>%
  inner_join(AFINN)
ggplot(word.mean.afinn, aes(afinn.score, average.rating, group = afinn.score)) +
  geom_boxplot() +
  xlab("AFINN score of word") +
  ylab("Mean rating of reviews with this word")
