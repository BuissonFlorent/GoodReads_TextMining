### GR_Textpreparation.R

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tm)
library(magrittr)
library(textcat)
library(tidytext)
library(RTextTools)

# %>%  <-

##### Text preparation #####

data = read.csv("GR_MeBeforeYou.csv", stringsAsFactors = F)
data=data.table(data)

# Selecting only the reviews in english
data$language=as.factor(textcat(data$review))
data=data[language=="english"]

# Removing non-standard ratings
data=data[rating %in% c('did not like it','it was ok','liked it','really liked it','it was amazing')]

# Filtering by review length
data=data[length(data$review)>=5]

#Recoding the ratings
data$rating[data$rating=='did not like it']=1
data$rating[data$rating=='it was ok']=2
data$rating[data$rating=='liked it']=3
data$rating[data$rating=='really liked it']=4
data$rating[data$rating=='it was amazing']=5
data$rating=as.integer(data$rating)

#Looking at the distribution of ratings
hist(data$rating)


##### Sentiment analysis with tidytext #####

# Loading the sentiment analysis lexicon
AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

# "tidying" up the data (1 word per row)
review_words <- data %>%
  select(-c(language, book)) %>%
  unnest_tokens(word, review) %>%
  filter(!word %in% stop_words$word)

# "lookup" the words in the sentiment lexicon and grouping by review 
review_sentiment <- review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(obs, rating) %>%
  summarize(sentiment = mean(afinn_score))

# Plotting the result
theme_set(theme_bw())
ggplot(review_sentiment, aes(rating, sentiment, group = rating)) +
  geom_boxplot() +
  ylab("Average sentiment score")

with(review_sentiment,plot(rating,sentiment))

# Checking which words appear in positive/negative reviews
review_words_counted <- review_words %>%
  count(obs, rating, word) #%>%
  #ungroup()

# Summarizing words by number of uses and average rating of review
word_summaries <- review_words_counted %>%
  group_by(word) %>%
  summarize(reviews = n(),
            uses = sum(n),
            average_rating = mean(rating)) %>%
  filter(uses >=5 & uses <= 1100) %>%
  arrange(average_rating) #%>%
  #ungroup()

words_afinn <- word_summaries %>%
  inner_join(AFINN)
ggplot(words_afinn, aes(afinn_score, average_rating, group = afinn_score)) +
  geom_boxplot() +
  xlab("AFINN score of word") +
  ylab("Average rating of reviews with this word")


##### Machine learning: predicting a rating from the text of the review #####

# Creating the Document-Term Matrix
train=100
test=50
sparsity=.99
dtm = create_matrix(data$review[1:(train+test)], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE, removeSparseTerms = sparsity) 

mat_dtm = as.matrix(dtm)
dim(mat_dtm)
mat_rating=as.matrix(as.numeric(data$rating[1:(train+test)]))

# # Naive Bayes predictor
# Bayes_classifier = naiveBayes(mat_dtm[1:train,], as.factor(mat_rating[1:train,]))
# # testing the validity
# Bayes_predicted = predict(Bayes_classifier, mat_dtm[(train+1):(train+test),])
# table_bayes = table(mat_rating[(train+1):(train+test),], Bayes_predicted)

# Running other models
container = create_container(dtm, mat_rating,
                             trainSize=1:train, testSize=(train+1):(train+test),virgin=FALSE)



models = c("MAXENT" , "SVM", "RF", "BAGGING", "TREE")
accuracy=data.frame(models=models, values=rep(0,5),sparsity=sparsity, stringsAsFactors = F)

for(mod in models){
  model = train_model(container, algorithm=mod)
  result = classify_model(container, model)
  if(mod=="MAXENT") {label="MAXENTROPY_LABEL"}
  else if(mod=="SVM") {label="SVM_LABEL"}
  else if(mod=="RF") {label="FORESTS_LABEL"}
  else if(mod=="BAGGING") {label="BAGGING_LABEL"}
  else if(mod=="TREE") {label="TREE_LABEL"}
  table = table(mat_rating[(train+1):(train+test),], results[,label])
  acc = sum(diag(table))/sum(table)
  #assign(mod,acc)
  accuracy$values[accuracy$models==mod]=acc
}


models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", 
                                              "BAGGING", "TREE"))

results = classify_models(container, models)


# accuracy tables
table_maxent = table(mat_rating[(train+1):(train+test),], results[,"MAXENTROPY_LABEL"])
table_svm = table(mat_rating[(train+1):(train+test),], results[,"SVM_LABEL"])
table_forest = table(mat_rating[(train+1):(train+test),], results[,"FORESTS_LABEL"])
table_bagging = table(mat_rating[(train+1):(train+test),], results[,"BAGGING_LABEL"])
table_tree = table(mat_rating[(train+1):(train+test),], results[,"TREE_LABEL"])

# accuracy scores
acc_bayes = sum(diag(table_bayes))/sum(table_bayes)
acc_maxent = sum(diag(table_maxent))/sum(table_maxent)
acc_svm = sum(diag(table_svm))/sum(table_svm)
acc_forest = sum(diag(table_forest))/sum(table_forest)
acc_bagging = sum(diag(table_bagging))/sum(table_bagging)
acc_tree = sum(diag(table_tree))/sum(table_tree)


##### Machine learning: dimensionality reduction #####




##### Code cemetery #####

names(data)[names(data)=="X"]="obs"

# Correcting the author names that were miscategorized -- data table way
to_correct = data[author=="" & regexpr(" marked it | added it ",rating)!=-1]
to_correct[,author:=substr(rating,1,regexpr(" marked it | added it ",rating)-1)]
to_correct[,rating:=substr(rating,regexpr(" marked it | added it ",rating),nchar(rating))]

with(word_summaries, plot(average_rating, uses))