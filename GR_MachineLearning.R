### GR_MachineLearning.R



### Libraries to load
library(data.table)
library(dplyr)
library(caret)
library(RTextTools)
library(xgboost)
library(ROCR)

##### General Setup #####

setwd("C:/Users/Florent/Desktop/Data_analysis_applications/GoodReads_TextMining")
data <- read.csv("GoodReadsCleanData.csv", stringsAsFactors = FALSE)

set.seed(1234)
# Creating the binary outcome feature
data$good.read <- 0
data$good.read[data$rating == 4 | data$rating == 5] <- 1

trainIdx <- createDataPartition(data$good.read, 
                                p = .75,
                                list = FALSE,
                                times = 1)
train <- data[trainIdx, ]
test <- data.table(data[-trainIdx, ])


#### Creating the Document-Term Matrix ####

# Creating a DTM for the negative reviews
sparsity <- .99
bad.dtm <- create_matrix(train$review[train$good.read == 0], 
                         language = "english",
                         removeStopwords = FALSE, 
                         removeNumbers = TRUE,
                         stemWords = FALSE, 
                         removeSparseTerms = sparsity) 
#Converting the DTM in a data frame
bad.dtm.df  <-  data.table(as.matrix(bad.dtm))
# Creating a DTM for the positive reviews
good.dtm <- create_matrix(train$review[train$good.read == 1], 
                          language="english",
                          removeStopwords = FALSE, 
                          removeNumbers = TRUE,
                          stemWords = FALSE, 
                          removeSparseTerms = sparsity) 
good.dtm.df <- data.table(as.matrix(good.dtm))
# Joining the two DTM together
train.dtm.df <- bind_rows(bad.dtm.df, good.dtm.df)
train.dtm.df$review.id <- c(train$review.id[train$good.read == 0],
                            train$review.id[train$good.read == 1])
train.dtm.df <- arrange(train.dtm.df, review.id)
train.dtm.df$good.read  <- train$good.read

train.dtm.df <- train %>%
  select(-c(book, rating, review, good.read)) %>%
  inner_join(train.dtm.df, by = "review.id") %>%
  select(-review.id)

train.dtm.df[is.na(train.dtm.df)] <- 0


# Creating the test DTM
test.dtm <- create_matrix(test$review, 
                          language = "english", 
                          removeStopwords = FALSE, 
                          removeNumbers = TRUE, 
                          stemWords = FALSE, 
                          removeSparseTerms = sparsity) 
test.dtm.df <- data.table(as.matrix(test.dtm))
test.dtm.df$review.id <- test$review.id
test.dtm.df$good.read <- test$good.read

test.dtm.df <- test %>%
  select(-c(book, rating, review, good.read)) %>%
  inner_join(test.dtm.df, by = "review.id") %>%
  select(-review.id)


# Ensuring that the test DTM has the same columns as the train dataset
test.dtm.df <- head(bind_rows(test.dtm.df, train.dtm.df[1, ]), -1)
test.dtm.df <- test.dtm.df %>% 
  select(one_of(colnames(train.dtm.df)))
test.dtm.df[is.na(test.dtm.df)] <- 0
test.dtm.df <- data.table(test.dtm.df)


##### Machine learning algorithm: XGBoost #####

baseline.acc <- sum(test$good.read == "1") / nrow(test)

XGB.train <-  as.matrix(select(train.dtm.df, -good.read),
                        dimnames = dimnames(train.dtm.df))
XGB.test <- as.matrix(select(test.dtm.df, -good.read),
                      dimnames = dimnames(test.dtm.df))
XGB.model <- xgboost(data = XGB.train, 
                     label = train.dtm.df$good.read, 
                     nrounds = 400, 
                     objective = "binary:logistic")

XGB.predict <- predict(XGB.model, XGB.test)

XGB.results <- data.frame(good.read = test$good.read, pred = XGB.predict)

ROCR.pred <- prediction(XGB.results$pred, XGB.results$good.read)
ROCR.perf <- performance(ROCR.pred, 'tnr','fnr') 
plot(ROCR.perf, colorize = TRUE)
XGB.table  <- table(true = XGB.results$good.read, 
                    pred = as.integer(XGB.results$pred >= 0.80))
XGB.table
XGB.acc <- sum(diag(XGB.table)) / nrow(test)

### Feature analysis with XGBoost
names <- colnames(test.dtm.df)
importance.matrix <- xgb.importance(names, model = XGB.model)
xgb.plot.importance(importance.matrix[1:20, ])