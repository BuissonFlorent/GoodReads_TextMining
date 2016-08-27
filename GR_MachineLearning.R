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
data=read.csv("GoodReadsCleanData.csv", stringsAsFactors = FALSE)

set.seed(1234)
# Creating the binary outcome feature
data$good_read=0
data$good_read[data$rating==4|data$rating==5]=1

trainIdx <- createDataPartition(data$good_read, p = .75,
                                  list = FALSE,
                                  times = 1)
train=data[trainIdx,]
test=data.table(data[-trainIdx,])


#### Creating the Document-Term Matrix ####

# Creating a DTM for the negative reviews
bad_sparsity=.95
bad_dtm = create_matrix(train$review[train$good_read==0], language="english", 
                    removeStopwords=FALSE, removeNumbers=TRUE, 
                    stemWords=FALSE, removeSparseTerms = bad_sparsity) 
#Converting the DTM in a data frame
bad_dtm_df = data.table(as.matrix(bad_dtm), 
                                   row.names = train$review_id[train$good_read==0])
# Creating a DTM for the positive reviews
good_sparsity=.95
good_dtm = create_matrix(train$review[train$good_read==1], language="english", 
                         removeStopwords=FALSE, removeNumbers=TRUE, 
                         stemWords=FALSE, removeSparseTerms = good_sparsity) 
good_dtm_df = data.table(as.matrix(good_dtm),row.names = train$review_id[train$good_read==1])
# Joining the two DTM together
train_dtm_df = bind_rows(bad_dtm_df,good_dtm_df)
train_dtm_df$review_id = c(train$review_id[train$good_read==0],train$review_id[train$good_read==1])
train_dtm_df = arrange(train_dtm_df, review_id)
train_dtm_df$good_read=train$good_read

train_dtm_df = train %>%
  select(-c(book,rating,review,good_read)) %>%
  inner_join(train_dtm_df, by = "review_id") %>%
  select(-review_id)

train_dtm_df[is.na(train_dtm_df)] = 0


# Creating the test DTM
test_sparsity=0.95
test_dtm = create_matrix(test$review, language="english", 
                        removeStopwords=FALSE, removeNumbers=TRUE, 
                        stemWords=FALSE, removeSparseTerms = test_sparsity) 
test_dtm_df=data.table(as.matrix(test_dtm))
test_dtm_df$review_id=test$review_id
test_dtm_df$good_read=test$good_read

test_dtm_df = test %>%
  select(-c(book,rating,review,good_read)) %>%
  inner_join(test_dtm_df, by = "review_id") %>%
  select(-review_id)


# Ensuring that the test DTM has the same columns as the train dataset
test_dtm_df = head(bind_rows(test_dtm_df, train_dtm_df[1,]),-1)
test_dtm_df = test_dtm_df %>% 
  select(one_of(colnames(train_dtm_df)))
test_dtm_df[is.na(test_dtm_df)] = 0
test_dtm_df=data.table(test_dtm_df)


##### Machine learning algorithm: XGBoost #####

baseline_acc=sum(test$good_read=="1")/nrow(test)

XGB_train=as.matrix(select(train_dtm_df,-good_read),
                    dimnames=dimnames(train_dtm_df))
XGB_test=as.matrix(select(test_dtm_df,-good_read),
                   dimnames=dimnames(test_dtm_df))
XGB_model <- xgboost(data=XGB_train, label=train_dtm_df$good_read, 
                     nrounds=300, objective="binary:logistic")

XGB_predict = predict(XGB_model,XGB_test)

XGB_results=data.frame(good_read=test$good_read,pred=XGB_predict)
ROCR_pred <- prediction(XGB_results$pred,XGB_results$good_read)
ROCR_perf <- performance(ROCR_pred, 'tnr','fnr') 
plot(ROCR_perf, colorize = TRUE)
XGB_table=table(true=XGB_results$good_read, pred=as.integer(XGB_results$pred >=0.50))
XGB_acc=sum(diag(XGB_table))/nrow(test)

### Feature analysis with XGBoost
names <- colnames(test_dtm_df)
importance_matrix <- xgb.importance(names, model = XGB_model)
xgb.plot.importance(importance_matrix[1:30,])