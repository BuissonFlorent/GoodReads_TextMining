### GR_MachineLearning.R

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tm)
library(magrittr)
library(textcat)
library(tidytext)
library(RTextTools)

library(e1071)
library(caret)
library(klaR)

#For the LDA analysis
library("NLP")
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")
library("stringr")
library("MASS") #for the LDA analysis
require("ggplot2") 
require("scales")
require("gridExtra")
library(randomForest)

# For the XGBoost analysis
library(xgboost)
library(ROCR)
library(Ckmeans.1d.dp)

### <- %>%

### General Setup ###

setwd("C:/Users/Florent/Desktop/Data_analysis_applications/GoodReads_TextMining")
data=read.csv("GoodReadsCleanData.csv", stringsAsFactors = FALSE)

set.seed(1234)
# Creating the outcome value
data$good_read=0
data$good_read[data$rating==4|data$rating==5]=1
sampleIdx <- createDataPartition(data$good_read, p = .2,
                              list = FALSE,
                              times = 1)
sample = data[sampleIdx,]
trainIdx <- createDataPartition(sample$good_read, p = .8,
                                  list = FALSE,
                                  times = 1)
train=sample[trainIdx,]
test=sample[-trainIdx,]


### Creating the Document-Term Matrix ###

# Creating a DTM for the negative reviews
bad_sparsity=.95
bad_dtm = create_matrix(train$review[train$good_read==0], language="english", 
                    removeStopwords=FALSE, removeNumbers=TRUE, 
                    stemWords=FALSE, removeSparseTerms = bad_sparsity) 
bad_dtm_df = as.data.frame(as.matrix(bad_dtm), 
                                   row.names = train$review_id[train$good_read==0])
# Creating a DTM for the positive reviews
good_sparsity=.95
good_dtm = create_matrix(train$review[train$good_read==1], language="english", 
                         removeStopwords=FALSE, removeNumbers=TRUE, 
                         stemWords=FALSE, removeSparseTerms = good_sparsity) 
good_dtm_df = as.data.frame(as.matrix(good_dtm), 
                                    row.names = train$review_id[train$good_read==1])
# Joining the two DTM together
train_dtm_df = bind_rows(bad_dtm_df,good_dtm_df)
train_dtm_df$review_id = c(train$review_id[train$good_read==0],train$review_id[train$good_read==1])
train_dtm_df = arrange(train_dtm_df, review_id)
train_dtm_df$good_read=train$good_read

train_dtm_df = train %>%
  dplyr::select(-c(book,rating,review,good_read)) %>%
  inner_join(train_dtm_df, by = "review_id") %>%
  dplyr::select(-review_id)

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
  dplyr::select(-c(book,rating,review,good_read)) %>%
  inner_join(test_dtm_df, by = "review_id") %>%
  dplyr::select(-review_id)


# Ensuring that the test DTM has the same columns as the train dataset
new_col_names = intersect(colnames(train_dtm_df),colnames(test_dtm_df))
test_dtm_df = data.table(head(bind_rows(test_dtm_df, train_dtm_df[1,]),-1))
test_dtm_df = as.data.frame(test_dtm_df %>% 
                              dplyr::select(one_of(colnames(train_dtm_df))))
test_dtm_df[is.na(test_dtm_df)] = 0

##### Black box approaches: Random Forests and XGBoost #####

test=data.table(test)
baseline_acc=sum(test$good_read=="1")/nrow(test)

### XGBoost ### 

#train_dtm_df$good_read=as.numeric(levels(train_dtm_df$good_read))[train_dtm_df$good_read]
XGB_train=as.matrix(dplyr::select(train_dtm_df,-good_read),
                    dimnames=dimnames(train_dtm_df))
XGB_test=as.matrix(dplyr::select(test_dtm_df,-good_read),
                   dimnames=dimnames(test_dtm_df))
XGB_model <- xgboost(data=XGB_train, label=train_dtm_df$good_read, 
                     nrounds=100, objective="binary:logistic")

XGB_predict = predict(XGB_model,XGB_test)

XGB_results=data.frame(good_read=test$good_read,pred=XGB_predict)

plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$good_read == 1, "TP", v)
  v <- ifelse(df$pred >= threshold & df$good_read == 0, "FP", v)
  v <- ifelse(df$pred < threshold & df$good_read == 1, "FN", v)
  v <- ifelse(df$pred < threshold & df$good_read == 0, "TN", v)
  
  df$pred_type <- v
  
  ggplot(data=df, aes(x=good_read, y=pred)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}

plot_pred_type_distribution(XGB_results,0.7)
XGB_table=table(true=XGB_results$good_read, pred=as.integer(XGB_results$pred >=0.80))

ROCR_pred <- prediction(XGB_results$pred,XGB_results$good_read)
ROCR_perf <- performance(ROCR_pred, 'tnr','fnr') 
plot(ROCR_perf, colorize = TRUE)

XGB_acc=sum(diag(XGB_table))/nrow(test)

# 0.8 seems to be a good threshold to catch a significant
# percentage of negative reviews (~50%)

### Feature analysis with XGBoost
names <- colnames(test_dtm_df)
importance_matrix <- xgb.importance(names, model = XGB_model)
xgb.plot.importance(importance_matrix[1:30,])

### Random Forest ###
train_dtm_df=data.frame(train_dtm_df)
train_dtm_df$good_read=as.factor(train_dtm_df$good_read)
RF_model <- randomForest(good_read ~ ., train_dtm_df, 
                         importance = TRUE, ntree=100)

test_dtm_df=data.frame(test_dtm_df)
test_dtm_df$good_read=as.factor(test_dtm_df$good_read)
RF_predict=predict(RF_model,test_dtm_df)
RF_predict=as.numeric(levels(RF_predict))[RF_predict]

RF_table=table(pred=RF_predict,true=test$good_read)
RF_acc=sum(diag(RF_table))/nrow(test)


### Comparison of RF and XGB predictions ### 

comparison=data.frame(XGB=as.numeric((XGB_results$pred >=0.8)), RF=RF_predict, good_read=XGB_results$good_read)
ftable(comparison)


### Support vector machine ###

SVM_model <- svm(good_read~., data=train_dtm_df)
SVM_predict = predict(SVM_model, dplyr::select(test_dtm_df,-good_read))
SVM_table=table(pred=SVM_predict,true=test_dtm_df$good_read)
SVM_table
SVM_acc=sum(diag(SVM_table))/nrow(test)


### Ensembling ###
Predictions = data.frame(XGB = as.numeric(XGB_predict>=0.8), 
                         RF = RF_predict, 
                         SVM = as.numeric(levels(SVM_predict))[SVM_predict])
Predictions_m=as.matrix(Predictions)

mode_f <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

consensus=apply(Predictions_m,1,mode_f)

Predictions$consensus=consensus

Consensus_table=table(pred=Predictions$consensus,true=test_dtm_df$good_read)
Consensus_acc=sum(diag(Consensus_table))/nrow(test)


Min=apply(Predictions_m,1,min)
Predictions$Min=Min

Min_table=table(pred=Predictions$Min,true=test_dtm_df$good_read)
Min_acc=sum(diag(Min_table))/nrow(test)