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

setwd("C:/Users/Florent/Desktop/Data_analysis_applications/GoodReads_TextMining")
data=read.csv("GoodReadsCleanData.csv", stringsAsFactors = FALSE)

# Creating a dataset with predictive features







# Creating training and testing subsamples
set.seed(1234)
N_train=1000
N_test=100
N=N_train+N_test
Subset_index=createDataPartition(data$rating, p=N/nrow(data), list = FALSE)
Subset=data[Subset_index,]
Train=Subset[1:N_train,]
Test=Subset[(N_train+1):nrow(Subset),]












# Creating the Document-Term Matrix



sparsity=.99
dtm = create_matrix(data$review[1:(train+test)], language="english", 
                    removeStopwords=FALSE, removeNumbers=TRUE, 
                    stemWords=FALSE, removeSparseTerms = sparsity) 

mat_dtm = as.matrix(dtm)
dim(mat_dtm)
mat_rating=as.matrix(as.numeric(data$rating[1:(train+test)]))

# Naive Bayes predictor
Bayes_classifier = naiveBayes(mat_dtm[1:train,], as.factor(mat_rating[1:train,]))
# testing the validity
Bayes_predicted = predict(Bayes_classifier, mat_dtm[(train+1):(train+test),])
table_bayes = table(mat_rating[(train+1):(train+test),], Bayes_predicted)

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
  table = table(mat_rating[(train+1):(train+test),], result[,label])
  acc = sum(diag(table))/sum(table)
  #assign(mod,acc)
  accuracy$values[accuracy$models==mod]=acc
}
