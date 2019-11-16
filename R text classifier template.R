# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Description: 
# JIRA:  
# Author: Mustafa Ellam
# Comments: 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Tidy text frame 
library(tidytext)
library(tm)
#library(widyr)
library(SnowballC)
library(caret)
library(purrr)


##############################
# CREATE CORPUS 
##############################

# Concatenate labelled dataframe to form doc corpus

df <- list(testdocs, sampledocs)
docCorpus <- do.call(rbind, df)

docCorpus$test <- as.factor(docCorpus$test)

docCorpus$doc_ID <- seq.int(nrow(docCorpus)) # assign id for each doc entry

rm(df)


##############################
# DOC DISTRIBUTION
##############################

# Extract time and day of the week from date/timestamps

library(hms)
docCorpus$timestamp <- as.POSIXct(docCorpus$created_time, format="%H:%M:%S")
docCorpus$time <- hms::hms(second(docCorpus$timestamp), minute(docCorpus$timestamp), hour(docCorpus$timestamp))
docCorpus$time <- as.POSIXct(docCorpus$time)

docCorpus$day <- weekdays(docCorpus$created_date) %>%
  factor(levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# Volume of doc messages over time
docCorpus %>% 
  ggplot() + 
  geom_density( aes(x = created_date, group = test, fill = test), alpha = 0.5) +
  theme_minimal()

# Distribution of doc messages recieved during the day
docCorpus %>% 
  ggplot() + 
  geom_density( aes(x = time, y = ..scaled.., group = test, fill = test), alpha = 0.5) +
  scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%H:%M")) +
  theme_minimal() +
  theme(legend.position=c(0.9, 0.9))

# Distribution of doc messages recieved over day of the week
docCorpus %>% 
  ggplot() + 
  geom_density( aes(x = day, y = ..scaled.., group = test, fill = test), alpha = 0.5) +
  theme_minimal() +
  theme(legend.position=c(0.9, 0.9))


# -- distribution of message length
docCorpus %>% 
  #filter(nchar(message)<10000) %>%
  ggplot() + 
  geom_density( aes(x = nchar(message), group = test, fill = test), alpha = 0.5) +
  #geom_histogram(aes(x = nchar(message), group = test, fill = test), 
  #               position="identity",alpha=0.5, binwidth = 100) +
  theme_minimal() + 
  theme(legend.position=c(0.9, 0.9))

exploredocs <- docCorpus %>%
  #filter(nchar(message)>2000) %>%
  filter(nchar(message)<20) %>%
  select(doc_ID, message, test)

# remove shortest messages 
docCorpus <- docCorpus %>%
  filter(nchar(message)>20) %>%
  select(-timestamp)


# docCorpus composition by class
docCorpus %>%  
  count(test)


##############################
# TOKENISATION
##############################

myCorpus <- Corpus(VectorSource(docCorpus$message))  

# Define text cleaning function

cleanCorpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  #corpus <- tm_map(train_corpus, stemDocument)
  return(corpus)
}

cleanedCorpus <- cleanCorpus(myCorpus) 

# Print out a cleaned up messages
cleanedCorpus[[227]][1]
cleanedCorpus[[15]]
myCorpus$message[227]


# Join cleaned message to the original doc dataframe 

cleanedMessage <-data.frame(text = sapply(cleanedCorpus, as.character), stringsAsFactors = FALSE)
docCorpus <- cbind(docCorpus, cleanedMessage)

rm(myCorpus, cleanedCorpus, cleanedMessage)


# Tokenisation - words

# remove ' from stopwords
cleanStopwords <- stop_words %>%
  mutate(word = gsub("[^[:space:][:alnum:]]","",word))

message_tokens <- docCorpus %>%
  unnest_tokens(word, text, to_lower = TRUE) %>% 
  filter(!str_detect(word, "[^A-Za-z]+")) %>% # remove punctuation, numbers and spaces
  anti_join(cleanStopwords)
# %>% mutate(word = SnowballC::wordStem(word))

# Tockenisation - bigrams

messages_bigrams <- docCorpus %>% 
  unnest_tokens(bigram, text, token ='ngrams', n=2) 


# Tockenisation - word/bigram combined features

message_words_bigrams <- map_df(1:2, ~ unnest_tokens(docCorpus, word, text, to_lower = TRUE, token = 'ngrams', n = .x)) %>%
  anti_join(cleanStopwords, by = "word") 

message_words_bigrams %>%
  group_by(word) %>%
  summarise(n = n()) %>% 
  filter(n >= 10) %>%
  select(word)
#mutate(word = reorder(word, n)) %>%
#top_n(25) %>%


##############################
# Data Exploration
##############################

# Display top n words across the corpus

message_tokens %>%
  count(test, word, sort = TRUE) %>%
  group_by(test) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(n = ifelse(test == 0, -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = test)) +  
  geom_bar(stat = "identity") +
  ylab("Word frequency") +
  xlab("") +
  #ggtitle('Top 25 words for each category') + 
  coord_flip() + 
  theme_minimal() +
  theme(legend.position=c(0.9, 0.2))


messages_bigrams %>%
  filter(test==1) %>%
  count(bigram, sort = TRUE) %>%
  top_n(40) %>%
  mutate(word = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +  
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  ggtitle('Top 25 words for each category') + 
  coord_flip()


# Display top n bigrams across the corpus 

messages_bigrams %>%
  count(test, bigram, sort = TRUE) %>%
  group_by(test) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(n = ifelse(test == 0, -n, n)) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = test)) +  
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  ggtitle('Top 25 phrases for each category') + 
  coord_flip()


# Display top n bigrams/words across the corpus 

message_words_bigrams %>%
  count(test, word, sort = TRUE) %>%
  group_by(test) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(n = ifelse(test == 0, -n, n)) %>%
  mutate(bigram = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = test)) +  
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  ggtitle('Top 25 phrases for each category') + 
  coord_flip()

# facetted charts comparing top words

message_tokens %>%
  count(test, word, sort = TRUE) %>%
  group_by(test) %>%
  arrange(desc(n)) %>%
  top_n(25) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  #mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = test)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~test, scales = "free") +
  coord_flip() +
  #labs(x = NULL, y = "Word count", title = "Most frequent words after removing stop words") +
  theme_minimal()
  
  
# Tf-idf scores by test content 
message_tokens_tfidf <- message_tokens %>%
  count(test, word) %>%
  bind_tf_idf(term = word, document = test, n = n)


labels <- c('0' = 'Other messages', '1' = "test messages")

message_tokens_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(test) %>%
  top_n(25) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = test)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf scores") +
  facet_wrap(~test, scales = "free", labeller=labeller(test = labels)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position='none')

##############################
# Create Document Term Matrix 
##############################

# Create dtm with bag-of-words and tf-idf vectors

dtm_freq <- message_tokens %>%
  count(doc_ID, word) %>% # get count of each token in each document  
  cast_dtm(document = doc_ID, term = word, value = n) 
                                                          
dtm_tfidf <- message_tokens %>%
  count(doc_ID, word) %>%
  bind_tf_idf(term = word, document = doc_ID, n = n) %>%
  cast_dtm(document = doc_ID, term = word, value = tf_idf) 
  #cast_dtm(term = word, document = doc_ID, n = n)#, weighting = tm::weigthTfIdf)


# Check dtm 
dtm <- as.matrix(dtm_freq)
dtm_tidy <- tidy(data_dtm) # convert to tidy df
dim(dtm)
explore_dtm <- dtm[1:100, 126:138]
Terms(dtm_freq)

# Remove sparse features
termsOriginal <- Terms(dtm_freq)
termsReduced <- Terms(removeSparseTerms(dtm_freq, 0.99))

termsRemoved <- setdiff(termsOriginal, termsReduced)

dtm_freq <- removeSparseTerms(dtm_freq, 0.99)

dtm <- as.matrix(dtm_freq)

##############################
# Model Selection
##############################

meta <- tibble(doc_ID = as.numeric(dimnames(dtm_freq)[[1]])) %>%
  left_join(docCorpus[!duplicated(docCorpus$doc_ID), ], by = "doc_ID")

# Look at doc entries that dropped out of Corpus from cleaning/tokenisation stage
docCorpus %>%
  anti_join(meta, by = "doc_ID") %>%
  head(25) %>%
  pull(text)

# Create training and test sets

set.seed(1234)
trainIndex <- createDataPartition(meta$test, p = 0.7, list = FALSE, times = 1)

df_train <- dtm_freq[trainIndex, ] %>% as.matrix() %>% as.data.frame()
df_test <- dtm_freq[-trainIndex, ] %>% as.matrix() %>% as.data.frame()

y_train <- meta$test[trainIndex]
y_test <- meta$test[-trainIndex]


# Benchmark models

control <- trainControl(method="repeatedcv", number=5, repeats=5)
x <- df_train
y <- y_train
  
set.seed(7)
model_rf <- train(x=x, y=y, method="rf", ntree= 100, trControl=control)
model_logit <- train(x=x, y=y, method="LogitBoost", trControl=control)
model_nb <- train(x=x, y=y, method="naive_bayes", trControl=control)
model_svm <- train(x=x, y=y, method="svmLinearWeights2", trControl=control)
model_dtree <- train(x=x, y=y, method = "rpart", trControl=control)
                  
# Collect banchmark results
results <- resamples(list(rf=model_rf, logit=model_logit, svm=model_svm, nb=model_nb, dtree=model_dtree))
summary(results)

# Visualise boxplot of results
bwplot(results)

# Model Corerlation
modelCor(results)


library(rpart.plot)
prp(model_dtree$finalModel, box.palette="RdBu", shadow.col="gray")



##############################
# Model Tuning
##############################

## 1. Support Vector Machine

# -- Grid search
control <- trainControl(search = 'grid')

model_svm_final <- train(x = df_train, y =  as.factor(y_train),
                 method = "svmLinearWeights2",
                 trControl = control,
                 tuneGrid = data.frame(cost = 0.01, 
                                       Loss = 0, 
                                       weight = seq(0.5, 1.5, 0.1))
                 )

model_svm_final$bestTune
model_svm_final$finalModel

plot(model_svm_final)
# model_svm performs automatically optimised across the set of parameters by default
# model_svm has therefore already been optimised through a random search

## 2. Random Forest 

# Random Search on mtry parameter 
control <- trainControl(method = "repeatedcv", number = 5, repeats = 1, search = "random", verboseIter = TRUE)

system.time({
  model_rf_tune <- train(x = df_train, y = as.factor(y_train),
                    method = "rf", 
                    ntree = 100,
                    trControl = control)
})

model_rf_tune$finalModel 
plot(model_rf_tune$err.rate)

plot(model_rf_tune)
# Best mtry parameter ~300


# Grid Search optimisation using custom RF algorithm

# Custom random forest algorithm to tune multiple parameters; mtry and ntree
library(randomForest)
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


# train model (note: tuning time ~1.5 hrs) 
control <- trainControl(method="repeatedcv", number=5, repeats=3, verboseIter = TRUE)
tunegrid <- expand.grid(.mtry=c(200,250,300,350), .ntree=c(100, 500, 750, 1000))

set.seed(1234)
system.time({
  model_rf_custom <- train(x = df_train, y = as.factor(y_train), 
                        method=customRF, 
                        metric='Accuracy', 
                        tuneGrid=tunegrid, trControl=control)
})

summary(model_rf_custom)
model_rf_custom
plot(model_rf_custom)

model_rf_custom$finalModel 

model_rf_custom$bestTune


# Train final random forest model with the optimum parameters found above
control <- trainControl(method = "repeatedcv", number = 2, repeats = 1, verboseIter = TRUE)

set.seed(1234)
mtry <- 250 
ntree <- 100
tunegrid <- expand.grid(.mtry=mtry)


system.time({
  model_rf_final <- train(x = df_train, y = as.factor(y_train),
                         method = "rf", 
                         tuneGrid=tunegrid, ntree = ntree, 
                         metric='Accuracy', 
                         trControl = control)
})

model_rf_final$finalModel 





## Save Best Models 
saveRDS(model_rf_final, 'model_rf.rds')
saveRDS(model_svm, 'model_svm.rds') 

# Save Reduced Vocab/DTM to use when applying model to new doc messages
saveRDS(dtm_freq, 'dtm_reduced.rds')
saveRDS(Terms(dtm_freq), 'dtm_vocab.rds')

saveRDS(model_rf_final, 'model_rf_reduced.rds')
saveRDS(model_svm, 'model_svm_reduced.rds') 


##############################
# Model Evaluation 
##############################

model_svm_final <- readRDS('model_svm_reduced.rds')
model_rf_final <- readRDS('model_rf_reduced.rds')

plot(model_svm_final)


# Variable importance
varImpPlot(model_rf_final$finalModel)

varImp <- varImp(model_svm_final) 
  

# Test set performance
rf_pred <- predict(model_rf_final, newdata = df_test)
rf_cm <- confusionMatrix(rf_pred, y_test)
rf_cm

svm_pred <- predict(model_svm_final, newdata = df_test)
svm_cm <- confusionMatrix(svm_pred, y_test)
svm_cm$byClass

svm_cm

# Plot confusion matrix for each classifier

cm_df <- data.frame(cbind(pred = svm_pred, obs = y_test))

cm_df %>% 
  mutate(predicted = pred - 1, actual = obs - 1) %>%
  mutate_if(is.numeric, as.factor) %>%
  ggplot(aes(predicted, actual, color = predicted)) +
  geom_jitter() + 
  geom_hline(yintercept=1.5, color = 'grey') +
  geom_vline(xintercept=1.5, color = 'grey') +
  theme_minimal() + 
  ggtitle('SVM Actual vs Prediction') + 
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.background = element_rect(colour = 'grey'), 
        legend.position='none')

cm_df <- data.frame(cbind(pred = rf_pred, obs = y_test))

cm_df %>% 
  mutate(predicted = pred - 1, actual = obs - 1) %>%
  mutate_if(is.numeric, as.factor) %>%
  ggplot(aes(predicted, actual, color = predicted)) +
  geom_jitter() + 
  geom_hline(yintercept=1.5, color = 'grey') +
  geom_vline(xintercept=1.5, color = 'grey') +
  theme_minimal() + 
  ggtitle('RF Actual vs Prediction') + 
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.background = element_rect(colour = 'grey'), 
        legend.position='none')


# plot the ROC curve
library(pROC)				 

plot(roc(predictor=cm_df$pred, response=cm_df$obs))







# Search terms

search_pred <- meta$message[-trainIndex] %>%
  as.data.frame(stringsAsFactors = TRUE) %>%
  rename('text'='.') %>%
  mutate(search_pred = case_when(grepl('test', text) |
                                   grepl('test2', text)
                             ~ '1')) %>%
  select(search_pred) %>%
  replace(is.na(.), 0)

search_pred <- data.matrix(search_pred)

search_cm <- confusionMatrix(as.factor(search_pred), (y_test))
search_cm$byClass



##############################
# Model Evaluation 
##############################


evaluate <- cbind(meta$message[-trainIndex], meta$test[-trainIndex], as.data.frame(svm_pred))

falseNegatives <- evaluate %>%
  filter(svm_pred == 0 & meta$test[-trainIndex] == 1)

