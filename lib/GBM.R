### Author: Yuexuan Huang
### ADS Project 3 Spring 2018


gbm_train <- function(data, label, n.trees, n.shrinkage=0.1, run.cv=F){
  library('gbm')
  
  train_df <- data
  train_df$label <- label
  if(run.cv){
    best.n.trees = cv.f(trees = 500, K = 10, train_df = train_df)
    best.shrinkage <- n.shrinkage
    
  } 
  else{
    best.n.trees <- n.trees
    best.shrinkage <- n.shrinkage
    
  }
  cat('best number of tress is: ',best.n.trees)
  
  best_gbm_fit <- gbm(label~ ., data = train_df, interaction.depth = 1, 
                      distribution="multinomial", n.trees = best.n.trees, 
                      shrinkage = best.shrinkage)
  return(best_gbm_fit)
  
}

cv.f <- function(trees, K, train_df){
  
  gbmWithCrossValidation = gbm(label~ ., data = train_df, distribution = "multinomial", 
                               n.trees = trees, shrinkage = .1, cv.folds = 10, n.cores = 1)
  
  return(gbm.perf(gbmWithCrossValidation))
}

gbm_test <- function(model_fit,data){
  
  pred <- predict(model_fit, newdata=data, n.trees=model_fit$n.trees, type="response")
  
  pred <- data.frame(pred[,,1])
  
  colnames(pred) <- c('0','1','2')
  
  pred_label <- apply(pred,1,function(x){return(which.max(x)-1)})
  
  return(pred_label)
}

###### SIFT ######
########## Apply GBM on SIFT ##########
sift_train <- read.csv("~/Documents/GitHub/Spring2018-Project3-Group7/data/image/SIFT_train.csv",as.is = T, header = F)[,-1]
label_train <- as.vector(read.csv("~/Documents/GitHub/Spring2018-Project3-Group7/data/image/label_train.csv",as.is = T)[,3])
label_train <- label_train - 1


######### set 70% sift_train_data as training data #############
set.seed(1)
train_index <- sort(sample(1:length(label_train),0.7*length(label_train)))

train_df <- data.frame(sift_train[train_index,])
train_df$label <- label_train[train_index]
test_df <- data.frame(sift_train[-train_index,])
test_df$label <- label_train[-train_index]

tm_gbm_train <- system.time(gbm_sift_fit_subset <- gbm_train(train_df,train_df$label, n.trees=482, run.cv = F))
tm_gbm_predict <- system.time(pred_label <- gbm_test(gbm_sift_fit_subset,test_df))

mean(test_df$label == pred_label) 
## 0.7111111
tm_gbm_train[3]
## 91.766
tm_gbm_predict[3]
## 0.805


###### HOG ######
########## Apply GBM on HOG ##########
hog_train <- read.csv("~/Documents/GitHub/Spring2018-Project3-Group7/output/hog_feature.csv",as.is = T)[,-1]
######### set 70% sift_train_data as training data #############
set.seed(2)
train_index <- sort(sample(1:length(label_train),0.7*length(label_train)))
train_df2 <- data.frame(hog_train[train_index,])
train_df2$label <- label_train[train_index]
test_df2 <- data.frame(hog_train[-train_index,])
test_df2$label <- label_train[-train_index]

tm_gbm_train <- system.time(gbm_hog_fit_subset <- gbm_train(train_df2,train_df2$label, n.trees=499, run.cv = F))
tm_gbm_predict <- system.time(pred_label2 <- gbm_test(gbm_hog_fit_subset,test_df2))

mean(test_df2$label == pred_label2) 
## 0.7477778
tm_gbm_train[3]
## 3.447 
tm_gbm_predict[3]
## 0.044 



###### GRAY ######
########## Apply GBM on GRAY ##########
gray_train <- read.csv("~/Documents/GitHub/Spring2018-Project3-Group7/data/gray_features.csv",as.is = T)[,-c(1:2)]
######### set 70% sift_train_data as training data #############
set.seed(3)
train_index <- sort(sample(1:length(label_train),0.7*length(label_train)))
train_df3 <- data.frame(gray_train[train_index,])
train_df3$label <- label_train[train_index]
test_df3 <- data.frame(gray_train[-train_index,])
test_df3$label <- label_train[-train_index]

tm_gbm_train <- system.time(gbm_gray_fit_subset <- gbm_train(train_df3,train_df3$label, n.trees=132, run.cv = F))
tm_gbm_predict <- system.time(pred_label3 <- gbm_test(gbm_gray_fit_subset,test_df3))

mean(test_df3$label == pred_label3) 
## 0.5444444
tm_gbm_train[3]
## 4.271 
tm_gbm_predict[3]
## 0.036 



###### PCA ######
########## Apply GBM on PCA ##########
pca_train <- read.csv("~/Documents/GitHub/Spring2018-Project3-Group7/data/sift_pca.csv",as.is = T)[,-1]
######### set 70% sift_train_data as training data #############
set.seed(4)
train_index <- sort(sample(1:length(label_train),0.7*length(label_train)))
train_df4 <- data.frame(pca_train[train_index,])
train_df4$label <- label_train[train_index]
test_df4 <- data.frame(pca_train[-train_index,])
test_df4$label <- label_train[-train_index]

tm_gbm_train <- system.time(gbm_pca_fit_subset <- gbm_train(train_df4,train_df4$label, n.trees=182, run.cv = F))
tm_gbm_predict <- system.time(pred_label4 <- gbm_test(gbm_pca_fit_subset,test_df4))

mean(test_df4$label == pred_label4) 
## 0.6577778
tm_gbm_train[3]
## 0.365
tm_gbm_predict[3]
## 0.02


