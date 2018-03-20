### Author: Yuexuan Huang
### ADS Project 3 Spring 2018


gbm_train <- function(data, label, n.trees=250, n.shrinkage=0.1, run.cv=F){
  library('gbm')
  
  train_df <- data
  train_df$label <- label
  if(run.cv){
    best.n.trees = cv.gbm(trees = 500, K = 10, train_df = train_df)
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

cv.gbm <- function(trees, K, train_df){
  
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
train_label <- gbm_test(gbm_sift_fit_subset,train_df)
tm_gbm_predict <- system.time(pred_label <- gbm_test(gbm_sift_fit_subset,test_df))

mean(train_df$label == train_label) 
## 0.9780952
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

train_df2 <- data.frame(hog_train[train_index,])
train_df2$label <- label_train[train_index]
test_df2 <- data.frame(hog_train[-train_index,])
test_df2$label <- label_train[-train_index]

tm_gbm_train <- system.time(gbm_hog_fit_subset <- gbm_train(train_df2,train_df2$label, n.trees=463, run.cv = F))
train_label2 <- gbm_test(gbm_hog_fit_subset,train_df2)
tm_gbm_predict <- system.time(pred_label2 <- gbm_test(gbm_hog_fit_subset,test_df2))

mean(train_df2$label == train_label2) 
## 0.8861905
mean(test_df2$label == pred_label2) 
## 0.76
tm_gbm_train[3]
## 2.978 
tm_gbm_predict[3]
## 0.035 



###### GRAY ######
########## Apply GBM on GRAY ##########
gray_train <- read.csv("~/Documents/GitHub/Spring2018-Project3-Group7/output/gray_features.csv",as.is = T)[,-c(1:2)]
######### set 70% sift_train_data as training data #############

train_df3 <- data.frame(gray_train[train_index,])
train_df3$label <- label_train[train_index]
test_df3 <- data.frame(gray_train[-train_index,])
test_df3$label <- label_train[-train_index]

tm_gbm_train <- system.time(gbm_gray_fit_subset <- gbm_train(train_df3,train_df3$label, n.trees=117, run.cv = F))
train_label3 <- gbm_test(gbm_gray_fit_subset,train_df3)
tm_gbm_predict <- system.time(pred_label3 <- gbm_test(gbm_gray_fit_subset,test_df3))

mean(train_df3$label == train_label3) 
## 0.6561905
mean(test_df3$label == pred_label3) 
## 0.5544444
tm_gbm_train[3]
## 3.927 
tm_gbm_predict[3]
## 0.034 



###### PCA ######
########## Apply GBM on PCA ##########
pca_train <- read.csv("~/Documents/GitHub/Spring2018-Project3-Group7/output/sift_pca.csv",as.is = T)[,-1]
######### set 70% sift_train_data as training data #############

train_df4 <- data.frame(pca_train[train_index,])
train_df4$label <- label_train[train_index]
test_df4 <- data.frame(pca_train[-train_index,])
test_df4$label <- label_train[-train_index]

tm_gbm_train <- system.time(gbm_pca_fit_subset <- gbm_train(train_df4,train_df4$label, n.trees=315, run.cv = F))
train_label4 <- gbm_test(gbm_pca_fit_subset,train_df4)
tm_gbm_predict <- system.time(pred_label4 <- gbm_test(gbm_pca_fit_subset,test_df4))

mean(train_df4$label == train_label4) 
## 0.8642857
mean(test_df4$label == pred_label4) 
## 0.7388889
tm_gbm_train[3]
## 3.728
tm_gbm_predict[3]
## 0.029 


###### COLOR ######
########## Apply GBM on COLOR ##########
color_train <- read.csv("~/Documents/GitHub/Spring2018-Project3-Group7/output/color_features.csv",as.is = T)[,-1]
######### set 70% sift_train_data as training data #############

train_df5 <- data.frame(color_train[train_index,])
train_df5$label <- label_train[train_index]
test_df5 <- data.frame(color_train[-train_index,])
test_df5$label <- label_train[-train_index]

tm_gbm_train <- system.time(gbm_color_fit_subset <- gbm_train(train_df5,train_df5$label, n.trees=441, run.cv = F))
train_label5 <- gbm_test(gbm_color_fit_subset,train_df5)
tm_gbm_predict <- system.time(pred_label5 <- gbm_test(gbm_color_fit_subset,test_df5))

mean(train_df5$label == train_label5) 
## 0.9638095
mean(test_df5$label == pred_label5) 
## 0.87
tm_gbm_train[3]
## 95.452
tm_gbm_predict[3]
## 0.86

