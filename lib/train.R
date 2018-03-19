#########################################################
### Train a classification model with training images ###
#########################################################

### Author: Fangbing Liu, Yuexuan Huang
### ADS Project 3 Spring 2018

## Baseline Model(GBM)
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


## SVM with RBF Kernel Model
train.svm.rbf <- function(dat_train, label_train, par=NULL){
  
  ### Train a SVM classifier with RBF kernel using features of training images
  
  ### Input: 
  ###  -  processed features from images 
  ###  -  class labels for training images
  ### Output: training model specification
  
  ### load libraries
  library(e1071)
  
  ### Train with gradient boosting model
  if(is.null(par)){
    cost <- 150
    gamma <- 10
  } else {
    cost <- par$cost
    gamma <- par$gamma
  }
  fit_svm_rbf <- svm(y~., data = dat_train,
                     kernel = "radial", 
                     cost = cost,
                     gamma = gamma, 
                     scale = F)

  return(fit_svm_rbf)
}
