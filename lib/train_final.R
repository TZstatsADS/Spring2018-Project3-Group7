#########################################################
### Train a classification model with training images ###
#########################################################

### Group 7
### ADS Project 3 Spring 2018

train <- function(dat_train, label_train, params=NULL,
                  run.gbm = F, run.svm.lin = F, 
                  run.svm.rbf = F, run.xgboost = F,
                  run.rf = F, run.lg = F, run.adaboost = F){
  
  # Train with GBM(Baseline) model
  
  if(!require("gbm")){
    install.packages("gbm")
  }
  library("gbm")
  
  gbm <- NULL
  if(run.gbm){
    train_df <- data.frame(label = label_train[,3]-1, dat_train[,-1])
    gbm <- gbm(label~ ., data = train_df, interaction.depth = 1, 
               distribution="multinomial", shrinkage = 0.1, n.trees = params[1])
    return(gbm)
  }
  
  # Train with linear SVM model
  
  if(!require("e1071")){
    install.packages("e1071")
  }
  library("e1071")
  
  svm.lin <- NULL
  if(run.svm.lin){
    svm.lin <- svm(dat_train[,-1], label_train[,-1], cost = params[1],
                   scale = F, kernel = "linear")
    return(svm.lin)
  }
  
  # Train with RBF Kernel SVM model
  svm.rbf <- NULL
  if(run.svm.rbf){
    svm.rbf <- svm(dat_train[,-1], label_train[,-1], cost = params[1], gamma = params[2],
                   scale = F, kernel = "radial")
    return(svm.rbf)
  }
  
  # Train with XGBoost model
  xgboost <- NULL
  if(run.xgboost){
    xgboost <- xgboostfit(dat_train, label_train, params)
    return(xgboost)
  }
  
  # Train with Random Forest model
  if(!require("randomForest")){
    install.packages("randomForest")
  }
  library("randomForest")
  
  rf <- NULL
  if(run.rf){
    rf <- randomForest(dat_train[,-1], label_train[,3], ntree = params)
    return(rf)
  }
  
  # Train with AdaBoost model
  
  
  if(!require("adabag")){
    install.packages("adabag")
  }
  library("adabag")
  
  ada <- NULL
  if(run.ada){
    ada <- adaboost(dat_train[,-1], label_train[,3], ntree = params)
    return(ada)
  }
  
  # Train with logistic regression model
  
  if(!require("nnet")){
    install.packages("nnet")
  }
  library("nnet")
  
  lg <- NULL
  if(run.lg){
    lg <- logistic(dat_train[,-1], label_train[,3], MaxNWts = 20000, maxit = maxit)
    return(lg)
  }
  
  
}