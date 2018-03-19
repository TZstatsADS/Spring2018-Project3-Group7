######################################################
### Fit the classification model with testing data ###
######################################################

### Author: Fangbing Liu, Yuexuan Huang
### ADS Project 3 Spring 2018

## Baseline Model(GBM)
gbm_test <- function(model_fit, data){
  
  pred <- predict(model_fit, newdata=data, n.trees=model_fit$n.trees, type="response")
  
  pred <- data.frame(pred[,,1])
  
  colnames(pred) <- c('0','1','2')
  
  pred_label <- apply(pred,1,function(x){return(which.max(x)-1)})
  
  return(pred_label)
}


## SVM with RBF Kernel Model
test.svm.rbf <- function(fit_train, dat_test){
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  -  processed features from testing images 
  ### Output: training model specification
  
  ### load libraries
  library(e1071)
  
  pred_svm_rbf <- predict(fit_train, newdata = dat_test)
  
  return(pred_svm_rbf)
}

