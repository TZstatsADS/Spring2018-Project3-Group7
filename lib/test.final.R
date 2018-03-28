######################################################
### Fit the classification model with testing data ###
######################################################

### Group 7
### ADS Project 3 Spring 2018

test <- function(fit_train, dat_test, params=NULL,
                 test.gbm = F, test.svm.lin = F, 
                 test.svm.rbf = F, test.xgboost = F,
                 test.rf = F, test.lg = F, test.adboost = F){
  # test GBM
  if(test.gbm){
    pred <- predict(fit_train, newdata = dat_test[,-1], n.trees = params[1], type="response")
    pred <- data.frame(pred[,,1])
    colnames(pred) <- c('0','1','2')
    pred_label <- apply(pred,1,function(x){return(which.max(x)-1)})
    return(pred_label+1)
  }
  
  # test linear SVM
  if(test.svm.lin){
    pred <- predict(fit_train, newdata = dat_test[,-1])
  }
  
  # test RBF Kernel SVM
  if(test.svm.rbf){
    pred <- predict(fit_train, newdata = dat_test[,-1])
  }
  
  # test XGboost
  if(test.xgboost){
    pred <- predict(fit_train, newdata = dat_test[,-1])
  }
  
  
  # test random forest
  
  if(test.rf){
    pred <- predict(fit_train, newdata = dat_test[,-1])
  }
  
  # test adaboost
  if(test.adaboost){
    pred <- predict(fit_train, newdata = dat_test[,-1])
  }
  # test logistic regression
  if(test.lg){
    pred <- predict(fit_train, newdata = dat_test[,-1])
  } 
}