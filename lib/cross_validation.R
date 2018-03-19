########################
### Cross Validation ###
########################

### Author: Fangbing Liu, Yuexuan Huang
### Project 3
### ADS Spring 2016

## Baseline Model
cv.gbm <- function(trees, K, train_df){
  
  gbmWithCrossValidation = gbm(label~ ., data = train_df, distribution = "multinomial", 
                               n.trees = trees, shrinkage = .1, cv.folds = 10, n.cores = 1)
  
  return(gbm.perf(gbmWithCrossValidation))
}

## SVM with RBF Kernel Model
cv.svm.rbf <- function(train.df, par.range, K){
  # tune svm with multiple classes using the one-versus-one approach
  tune.out = tune(svm, label~., data = trian_df, 
                  kernel = "radial",
                  scale = FALSE, 
                  ranges = par.range, 
                  tunecontrol = tune.control(cross = K))
  
  best.para.svm.rbf <- tune.out$best.parameters 
  
  return(best.par.svm.rbf)
}
