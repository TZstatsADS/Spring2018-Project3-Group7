########################
### Cross Validation ###
########################

### Group 7
### Project 3
### ADS Spring 2018

cv.function <- function(X.train, y.train, d, K,
                        cv.gbm = F, cv.svm.lin = F, 
                        cv.svm.rbf = F, cv.xgboost = F,
                        cv.rf = F, cv.lr = F, cv.adaboost = F){
  
  n <- length(y.train[,2])
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X.train[s != i,]
    train.label <- y.train[s != i,]
    test.data <- X.train[s == i,]
    test.label <- y.train[s == i,]
    print('train.data:')
    print(dim(train.data))
    
    ## cross validate to GBM model
    if(cv.gbm){
      
      params <- d
      fit <- train(train.data, train.label, params, run.gbm = TRUE)
      pred <- test(fit, test.data, test.gbm = T)
      
    }
    
    ## cross validate to Linear SVM model
    if(cv.svm.lin){
      params <- d
      fit <- train(train.data, train.label, params, run.svm.lin = TRUE)
      pred <- test(fit, test.data, test.svm.lin = T)
      
    }
    
    ## cross validate to RBF Kernel SVM model
    if(cv.svm.rbf){
      params <- d
      fit <- train(train.data, train.label, params, run.svm.rbf = TRUE)
      pred <- test(fit, test.data, test.svm.rbf = T)
      
    }
    
    ## cross validate to RF model
    if(cv.rf){
      params <- d
      fit <- train(train.data, train.label, params, run.rf = T)
      pred <- test(fit, test.data, test.rf = T)

    }
    
    ## cross validate xgboost model
    if(cv.xgboost){
      
      fit <- train(train.data, train.label, params = NULL, run.xgboost = T)
      pred <- test(fit, test.data, test.xgboost = T)
      
    }
    
    ## cross validate logistic regression model
    if(cv.lr){
      
      fit <- train(train.data, train.label, params = NULL, run.lr = T)
      pred <- test(fit, test.data, test.lr = T)
      
    }
    
    ## cross validate adaboost model
    if(cv.adaboost){
      
      fit <- train(train.data, train.label, params = NULL, run.adaboost = T)
      pred <- test(fit, test.data, test.adaboost = T)
      
    }
    
    cv.error[i] <- mean(pred != y.train[s == i,2])  
    
  }			
  
  return(c(mean(cv.error),sd(cv.error)))
  
}