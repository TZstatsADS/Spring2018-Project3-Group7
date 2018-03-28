###Author : Fangbing Liu
###ADS Project 3 Spring 2018
###Linear SVM Classification

# load library
library(e1071)

train.svm <- function(data_train, label_train, par){
  label_train <- factor(label_train)
  svm.lin <- svm(data_train, label_train, kernel = "linear", cost = par, scale = F)
  return(svm.lin)
}

### SIFT feature###
set.seed(1234)
# load data
X <- read.csv("../data/train/SIFT_train.csv")[,-1]
y <- read.csv("../data/train/label_train.csv")[,3]
y <- as.factor(y)
index <- sample(1:nrow(X), 0.75*nrow(X), replace=FALSE)
X.train <- X[index,]
X.test  <- X[-index,]
y.train <- y[index]
y.test  <- y[-index]

# tune parameters and tune control
par.list = list(cost = c(0.1,1,10,50,100,150))
k = tune.control(cross = 5)

# tune svm with multiple classes using the one-versus-one approach
tune.out <- tune(svm, train.x = X.train, train.y = y.train, kernel = "linear",
                scale = FALSE, ranges = par.list, tunecontrol = k)

tune.out$best.parameters # cost = 100
tune.out$best.performance # 0.2966
performances <- tune.out$performances
save(tune.out, file="../output/SIFT_fit_train_svm_lin.RData")

# train the best model on the whole training set
best_model_SIFT_lin <- tune.out$best.model 
best_model_SIFT_lin # cost = 100
tm_train <- system.time(train.svm(X.train, y.train, tune.out$best.parameters[[1]]))
cat("Time for training model = ", tm_train[3], "s \n")  # 28.109 s
tm_test <- system.time(pred <- predict(best_model_SIFT_lin, X.test)) 
save(pred, file = "../output/SIFT_pred_test_svm_lin.RData")
error <- mean(pred != y.test) 
cat("Time for testing model = ", tm_test[3], "s \n") # 2.987 s
cat("Liner SVM with SIFT Accuracy = ", 1-error) #0.71

###HOG feature###
set.seed(1324)
# load data
X <- read.csv("../output/hog_feature.csv", header = T)[,-1]
index <- sample(1:nrow(X), 0.75*nrow(X), replace=FALSE)
X.train <- X[index,]
X.test  <- X[-index,]
y.train <- y[index]

# tune svm with multiple classes using the one-versus-one approach
tune.out <- tune(svm, train.x = X.train, train.y = y.train, kernel = "linear",
                 scale = FALSE, ranges = par.list, tunecontrol = k)

tune.out$best.parameters # cost = 150
tune.out$best.performance # 0.3107
performances <- tune.out$performances
save(tune.out, file="../output/HOG_fit_train_svm_lin.RData")

# train the best model on the whole training set
best_model_HOG_lin <- tune.out$best.model  
best_model_HOG_lin # cost = 150
tm_train <- system.time(train.svm(X.train, y.train, tune.out$best.parameters[[1]]))
cat("Time for training model = ", tm_train[3], "s \n")  # 1.065 s
tm_test <- system.time(pred <- predict(best_model_HOG_lin, X.test)) 
save(pred, file = "../output/HOG_pred_test_svm_lin.RData")
error <- mean(pred != y.test) 
cat("Time for testing model = ", tm_test[3], "s \n") # 0.125 s
cat("Liner SVM with HOG Accuracy = ", 1-error) #0.71

###SIFT+PCA###
set.seed(1423)
# load data
X <- read.csv("../output/sift_pca.csv", header = T)[,-1]
index <- sample(1:nrow(X), 0.75*nrow(X), replace=FALSE)
X.train <- X[index,]
X.test  <- X[-index,]
y.train <- y[index]

# tune svm with multiple classes using the one-versus-one approach
tune.out <- tune(svm, train.x = X.train, train.y = y.train, kernel = "linear",
                 scale = FALSE, ranges = par.list, tunecontrol = k)

tune.out$best.parameters # cost = 0.1
tune.out$best.performance # 0.3556
performances <- tune.out$performances
save(tune.out, file="../output/PCA_fit_train_svm_lin.RData")

# train the best model on the whole training set
best_model_pca_lin <- tune.out$best.model  
best_model_pca_lin # cost = 0.1
tm_train <- system.time(train.svm(X.train, y.train, tune.out$best.parameters[[1]]))
cat("Time for training model = ", tm_train[3], "s \n")  # 0.38 s
tm_test <- system.time(pred <- predict(best_model_pca_lin, X.test)) 
save(pred, file = "../output/PCA_pred_test_svm_lin.RData")
error <- mean(pred != y.test) 
cat("Time for testing model = ", tm_test[3], "s \n") # 0.034 s
cat("Liner SVM with PCA Accuracy = ", 1-error) #0.61

###Gray Feature###
set.seed(4321)
# load data
X <- read.csv("../output/gray_features.csv", header = T)[,-c(1,2)]
index <- sample(1:nrow(X), 0.75*nrow(X), replace=FALSE)
X.train <- X[index,]
X.test  <- X[-index,]
y.train <- y[index]

# tune svm with multiple classes using the one-versus-one approach
tune.out <- tune(svm, train.x = X.train, train.y = y.train, kernel = "linear",
                 scale = FALSE, ranges = par.list, tunecontrol = k)

tune.out$best.parameters # cost = 150
tune.out$best.performance # 0.4996
performances <- tune.out$performances
save(tune.out, file="../output/gray_fit_train_svm_lin.RData")

# train the best model on the whole training set
best_model_gray_lin <- tune.out$best.model  
best_model_gray_lin # cost = 150
tm_train <- system.time(train.svm(X.train, y.train, tune.out$best.parameters[[1]]))
cat("Time for training model = ", tm_train[3], "s \n")  # 6.33 s
tm_test <- system.time(pred <- predict(best_model_gray_lin, X.test)) 
save(pred, file = "../output/gray_pred_test_svm_lin.RData") 
error <- mean(pred != y.test) 
cat("Time for testing model = ", tm_test[3], "s \n") # 0.612 s
cat("Liner SVM with Gray Accuracy = ", 1-error) #0.48

###Color Feature###
set.seed(4321)
# load data
X <- read.csv("../output/color_features.csv", header = T)[,-1]
index <- sample(1:nrow(X), 0.75*nrow(X), replace=FALSE)
X.train <- X[index,]
X.test  <- X[-index,]
y.train <- y[index]

# tune svm with multiple classes using the one-versus-one approach
tune.out <- tune(svm, train.x = X.train, train.y = y.train, kernel = "linear",
                 scale = FALSE, ranges = par.list, tunecontrol = k)

tune.out$best.parameters # cost = 150
tune.out$best.performance # 0.2773
performances <- tune.out$performances
save(tune.out, file="../output/color_fit_train_svm_lin.RData")

# train the best model on the whole training set
best_model_color_lin <- tune.out$best.model  
best_model_color_lin # cost = 150
tm_train <- system.time(train.svm(X.train, y.train, tune.out$best.parameters[[1]]))
cat("Time for training model = ", tm_train[3], "s \n")  # 23.969 s
tm_test <- system.time(pred <- predict(best_model_color_lin, X.test)) 
save(pred, file = "../output/color_pred_test_svm_lin.RData") 
error <- mean(pred != y.test) 
cat("Time for testing model = ", tm_test[3], "s \n") # 2.624 s
cat("Liner SVM with color Accuracy = ", 1-error) #0.7031