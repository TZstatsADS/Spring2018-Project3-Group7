###Author : Fangbing Liu
###ADS Project 3 Spring 2018
###SVM with RBF Kernel Classification

# load library
library(e1071)

train.svm <- function(data_train, label_train, par){
  label_train <- factor(label_train)
  svm.rbf <- svm(data_train, label_train, kernel = "radial", cost = par[[1]], gamma = par[[2]], scale = F)
  return(svm.rbf)
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
par.list = list(cost = c(0.1,1,10,50,100,150),
                gamma = c(0.01,0.5,1,5,10))
k = tune.control(cross = 5)

# tune svm with multiple classes using the one-versus-one approach
tune.out <- tune(svm, train.x = X.train, train.y = y.train, kernel = "radial",
                 scale = FALSE, ranges = par.list, tunecontrol = k)

tune.out$best.parameters # cost = 10, gamma = 10
tune.out$best.performance # 0.2597
performances <- tune.out$performances
save(tune.out, file="../output/SIFT_fit_train_svm_rbf.RData")

# train the best model on the whole training set
best_model_SIFT_rbf <- tune.out$best.model 
best_model_SIFT_rbf # cost = 10, gamma = 10
tm_train <- system.time(train.svm(X.train, y.train, tune.out$best.parameters))
cat("Time for training model = ", tm_train[3], "s \n")  # 25.623 s
tm_test <- system.time(pred <- predict(best_model_SIFT_rbf, X.test)) 
save(pred, file = "../output/SIFT_pred_test_svm_rbf.RData")
error <- mean(pred != y.test) 
cat("Time for testing model = ", tm_test[3], "s \n") # 2.88 s
cat("RBF Kernel SVM with SIFT Accuracy = ", 1-error) #0.74

###HOG feature###
set.seed(1324)
# load data
X <- read.csv("../output/hog_feature.csv", header = T)[,-1]
index <- sample(1:nrow(X), 0.75*nrow(X), replace=FALSE)
X.train <- X[index,]
X.test  <- X[-index,]
y.train <- y[index]

# tune svm with multiple classes using the one-versus-one approach
tune.out <- tune(svm, train.x = X.train, train.y = y.train, kernel = "radial",
                 scale = FALSE, ranges = par.list, tunecontrol = k)

tune.out$best.parameters # cost = 150, gamma = 10
tune.out$best.performance # 0.2213
performances <- tune.out$performances
save(tune.out, file="../output/HOG_fit_train_svm_rbf.RData")

# train the best model on the whole training set
best_model_HOG_rbf <- tune.out$best.model  
best_model_HOG_rbf # cost = 150, gamma = 10
tm_train <- system.time(train.svm(X.train, y.train, tune.out$best.parameters))
cat("Time for training model = ", tm_train[3], "s \n")  # 1.017 s
tm_test <- system.time(pred <- predict(best_model_HOG_rbf, X.test)) 
save(pred, file = "../output/HOG_pred_test_svm_rbf.RData")
error <- mean(pred != y.test) 
cat("Time for testing model = ", tm_test[3], "s \n") # 0.125 s
cat("RBF Kernel SVM with HOG Accuracy = ", 1-error) #0.76

###SIFT+PCA###
set.seed(1432)
# load data
X <- read.csv("../output/sift_pca.csv", header = T)[,-1]
index <- sample(1:nrow(X), 0.75*nrow(X), replace=FALSE)
X.train <- X[index,]
X.test  <- X[-index,]
y.train <- y[index]

# tune svm with multiple classes using the one-versus-one approach
tune.out <- tune(svm, train.x = X.train, train.y = y.train, kernel = "radial",
                 scale = FALSE, ranges = par.list, tunecontrol = k)

tune.out$best.parameters # cost = 10, gamma = 0.01
tune.out$best.performance # 0.3373
performances <- tune.out$performances
save(tune.out, file="../output/PCA_fit_train_svm_rbf.RData")

# train the best model on the whole training set
best_model_PCA_rbf <- tune.out$best.model  
best_model_PCA_rbf # cost = 150, gamma = 10
tm_train <- system.time(train.svm(X.train, y.train, tune.out$best.parameters))
cat("Time for training model = ", tm_train[3], "s \n")  # 0.426 s
tm_test <- system.time(pred <- predict(best_model_PCA_rbf, X.test)) 
save(pred, file = "../output/PCA_pred_test_svm_rbf.RData")
error <- mean(pred != y.test) 
cat("Time for testing model = ", tm_test[3], "s \n") # 0.043 s
cat("RBF Kernel SVM with PCA Accuracy = ", 1-error) #0.64

###Gray Feature###
set.seed(4321)
# load data
X <- read.csv("../output/gray_features.csv", header = T)[,-c(1,2)]
index <- sample(1:nrow(X), 0.75*nrow(X), replace=FALSE)
X.train <- X[index,]
X.test  <- X[-index,]
y.train <- y[index]

# tune svm with multiple classes using the one-versus-one approach
tune.out <- tune(svm, train.x = X.train, train.y = y.train, kernel = "radial",
                 scale = FALSE, ranges = par.list, tunecontrol = k)

tune.out$best.parameters # cost = 150, gamma = 10
tune.out$best.performance # 0.4542
performances <- tune.out$performances
save(tune.out, file="../output/Gray_fit_train_svm_rbf.RData")

# train the best model on the whole training set
best_model_gray_rbf <- tune.out$best.model  
best_model_gray_rbf # cost = 150, gamma = 10
tm_train <- system.time(train.svm(X.train, y.train, tune.out$best.parameters))
cat("Time for training model = ", tm_train[3], "s \n")  # 6.217 s
tm_test <- system.time(pred <- predict(best_model_gray_rbf, X.test)) 
save(pred, file = "../output/Gray_pred_test_svm_rbf.RData")
error <- mean(pred != y.test) 
cat("Time for testing model = ", tm_test[3], "s \n") # 0.676 s
cat("RBF Kernel SVM with Gray Accuracy = ", 1-error) #0.53

###Color Feature###
set.seed(4321)
# load data
X <- read.csv("../output/color_features.csv", header = T)[,-1]
index <- sample(1:nrow(X), 0.75*nrow(X), replace=FALSE)
X.train <- X[index,]
X.test  <- X[-index,]
y.train <- y[index]

# tune svm with multiple classes using the one-versus-one approach
tune.out <- tune(svm, train.x = X.train, train.y = y.train, kernel = "radial",
                 scale = FALSE, ranges = par.list, tunecontrol = k)

tune.out$best.parameters # cost = 50, gamma = 1
tune.out$best.performance # 0.2356
performances <- tune.out$performances
save(tune.out, file="../output/color_fit_train_svm_rbf.RData")

# train the best model on the whole training set
best_model_color_rbf <- tune.out$best.model  
best_model_color_rbf # cost = 50, gamma = 1
tm_train <- system.time(train.svm(X.train, y.train, tune.out$best.parameters))
cat("Time for training model = ", tm_train[3], "s \n")  # 23.969 s
tm_test <- system.time(pred <- predict(best_model_color_rbf, X.test)) 
save(pred, file = "../output/color_pred_test_svm_rbf.RData")
error <- mean(pred != y.test) 
cat("Time for testing model = ", tm_test[3], "s \n") # 2.683 s
cat("RBF Kernel SVM with color Accuracy = ", 1-error) #0.7244