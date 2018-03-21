library(nnet)
library(dplyr)
library(data.table)


train.lg <- function(dat_train, label_train, par = NULL){
  
  ### Train a multinomial logistic regression using processed features from training images
  
  ### Input: 
  ###  -  processed features from images, 
  ###  -  class labels for training images
  ### Output: training model specification
  
  ### combine the features and the labels together
  mydata<-cbind(dat_train,label_train)
  label_train<-factor(label_train)
 
  ### train with multinomial logistic regression model
  if(is.null(par)){
    maxit <- 100
  } else {
    maxit <- par$maxit
  }
  fit_lg <- multinom(label_train~., data = mydata[,-1], MaxNWts = 20000, maxit = maxit)
  
  return(list(fit=fit_lg))
}


test.lg <- function(fit_train, dat_test){
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  -  processed features from testing images 
  ### Output: training model specification
  
  pred <- predict(fit_train$fit, dat_test[,-1])
  
  return(pred)
}


cv.function <- function(X.train, y.train, iter, K){
  
  set.seed(0)
  
  n <- length(y.train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X.train[s != i,]
    train.label <- y.train[s != i]
    test.data <- X.train[s == i,]
    test.label <- y.train[s == i]
    
    par <- list(maxit=iter)
    fit <- train.lg(train.data, train.label, par)
    pred <- test.lg(fit, test.data)  
    cv.error[i] <- mean(pred != test.label)  
    
  }			
  return(c(mean(cv.error),sd(cv.error)))
  
}



## load data
img_labels<-read.csv("../data/image/label_train.csv")
colnames(img_labels)=c("Image","labels")

features<-read.csv("../data/image/SIFT_train.csv",as.is = F, header=F)
gray<-read.csv("../output/gray_features.csv",as.is = F)
hog<-read.csv("../output/hog_feature.csv",as.is = F)
sift_pca<-read.csv("../output/sift_pca.csv",as.is = F)
color<-read.csv("../output/color_features.csv",as.is = F)

##split data into training set and testing set
set.seed(90)
train_index<-sample(1:3000,floor(nrow(img_labels)*0.75))
train_labels<-img_labels[train_index,3]
test_labels<-img_labels[-train_index,3]



###########sift
train_data<-features[train_index,]
test_data<-features[-train_index,]


# Choosing between different values of maximum interation for Logistic
iter_values <- seq(10,30,1)
err_cv <- array(dim=c(length(iter_values), 2))
K <- 5  # number of CV folds
for(k in 1:length(iter_values)){
  cat("k=", k, "\n")
  err_cv[k,] <- cv.function(train_data, train_labels, iter_values[k], K)
}


# Visualize CV results
plot(iter_values, err_cv[,1], xlab="Maximum Iteration", ylab="CV Error",
     main="Cross Validation Error", ylim=c(0.2, 0.4))
points(iter_values, err_cv[,1], col="blue", pch=16)
lines(iter_values, err_cv[,1], col="blue")
arrows(iter_values, err_cv[,1]-err_cv[,2],iter_values, err_cv[,1]+err_cv[,2], 
       length=0.1, angle=90, code=3)


# Choose the best parameter value
min(err_cv[,1])  # 0.2768889
iter_best <- iter_values[which.min(err_cv[,1] + err_cv[,2])]
par_best <- list(maxit=iter_best) 
par_best # maxit=29 is the best

# train the model with the entire training set
tm_train <- system.time(fit_train <- train.lg(train_data, train_labels, par_best))
pred_train <- test.lg(fit_train, train_data)
train.error <- mean(pred_train != train_labels)
train.error #0.1013333
1-train.error #0.8986667

### Make prediction 
tm_test <- system.time(pred_test <- test.lg(fit_train, test_data))
test.error <- mean(pred_test != test_labels)
test.error #0.2653333
1-test.error  #73.46667

### Summarize Running Time
cat("Time for training model=", tm_train[1], "s \n") # 13.002 s 
cat("Time for making prediction=", tm_test[1], "s \n")  # 0.225 s





##########gray
train_data<-gray[train_index,]
test_data<-gray[-train_index,]


# Choosing between different values of maximum interation for Logistic
iter_values <- seq(10,30,1)
err_cv <- array(dim=c(length(iter_values), 2))
K <- 5  # number of CV folds
for(k in 1:length(iter_values)){
  cat("k=", k, "\n")
  err_cv[k,] <- cv.function(train_data, train_labels, iter_values[k], K)
}


# Visualize CV results
plot(iter_values, err_cv[,1], xlab="Maximum Iteration", ylab="CV Error",
     main="Cross Validation Error", ylim=c(0.2, 0.4))
points(iter_values, err_cv[,1], col="blue", pch=16)
lines(iter_values, err_cv[,1], col="blue")
arrows(iter_values, err_cv[,1]-err_cv[,2],iter_values, err_cv[,1]+err_cv[,2], 
       length=0.1, angle=90, code=3)


# Choose the best parameter value
min(err_cv[,1])  # 0604
iter_best <- iter_values[which.min(err_cv[,1] + err_cv[,2])]
par_best <- list(maxit=iter_best) 
par_best # maxit=11 is the best

# train the model with the entire training set
tm_train <- system.time(fit_train <- train.lg(train_data, train_labels, par_best))
pred_train <- test.lg(fit_train, train_data)
train.error <- mean(pred_train != train_labels)
train.error #0.000444
1-train.error 

### Make prediction 
tm_test <- system.time(pred_test <- test.lg(fit_train, test_data))
test.error <- mean(pred_test != test_labels)
test.error #0.572
1-test.error  #0.428

### Summarize Running Time
cat("Time for training model=", tm_train[1], "s \n") # 33.1 s 
cat("Time for making prediction=", tm_test[1], "s \n")  # 0.202 s



###########hog
train_data<-hog[train_index,]
test_data<-hog[-train_index,]


# Choosing between different values of maximum interation for Logistic
iter_values <- seq(10,30,1)
err_cv <- array(dim=c(length(iter_values), 2))
K <- 5  # number of CV folds
for(k in 1:length(iter_values)){
  cat("k=", k, "\n")
  err_cv[k,] <- cv.function(train_data, train_labels, iter_values[k], K)
}


# Visualize CV results
plot(iter_values, err_cv[,1], xlab="Maximum Iteration", ylab="CV Error",
     main="Cross Validation Error", ylim=c(0.2, 0.4))
points(iter_values, err_cv[,1], col="blue", pch=16)
lines(iter_values, err_cv[,1], col="blue")
arrows(iter_values, err_cv[,1]-err_cv[,2],iter_values, err_cv[,1]+err_cv[,2], 
       length=0.1, angle=90, code=3)


# Choose the best parameter value
min(err_cv[,1])  # 0.279
iter_best <- iter_values[which.min(err_cv[,1] + err_cv[,2])]
par_best <- list(maxit=iter_best) 
par_best # maxit=28 is the best

# train the model with the entire training set
tm_train <- system.time(fit_train <- train.lg(train_data, train_labels, par_best))
pred_train <- test.lg(fit_train, train_data)
train.error <- mean(pred_train != train_labels)
train.error #0.26
1-train.error #0.74

### Make prediction 
tm_test <- system.time(pred_test <- test.lg(fit_train, test_data))
test.error <- mean(pred_test != test_labels)
test.error #0.3
1-test.error #0.7

### Summarize Running Time
cat("Time for training model=", tm_train[1], "s \n") # 0.107s
cat("Time for making prediction=", tm_test[1], "s \n")  # 0.004s




###########pca
train_data<-sift_pca[train_index,]
test_data<-sift_pca[-train_index,]



# Choosing between different values of maximum interation for Logistic
iter_values <- seq(10,30,1)
err_cv <- array(dim=c(length(iter_values), 2))
K <- 5  # number of CV folds
for(k in 1:length(iter_values)){
  cat("k=", k, "\n")
  err_cv[k,] <- cv.function(train_data, train_labels, iter_values[k], K)
}


# Visualize CV results
plot(iter_values, err_cv[,1], xlab="Maximum Iteration", ylab="CV Error",
     main="Cross Validation Error", ylim=c(0.2, 0.4))
points(iter_values, err_cv[,1], col="blue", pch=16)
lines(iter_values, err_cv[,1], col="blue")
arrows(iter_values, err_cv[,1]-err_cv[,2],iter_values, err_cv[,1]+err_cv[,2], 
       length=0.1, angle=90, code=3)


# Choose the best parameter value
min(err_cv[,1])  # 0.314
iter_best <- iter_values[which.min(err_cv[,1] + err_cv[,2])]
par_best <- list(maxit=iter_best) 
par_best # maxit=27 is the best

# train the model with the entire training set
tm_train <- system.time(fit_train <- train.lg(train_data, train_labels, par_best))
pred_train <- test.lg(fit_train, train_data)
train.error <- mean(pred_train != train_labels)
train.error #0.272
1-train.error #0.728

### Make prediction 
tm_test <- system.time(pred_test <- test.lg(fit_train, test_data))
test.error <- mean(pred_test != test_labels)
test.error #0.309
1-test.error #0.691

### Summarize Running Time
cat("Time for training model=", tm_train[1], "s \n") # 0.267 s
cat("Time for making prediction=", tm_test[1], "s \n")  # 0.008 s



###########color
train_data<-color[train_index,]
test_data<-color[-train_index,]



# Choosing between different values of maximum interation for Logistic
iter_values <- seq(10,30,1)
err_cv <- array(dim=c(length(iter_values), 2))
K <- 5  # number of CV folds
for(k in 1:length(iter_values)){
  cat("k=", k, "\n")
  err_cv[k,] <- cv.function(train_data, train_labels, iter_values[k], K)
}


# Visualize CV results
plot(iter_values, err_cv[,1], xlab="Maximum Iteration", ylab="CV Error",
     main="Cross Validation Error", ylim=c(0.2, 0.4))
points(iter_values, err_cv[,1], col="blue", pch=16)
lines(iter_values, err_cv[,1], col="blue")
arrows(iter_values, err_cv[,1]-err_cv[,2],iter_values, err_cv[,1]+err_cv[,2], 
       length=0.1, angle=90, code=3)


# Choose the best parameter value
min(err_cv[,1])  # 0.2702222
iter_best <- iter_values[which.min(err_cv[,1] + err_cv[,2])]
par_best <- list(maxit=iter_best) 
par_best # maxit=13 is the best

# train the model with the entire training set
tm_train <- system.time(fit_train <- train.lg(train_data, train_labels, par_best))
pred_train <- test.lg(fit_train, train_data)
train.error <- mean(pred_train != train_labels)
train.error #0.2315556
1-train.error #0.7684444

### Make prediction 
tm_test <- system.time(pred_test <- test.lg(fit_train, test_data))
test.error <- mean(pred_test != test_labels)
test.error #0.2946667
1-test.error #0.7053333

### Summarize Running Time
cat("Time for training model=", tm_train[1], "s \n") # 10.439 s 
cat("Time for making prediction=", tm_test[1], "s \n")  # 0.202 s 

