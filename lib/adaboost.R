library("adabag")

##load data

img_labels<-read.csv("../data/image/label_train.csv")
colnames(img_labels)=c("Image","labels")

features<-read.csv("../data/image/SIFT_train.csv",as.is = F, header=F)
gray<-read.csv("../output/gray_features.csv",as.is = F)
hog<-read.csv("../output/hog_feature.csv",as.is = F)
sift_pca<-read.csv("../output/sift_pca.csv",as.is = F)
color<-read.csv("../output/color_features.csv",as.is = F)

set.seed(90)
train_index<-sample(1:3000,floor(nrow(img_labels)*0.75))
train_labels<-img_labels[train_index,3]
test_labels<-img_labels[-train_index,3]


##############sift
train_data<-features[train_index,]
test_data<-features[-train_index,]
train_data$labels<-factor(train_labels)



######## ntree=30
begin=Sys.time()
adabag_fit30=boosting(labels~.,train_data[,-1],mfinal=30,coeflearn="Zhu")
end=Sys.time()
end-begin
#

pred_begin=Sys.time()
pred_adabag30=predict.boosting(adabag_fit30,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#

mean((as.integer(pred_adabag30$class)) ==test_labels)
#

######## ntree=20
begin=Sys.time()
adabag_fit20=boosting(labels~.,train_data[,-1],mfinal=20,coeflearn="Zhu")
end=Sys.time()
end-begin
#

pred_begin=Sys.time()
pred_adabag20=predict.boosting(adabag_fit20,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#

mean((as.integer(pred_adabag20$class))==test_labels)
#

######## ntree=10
begin=Sys.time()
adabag_fit10=boosting(labels~.,train_data[,-1],mfinal=10,coeflearn="Zhu")
end=Sys.time()
end-begin
#

pred_begin=Sys.time()
pred_adabag10=predict.boosting(adabag_fit10,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#
mean((as.integer(pred_adabag10$class))==test_labels)





##########gray
train_data<-gray[train_index,]
test_data<-gray[-train_index,]
train_data$labels<-factor(train_labels)

######## ntree=30
begin=Sys.time()
adabag_fit30=boosting(labels~.,train_data[,-1],mfinal=30,coeflearn="Zhu")
end=Sys.time()
end-begin
#

pred_begin=Sys.time()
pred_adabag30=predict.boosting(adabag_fit30,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#

mean((as.integer(pred_adabag30$class)) ==test_labels)
#

######## ntree=20
begin=Sys.time()
adabag_fit20=boosting(labels~.,train_data[,-1],mfinal=20,coeflearn="Zhu")
end=Sys.time()
end-begin
#

pred_begin=Sys.time()
pred_adabag20=predict.boosting(adabag_fit20,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#

mean((as.integer(pred_adabag20$class))==test_labels)
#

######## ntree=10
begin=Sys.time()
adabag_fit10=boosting(labels~.,train_data[,-1],mfinal=10,coeflearn="Zhu")
end=Sys.time()
end-begin
#

pred_begin=Sys.time()
pred_adabag10=predict.boosting(adabag_fit10,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#

mean((as.integer(pred_adabag10$class))==test_labels)
#



###########hog
train_data<-hog[train_index,]
test_data<-hog[-train_index,]
train_data$labels<-factor(train_labels)


######## ntree=30
begin=Sys.time()
adabag_fit30=boosting(labels~.,train_data[,-1],mfinal=30,coeflearn="Zhu")
end=Sys.time()
end-begin
#17.88422 secs

pred_begin=Sys.time()
pred_adabag30=predict.boosting(adabag_fit30,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#0.182157 secs

mean((as.integer(pred_adabag30$class)) !=test_labels)
mean((as.integer(pred_adabag30$class)) ==test_labels)
#0.704

######## ntree=20
begin=Sys.time()
adabag_fit20=boosting(labels~.,train_data[,-1],mfinal=20,coeflearn="Zhu")
end=Sys.time()
end-begin
#12.9455 secs

pred_begin=Sys.time()
pred_adabag20=predict.boosting(adabag_fit20,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#0.117924 secs

mean((as.integer(pred_adabag20$class))!=test_labels)
#0.292
mean((as.integer(pred_adabag20$class))==test_labels)
#0.708

######## ntree=10
begin=Sys.time()
adabag_fit10=boosting(labels~.,train_data[,-1],mfinal=10,coeflearn="Zhu")
end=Sys.time()
end-begin
#6.872449 secs

pred_begin=Sys.time()
pred_adabag10=predict.boosting(adabag_fit10,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#0.08447599 secs

mean((as.integer(pred_adabag10$class))!=test_labels)

mean((as.integer(pred_adabag10$class))==test_labels)
#0.692





###############pca
train_data<-sift_pca[train_index,]
test_data<-sift_pca[-train_index,]
train_data$labels<-factor(train_labels)

######## ntree=30
begin=Sys.time()
adabag_fit30=boosting(labels~.,train_data[,-1],mfinal=30,coeflearn="Zhu")
end=Sys.time()
end-begin
#28.71619 secs

pred_begin=Sys.time()
pred_adabag30=predict.boosting(adabag_fit30,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#0.262198 secs

mean((as.integer(pred_adabag30$class)) !=test_labels)
#0.3133333
mean((as.integer(pred_adabag30$class)) ==test_labels)
#0.6866667

######## ntree=20
begin=Sys.time()
adabag_fit20=boosting(labels~.,train_data[,-1],mfinal=20,coeflearn="Zhu")
end=Sys.time()
end-begin
#19.9272 secs

pred_begin=Sys.time()
pred_adabag20=predict.boosting(adabag_fit20,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#0.189779 secs

mean((as.integer(pred_adabag20$class))!=test_labels)
#0.3533333
mean((as.integer(pred_adabag20$class))==test_labels)
#0.6466667

######## ntree=10
begin=Sys.time()
adabag_fit10=boosting(labels~.,train_data[,-1],mfinal=10,coeflearn="Zhu")
end=Sys.time()
end-begin
#10.50087 secs

pred_begin=Sys.time()
pred_adabag10=predict.boosting(adabag_fit10,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#0.09903789 secs

mean((as.integer(pred_adabag10$class))!=test_labels)
#0.36
mean((as.integer(pred_adabag10$class))==test_labels)
#0.64



##############color
train_data<-color[train_index,]
test_data<-color[-train_index,]
train_data$labels<-factor(train_labels)


######## ntree=30
begin=Sys.time()
adabag_fit30=boosting(labels~.,train_data[,-1],mfinal=30,coeflearn="Zhu")
end=Sys.time()
end-begin
#4.310529 mins

pred_begin=Sys.time()
pred_adabag30=predict.boosting(adabag_fit30,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#6.888564 secs

mean((as.integer(pred_adabag30$class)) !=test_labels)
#0.18
mean((as.integer(pred_adabag30$class)) ==test_labels)
#0.82

######## ntree=20
begin=Sys.time()
adabag_fit20=boosting(labels~.,train_data[,-1],mfinal=20,coeflearn="Zhu")
end=Sys.time()
end-begin
#3.119595 mins
adabag_fit20

pred_begin=Sys.time()
pred_adabag20=predict.boosting(adabag_fit20,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#4.597286 secs

mean((as.integer(pred_adabag20$class))!=test_labels)
#0.1813333
mean((as.integer(pred_adabag20$class))==test_labels)
#0.8186667

######## ntree=10
begin=Sys.time()
adabag_fit10=boosting(labels~.,train_data[,-1],mfinal=10,coeflearn="Zhu")
end=Sys.time()
end-begin
#1.681485 mins

pred_begin=Sys.time()
pred_adabag10=predict.boosting(adabag_fit10,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin
#2.395044 secs

mean((as.integer(pred_adabag10$class))!=test_labels)
#0.2306667
mean((as.integer(pred_adabag10$class))==test_labels)
# 0.7693333

