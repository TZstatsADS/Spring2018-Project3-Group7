library("adabag")


img_labels<-read.csv("data/image/label_train.csv")
colnames(img_labels)=c("Image","labels")

features<-read.csv("data/image/sift_train.csv",as.is = F, header=F)
gray<-read.csv("output/gray_features.csv",as.is = F)
hog<-read.csv("output/hog_feature.csv",as.is = F)
sift_pca<-read.csv("output/sift_pca.csv",as.is = F)
color<-read.csv("output/color_features.csv",as.is = F)

set.seed(90)
train_index<-sample(1:3000,floor(nrow(img_labels)*0.75))

###########sift
train_data<-features[train_index,]
test_data<-features[-train_index,]

##########gray
train_data<-gray[train_index,]
test_data<-gray[-train_index,]

###########hog
train_data<-hog[train_index,]
test_data<-hog[-train_index,]

###########pca
train_data<-sift_pca[train_index,]
test_data<-sift_pca[-train_index,]

###########color
train_data<-color[train_index,]
test_data<-color[-train_index,]


train_labels<-img_labels[train_index,3]
test_labels<-img_labels[-train_index,3]

train_data$labels<-factor(train_labels)









######## ntree=70
library("adabag")
begin=Sys.time()
adabag_fit=boosting(labels~.,train_data[,-1],mfinal=70,coeflearn="Zhu")
end=Sys.time()
end-begin
#1.312945 hours

pred_begin=Sys.time()
pred_adabag=predict.boosting(adabag_fit,newdata = test_data[,-1])
pred_end=Sys.time()
pred_end-pred_begin

mean((as.integer(pred_adabag$class))!=test_labels)

save(adabag_fit,train_index,file="~/Desktop/adabag_model_70.RData")


