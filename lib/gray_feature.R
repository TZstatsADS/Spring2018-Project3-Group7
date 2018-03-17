library("EBImage")
img_dir<-"../data/image/images/"
img_names<-list.files(img_dir)

Gbin<-seq(0,1,length.out =  250) 
gray_features<-data.frame(matrix(NA,3000,251))
colnames(gray_features)<-c('Image',paste('gray_',1:250,sep=""))
gray_features$Image<-img_names

for(i in 1:3000){
  print(i)
  img<-readImage(paste(img_dir,img_names[i],sep=""))
  img<-channel(img,"gray")
  img<-resize(img,256,256)
  img_mat<-imageData(img)
  
  ##grey
  freq_gray <- as.data.frame(table(factor(findInterval(img_mat, Gbin), levels = 1:250)))
  gray_features[i,2:251] <- as.numeric(freq_gray$Freq)/(ncol(img_mat)*nrow(img_mat))
  
}

write.csv(gray_features,file ="../data/gray_features.csv")
