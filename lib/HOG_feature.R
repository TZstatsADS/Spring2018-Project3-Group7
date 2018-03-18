###Author??? Fangbing Liu
###ADS Project 3 - Spring 2018

###This file using HOG to ectract the feature.

### load libraries
if(!require(EBImage)){
  source("http://bioconductor.org/biocLite.R")
  biocLite("EBImage")
}
if(!require(OpenImageR, dplyr)){
  install.packages("OpenImageR", "dplyr")
}
library("EBImage")
library("OpenImageR")
library("dplyr")

img_dir <- "../data/train/images/"

dir_names <- list.files(img_dir)
n_files <- length(dir_names)

img0 <- readImage(paste0(img_dir, dir_names[1]))
n_r <- length(HOG(img0))

### extract HOG features of images
H <- matrix(NA, n_files, n_r) 

for(i in 1:n_files){
  img <- readImage(paste0(img_dir, dir_names[i]))
  H[i,] <- HOG(img)
}

### output constructed features
write.csv(H,"../output/hog_feature.csv")




