#############################################################
### We will use HOG to extract the features for the images ###
#############################################################

### Authors: Fangbing Liu
### Project 3
### ADS Spring 2017

feature <- function(img_dir, export=T){
  
  ### Construct process features for training/testing images
  ### Input: a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  
  ### load libraries
  library("EBImage")
  library("OpenImageR")
  
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
  if(export){
    save(H, file=paste0("../output/HOG.RData"))
  }
  return(H)
}

