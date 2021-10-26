rm(list=ls())
set.seed(1337)

packages_used = c("rstudioapi")

for(package in packages_used){
  if(package %in% rownames(installed.packages()) == FALSE) {
    install.packages(package)
  }
}

setwd_current_path = function(){
  library(rstudioapi)
  current_path = getActiveDocumentContext()$path
  setwd(dirname(current_path)) #get this current folder
  #setwd('..') #go 1 up for scalability
  print(getwd())
}
setwd_current_path()


load("./3Dgauss.RData")

#assuming labeled data
#PCA is generally applied after subtracting the mean of the data set from each data point.

pca_self = function(x, variability = 0.95, center_it=TRUE, scale_it = TRUE){
  
  if(scale_it){
    x <- scale(x, center = center_it, scale = scale_it)
   }
  
  if(center_it){
    x_centered = matrix(NA, nrow = dim(x)[1], ncol = dim(x)[2])
    
    for(i in seq(along = 1:dim(x)[2])){
      x_centered[,i] = x[,i] - mean(x[,i])
      #print(mean(x[,i]))
    }
    x = x_centered
  }
  

  
  covar_x = cov(x)
  P = eigen(covar_x)
  
  total_variab = sum(P$values)
  ##Number of required components

  
  pca_comp = 0
  for(z in seq(along= 1:length(P$values))){

    if(sum(P$values[1:z])/sum(P$values) >= variability){
      pca_comp = z
      break
    }
  }

  rotated_x = x[, 1:z] %*% P$vectors[1:z, 1:z] #eigenvectors, aka rotation
  
  
  metadata <- list(rotated_data = rotated_x, rotation = P$vectors,
                   is_centered = center_it, is_scaled = scale_it, no_components = pca_comp, 
                   cum_variab = sum(P$values[1:pca_comp])/sum(P$values), total_variab = P$values)
  
  class(metadata) <- "PCA Omar"
  return(metadata)
}

x= generated_data[, 1:(dim(generated_data)[2] - 1)]


data(iris)

x=as.matrix(iris[,1:4])

pca = pca_self(x, variability = 1, center_it=TRUE, scale_it = TRUE)

x11()
plot(pca$rotated_data[,1], pca$rotated_data[,2], col = iris[,5])



### 3rd party validation

library(stats)

pc <- prcomp(x,
             center = TRUE,
             scale. = TRUE)

summary(pc)
