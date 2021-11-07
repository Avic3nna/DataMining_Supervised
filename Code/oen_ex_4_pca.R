rm(list=ls())
set.seed(1337)

packages_used = c("rstudioapi",
                  "stats")

for(package in packages_used){
  if(package %in% rownames(installed.packages()) == FALSE) {
    install.packages(package)
  }
}

setwd_current_path = function(){
  library(rstudioapi)
  current_path = getActiveDocumentContext()$path
  setwd(dirname(current_path)) #get this current folder
  print(getwd())
}
setwd_current_path()

library(stats)

load("./3Dgauss.RData")

#assuming labeled data
#PCA is generally applied after subtracting the mean of the data set from each data point.

pca_self = function(x, variability = 0.95, center_it=TRUE, scale_it = TRUE){
  
  if(scale_it){
    x <- scale(x, center = center_it, scale = scale_it)
   }
  
  # if(center_it){
  #   x_centered = matrix(NA, nrow = dim(x)[1], ncol = dim(x)[2])
  #   
  #   for(i in seq(along = 1:dim(x)[2])){
  #     x_centered[,i] = x[,i] - mean(x[,i])
  #     #print(mean(x[,i]))
  #   }
  #   x = x_centered
  # }
  

  
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
  print(z)
  rotated_x = x[, 1:z] %*% P$vectors[1:z, 1:z] #eigenvectors, aka rotation
  
  
  metadata <- list(rotated_data = rotated_x, rotation = P$vectors,
                   is_centered = center_it, is_scaled = scale_it, no_components = pca_comp, 
                   cum_variab = sum(P$values[1:pca_comp])/sum(P$values), total_variab = P$values)
  
  class(metadata) <- "PCA Omar"
  return(metadata)
}

x= generated_data[, 1:(dim(generated_data)[2] - 1)]




# If PCA does not work, your features either have non-linear relationships 
# or no relationships at all.


cols = 5
dataset = matrix(rnorm(cols * 500),ncol= cols)
dataset=cbind(dataset,dataset[,1]*sin(dataset[,1]))
dataset=cbind(dataset,dataset[,1]*cos(dataset[,1]))
dataset=cbind(dataset,dataset[,1]*tanh(dataset[,2]))

cor_b4 = cor(dataset)
cor_b4

pca = pca_self(dataset, variability = 1, center_it=TRUE, scale_it = TRUE)
expl_var = pca$total_variab/sum(pca$total_variab)

barplot(expl_var, col = 'blue', xlab = 'PC', ylab = 'Variance')
title('Variance per Principal Component')

cor(dataset) #before pca
cor(pca$rotated_data) #after pca
