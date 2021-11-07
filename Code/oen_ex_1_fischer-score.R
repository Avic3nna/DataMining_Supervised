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
  print(getwd())
}
setwd_current_path()

load("./3Dgauss.RData")


x=generated_data


#assumes dataset with labels included
num_features = dim(x)[2]-1
num_samples = dim(x)[1]

features = x[,1:num_features]
labels = x[,num_features+1]

num_classes = length(unique(labels))


#fischer score list
fischer_score = list()

for(y in seq(along = 1:num_features)){
  #initialise intermediate calculations per feature
  im_result = 0
  sum_top = 0
  sum_bottom = 0 
  
  #calculate the 'global mean' for this feature
  mu = mean(x[,y])
  
  for(z in seq(along = 1:num_classes)){
    #get data for specific class (situated in last column features+1)
    data_for_class = x[x[,num_features+1] == z,]
    
    #calculate mean and std for this feature and class
    mu_j = mean(data_for_class[,y], na.rm = TRUE)
    std_j = sd(data_for_class[,y], na.rm = TRUE)
    
    #get number of datapoints
    p_j = nrow(data_for_class)/nrow(x)
    
    #perform calculations according to formula
    sum_top = sum_top + p_j*(mu_j - mu)**2
    sum_bottom = sum_bottom + p_j*(std_j**2)
  }
  #perform final computation
  im_result = sum_top/sum_bottom
  
  #append to the list with Fischer scores
  fischer_score[[y]] = im_result
}
print(fischer_score)

#library(Rdimtools)
# # third party implementation for validation
# fscore_thirdparty = do.fscore(features, labels)
# print("Most important features according to 3rd party: ")
# print(fscore_thirdparty$featidx)
# plot(fscore_thirdparty$Y, pch=19, col=factor(labels), main="Fisher Score")

