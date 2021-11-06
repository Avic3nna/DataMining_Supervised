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

source('./oen_knn-util.R')
source('./oen_metrics-util.R')
load("./3Dgauss.RData")

x = generated_data

#90/10 split of data
sample = sample.int(n = nrow(x), size = floor(.9*nrow(x)), replace = F)
train = x[sample, ]
test  = x[-sample, ]

#bruteforce find optimal k
misclassif_list = bruteforce_k(train, test, max_k = 5)
optim_neighs = find_optimal_k(misclassif_list)

#make final model
preds = knn_omar_pred(train, test, num_neigh = optim_neighs)
acc = sum(preds == test[,ncol(test)])/nrow(test)

cat(optim_neighs, '-NN accuracy: ', round(acc,3)*100, '%\n\n', sep='')

#confusion matrix
eval = goodness_params(preds, test[,4])
print(eval)

#plot misclassification
misclassif_col = c("indianred1", "seagreen3")[1*(preds == test[,ncol(test)]) +1]

plot(test[,1], test[,2], col = misclassif_col, pch = 20)
title(paste(optim_neighs, '-NN with ', misclassif_list[optim_neighs],
            ' out of ', nrow(test), ' misclassifications', sep=''))


