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

source('./Utilities/oen_dt-util.R')
source('./Utilities/oen_metrics-util.R')
load("./3Dgauss.RData")

generated_data = as.data.frame(generated_data)

colnames(generated_data) = paste('feature', 1:4)

#split in train and test data
sample = sample.int(n = nrow(generated_data), size = floor(.9*nrow(generated_data)), replace = F)
train = generated_data[sample, ]
test  = generated_data[-sample, ]

dt_bts = OmarDecisionTree(max_depth=20,min_leaf_size=5,min_information_gain=1e-5)
dt_bts$fit(train[,1:(ncol(train)-1)],train[,(ncol(train))])

print(dt_bts)

pred = dt_bts$predict(test[,1:3])

#confusion matrix
eval = goodness_params(pred, test[,4])
print(eval)

#plot misclassification
misclassif_col = c("indianred1", "seagreen3")[1*(pred == test[,ncol(test)]) +1]

plot(test[,1], test[,2], col = misclassif_col, pch = 20, xlab='x', ylab='y')
title(paste('DT with ', nrow(test)-sum(pred == test[,ncol(test)]),
            ' out of ', nrow(test), ' misclassifications', sep=''))


