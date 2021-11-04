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

source('./oen_dt-util.R')

load("./3Dgauss.RData")
generated_data = as.data.frame(generated_data)

colnames(generated_data) = paste('feature', 1:4)
sample = sample.int(n = nrow(generated_data), size = floor(.9*nrow(generated_data)), replace = F)
train = generated_data[sample, ]
test  = generated_data[-sample, ]

dt_bts = OmarDecisionTree(max_depth=20,min_leaf_size=20,min_information_gain=1e-7)
dt_bts$fit(train[,1:3],train[,4])

print(dt_bts)

pred = dt_bts$predict(test[,1:3])
acc = sum(pred == test[,4])/nrow(test)

#0.9125 acc
