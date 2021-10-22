rm(list=ls())
set.seed(1337)


library(Rdimtools)

#source("./Exercise 1. Distance function/oen_minkowski.R")

data(iris)

x=as.matrix(iris[,1:4])
labels = as.factor(iris[,5])

#assumes dataset with labels included
num_features = dim(x)[2]
num_samples = dim(x)[1]

features = x[,1:num_features]


num_classes = length(unique(labels))

# can we use the mean / sd function?


#fischer score list
fischer_score = list()
library(Matrix)
for(y in colnames(iris)[1:4]){
  im_result = 0
  sum_top = 0
  sum_bottom = 0
  mu = mean(as.matrix(iris[,y]))
  for(z in labels){
    data_for_class = as.matrix(iris[iris[,5] == z,y])
    mu_j = mean(data_for_class, na.rm = TRUE)
    std_j = sd(data_for_class, na.rm = TRUE)
    p_j = nrow(data_for_class)
    sum_top = sum_top + p_j*(mu_j - mu)**2
    sum_bottom = sum_bottom + p_j*(std_j**2)
  }
  im_result = sum_top/sum_bottom
  fischer_score[[y]] = im_result
}
print(fischer_score)

x11()

plot(x[,1], x[,2], pch=19, col=factor(labels), main="Fisher Score")

fscore_thirdparty = do.fscore(features, labels)
print("Most important features according to 3rd party: ")
print(fscore_thirdparty$featidx)
x11()
plot(fscore_thirdparty$Y, pch=19, col=factor(labels), main="Fisher Score")
