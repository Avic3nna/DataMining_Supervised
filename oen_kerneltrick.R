rm(list=ls())


# 
# library(e1071)
# 
# model = svm(stm$clusters ~ stm$data)
# 
# predictedY= predict(model, stm$data)
# 
# points(predictedY, col = "blue", pch=4)
# 
# error = stm$clusters - predictedY
# svrPredictionRMSE = mean(error**2)  # 3.157061
# svrPredictionRMSE
# 
# 
# #### K(Xi, Xj ) = e^−||Xi−Xj ||^2/2σ2
# 
# # Parameters:
# # SVM-Type:  eps-regression 
# # SVM-Kernel:  radial 
# # cost:  1 
# # gamma:  0.5 
# # epsilon:  0.1
# 
# library(kernlab)
# 
# rbf <- rbfdot(sigma = 0.5)
# rbf
# kpar(rbf)
# 
# K <- rbf(stm$data[,1], stm$data[,2])
# K
# ?ksvm
# 
# mdo = ksvm(stm$data, kernel = rbfdot)
# ####
# 
# x_1 = stm$data[,1]
# x_2 = stm$data[,2]
# 
# fullvect = append(x_1,x_2)
# 
# 
# Z_x = array()
# sigma = 2
# 
# library(pracma)
# library(rdist)
# D = (pdist(stm$data,metric = "euclidean"))
# 
# D = exp(-(D*D)/ ( 2*sigma**2));
# z = x_1*x_2^2
# 
# 
# source('./oen_minkowski.R')
# 
# for(x1 in fullvect){
#   sum = 0
#   for(x2 in fullvect){
#     sum = sum + exp(-(minkowsky(x1,x2,2)**2)/(2*sigma**2))
#   }
#   Z_x = append(Z_x, sum)#/length(fullvect)
# }
# 
# Z_x= Z_x[-1] #remove the NAN
# #### K(Xi, Xj ) = e^−||Xi−Xj ||^2/2σ2
# 
# dat = Z_x[1:720]+Z_x[721:1440]
# 
# library(rgl)
# plot3d(x_1, x_2, (x_1**1 + x_2**2), col = stm$clusters)
# 
# 
# #########################
# 
# dataset = stm$data
# dataset = cbind(dataset, stm$clusters)
# library(e1071)

library(clusterSim)
stm<-shapes.two.moon(360)
x11()
plot(stm$data,col=rainbow(3)[stm$clusters])

x_1 = stm$data[,1]
x_2 = stm$data[,2]

# https://www.dcode.fr/lagrange-interpolating-polynomial
# picked 4 random points

z = -1.37845*x_2**3 - 2.11278*x_2**2 + 0.0656642*x_2 + 0.2 - x_1
y = function(x_2)(-1.37845*x_2**3 - 2.11278*x_2**2 + 0.0656642*x_2 + 0.2)

x11()
curve(y, from=-3, to=2, ylim=c(-1.5,1))
points(x_2, x_1, col=stm$clusters)


library(rgl)
plot3d(x_2, x_1, z, col = stm$clusters)
planes3d(0,0,1, alpha=0.5)


#to-do: make kernel from polynomial