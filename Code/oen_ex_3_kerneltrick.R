rm(list=ls())
set.seed(1337)

packages_used = c("rstudioapi",
                  "rgl",
                  "clusterSim")

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

library(rgl)
library(clusterSim)

stm<-shapes.two.moon(360)

plot(stm$data,col=rainbow(3)[stm$clusters])

x_1 = stm$data[,1]
x_2 = stm$data[,2]

# https://www.dcode.fr/lagrange-interpolating-polynomial
# picked points between the gap to go through

z = -1.37845*x_2**3 - 2.11278*x_2**2 + 0.0656642*x_2 + 0.2 - x_1
y = function(x_2)(-1.37845*x_2**3 - 2.11278*x_2**2 + 0.0656642*x_2 + 0.2)


curve(y, from=-3, to=2, ylim=c(-1.5,1))
#title('Polynomial kernel separating two moons')
points(x_2, x_1, col=stm$clusters)


plot3d(x_2, x_1, z, col = stm$clusters, xlab='x', ylab='y', zlab='z')
planes3d(0,0,1, alpha=0.5)
