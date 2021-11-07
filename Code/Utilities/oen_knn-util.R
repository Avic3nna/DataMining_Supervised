knn_omar_train = function(x, num_neigh = 0){
  labels = array(data=NA, dim = nrow(x))
  dist = array(data=NA, dim = nrow(x))
  
  for(px in seq(along = 1:dim(x)[1])){
    for(py in seq(along = 1:dim(x)[1]))
    {
      dist[py] = minkowsky(x[px, 1:(dim(x)[2]-1)], x[py,1:(dim(x)[2]-1)], 2)
    }
    sorted_dist = sort(dist, index.return=TRUE) #sort increasing
    
    neighbours = sorted_dist$ix[2:(num_neigh+1)]
    
    #get dominant class
    #returns named int, so only get name because thats the label
    dominant_class = names(sort(summary(as.factor(x[neighbours,ncol(x)])), decreasing=T))[1]
    labels[px] = dominant_class
  }
  return(labels)
}

knn_omar_pred = function(x_train, x_test, num_neigh = 3){
  prediction = array(data=NA, dim = nrow(x_test))
  dist = array(data=NA, dim = nrow(x_train))
  
  for(px in seq(along = 1:dim(x_test)[1])){
    for(py in seq(along = 1:dim(x_train)[1]))
    {
      dist[py] = minkowsky(x_test[px, 1:(dim(x_test)[2]-1)], x_train[py,1:(dim(x_train)[2]-1)], 2)
    }
    sorted_dist = sort(dist, index.return=TRUE) #sort increasing
    
    neighbours = sorted_dist$ix[1:num_neigh]
    
    #get dominant class
    #returns named int, so only get name because thats the label
    dominant_train_class = names(sort(summary(as.factor(x_train[neighbours,ncol(x_train)])), decreasing=T))[1]
    prediction[px] = dominant_train_class
  }
  return(prediction)
}

bruteforce_k = function(x_train, x_test, max_k = 20){
  prediction = array(data=NA, dim = nrow(x_test))
  dist = array(data=NA, dim = nrow(x_train))
  misclassif = array(data=NA, dim = max_k)
  
  for(k in seq(along = 1:max_k)){ #k=1 is overfitting
    cat("Bruteforcing k = ", k, ' ...\n')
    for(px in seq(along = 1:dim(x_test)[1])){
      for(py in seq(along = 1:dim(x_train)[1]))
      {
        dist[py] = minkowsky(x_test[px, 1:(dim(x_test)[2]-1)], x_train[py,1:(dim(x_train)[2]-1)], 2)
      }
      
      sorted_dist = sort(dist, index.return=TRUE) #sort increasing
      
      neighbours = sorted_dist$ix[1:k] #k=1 is overfitting
      #get dominant class
      #returns named int, so only get name because thats the label
      dominant_train_class = names(sort(summary(as.factor(x_train[neighbours,ncol(x_train)])), decreasing=T))[1]
      prediction[px] = dominant_train_class
    }
    
    misclassif[k] = (sum(prediction != x_test[,ncol(x_test)]))

  }
  return(misclassif)
}


find_optimal_k = function(misclassif_list){
  optimal_k = which.min(misclassif_list[-1])+1 #k=1 is overfitting
  return(optimal_k)
}
