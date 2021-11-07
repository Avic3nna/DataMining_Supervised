# These classes (DTNode and DTree) contain all the variables and methods
# in order to recursively build the tree based on information gain.
# Object-oriented programming has been used because it felt the most logical
# way of getting the metadata of every node in the tree. I have got some inspi-
# ration from a OOP Random Forest in Python from scratch article:
# https://medium.com/@amaarora/implementing-random-forests-from-scratch-using-
# object-oriented-programming-in-python-in-5-simple-2f788afb9dbb
# And DT from scratch in Python implementation:
# https://github.com/bryantravissmith/FromScratch/blob/master/
# SupervisedLearning/DecisionTrees/Implementing%20Decision%20Trees%
# 20From%20Scratch%20Using%20Python%202.7.ipynb
# In order to get an idea about how to properly deal with this metadata.

OmarDecisionTreeNode = setRefClass("OmarDecisionTreeNode",
 fields = list(x = "data.frame",
               y = "ANY",
               is_leaf="logical",
               split_descr="character",
               best_split="list",
               treebranch="list",
               depth="numeric",
               min_info_gain="numeric",
               min_leaf_size="numeric",
               max_depth="numeric"),
 methods = list(
   initialize = function(...){
     defaults = list(x = data.frame(),
                     y=c(),
                     depth=0,
                     min_info_gain=1e-3,
                     min_leaf_size=20,
                     max_depth=3,
                     is_leaf=T,
                     split_descr="root",
                     best_split=NULL,
                     treebranch=list("left"=NULL,"right"=NULL))
     
     params = list(...)
     fields = names(getRefClass()$fields())
     for( field in fields){
       if (!(field %in% names(params))) {
         params[[field]] = defaults[[field]]
       }
     }
     for(param in names(params)){
       do.call("<<-",list(param, params[[param]]))
     }
   },
   
   #list of methods used
   entropy = function(y){
     # assumes y is a factor with all levels
     if(length(y)==0){
       return(0)
       }
     p = table(y)/length(y)
     return(sum(-p*log2(p)))
   },
   
   info_gain = function(split_lr){
     
     s1 = sum(split_lr)
     s2 = length(split_lr)-s1
     if( s1 == 0 | s2 == 0){
       return(0)
     }
     #info gain = Entropy(y) - Entropy_split_1 - Entropy_split_0
     return(entropy(y)-s1/(s1+s2)*entropy(y[split_lr])-s2/(s1+s2)*entropy(y[!split_lr]))
   },
   
   max_info_gain_split = function(feature){
     best_change = NA
     split_value = NA
     previous_val = NA
     
     for( val in sort(unique(feature))){
       split_lr = feature < val
       change = info_gain(split_lr) 
       
       s1 = sum(split_lr)
       s2 = length(split_lr)-s1
       if(is.na(best_change) & s1 >= min_leaf_size & s2 >= min_leaf_size){
         best_change = change
         split_value = ifelse(is.na(previous_val),
                              val,
                              mean(c(val,previous_val)))
       } else if( change > best_change & s1 >= min_leaf_size & s2 >= min_leaf_size ){
         best_change = change
         split_value = mean(c(val,previous_val))
       }
       previous_val = val
       
     }
     return(list("best_change"=best_change,
                 "split_value"=split_value))
   },
   best_feature_split = function(){
     results = sapply(x,function(feature) max_info_gain_split(feature))
     if (!all(is.na(unlist(results['best_change',])))) {
       best_name = names(which.max(results['best_change',]))
       best_result = results[,best_name]
       best_result[["name"]] = best_name
       best_split <<- best_result
     }
     
   },
   best_split_lr = function(){
     best_split_lr = x[,best_split$name] < best_split$split_value
     return(best_split_lr)
   },
   
   #Where the recursion happens
   split_node = function() {
     if(depth < max_depth){
       
       #get best split of features
       best_feature_split() 
       if(!is.null(best_split) & best_split$best_change > min_info_gain ){
         
         split_lr = best_split_lr()
         
         #leaf or node?
         if(sum(split_lr) >= min_leaf_size && length(split_lr)-sum(split_lr) >= min_leaf_size){
           #it's a node, so let's split it again recursively
           is_leaf <<- F
           
           treebranch$left <<- .self$copy()
           treebranch$left$is_leaf <<- T
           treebranch$left$x <<-  treebranch$left$x[split_lr,]
           treebranch$left$y <<-  treebranch$left$y[split_lr]
           
           #formatting for final tree output, feature x < value             
           treebranch$left$split_descr <<- paste(c(best_split$name,
                                                       "<",
                                                       as.numeric(as.character(round(best_split$split_value,2)))),
                                                     collapse = " ")
           
           #increase tree depth
           treebranch$left$depth <<-  treebranch$left$depth+1
           treebranch$left$treebranch <<- list("left"=NULL,"right"=NULL)
           #split this node again on the left side, recursively
           treebranch$left$split_node()
           
           treebranch$right <<- .self$copy()
           treebranch$right$is_leaf <<- T
           treebranch$right$x <<-  treebranch$right$x[!split_lr,]
           treebranch$right$y <<-  treebranch$right$y[!split_lr]
           
           #formatting for final tree output, feature x >= value
           treebranch$right$split_descr <<- paste(c(best_split$name, ">=",
                                                        round(best_split$split_value,2)),
                                                      collapse = " ")
           
           treebranch$right$depth <<-  treebranch$right$depth+1
           treebranch$right$treebranch <<- list("left"=NULL,"right"=NULL)
           #split this node again on the right side, recursively
           treebranch$right$split_node()
         }
       }
     }
     if(is_leaf){
       #it's a leaf, so get the dominant class in the remaining data
       split_descr <<- paste(c(split_descr,
                                     ":",
                                     "predict - ",
                                     names(which.max(table(y)))),
                                   collapse=" ")
     }
     
   },
   predict_row = function(row){
     if(is_leaf){
       predict_value = names(which.max(table(y)))
     } else {
       
       left = row[best_split$name] < best_split$split_value
       
       if(left){
         predict_value = treebranch$left$predict_row(row)
       } else {
         predict_value = treebranch$right$predict_row(row)
       }
     }
     return(predict_value)
   },
   predict = function(features){
     pred = character(length=dim(features)[1])
     for(i in 1:dim(features)[1]){
       pred[i] = predict_row(features[i,])
     }
     pred
   }
 )
)


# Final DTree class for oversight and scalability,
# using the DTnode class as root to build upon
OmarDecisionTree = setRefClass("OmarDecisionTree",
 fields = list(min_info_gain="numeric",
               min_leaf_size="numeric",
               max_depth="numeric",
               root = "OmarDecisionTreeNode"),
 methods = list(
   initialize = function(...){
     defaults = list( min_info_gain=1e-3,
                      min_leaf_size=20,
                      max_depth=3,
                      root=NULL)
     
     params = list(...)
     fields = names(getRefClass()$fields())
     for( field in fields){
       if (!(field %in% names(params))) {
         params[[field]] = defaults[[field]]
       }
     }
     # shortcut to initalise all the variables
     # within one loop ?do.call
     for( param in names(params)){
       do.call("<<-",list(param, params[[param]]))
     }
     },
   
     #fit tree with given features
     fit = function(features,target){
       root <<- OmarDecisionTreeNode$new(x=features,
                                     y=target,
                                     min_info_gain=min_info_gain,
                                     min_leaf_size=min_leaf_size,
                                     max_depth=max_depth
       )
       root$split_node()
       
     },
     #predict using tree obj
     predict = function(features){
       root$predict(features)
     }
   ))

print.OmarDecisionTree = function(tree){
  print(tree$root)
}


# prints the tree in a nice format
print.OmarDecisionTreeNode = function(node,level=0){
  response = paste("|->",node$split_descr)
  if(level < node$max_depth){
    if(!is.null(node$treebranch$left)){
      response = paste0(response,"\n",paste(rep(" ",2*(level+1)),collapse=" "),print(node$treebranch$left,level+1))
    }
    if(!is.null(node$treebranch$right)){
      response = paste0(response,"\n",paste(rep(" ",2*(level+1)),collapse=" "),print(node$treebranch$right,level+1))
    }
  }
  
  if(level==0) {
    cat(response)
  } else {
    return(response)
  }
}
