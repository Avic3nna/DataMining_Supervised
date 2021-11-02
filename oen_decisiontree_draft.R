rm(list=ls())
set.seed(1337)


entropy = function(y){
  # assumes y is a factor with all levels
  if(length(y)==0) return(0)
  p = table(y)/length(y)
  sum(-p*log2(p))
}


DecisionTreeNode = setRefClass("DecisionTreeNode",
  fields = list(x = "data.frame",
  y = "ANY",
  is_leaf="logical",
  split_description="character",
  best_split="list",
  branches="list",
  depth="numeric",
  minimize_func="function",
  min_information_gain="numeric",
  min_leaf_size="numeric",
  max_depth="numeric"),
  methods = list(
    initialize = function(...){
      defaults = list(x = data.frame(),
                       y=c(),
                       depth=0,
                       minimize_func=entropy,
                       min_information_gain=1e-3,
                       min_leaf_size=20,
                       max_depth=3,
                       is_leaf=T,
                       split_description="root",
                       best_split=NULL,
                       branches=list("left"=NULL,"right"=NULL))
      
      params = list(...)
      fields = names(getRefClass()$fields())
      for( field in fields){
        if (!(field %in% names(params))) {
          params[[field]] = defaults[[field]]
        }
      }
      for( param in names(params)){
        do.call("<<-",list(param, params[[param]]))
      }
      
    },
    information_gain = function(split_lr){
      
      s1 = sum(split_lr)
      s2 = length(split_lr)-s1
      if ( s1 == 0 | s2 == 0) return(0)
      minimize_func(y)-s1/(s1+s2)*minimize_func(y[split_lr])-s2/(s1+s2)*minimize_func(y[!split_lr])
    },
    max_information_gain_split = function(feature){
      
      best_change = NA
      split_value = NA

      previous_val = NA
      for( val in sort(unique(feature))){
        split_lr = feature < val
        change = information_gain(split_lr) 
        
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
      results = sapply(x,function(feature) max_information_gain_split(feature))
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
    split_node = function() {
      if(depth < max_depth){ 
        best_feature_split() 
        if(!is.null(best_split) & best_split$best_change > min_information_gain ){
          
          split_lr = best_split_lr()
          if(sum(split_lr) >= min_leaf_size && length(split_lr)-sum(split_lr) >= min_leaf_size){
            is_leaf <<- F
            
            branches$left <<- .self$copy()
            branches$left$is_leaf <<- T
            branches$left$x <<-  branches$left$x[split_lr,]
            branches$left$y <<-  branches$left$y[split_lr]
            
            branches$left$split_description <<- paste(c(best_split$name,
                                                        "<",
                                                        as.numeric(as.character(round(best_split$split_value,2)))),
                                                        collapse = " ")
            
            branches$left$depth <<-  branches$left$depth+1
            branches$left$branches <<- list("left"=NULL,"right"=NULL)
            branches$left$split_node()
            
            branches$right <<- .self$copy()
            branches$right$is_leaf <<- T
            branches$right$x <<-  branches$right$x[!split_lr,]
            branches$right$y <<-  branches$right$y[!split_lr]
            
            branches$right$split_description <<- paste(c(best_split$name, ">=",
                                                          round(best_split$split_value,2)),
                                                          collapse = " ")
            
            branches$right$depth <<-  branches$right$depth+1
            branches$right$branches <<- list("left"=NULL,"right"=NULL)
            branches$right$split_node()
          }
        }
      }
      if(is_leaf){
        split_description <<- paste(c(split_description,
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
          predict_value = branches$left$predict_row(row)
        } else {
          predict_value = branches$right$predict_row(row)
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

DecisionTree = setRefClass("DecisionTree",
                            fields = list(minimize_func="function",
                                          min_information_gain="numeric",
                                          min_leaf_size="numeric",
                                          max_depth="numeric",
                                          root = "DecisionTreeNode"),
                            methods = list(
                              initialize = function(...){
                                defaults = list(minimize_func=entropy,
                                                 min_information_gain=1e-3,
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
                                for( param in names(params)){
                                  do.call("<<-",list(param, params[[param]]))
                                }
                                
                              },
                              fit = function(features,target){
                                root <<- DecisionTreeNode$new(x=features,
                                                              y=target,
                                                              minimize_func=minimize_func,
                                                              min_information_gain=min_information_gain,
                                                              min_leaf_size=min_leaf_size,
                                                              max_depth=max_depth
                                )
                                root$split_node()
                                
                              },
                              predict = function(features){
                                root$predict(features)
                              }
                            ))

print.DecisionTree = function(tree){
  print(tree$root)
}
data(iris)

print.DecisionTreeNode = function(node,level=0){
  response = paste("|->",node$split_description)
  if(level < node$max_depth){
    if(!is.null(node$branches$left)){
      response = paste0(response,"\n",paste(rep(" ",2*(level+1)),collapse=" "),print(node$branches$left,level+1))
    }
    if(!is.null(node$branches$right)){
      response = paste0(response,"\n",paste(rep(" ",2*(level+1)),collapse=" "),print(node$branches$right,level+1))
    }
  }
  
  if(level==0) {
    cat(response)
  } else {
    return(response)
  }
}


load("./3Dgauss.RData")
generated_data = as.data.frame(generated_data)

colnames(generated_data) = paste('feature', 1:4)
sample = sample.int(n = nrow(generated_data), size = floor(.9*nrow(generated_data)), replace = F)
train = generated_data[sample, ]
test  = generated_data[-sample, ]

# x = data.frame(generated_data)[sample(1:nrow(generated_data), 100),]
# colnames(x) = paste('feature', 1:4)
#colnames(x) = 1:ncol(x) #give number to the colnames

dt_bts = DecisionTree(max_depth=20,min_leaf_size=20,min_information_gain=1e-7)
dt_bts$fit(train[,1:3],train[,4])

print(dt_bts)

pred = dt_bts$predict(test[,1:3])
acc = sum(pred == test[,4])/nrow(test)

#0.9125 acc

# library(tree)
# dt_tree = tree(train$`feature 4` ~ train$`feature 1` + train$`feature 2` + train$`feature 3`, minsize = 40,mincut=20,mindev=1e-7)
# print(dt_tree)
# 
# pred = round(predict(dt_tree, test[,1:3]))
# acc = sum(pred == test[,4])/nrow(test)