OmarDecisionTreeNode = setRefClass("OmarDecisionTreeNode",
                               fields = list(x = "data.frame",
                                             y = "ANY",
                                             is_leaf="logical",
                                             split_description="character",
                                             best_split="list",
                                             branches="list",
                                             depth="numeric",
                                             min_information_gain="numeric",
                                             min_leaf_size="numeric",
                                             max_depth="numeric"),
                               methods = list(
                                 initialize = function(...){
                                   defaults = list(x = data.frame(),
                                                   y=c(),
                                                   depth=0,
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
                                 
                                 entropy = function(y){
                                   # assumes y is a factor with all levels
                                   if(length(y)==0) return(0)
                                   p = table(y)/length(y)
                                   sum(-p*log2(p))
                                 },
                                 
                                 information_gain = function(split_lr){
                                   
                                   s1 = sum(split_lr)
                                   s2 = length(split_lr)-s1
                                   if ( s1 == 0 | s2 == 0) return(0)
                                   entropy(y)-s1/(s1+s2)*entropy(y[split_lr])-s2/(s1+s2)*entropy(y[!split_lr])
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

OmarDecisionTree = setRefClass("OmarDecisionTree",
                           fields = list(min_information_gain="numeric",
                                         min_leaf_size="numeric",
                                         max_depth="numeric",
                                         root = "OmarDecisionTreeNode"),
                           methods = list(
                             initialize = function(...){
                               defaults = list( min_information_gain=1e-3,
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
                               root <<- OmarDecisionTreeNode$new(x=features,
                                                             y=target,
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

print.OmarDecisionTree = function(tree){
  print(tree$root)
}

print.OmarDecisionTreeNode = function(node,level=0){
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
