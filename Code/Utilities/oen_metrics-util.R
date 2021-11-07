goodness_params = function(pred, actual){
  cnf_mat = table(factor(pred), factor(test[,ncol(test)]), dnn= c('Pred.', 'Ref.'))
  
  precision = array(NA, dim = nrow(cnf_mat))
  recall = array(NA, dim = nrow(cnf_mat))
  F1 = array(NA, dim = nrow(cnf_mat))
  accuracy = array(NA, dim = nrow(cnf_mat))
  
  for (i in seq(along = 1:nrow(cnf_mat))){
    precision[i] = cnf_mat[i,i] / sum (cnf_mat[i,])
    recall[i] = cnf_mat[i,i] / sum (cnf_mat[,i])
    F1[i] = 2 * precision[i] * recall[i] / (precision[i] + recall[i])
  }
  
  total_accuracy = sum(diag(cnf_mat)) / sum(cnf_mat)
  cat('\n\n')
  print(cnf_mat)
  cat('\n\nTotal acc.:', round(total_accuracy,3)*100, '%\n\n')
  
  means = matrix(c(round(mean(precision),4), round(mean(recall),4), 
                   round(mean(F1),4)), ncol=1)
  

  tab = matrix(c(round(precision,4), round(recall,4), round(F1,4)), ncol=ncol(cnf_mat), byrow=TRUE)

  
  colnames(tab)= paste('Class', 1:(ncol(cnf_mat)))
  tab = cbind(tab, means)
  colnames(tab)[ncol(cnf_mat)+1] = 'Overall'
  rownames(tab) <- c('Precision','Recall','F1-score')
  return (tab)
}

