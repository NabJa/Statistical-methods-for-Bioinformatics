
############# Accuracy measures and Visualisatin #########


#Get various accuracy measures. 
#Input: ture values, estimated values, measurement to be returned
get_acc_measures <- function(truth, pred, measure = "all"){
  inp <- data.frame(pred, truth)
  conf_mat <- table(inp)
  
  if(measure == "confusion"){
    conf_mat
    return(conf_mat)
  }
  
  if(measure == "all"){
    accuracy <- sum(diag(conf_mat))/sum(conf_mat)
    recall <- conf_mat[4]/sum(conf_mat[3], conf_mat[4])
    precision <- conf_mat[4]/sum(conf_mat[4], conf_mat[2])
    specificity <- conf_mat[1]/sum(conf_mat[1], conf_mat[2])
    fdr <- conf_mat[2]/sum(conf_mat[2], conf_mat[4])
    f_score <- (2*recall*precision)/(recall + precision)
    
    acc_measures <- list(accuracy = accuracy, recall = recall, precision = precision,
                         specificity = specificity, FDR = fdr, F_score = f_score)
    
    acc_measures
    return(acc_measures)
  }
  
  if(measure == "accuracy"){
    accuracy <- sum(diag(conf_mat))/sum(conf_mat)
    return(accuracy)    
  }
  if(measure == "recall"){
    recall <- conf_mat[4]/sum(conf_mat[3], conf_mat[4])
    return(recall)    
  }
  if(measure == "precision"){
    precision <- conf_mat[4]/sum(conf_mat[4], conf_mat[2])
    return(precision)    
  }
  if(measure == "specificity"){
    specificity <- conf_mat[1]/sum(conf_mat[1], conf_mat[2])
    return(specificity)    
  }
  if(measure == "fdr"){
    fdr <- conf_mat[2]/sum(conf_mat[2], conf_mat[4])
    return(fdr)    
  }
  if(measure == "f_score"){
    recall <- conf_mat[4]/sum(conf_mat[3], conf_mat[4])
    precision <- conf_mat[4]/sum(conf_mat[4], conf_mat[2])
    f_score <- (2*recall*precision)/(recall + precision)
    return(f_score)    
  }
}

#Calcs confusion matrix. Input: ture values and estimated values
#(simple version of get_acc_measures)
confusion_matrix <- function(true, est){
  est[est < 0] <- 0
  est[est > 0] <- 1
  tp <- sum(which(est == 1) %in% which(true == 1))
  tn <- sum(which(est == 0) %in% which(true == 0))
  fp <- sum(which(est == 1) %in% which(true == 0))
  fn <- sum(which(est == 0) %in% which(true == 1))
  sens <- tp / (tp+fn)
  spec <- tn / (tn+fp)
  ppv <- tp / (tp+fp)
  
  data.frame(tp = tp, tn = tn, fp = fp, fn = fn, sens = sens, spec = spec, ppv = ppv)
  }

#Plots a roc. Input: ture values and estimated values
plot_simple_roc <- function(true, est){
  
  true <- true[order(est, decreasing=TRUE)]
  
  a <- data.frame(TPR=cumsum(true)/sum(true), FPR=cumsum(!true)/sum(!true), true)
  
  plot(a$FPR, a$TPR, type = "l", main = "ROC", xlab = "FPR", ylab = "TPR")
  abline(0,1, col = "grey")
}