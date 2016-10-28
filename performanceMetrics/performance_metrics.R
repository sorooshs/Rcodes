


require(ggplot2)
require(gridExtra)
require(ROCR)

Metrics <- function(predic,
                    label,
                    threshold = 0.5,
                    plot  = c()) {
  # Measurements:
  
  #       tpr: True Positive Rate
  #       tnr: True Negative Rate
  #       fpr: False Positive Rate
  #       fnr: False Negative Rate
  
  #       acc  : Accuracy
  #       f    : F-score
  #       lift : Lift         (p = %25)?
  #       roc  : Receiver operating characteristic
  #       auc  : area under ROC
  #       prbe : precision/recall break even point
  #       rmse : Root-mean-squared error
  #       mxe  : Mean cross-entropy
  #       phi  : Phi correlation coefﬁcient
  
  
  result <- data.frame(
    n.pos  = NA,
    n.neg = NA,
    n.pos.pred = NA,
    n.neg.pred = NA,
    tp = NA,
    tn = NA,
    fp = NA,
    fn = NA,
    tpr = NA,
    tnr = NA,
    fpr = NA,
    fnr = NA,
    ppv = NA,
    npv = NA,
    acc = NA,
    auc = NA,
    rmse = NA,
    f = NA
  )
  
  
  if (length(unique(label)) == 1) {
    return(OneClassMeasurements(
      predic = predic,
      label  = label,
      threshold = threshold
    ))
  }
  
  pred <- ROCR::prediction(predic, label)
  
  for (slot.name in c('tp', 'tn', 'fp', 'fn',
                      'n.pos.pred', 'n.neg.pred')) {
    cutoff.slot <- max(which(slot(pred, 'cutoffs')[[1]] > threshold))
    
    result[, slot.name] <-
      slot(pred, slot.name)[[1]][cutoff.slot]
  }
  
  for (slot.name in c('n.pos', 'n.neg')) {
    result[, slot.name] <- slot(pred, slot.name)[[1]]
  }
  
  for (slot.name in c('tpr', 'tnr', 'fpr', 'fnr',
                      'ppv', 'npv',
                      'acc', 'f')) {
    perf.slot <- performance(pred, slot.name)
    
    cutoff.slot <-
      max(which(slot(perf.slot, 'x.values')[[1]] > threshold))
    
    result[, slot.name] <-
      slot(perf.slot, 'y.values')[[1]][cutoff.slot]
    
    if (slot.name %in% plot) {
      library(gplots)
      plot(perf.slot)
      title(slot.name)
    }
  }
  
  
  for (slot.name in c('rmse', 'auc')) {
    perf.slot <- performance(pred, slot.name)
    result[, slot.name] <-
      slot(perf.slot, 'y.values')[1]
  }
  
  
  return(result)
  
}

OneClassMetrics <- function(predic,
                            label,
                            threshold) {
  result <- data.frame(
    n.pos  = NA,
    n.neg = NA,
    n.pos.pred = NA,
    n.neg.pred = NA,
    tp = NA,
    tn = NA,
    fp = NA,
    fn = NA,
    tpr = NA,
    tnr = NA,
    fpr = NA,
    fnr = NA,
    ppv = NA,
    npv = NA,
    acc = NA,
    auc = NA,
    rmse = NA,
    f = NA
  )
  
  result.table <-
    data.frame(predic = ifelse(predic > threshold, 1, 0),
               label = label)
  
  result$n.pos <- length(which(label == 1))
  result$n.neg <- length(which(label == 0))
  
  result$tp <- nrow(result.table[result.table$predic == 1 &
                                   result.table$label  == 1,])
  
  result$tn <- nrow(result.table[result.table$predic == 0 &
                                   result.table$label  == 0,])
  
  result$fp <- nrow(result.table[result.table$predic == 1 &
                                   result.table$label  == 0,])
  
  result$fn <- nrow(result.table[result.table$predic == 0 &
                                   result.table$label  == 1,])
  
  result$n.pos.pred <- result$tp + result$fp
  result$n.neg.pred <- result$tn + result$fn
  
  result$acc <- (result$tp + result$tn) / length(label)
  
  return(result)
}



which.thr <- function(measure = 'tpr',
                      value = 0.5,
                      predic,
                      label) {

  pred <- ROCR::prediction(predic, label)
  
  if (measure %in% c('tp', 'tn', 'fp', 'fn')) {
    
    slot.value <-
      max(which(slot(pred, measure)[[1]] >= value))
    
    threshold.value <-
      slot(pred, 'cutoffs')[[1]][slot.value]
    
  } else {

    perf.slot <- performance(pred, measure)
    
    slot.value <-
      max(which(slot(perf.slot, 'y.values')[[1]] >= value))
    
    threshold.value <-
      slot(perf.slot, 'x.values')[[1]][slot.value]
  }
  
  return(threshold.value)
  
}


PlotMetrics <- function(predic,
                        label) {
}


##
## Test cases
##

predic <- c(0.1, 0.21, 0.23, 0.25, 0.51, 0.59, 0.63, 0.71, 0.78, 0.86)
label  <- c(0,    0,    0,    1,    0,     1,    1,    1,    1,     1)


Metrics(predic = predic, label = label, threshold = .8)

which.thr(predic = predic, label = label,
                    value = .4, measure = 'tnr')

which.thr(predic = predic, label = label,
          value = 3, measure = 'tn')


## MY test
pred <- ROCR::prediction(predic, label)

cutoff <- slot(pred, 'cutoffs')[[1]]
tn <- slot(pred, 'tn')[[1]]
fn <- slot(pred, 'fn')[[1]]
npv <- slot(performance(pred, 'npv'), 'y.values')[[1]]
tnr <- slot(performance(pred, 'tnr'), 'y.values')[[1]]
b <- .1
fs1 <- (tnr * npv) * (b ^ 2 + 1) / (tnr + (b ^ 2 * npv)) 
b <- .3
fs3 <- (tnr * npv) * (b ^ 2 + 1) / (tnr + (b ^ 2 * npv)) 

obj.fun <- tn - 5*fn


m.val <- data.frame(cutoff = cutoff,
                         tn = tn,
                         fn = fn,
                         fs1 = fs1,
                         fs3 = fs3,
                         npv = npv,
                         tnr = tnr,
                         ojf = obj.fun)

m.val %>% top_n(2, fs1)
m.val %>% top_n(2, fs3)
m.val %>% top_n(2, obj.fun)





