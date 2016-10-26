




# Set working directory and location of packages
#.libPaths( c( .libPaths(), 'location-of-packages') )
#setwd('')

library(dplyr)
library(randomForest)
library(foreach)
library(doSNOW)

NUM_OF_PROCESSOR <- 4
cluster <-
  registerDoSNOW(makeCluster(NUM_OF_PROCESSOR, type = "SOCK"))

rf.model <- function(formula,
                     input,
                     ntree = 256,
                     seed.value = 1234) {
  set.seed(seed.value)
  
  #  start.time <- Sys.time()
  model.rf <-
    randomForest(
      formula = formula,
      data = input,
      importance = FALSE,
      keep.forest = TRUE,
      ntree = ntree
    )
  #  end.time <- Sys.time()
  #  time.taken <- end.time - start.time
  # print(paste('time: ', time.taken))
  
  return(model.rf)
  
}

# Parallel Random Forest
rf.model.parallel <-
  function(formula,
           input.train,
           input.test,
           n.tree = 256,
           seed.value = 1234) {
    set.seed(seed.value)
    
    ## Multiprocessor RF
    model.rf <-
      foreach(
        ntree = rep(round(n.tree / NUM_OF_PROCESSOR), NUM_OF_PROCESSOR),
        .combine = combine,
        .packages = 'randomForest'
      ) %dopar%
      randomForest(
        formula = formula,
        data = input.train,
        importance = FALSE,
        keep.forest = TRUE,
        ntree = ntree
      )
    
    return(predict(model.rf, input.test))
    
  }


#**
rf.leave_one_out <-
  function(formula = formula,
           input = input,
           ntree = 256,
           seed.value = 1234)
  {
    set.seed(seed.value)
    
    result <-
      foreach(
        ind = 1:nrow(input),
        .combine = 'c'
      ) %do% {
        rf.model.parallel(
          formula = formula,
          input.train = input[-ind,],
          input.test = input[ind,]
        )
      }
    result
  }


data(mtcars)
formula <- as.formula('mpg ~ .')
rf.model(formula = formula,
         input = mtcars)


# rf.model.parallel example
cat(paste0(
  'Prediction = ',
  rf.model.parallel(
    formula = formula,
    input.train = mtcars[-1, ],
    input.test  = mtcars[1, ]
  ) %>% round(3),
  ' -- ',
  'Actual Value = ',
  mtcars[1, 'mpg']
))

# rf.leave_one_out Example
rf.leave_one_out(formula = formula, input = mtcars)
