

# Set working directory and location of packages
#.libPaths( c( .libPaths(), 'location-of-packages') )
#setwd('')

# library(dplyr)
# library(randomForest)
# library(foreach)
# library(doSNOW)
# library(quantmod)

library(pacman)
p_load(dplyr, randomForest, foreach, doSNOW, quantmod)

NUM_OF_PROCESSOR <- 8
cluster <-
  registerDoSNOW(makeCluster(NUM_OF_PROCESSOR, type = "SOCK"))

rf.model <- function(formula,
                     input.train,
                     importance = FALSE,
                     ntree = 256,
                     model.path = '.', # Path to save the model
                     model.name = '') # Name of model to save
  {
  #  start.time <- Sys.time()
  model.rf <- randomForest(
    formula = formula,
    data = input.train,
    importance = importance,
    keep.forest = TRUE,
    ntree = ntree
  )
  #  end.time <- Sys.time()
  #  time.taken <- end.time - start.time
  # print(paste('time: ', time.taken))
  
  if( model.name != '' )
    saveRDS(model.rf, file = file.path(model.path, 
                                       paste0(model.name, '.rds')))
  model.rf
}

rf.predict <- function(..., input.test) {
  predict(rf.model(...), input.test)
}

# Parallel Random Forest
rf.model.parallel <-
  function(formula,
           input.train,
           n.tree = 256,
           importance = FALSE,
           model.path = '.', # Path to save the model
           model.name = '') # Name of model to save
  {
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
        importance = importance,
        keep.forest = TRUE,
        ntree = ntree
      )
    
    if( model.name != '' )
      saveRDS(model.rf, file = file.path(model.path, 
                                         paste0(model.name, '.rds')))
    
    model.rf
  }


rf.parallel.predict <- function(..., input.test) {
  predict(rf.model.parallel(...), input.test)
}


#**
rf.leave_one_out <-
  function(formula = formula,
           input = input,
           ntree = 256)
  {
    result <-
      foreach(ind = 1:nrow(input),
              .combine = 'c') %do% {
                rf.parallel.predict(
                  formula = formula,
                  input.train = input[-ind, ],
                  input.test = input[ind, ]
                )
              }
    
    result
  }



## TEST
# data(mtcars)
# formula <- as.formula('Final_Stage + Opp_Duplicate + Conversion_Prospecting + Initial_Meeting + RFP_Submitted + Verbal IO_Issued ~ IO_Signed-Counter_Signed')
# 
# ## TEST
# # one core random forest
# model.rf <- rf.model(formula = formula, input = mtcars)
# 
# ## TEST
# # one core random forest
# rf.predict(formula = formula,
#            input.train = mtcars[-1,],
#            input.test = mtcars[1,])
# 
# 
# ## TEST
# # Parallel Random Forest
# rf.model.parallel(formula = formula,
#                   input.train = mtcars,
#                   model.name = 'test')
# 
# 
# ## TEST
# # Parallel random forest
# rf.parallel.predict(formula = formula,
#                     input.train = mtcars[-1, ],
#                     input.test = mtcars[1, ])
# 
# ## TEST
# # rf.leave_one_out Example
# rf.leave_one_out(formula = formula, input = mtcars)
