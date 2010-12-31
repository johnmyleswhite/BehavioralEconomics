fit.discounting.model <-
function(choices,
                                  model = 'exponential',
                                  method = 'bayes',
                                  n.adapt = 1000,
                                  n.iter = 1000)
{
  # We assume that choices contains
  # X1,T1,X2,T2,C
  # as columns.
  models <- list('exponential' = fit.exponential.discounting.model)
  
  return(models[[model]])
}
