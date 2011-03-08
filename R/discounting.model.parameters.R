discounting.model.parameters <- function(model)
{
  parameters <- list('exponential' = c('a', 'gamma', 'delta'),
                     'generalized-hyperbolic' = c('a', 'gamma', 'alpha', 'beta'),
                     'concave-waiting' = c('a', 'gamma', 'delta', 'alpha', 'eta'))
  
  return(parameters[[model]])
}
