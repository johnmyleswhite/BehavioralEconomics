discounted.value <- function(x, t, model, parameters)
{
  exponential.model <- function(x, t, parameters)
  {
    delta <- parameters[['delta']]
    return(x * delta ^ t)
  }
  
  hyperbolic.model <- function(x, t, parameters)
  {
    k <- parameters[['k']]
    return(x / (1 + k * t))
  }
  
  quasihyperbolic.model <- function(x, t, parameters)
  {
    beta <- parameters[['beta']]
    delta <- parameters[['delta']]
    
    if (t == 0)
    {
      return(x)
    }
    else
    {
      return(x * beta * delta ^ t)
    }
  }
  
  generalized.hyperbolic.model <- function(x, t, parameters)
  {
    alpha <- parameters[['alpha']]
    beta <- parameters[['beta']]
    
    return(x * (1 + alpha * t) ^ (- (beta / alpha)))
  }
  
  # Check parameters as in previous package.
  
  models <- list('exponential' = exponential.model,
                 'hyperbolic' = hyperbolic.model,
                 'quasihyperbolic' = quasihyperbolic.model,
                 'generalized-hyperbolic' = generalized.hyperbolic.model)
  
  return(do.call(models[[model]], list(x = x, t = t, parameters = parameters)))
}
