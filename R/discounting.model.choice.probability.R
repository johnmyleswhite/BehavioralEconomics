discounting.model.choice.probability <- function(x1, t1, x2, t2, choice, model, parameters)
{
  exponential.du <- function(x1, t1, x2, t2, parameters)
  {
    u1 <- discounted.value(x1, t1, 'exponential', parameters)
    u2 <- discounted.value(x2, t2, 'exponential', parameters)
    return(u2 - u1)
  }
  
  hyperbolic.du <- function(x1, t1, x2, t2, parameters)
  {
    u1 <- discounted.value(x1, t1, 'hyperbolic', parameters)
    u2 <- discounted.value(x2, t2, 'hyperbolic', parameters)
    return(u2 - u1)
  }
  
  quasihyperbolic.du <- function(x1, t1, x2, t2, parameters)
  {
    u1 <- discounted.value(x1, t1, 'quasihyperbolic', parameters)
    u2 <- discounted.value(x2, t2, 'quasihyperbolic', parameters)
    return(u2 - u1)
  }
  
  generalized.hyperbolic.du <- function(x1, t1, x2, t2, parameters)
  {
    u1 <- discounted.value(x1, t1, 'generalized-hyperbolic', parameters)
    u2 <- discounted.value(x2, t2, 'generalized-hyperbolic', parameters)
    return(u2 - u1)
  }

  generalized.hyperbolic.du <- function(x1, t1, x2, t2, parameters)
  {
    u1 <- discounted.value(x1, t1, 'generalized-hyperbolic', parameters)
    u2 <- discounted.value(x2, t2, 'generalized-hyperbolic', parameters)
    return(u2 - u1)
  }

  kable.glimcher.du <- function(x1, t1, x2, t2, parameters)
  {
    u1 <- discounted.value(x1, 0, 'hyperbolic', parameters)
    u2 <- discounted.value(x2, t2 - t1, 'hyperbolic', parameters)
    return(u2 - u1)
  }

  concave.waiting.du <- function(x1, t1, x2, t2, parameters)
  {
    u1 <- discounted.value(x1, t1, 'exponential', parameters)
    u2 <- discounted.value(x2, t2, 'exponential', parameters) - parameters[['alpha']] * (t2 - t1) ^ parameters[['eta']]
    return(u2 - u1)
  }

  benhabib.du <- function(x1, t1, x2, t2, parameters)
  {
    u1 <- x1
    u2 <- x2 - parameters[['alpha']]
    return(u2 - u1)
  }

  # Check parameters as in previous package.
  
  models <- list('exponential' = exponential.du,
                 'hyperbolic' = hyperbolic.du,
                 'quasihyperbolic' = quasihyperbolic.du,
                 'generalized-hyperbolic' = generalized.hyperbolic.du,
                 'kable-glimcher' = kable.glimcher.du,
                 'concave-waiting' = concave.waiting.du,
                 'benhabib' = benhabib.du)
  
  utility <- function(x, parameters)
  {
    return(x ^ parameters[['gamma']])
  }
  
  if (is.null(parameters[['gamma']]))
  {
    parameters[['gamma']] <- 1
  }
  
  x1 <- utility(x1, parameters)
  x2 <- utility(x2, parameters)
  
  du <- do.call(models[[model]], list(x1 = x1, t1 = t1, x2 = x2, t2 = t2, parameters = parameters))
  
  z <- parameters[['a']] * du
  
  p <- 1 / (1 + exp(-z))
  
  if (choice == 1)
  {
    return(p)
  }
  else
  {
    return(1 - p)
  }
}
