discounting.model.generate.choice.set <- function(n, x1.range, t1.range, x2.range, t2.range)
{
  choices <- data.frame(X1 = rep(NA, n),
                        T1 = rep(NA, n),
                        X2 = rep(NA, n),
                        T2 = rep(NA, n),
                        C = rep(NA, n))
  
  for (i in 1:nrow(choices))
  {
    x1 <- sample(x1.range, 1)
    t1 <- sample(t1.range, 1)
    x2 <- sample(x2.range[x2.range >= x1], 1)
    t2 <- sample(t2.range[t2.range >= t1], 1)
    choices[i, 'X1'] <- x1
    choices[i, 'T1'] <- t1
    choices[i, 'X2'] <- x2
    choices[i, 'T2'] <- t2
  }
  
  return(choices)
}
