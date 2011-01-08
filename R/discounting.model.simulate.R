discounting.model.simulate <- function(choices, model, parameters)
{
  for (i in 1:nrow(choices))
  {
    x1 <- choices[i, 'X1']
    t1 <- choices[i, 'T1']
    x2 <- choices[i, 'X2']
    t2 <- choices[i, 'T2']
    p <- discounting.model.choice.probability(x1, t1, x2, t2, 1, model, parameters)
    choices[i, 'C'] <- rbinom(1, 1, p)
  }
  
  return(choices)
}
