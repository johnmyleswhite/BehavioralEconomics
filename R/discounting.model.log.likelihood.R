discounting.model.log.likelihood <- function(choices, model, parameters)
{
  discounting.model.hierarchical.log.likelihood <- function(choices, model, parameters)
  {
    ll <- 0

    for (i in 1:nrow(choices))
    {
      x1 <- choices[i, 'X1']
      t1 <- choices[i, 'T1']
      x2 <- choices[i, 'X2']
      t2 <- choices[i, 'T2']
      subject <- choices[i, 'Subject']
      subject.parameters <- list()
      for (parameter in discounting.model.parameters(model))
      {
        subject.parameters[[parameter]] <- parameters[[paste(parameter, '[', subject, ']', sep = '')]]
      }
      ll <- ll + log(discounting.model.choice.probability(x1, t1, x2, t2, 1, model, subject.parameters))
    }

    return(ll)
  }
  
  ll <- 0
  
  for (i in 1:nrow(choices))
  {
    x1 <- choices[i, 'X1']
    t1 <- choices[i, 'T1']
    x2 <- choices[i, 'X2']
    t2 <- choices[i, 'T2']
    choice <- choices[i, 'C']
    ll <- ll + log(discounting.model.choice.probability(x1, t1, x2, t2, choice, model, parameters))
  }
  
  return(ll)
}
