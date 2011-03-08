discounting.model.predict <- function(choices, model, parameters)
{
  discounting.model.hierarchical.predict <- function(choices, model, parameters)
  {
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
      p <- discounting.model.choice.probability(x1, t1, x2, t2, 1, model, subject.parameters)
      choices[i, 'C'] <- round(p)
    }

    return(choices)
  }
  
  for (i in 1:nrow(choices))
  {
    x1 <- choices[i, 'X1']
    t1 <- choices[i, 'T1']
    x2 <- choices[i, 'X2']
    t2 <- choices[i, 'T2']
    p <- discounting.model.choice.probability(x1, t1, x2, t2, 1, model, parameters)
    choices[i, 'C'] <- round(p)
  }
  
  return(choices)
}
