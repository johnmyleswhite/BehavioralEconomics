fit.benhabib.discounting.model <-
function(choices,
                                           method = 'bayes',
                                           n.adapt = 1000,
                                           n.iter = 1000)
{
  bugs.file <- file.path(system.file('jags',
                                     package = 'BehavioralEconomics'),
                         'benhabib.bug')

  jags <- jags.model(bugs.file,
                     data = list('X1' = choices$X1,
                                 'X2' = choices$X2,
                                 'C' = choices$C,
                                 'N' = nrow(choices)),
                     n.adapt = n.adapt)

  mcmc.samples <- jags.samples(jags,
                               variable.names = c('a', 'c'),
                               n.iter = n.iter)

  inferred.parameters <- list()

  for (parameter in names(mcmc.samples))
  {
    inferred.parameters[[parameter]] <- apply(mcmc.samples[[parameter]], 1, mean)
  }

  return(inferred.parameters)
}
