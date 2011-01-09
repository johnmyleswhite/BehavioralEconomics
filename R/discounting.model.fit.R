discounting.model.fit <- function(choices, model = 'exponential', method = 'bayes', control = list(n.adapt = 1000, n.iter = 1000, raw.samples = FALSE))
{
  # method: mle or bayes
  # control: n.adapt, n.iter, etc.
  
  # Screw MLE's for the moment; they keep giving weird answers
  # Plus they're initial point dependent.
  #mle.fit.exponential.model <- function()
  #{
  #  optim.format <- function (x)
  #  {
  #    parameters <- list('a' = x[1], 'delta' = x[2])
  #    return(discounting.model.log.likelihood(choices, 'exponential', parameters))
  #  }
  #  
  #  optim(c(1, 0.99),
  #        optim.format,
  #        method = 'L-BFGS-B',
  #        lower = c(0.99, 0.01),
  #        upper = c(1.01, 0.99),
  #        control = list(fnscale = -1))
  #}
  
  bayes.fit <- function(choices, bugs.file, variable.names, control)
  {
    library('rjags')

    jags <- jags.model(bugs.file,
                       data = list('X1' = choices$X1,
                                   'T1' = choices$T1,
                                   'X2' = choices$X2,
                                   'T2' = choices$T2,
                                   'C' = choices$C,
                                   'N' = nrow(choices)),
                       n.adapt = control[['n.adapt']],
                       n.chains = 4)

    mcmc.samples <- jags.samples(jags,
                                 variable.names = variable.names,
                                 n.iter = control[['n.iter']])

    inferred.parameters <- list()

    for (parameter in names(mcmc.samples))
    {
      inferred.parameters[[parameter]] <- apply(mcmc.samples[[parameter]], 1, mean)
    }

    if (control[['raw.samples']])
    {
      inferred.parameters[['coda']] <- coda.samples(jags,
                                                    variable.names = variable.names,
                                                    n.iter = control[['n.iter']])
    }

    return(inferred.parameters)
  }

  bayes.fit.benhabib.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'logit',
                           'benhabib.bug')
    return(bayes.fit(choices, bugs.file, c('a', 'gamma', 'alpha'), control))
  }
  
  bayes.fit.concave.waiting.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'logit',
                           'concave_waiting.bug')
    return(bayes.fit(choices, bugs.file, c('a', 'gamma', 'delta', 'alpha', 'eta'), control))
  }
  
  bayes.fit.exponential.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'logit',
                           'exponential.bug')
    return(bayes.fit(choices, bugs.file, c('a', 'gamma', 'delta'), control))
  }
  
  bayes.fit.generalized.hyperbolic.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'logit',
                           'generalized_hyperbolic.bug')
    return(bayes.fit(choices, bugs.file, c('a', 'gamma', 'alpha', 'beta'), control))
  }
  
  bayes.fit.hyperbolic.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'logit',
                           'hyperbolic.bug')
    return(bayes.fit(choices, bugs.file, c('a', 'gamma', 'k'), control))
  }

  bayes.fit.kable.glimcher.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'logit',
                           'kable_glimcher.bug')
    return(bayes.fit(choices, bugs.file, c('a', 'gamma', 'k'), control))
  }

  bayes.fit.quasihyperbolic.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'logit',
                           'quasihyperbolic.bug')
    return(bayes.fit(choices, bugs.file, c('a', 'gamma', 'beta', 'delta'), control))
  }
  
  models <- list('benhabib' = bayes.fit.benhabib.model,
                 'concave-waiting' = bayes.fit.concave.waiting.model,
                 'exponential' = bayes.fit.exponential.model,
                 'generalized-hyperbolic' = bayes.fit.generalized.hyperbolic.model,
                 'hyperbolic' = bayes.fit.hyperbolic.model,
                 'kable-glimcher' = bayes.fit.kable.glimcher.model,
                 'quasihyperbolic' = bayes.fit.quasihyperbolic.model)

  return(do.call(models[[model]], list(choices = choices, control = control)))
}
