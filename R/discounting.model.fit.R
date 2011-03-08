discounting.model.fit <- function(choices,
                                  model = 'exponential',
                                  method = 'bayes',
                                  control = list(n.adapt = 1000,
                                                 n.iter = 1000,
                                                 raw.samples = FALSE,
                                                 median = FALSE,
                                                 mode = FALSE,
                                                 hierarchical = FALSE))
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

    mcmc.samples <- coda.samples(jags,
                                 variable.names = variable.names,
                                 n.iter = control[['n.iter']])

    inferred.parameters <- list()

    for (parameter in variable.names)
    {
      if (control[['median']])
      {
        inferred.parameters[[parameter]] <- median(as.array(mcmc.samples)[,parameter,])
      }
      else
      {
        inferred.parameters[[parameter]] <- mean(as.array(mcmc.samples)[,parameter,])
      }
    }

    if (control[['raw.samples']])
    {
      inferred.parameters[['samples']] <- mcmc.samples
    }

    return(inferred.parameters)
  }

  bayes.hierarchical.fit <- function(choices, bugs.file, variable.names, control)
  {
    library('rjags')

    jags <- jags.model(bugs.file,
                       data = list('X1' = choices$X1,
                                   'T1' = choices$T1,
                                   'X2' = choices$X2,
                                   'T2' = choices$T2,
                                   'C' = choices$C,
                                   'Subject' = choices$Subject,
                                   'N' = nrow(choices),
                                   'M' = max(choices$Subject)),
                       n.adapt = control[['n.adapt']],
                       n.chains = 4)

    mcmc.samples <- coda.samples(jags,
                                 variable.names = variable.names,
                                 n.iter = control[['n.iter']])

    inferred.parameters <- list()

    # Need to edit this.
    for (parameter in dimnames(as.array(mcmc.samples))$var)
    {
      if (control[['median']])
      {
        inferred.parameters[[parameter]] <- median(as.array(mcmc.samples)[,parameter,])
      }
      else
      {
        inferred.parameters[[parameter]] <- mean(as.array(mcmc.samples)[,parameter,])
      }
    }

    if (control[['raw.samples']])
    {
      inferred.parameters[['samples']] <- mcmc.samples
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
  
  bayes.hierarchical.fit.benhabib.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'hierarchical_logit',
                           'benhabib.bug')
    return(bayes.hierarchical.fit(choices, bugs.file, c('a', 'gamma', 'alpha'), control))
  }
  
  bayes.hierarchical.fit.concave.waiting.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'hierarchical_logit',
                           'concave_waiting.bug')
    return(bayes.hierarchical.fit(choices, bugs.file, c('a', 'gamma', 'delta', 'alpha', 'eta'), control))
  }
  
  bayes.hierarchical.fit.exponential.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'hierarchical_logit',
                           'exponential.bug')
    return(bayes.hierarchical.fit(choices, bugs.file, c('a', 'gamma', 'delta'), control))
  }
  
  bayes.hierarchical.fit.generalized.hyperbolic.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'hierarchical_logit',
                           'generalized_hyperbolic.bug')
    return(bayes.hierarchical.fit(choices, bugs.file, c('a', 'gamma', 'alpha', 'beta'), control))
  }
  
  bayes.hierarchical.fit.hyperbolic.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'hierarchical_logit',
                           'hyperbolic.bug')
    return(bayes.hierarchical.fit(choices, bugs.file, c('a', 'gamma', 'k'), control))
  }

  bayes.hierarchical.fit.kable.glimcher.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'hierarchical_logit',
                           'kable_glimcher.bug')
    return(bayes.hierarchical.fit(choices, bugs.file, c('a', 'gamma', 'k'), control))
  }

  bayes.hierarchical.fit.quasihyperbolic.model <- function(choices, control)
  {
    bugs.file <- file.path(system.file('jags',
                                       package = 'BehavioralEconomics'),
                           'hierarchical_logit',
                           'quasihyperbolic.bug')
    return(bayes.hierarchical.fit(choices, bugs.file, c('a', 'gamma', 'beta', 'delta'), control))
  }
  
  if (is.null(control[['n.iter']]))
  {
    control[['n.iter']] <- 1000
  }
  
  if (is.null(control[['n.adapt']]))
  {
    control[['n.adapt']] <- 1000
  }

  if (is.null(control[['median']]))
  {
    control[['median']] <- FALSE
  }

  if (is.null(control[['mode']]))
  {
    control[['mode']] <- FALSE
  }
  
  if (is.null(control[['hierarchical']]))
  {
    control[['hierarchical']] <- FALSE
  }
  
  if (control[['hierarchical']])
  {
    models <- list('benhabib' = bayes.hierarchical.fit.benhabib.model,
                   'concave-waiting' = bayes.hierarchical.fit.concave.waiting.model,
                   'exponential' = bayes.hierarchical.fit.exponential.model,
                   'generalized-hyperbolic' = bayes.hierarchical.fit.generalized.hyperbolic.model,
                   'hyperbolic' = bayes.hierarchical.fit.hyperbolic.model,
                   'kable-glimcher' = bayes.hierarchical.fit.kable.glimcher.model,
                   'quasihyperbolic' = bayes.hierarchical.fit.quasihyperbolic.model)
  }
  else
  {
    models <- list('benhabib' = bayes.fit.benhabib.model,
                   'concave-waiting' = bayes.fit.concave.waiting.model,
                   'exponential' = bayes.fit.exponential.model,
                   'generalized-hyperbolic' = bayes.fit.generalized.hyperbolic.model,
                   'hyperbolic' = bayes.fit.hyperbolic.model,
                   'kable-glimcher' = bayes.fit.kable.glimcher.model,
                   'quasihyperbolic' = bayes.fit.quasihyperbolic.model)
  }

  return(do.call(models[[model]], list(choices = choices, control = control)))
}
