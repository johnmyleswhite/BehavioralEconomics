library('testthat')
library('BehavioralEconomics')

n <- 10
x1.range <- 10:20
t1.range <- 0:5
x2.range <- 10:30
t2.range <- 0:10

choices <- discounting.model.generate.choice.set(n, x1.range, t1.range, x2.range, t2.range)

# Simulate choices test.
parameters <- list('a' = 10000, 'gamma' = 1, 'alpha' = 1, 'beta' = 1)

choices <- discounting.model.simulate(choices, 'generalized-hyperbolic', parameters)

fitted.parameters <- discounting.model.fit(choices,
                                           'generalized-hyperbolic',
                                           control = list(n.adapt = 1000,
                                                          n.iter = 1000,
                                                          raw.samples = TRUE))

plot(fitted.parameters$samples)
