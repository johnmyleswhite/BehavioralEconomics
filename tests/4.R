library('testthat')
library('BehavioralEconomics')

# Simulate choices; test that inference works properly.

# Exponential model.
n <- 100
x1.range <- 10:20
t1.range <- 0:5
x2.range <- 10:30
t2.range <- 0:10

choices <- discounting.model.generate.choice.set(n, x1.range, t1.range, x2.range, t2.range)

parameters <- list('a' = 10, 'gamma' = 1, 'delta' = 0.66)

choices <- discounting.model.simulate(choices, 'exponential', parameters)

fitted.parameters <- discounting.model.fit(choices, 'exponential')
plot(discounting.model.fit(choices,
                           'exponential',
                           control = list(n.iter = 1000, n.adapt = 1000, raw.samples = TRUE))$samples)
