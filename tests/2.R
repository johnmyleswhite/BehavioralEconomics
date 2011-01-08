library('testthat')
library('BehavioralEconomics')

# Basic discounted value tests.
expect_that(discounted.value(1, 0, 'exponential', list('delta' = 0.9)), equals(1.0))
expect_that(discounted.value(1, 1, 'exponential', list('delta' = 0.9)), equals(0.9))
expect_that(discounted.value(1, 2, 'exponential', list('delta' = 0.9)), equals(0.81))

expect_that(discounted.value(1, 0, 'hyperbolic', list('k' = 1)), equals(1))
expect_that(discounted.value(1, 1, 'hyperbolic', list('k' = 1)), equals(1 / 2))
expect_that(discounted.value(1, 2, 'hyperbolic', list('k' = 1)), equals(1 / 3))

expect_that(discounted.value(1, 0, 'quasihyperbolic', list('beta' = 0.5, 'delta' = 0.9)), equals(1))
expect_that(discounted.value(1, 1, 'quasihyperbolic', list('beta' = 0.5, 'delta' = 0.9)), equals(0.5 * 0.9))
expect_that(discounted.value(1, 2, 'quasihyperbolic', list('beta' = 0.5, 'delta' = 0.9)), equals(0.5 * 0.81))

expect_that(discounted.value(1, 0, 'generalized.hyperbolic', list('alpha' = 1, 'beta' = 1)), equals(1.0))
expect_that(discounted.value(1, 1, 'generalized.hyperbolic', list('alpha' = 1, 'beta' = 1)), equals(1 / 2))
expect_that(discounted.value(1, 2, 'generalized.hyperbolic', list('alpha' = 1, 'beta' = 1)), equals(1 / 3))

# Basic choice probability tests.
expect_that(discounting.model.choice.probability(1,
                                                 0,
                                                 2,
                                                 1,
                                                 1,
                                                 'exponential',
                                                 list('a' = 1, 'delta' = 0.9)),
            equals(1 / (1 + exp(- (1 * (2 * 0.9 - 1))))))
expect_that(discounting.model.choice.probability(1,
                                                 0,
                                                 2,
                                                 1,
                                                 1,
                                                 'exponential',
                                                 list('a' = 1, 'delta' = 0.9, 'gamma' = 1)),
            equals(1 / (1 + exp(- (1 * (2 * 0.9 - 1))))))
expect_that(discounting.model.choice.probability(1,
                                                 0,
                                                 2,
                                                 1,
                                                 1,
                                                 'exponential',
                                                 list('a' = 2, 'delta' = 0.9, 'gamma' = 1)),
            equals(1 / (1 + exp(- (2 * (2 * 0.9 - 1))))))
expect_that(discounting.model.choice.probability(1,
                                                 0,
                                                 3,
                                                 2,
                                                 1,
                                                 'exponential',
                                                 list('a' = 2, 'delta' = 0.9, 'gamma' = 1)),
            equals(1 / (1 + exp(- (2 * (3 * 0.9 ^ 2 - 1))))))
expect_that(discounting.model.choice.probability(1,
                                                 0,
                                                 3,
                                                 2,
                                                 1,
                                                 'exponential',
                                                 list('a' = 2, 'delta' = 0.9, 'gamma' = 0.9)),
            equals(1 / (1 + exp(- (2 * ((3 ^ 0.9) * 0.9 ^ 2 - (1 ^ 0.9)))))))

# Generate choice set tests.
n <- 100
x1.range <- 10:20
t1.range <- 0:5
x2.range <- 10:30
t2.range <- 0:10

choices <- discounting.model.generate.choice.set(n, x1.range, t1.range, x2.range, t2.range)

# Simulate choices test.
parameters <- list('a' = 1, 'gamma' = 1, 'delta' = 0.9)
choices <- discounting.model.simulate(choices, 'exponential', parameters)

# Predict choice set tests.
choices <- discounting.model.predict(choices, 'exponential', parameters)

# Fitting test.
fitted.parameters <- discounting.model.fit(choices, 'exponential')
