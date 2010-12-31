library('BehavioralEconomics')

choices <- data.frame(X1 = 1, X2 = 2, T1 = 0, T2 = 1, C = 1)

fit.exponential.discounting.model(choices)

fit.hyperbolic.discounting.model(choices)

fit.quasihyperbolic.discounting.model(choices)

fit.generalized.hyperbolic.discounting.model(choices)

fit.kable.glimcher.discounting.model(choices)

fit.benhabib.discounting.model(choices)
