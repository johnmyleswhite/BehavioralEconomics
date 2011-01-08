library('testthat')
library('BehavioralEconomics')

# Check that functions exist and do something.

choices <- data.frame(X1 = 1, X2 = 2, T1 = 0, T2 = 1, C = 1)

discounting.model.fit(choices, 'exponential')

discounting.model.fit(choices, 'hyperbolic')

discounting.model.fit(choices, 'quasihyperbolic')

discounting.model.fit(choices, 'generalized-hyperbolic')

discounting.model.fit(choices, 'kable-glimcher')

discounting.model.fit(choices, 'benhabib')

discounting.model.fit(choices, 'concave-waiting')
