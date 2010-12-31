# Introduction
The BehavioralEconomics package is a collection of statistical methods that will be useful to researchers in behavioral economics who want to work with econometric models for discrete choice data, particularly involving intertemporal choice data.

# Current Models
At present, the package implements six models for intertemporal choice:
* Exponential discounting
* Hyperbolic discounting
* Quasi-hyperbolic discounting
* Generalized hyperbolic discounting
* Kable-Glimcher "as soon as possible" discounting
* Benhabib fixed cost discounting
These are fit using JAGS through the rjags package. Priors are used that 

# Intertemporal Choice Models
Every intertemporal choice model assumes that your data is in the form of a data frame with five columns:
* X1: The monetary value of the first option
* T1: The time when the first option would be received
* X2: The monetary value of the second option
* T2: The time when the second option would be received
* C: Binary variable indicating whether the second option (X2, T2) was chosen.

# Future Work
In the future, there will be:
* Models of Decision-Making under Uncertainty
  * EU Theory
  * Prospect Theory
* Maximum Likelihood estimation
* Alternative priors for Bayesian estimation
* Simulation tools
* Generic helper functions
