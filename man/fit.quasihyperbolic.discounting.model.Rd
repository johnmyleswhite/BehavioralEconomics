\name{fit.quasihyperbolic.discounting.model}
\alias{fit.quasihyperbolic.discounting.model}
\title{
Fit the quasi-hyperbolic discounting model.
}
\description{
Fit the quasi-hyperbolic discounting model.
}
\usage{
fit.quasihyperbolic.discounting.model(choices, method = "bayes", n.adapt = 1000, n.iter = 1000)
}
\arguments{
  \item{choices}{
	Standard choices data frame.
}
  \item{method}{
	bayes
}
  \item{n.adapt}{
	1000
}
  \item{n.iter}{
	1000
}
}
\value{
	\item{a}{Sensitivity of logistic curve to utility, a}
	\item{beta}{Present bias term, beta}
	\item{delta}{Exponential discount factor, delta}
}
\examples{
choices <- data.frame(X1 = 1, X2 = 2, T1 = 0, T2 = 1, C = 1)

fit.quasihyperbolic.discounting.model(choices, method = "bayes", n.adapt = 1000, n.iter = 1000)
}
