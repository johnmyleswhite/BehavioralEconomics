\name{fit.generalized.hyperbolic.discounting.model}
\alias{fit.generalized.hyperbolic.discounting.model}
\title{
Fit the generalized hyperbolic discounting model.
}
\description{
Fit the generalized hyperbolic discounting model.
}
\usage{
fit.generalized.hyperbolic.discounting.model(choices, method = "bayes", n.adapt = 1000, n.iter = 1000)
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
	\item{alpha}{alpha}
	\item{gamma}{gamma}
}
\examples{
choices <- data.frame(X1 = 1, X2 = 2, T1 = 0, T2 = 1, C = 1)

fit.generalized.hyperbolic.discounting.model(choices, method = "bayes", n.adapt = 1000, n.iter = 1000)
}
