\name{fit.kable.glimcher.discounting.model}
\alias{fit.kable.glimcher.discounting.model}
\title{
Fit the Kable-Glimcher hyperbolic discounting model.
}
\description{
Fit the Kable-Glimcher hyperbolic discounting model.
}
\usage{
fit.kable.glimcher.discounting.model(choices, method = "bayes", n.adapt = 1000, n.iter = 1000)
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
	\item{k}{Hyperbolic discount parameter, k}
}
\examples{
choices <- data.frame(X1 = 1, X2 = 2, T1 = 0, T2 = 1, C = 1)

fit.kable.glimcher.discounting.model(choices, method = "bayes", n.adapt = 1000, n.iter = 1000)
}
