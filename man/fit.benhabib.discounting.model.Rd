\name{fit.benhabib.discounting.model}
\alias{fit.benhabib.discounting.model}
\title{
Fit the GEB Benhabib fixed cost model.
}
\description{
Fit the GEB Benhabib fixed cost model.
}
\usage{
fit.benhabib.discounting.model(choices, method = "bayes", n.adapt = 1000, n.iter = 1000)
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
	\item{c}{Fixed cost term, c}
}
\examples{
choices <- data.frame(X1 = 1, X2 = 2, T1 = 0, T2 = 1, C = 1)

fit.benhabib.discounting.model(choices, method = "bayes", n.adapt = 1000, n.iter = 1000)
}
