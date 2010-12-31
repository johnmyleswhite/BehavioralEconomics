\name{fit.discounting.model}
\alias{fit.discounting.model}
\title{
Fit a discounting model.
}
\description{
Fit a discounting model.
}
\usage{
fit.discounting.model(choices, model = "exponential", method = "bayes", n.adapt = 1000, n.iter = 1000)
}
\arguments{
  \item{choices}{
	Standard choices data frame.
}
  \item{model}{
	exponential
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
	Varies. See specific model for details.
}
\examples{
choices <- data.frame(X1 = 1, X2 = 2, T1 = 0, T2 = 1, C = 1)

fit.discounting.model(choices, model = 'exponential', method = "bayes", n.adapt = 1000, n.iter = 1000)
}
