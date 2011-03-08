\name{discounting.model.fit}
\alias{discounting.model.fit}
\title{
Fit a discounting model.
}
\description{
Fit a discounting model.
}
\usage{
discounting.model.fit(choices, model = 'exponential', method = 'bayes', control = list(n.adapt = 1000, n.iter = 1000, raw.samples = FALSE, median = FALSE, mode = FALSE, hierarchical = FALSE))
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
  \item{control}{
	Contains n.adapt = 1000, n.iter = 1000, raw.false = FALSE, median = FALSE, mode = FALSE, hierarchical = FALSE
}
}
\value{
	Varies. See specific model for details.
}
\examples{
choices <- data.frame(X1 = 1, X2 = 2, T1 = 0, T2 = 1, C = 1)

discounting.model.fit(choices, model = 'exponential')
}
