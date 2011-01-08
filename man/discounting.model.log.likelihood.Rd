\name{discounting.model.log.likelihood}
\alias{discounting.model.log.likelihood}
\title{
What's the log likelihood of the choice data given a parameterized model?
}
\description{
What's the log likelihood of the choice data given a parameterized model?
}
\usage{
discounting.model.log.likelihood(choices, model, parameters)
}
\arguments{
  \item{choices}{
	Standard choices data frame.
}
  \item{model}{
	exponential
}
  \item{parameters}{
	model parameters
}
}
\value{
	A negative scalar value.
}
\examples{
parameters <- list('a' = 1, 'delta' = 0.9)

choices <- discounting.model.simulate(discounting.model.generate.choice.set(10, 10:20, 0:5, 10:30, 0:10),
														'exponential',
														parameters)

discounting.model.log.likelihood(choices, model = 'exponential', parameters)
}
