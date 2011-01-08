\name{discounting.model.simulate}
\alias{discounting.model.simulate}
\title{
Simulate choices given a choice set and a parameterized choice model.
}
\description{
Simulate choices given a choice set and a parameterized choice model.
}
\usage{
discounting.model.simulate(choices, model, parameters)
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
	A new data.frame with random choices.
}
\examples{
parameters <- list('a' = 1, 'delta' = 0.9)

choices <- discounting.model.simulate(discounting.model.generate.choice.set(10, 10:20, 0:5, 10:30, 0:10),
														'exponential',
														parameters)
}
