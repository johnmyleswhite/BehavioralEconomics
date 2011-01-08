\name{discounted.value}
\alias{discounted.value}
\title{
Find the discounting value of an option under a parameterized utility model.
}
\description{
Find the discounting value of an option under a parameterized utility model.
}
\usage{
discounted.value(x, t, model, parameters)
}
\arguments{
  \item{x}{
	The monetary value of the option.
}
	\item{t}{
	The time of receipt of the option.
}
  \item{model}{
	exponential
}
  \item{parameters}{
	model parameters
}
}
\value{
	The scalar, strictly non-negative utility of the option.
}
\examples{
discounted.value(1, 0, 'exponential', list('delta' = 0.9))
}
