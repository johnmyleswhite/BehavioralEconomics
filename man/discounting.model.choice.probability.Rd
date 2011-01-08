\name{discounting.model.choice.probability}
\alias{discounting.model.choice.probability}
\title{
Find the probability of selecting (x2, t2) given a parameterized model.
}
\description{
Find the probability of selecting (x2, t2) given a parameterized model.
}
\usage{
discounting.model.choice.probability(x1, t1, x2, t2, choice, model, parameters)
}
\arguments{
  \item{x1}{
	Monetary value of earlier option.
}
  \item{t1}{
	Time of receipt of the earlier option.
}
  \item{x2}{
	Monetary value of later option.
}
  \item{t2}{
	Time of receipt of the later option.
}
	\item{choice}{
	1 if X2,T2 was chosen. 0 if X1,T1 was chosen.
}
  \item{model}{
	exponential
}
  \item{parameters}{
	model parameters
}
}
\value{
	Varies. See specific model for details.
}
\examples{
discounting.model.choice.probability(1, 0, 2, 0, 1, 'exponential', list('a' = 1, 'delta' = 0.9))
}
