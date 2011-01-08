\name{discounting.model.generate.choice.set}
\alias{discounting.model.generate.choice.set}
\title{
Generate a random choice set for simulations.
}
\description{
Generate a random choice set for simulations.
}
\usage{
discounting.model.generate.choice.set(n, x1.range, t1.range, x2.range, t2.range)
}
\arguments{
  \item{n}{
	How many choice sets should be created?
}
  \item{x1.range}{
	What is the range of the earlier option's monetary value?
}
  \item{t1.range}{
	What is the range of the earlier option's time of receipt?
}
  \item{x2.range}{
	What is the range of the later option's monetary value?
}
  \item{t2.range}{
	What is the range of the later option's time of receipt?
}
}
\value{
	A data frame containing choice sets and NA choices.
}
\examples{
choices <- discounting.model.generate.choice.set(10, 10:20, 0:5, 10:30, 0:10)
}
