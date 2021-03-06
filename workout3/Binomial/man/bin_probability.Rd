% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scripts.R
\name{bin_probability}
\alias{bin_probability}
\title{Binomial Probability}
\usage{
bin_probability(success, trials, prob)
}
\arguments{
\item{success}{- number of successes}

\item{trials}{- number of trials}

\item{prob}{- probability of success}
}
\value{
The probability of attaining 'success'
 successes in 'trial' trials.
}
\description{
Binomial Probability
}
\examples{
#Simple  case
simple <- bin_probability(success = 2, trials = 5, prob = 0.5)

#With vector as input
vect <- bin_probability(success = 0:2, trials = 5, prob = 0.5)
}
