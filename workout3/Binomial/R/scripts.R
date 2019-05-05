

#check_prob determines if 'prob' input valid
check_prob <- function(prob) {
  if (prob < 0 | prob > 1) {
    stop("p must be between 0 and 1")
  }
  else {
    TRUE
  }
}

#check_trials determines if 'trials' input valid
check_trials <- function(trials) {
  if (trials %% 1 != 0) {
    stop("trials must be an integer")
  }
  else if (trials < 0) {
    stop("trials must be non-negative")
  }
  else {
    TRUE
  }
}

#check_success determines if 'success' input valid
check_success <- function(trials, success) {
  if (success %% 1 != 0) {
    stop("success must be an integer")
  }
  else if (success < 0) {
    stop("success must be non-negative")
  }
  else if (success > trials) {
    stop("successes must be less than trials")
  }
  else {
    TRUE
  }
}

#Auxillary functions:  calculate mean, variance, mode, skewness
#and kurtosis of BRV.  Do not constrain inputs/outputs.

aux_mean <- function(trials, prob) {
  return(trials * prob)
}

aux_variance <- function(trials, prob) {
  return(trials * prob * (1 - prob))
}

aux_mode <- function(trials, prob) {
  m <- trials * prob + prob
  if (m %% 1 != 0) {
    return(m %/% 1)
  }
  else {
    return(c(m - 1, m))
  }
}

aux_skewness <- function(trials, prob) {
  return((1 - 2 * prob) / ((trials * prob * (1 - prob)) ^ 0.5))
}

aux_kurtosis <- function(trials, prob) {
  return((1 - 6 * prob * (1 - prob)) / (trials * prob * (1 - prob)))
}


#' Choose
#'
#' @param n - number of trials
#' @param k - number of successes of n trials
#'
#' @return The number of combinations in which k successes can occur in n trials.
#' @export
#'
#' @examples
#'
#' #default
#' bin <- bin_choose()
#'
#' #simple case
#' simple <- bin_choose(n = 5, k = 2)
#'
#' #With vector
#' vect <- bin_choose(5, 1:3)

bin_choose <- function(n, k) {
  check_success(trials = n, success = k)
  return(factorial(n) / (factorial(k) * factorial(n - k)))
}


#' Binomial Probability
#'
#' @param success - number of successes
#' @param trials - number of trials
#' @param prob - probability of success
#'
#' @return The probability of attaining 'success'
#'  successes in 'trial' trials.
#' @export
#'
#' @examples
#' #Simple  case
#' simple <- bin_probability(success = 2, trials = 5, prob = 0.5)
#'
#' #With vector as input
#' vect <- bin_probability(success = 0:2, trials = 5, prob = 0.5)

bin_probability <- function(success, trials, prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  check_success(trials = trials, success = success)
  return(bin_choose(n = trials, k = success) * (prob ^ success) * ((1 -
                                                                      prob) ^ (trials - success)))
}


#' Binomial distribution
#'
#' @param trials - number of trials
#' @param prob - probability of success
#'
#' @return A data frame describing the discrete probability for each possible binomial outcome
#' from 'trials' trials
#' @export
#'
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
#'
bin_distribution <- function(trials, prob) {
  len <- trials + 1
  bindi <- data.frame(array(0, c(len, 2)), row.names = c(1:len))
  names(bindi) <- c("Number of Successes", "Probability")
  bindi[1] <- c(0:trials)
  class(bindi) <- append("bindis", class(bindi))
  bindi[2] <- bin_probability(bindi[1], trials, prob)
  bindi
}

#' @export
plot.bindis <- function(x) {
  barplot(
    height = x$Probability,
    names.arg = x$`Number of Successes`,
    xlab = "Number of Successes",
    ylab = "Probability",
    main = "Binomial Distribution"
  )
}

#' Cumulative distribution
#'
#' @param trials - number of trials
#' @param prob - probability of success
#'
#' @return A data frame describing the discrete and probabilities for each possible binomial outcome
#' from 'trials' trials
#' @export
#'
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
#'
bin_cumulative <- function(trials, prob) {
  len <- trials + 1
  bindi <- data.frame(array(0, c(len, 3)), row.names = c(1:len))
  names(bindi) <-
    c("Number of Successes", "Probability", "Cumulative")
  bindi[1] <- c(0:trials)
  class(bindi) <- append("bincum", class(bindi))
  bindi[2] <- bin_probability(bindi[1], trials, prob)
  bindi[3] <- apply(bindi[2], 2, cumsum)
  bindi
}

#' @export
plot.bincum <- function(x) {
  plot(
    x = x$`Number of Successes`,
    y = x$Cumulative,
    type = "p",
    xlab = "Number of Successes",
    ylab = "Probability",
    main = "Cumulative Binomial Distribution"
  )
  lines(x = x$`Number of Successes`, y = x$Cumulative)
}


#' Binomial variable
#'
#' @param trials - number of trials
#' @param prob - probability of success
#'
#' @return A simple description of the binomial random variable.
#' @export
#'
#' @examples
#' bin_variable(trials = 10, p = 0.3)
#'
bin_variable <- function(trials, prob) {
  check_trials(trials = trials)
  check_prob(prob = prob)
  binvar <- list("Number of trials:" = trials,
                 "Probability of success:" = prob)
  class(binvar) <- "binvar"
  binvar
}

#' @export
print.binvar <- function(x) {
  cat(
    paste(
      "Binomial Variable",
      "\n",
      "\n",
      "Parameters",
      "\n",
      "Number of trials:",
      x["Number of trials:"],
      "\n",
      "Probability of success:",
      x["Probability of success:"]
    )
  )
}

#' @export
summary.binvar <- function(x) {
  summary <- list(
    "trials" = x[["Number of trials:"]],
    "success" = x[["Probability of success:"]],
    "mean" = aux_mean(x[["Number of trials:"]],
                      x[["Probability of success:"]]),
    "variance" = aux_variance(x[["Number of trials:"]],
                              x[["Probability of success:"]]),
    "mode" = aux_mode(x[["Number of trials:"]],
                      x[["Probability of success:"]]),
    "skewness" = aux_skewness(x[["Number of trials:"]],
                              x[["Probability of success:"]]),
    "kurtosis" = aux_kurtosis(x[["Number of trials:"]],
                              x[["Probability of success:"]])
  )
  class(summary) <- "summary.binvar"
  print(summary)
}

#' @export
print.summary.binvar <- function(x) {
  cat(
    paste(
      "Summary Binomial",
      "\n",
      "\n",
      "Parameters",
      "\n",
      "Number of trials:",
      x["trials"],
      "\n",
      "Probability of success:",
      x["success"],
      "\n",
      "\n",
      "Measures",
      "\n",
      "mean:",
      x["mean"],
      "\n",
      "variance:",
      x["variance"],
      "\n",
      "mode:",
      x["mode"],
      "\n",
      "skewness:",
      x["skewness"],
      "\n",
      "kurtosis:",
      x["kurtosis"]
    )
  )
}

#' Binomial mean
#'
#' @param trials
#' @param prob
#'
#' @return The mean of the distribution
#' @export
#'
#' @examples
#' mean <- bin_mean(5, 0.5)
#' means <- bin_mean(1:5, 0.5)
#'
bin_mean <- function(trials, prob) {
  check_prob(prob)
  check_trials(trials)
  aux_mean(trials, prob)
}

#' Binomial variance
#'
#' @param trials
#' @param prob
#'
#' @return The variance of the distribution
#' @export
#'
#' @examples
#' var <- bin_variance(5, 0.5)
#' vars <- bin_mean(1:5, 0.5)
#'
bin_variance <- function(trials, prob) {
  check_prob(prob)
  check_trials(trials)
  aux_variance(trials, prob)
}

#' Binomial mode
#'
#' @param trials
#' @param prob
#'
#' @return The modes of the distribution
#' @export
#'
#' @examples
#' mode <- bin_mode(5, 0.5)
#' modes <- bin_mode(1:5, 0.5)
#'

bin_mode <- function(trials, prob) {
  check_prob(prob)
  check_trials(trials)
  aux_mode(trials, prob)
}

#' Binomial skewness
#'
#' @param trials
#' @param prob
#'
#' @return The skewness of the distribution
#' @export
#'
#' @examples
#' skew <- bin_skewness(5, 0.5)
#' skews <- bin_skewness(1:5, 0.5)
#'

bin_skewness <- function(trials, prob) {
  check_prob(prob)
  check_trials(trials)
  aux_skewness(trials, prob)
}

#' Binomial kurtosis
#'
#' @param trials
#' @param prob
#'
#' @return The kurtosis of the distribution
#' @export
#'
#' @examples
#' kurt <- bin_kurtosis(5, 0.5)
#' kurts <- bin_kurtosis(1:5, 0.5)
#'

bin_kurtosis <- function(trials, prob) {
  check_prob(prob)
  check_trials(trials)
  aux_kurtosis(trials, prob)
}


