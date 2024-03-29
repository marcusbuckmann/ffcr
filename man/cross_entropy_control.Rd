% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross-entropy.R
\name{cross_entropy_control}
\alias{cross_entropy_control}
\title{Hyperparmaeters for the cross-entropy models}
\usage{
cross_entropy_control(
  starts = 10,
  learning_rate = 0.05,
  maximum_time = 3600,
  iterations = 500,
  early_stopping = 25,
  thresholds = 100,
  split_percentiles = FALSE,
  samples = 100,
  elite_samples = 5,
  threads = parallel::detectCores() - 1
)
}
\arguments{
\item{starts}{Number of fast-and-frugal trees or tallying models generated with different seeds. The best-fitting model will be returned. The default is 10.}

\item{learning_rate}{Learning rate used in the optimization procedure. The higher the value, the quicker the model will converge. However, at higher learning rates, the model is more likely to converge to less accurate fast-and-frugal trees and tallying models. The default value is 0.1.}

\item{maximum_time}{Maximum training time (in seconds) to obtain the final model.}

\item{iterations}{The maximum number of iterations. The default is 500.}

\item{early_stopping}{Training stops early if the performance does not improve after this many iterations in a row. The default is 25.}

\item{thresholds}{The maximum number of numeric thresholds that are tested for a numeric feature. The default is 100.}

\item{split_percentiles}{When TRUE, the candidate thresholds at which features can be split are the percentiles of the features' distribution. When FALSE the candidate values are obtained by dividing a feature's values in equidistant bins. The default value is \code{FALSE}.}

\item{samples}{Number of models created in each iteration. The default is 100.}

\item{elite_samples}{The number of best-performing models that are used to update the parameter distribution. The default is 10.}

\item{threads}{Number of CPU cores used. The default is the number of available cores minus 1.}
}
\description{
\code{cross_entropy_control} Returns a list of parameters that control the cross-entropy optimization procedure. The output of this function is be passed when training a fast-and-frugal tree or tallying model using the cross-entropy method (see examples).
}
\examples{
\dontrun{
data(liver)
liver$sex <- ifelse(liver$sex == "Female", 1,0) # Recoding categorical feature because the cross-entropy method only works with numeric features.
model_tree <- fftree(data = liver, formula = diagnosis~., method = "cross-entropy", cross_entropy_parameters = cross_entropy_control(starts = 5))
model_tally <- tally(data = liver, formula = diagnosis~., method = "cross-entropy", cross_entropy_parameters = cross_entropy_control(starts = 5))
}
}
