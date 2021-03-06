% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculator.R
\name{ss_benefits}
\alias{ss_benefits}
\title{Computes social security benefits as a function of an individual's history
of earnings, retirement age, year in which they retired, and disability /
household status.}
\usage{
ss_benefits(ss_policy, individual_profile, begin_year = 1951,
  end_year = 2100)
}
\arguments{
\item{ss_policy}{A list containing parameters for social security policy. See
'Details'.}

\item{individual_profile}{A list containing the information about the
individual for whom social security benefits will be computed.}
}
\value{
Vector of monthly social security benefit earnings, per year in
\code{current_year}.
}
\description{
Computes social security benefits as a function of an individual's history
of earnings, retirement age, year in which they retired, and disability /
household status.
}
\details{
The policy parameters consist of a wage index, so-called 'bend
points', retirement bonuses for retiring late, as well as cost-of-living
increases. The current values for these policies may be found on the social
security website, https://www.ssa.gov/oact/cola/Benefits.html. The
`ss_policy` list must contain the following items:
\itemize{
  \item{\code{wage_index}: }{Wage indices used to inflate past earnings.
  (See here: https://www.ssa.gov/oact/cola/AWIgrowth.html.)}
  \item{\code{bend_points}: }{So-called 'bend points'.
  (See here: https://www.ssa.gov/oact/cola/bendpoints.html.)}
  \item{\code{pia_percentages}: }{Discounts applied to benefits depending on
  age of retirement.}
  \item{\code{cola_increases}: }{Annual cost of living increases applied to
  benefits.}
}
For formatting these inputs, see the data \code{ss_policy_defaults}, which
returns default values in the correct format, using current social security
administration policy as a guide. Load it with
\code{data(ss_policy_defaults)}.
}

