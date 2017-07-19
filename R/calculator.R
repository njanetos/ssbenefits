#' Computes social security benefits as a function of an individual's history
#' of earnings, retirement age, year in which they retired, and disability /
#' household status.
#'
#' @param ss_policy A list containing parameters for social security policy. See
#' 'Details'.
#' @param earnings_history A data frame, with two columns: \code{wage} and
#' \code{date}, recording the income paid in that year of an individual.
#' @param retirement_age The age at which an individual retires. Must be larger
#' than 62.
#' @param retirement_year The year in which an individual retires.
#' @param current_years A list of years in which to compute social security
#' benefits.
#' @param disability_status True if individual is on social security.
#' @return Vector of monthly social security benefit earnings, per year in
#' \code{current_year}.
#' @details The policy parameters consist of a wage index, so-called 'bend
#' points', retirement bonuses for retiring late, as well as cost-of-living
#' increases. The current values for these policies may be found on the social
#' security website, https://www.ssa.gov/oact/cola/Benefits.html. The
#' `ss_policy` list must contain the following items:
#' \itemize{
#'   \item{\code{wage_index}: }{Wage indices used to inflate past earnings.
#'   (See here: https://www.ssa.gov/oact/cola/AWIgrowth.html.)}
#'   \item{\code{bend_points}: }{So-called 'bend points'.
#'   (See here: https://www.ssa.gov/oact/cola/bendpoints.html.)}
#'   \item{\code{pia_percentages}: }{Discounts applied to benefits depending on age of
#' retirement.}
#'   \item{\code{cola_increases}: }{Annual cost of living increases applied to benefits.}
#' }
#' For formatting these inputs, see the data \code{ss_policy_defaults}, which
#' returns default values in the correct format, using current social security
#' administration policy as a guide. Load it with \code{data(ss_policy_defaults)}.
ss_benefits = function(ss_policy,
                       earnings_history,
                       retirement_age,
                       retirement_year,
                       current_years,
                       disability_status = NULL) {

  wage_index = ss_policy$wage_index
  bend_points = ss_policy$bend_points
  pia_percentage = ss_policy$pia_percentage
  cola_increases = ss_policy$cola_increases

  # check inputs
  if (retirement_age < 62) {
    stop('Retirement age must be larger than 62.')
  }

  # fill out earnings to have all dates
  earnings_history = merge(expand.grid(date = seq(1951, 2017)), earnings_history, all = TRUE)
  earnings_history$wage[is.na(earnings_history$wage)] = 0

  # find year of eligibility
  # year of eligibility the year turned 62
  # source: https://www.ssa.gov/oact/cola/awifactors.html
  eligibility_year = retirement_year - (retirement_age - 62)

  # cap earnings
  earnings_history_capped = earnings_history
  earnings_history_capped$wage = pmin(earnings_history$wage, maximum_taxable_income$cap)

  # index earnings
  # earnings indexed to two years before eligibility year
  # source: https://www.ssa.gov/oact/cola/awifactors.html
  index_year = eligibility_year - 2
  index_wage = wage_index[date == index_year]$index
  normalized_wage_index = wage_index[date %in% earnings_history_capped$date]
  normalized_wage_index$index = index_wage / normalized_wage_index$index
  normalized_earnings_history = earnings_history_capped
  normalized_earnings_history$wage = normalized_earnings_history$wage * normalized_wage_index$index

  # compute aime
  # pick top 35 years of earnings
  # find average monthly amount, rounded down to the nearest dollar
  # source: https://www.ssa.gov/oact/cola/Benefits.html
  top_35 = tail(sort(normalized_earnings_history$wage), 35)
  if (length(top_35) < 35) { # if there are fewer than 35 years of earnings, count as zero
    top_35 = c(top_35, rep(0, 35 - length(top_35)))
  }
  aime = floor(mean(top_35) / 12)

  # compute pia
  # three marginal rates, 90%, 32%, 15%
  # marginal rate changes determined by 'bend points'
  # bend points are dependent on the year of eligibility
  # source: https://www.ssa.gov/oact/cola/bendpoints.html
  eligibility_bend_points = bend_points[year == eligibility_year]
  pia = 0
  pia = pia + 0.9 * (min(eligibility_bend_points$first, aime))
  if (eligibility_bend_points$first < aime) {
    pia = pia + 0.32 * (min(eligibility_bend_points$second, aime) - eligibility_bend_points$first)
  }
  if (eligibility_bend_points$second < aime) {
    pia = pia + 0.15 * (aime - eligibility_bend_points$second)
  }

  # apply cola for each of the current years
  insurance_benefits = cbind(lapply(current_years, FUN = function(current_year) {
    pia_cola_return = pia
    for (year in seq(eligibility_year, current_year)) {
      pia_cola_return = pia_cola_return * (1 + cola_increases[date == year]$value / 100)
    }
    return(round(pia_percentage[date == eligibility_year][[as.character(retirement_age)]] * pia_cola_return, 2))
  }))

  # TODO: more sophisticated spousal and child insurance
  return(list(personal_insurance = insurance_benefits,
              spousal_insurance = 0.5 * insurance_benefits,
              child_insurance = 0.5 * insurance_benefits))
}
