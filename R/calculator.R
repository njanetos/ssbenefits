#' Computes social security benefits as a function of an individual's history
#' of earnings, retirement age, year in which they retired, and disability /
#' household status.
#'
#' @param ss_policy A list containing parameters for social security policy. See
#' 'Details'.
#' @param individual_profile A list containing the information about the
#' individual for whom social security benefits will be computed.
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
#'   \item{\code{pia_percentages}: }{Discounts applied to benefits depending on
#'   age of retirement.}
#'   \item{\code{cola_increases}: }{Annual cost of living increases applied to
#'   benefits.}
#' }
#' For formatting these inputs, see the data \code{ss_policy_defaults}, which
#' returns default values in the correct format, using current social security
#' administration policy as a guide. Load it with
#' \code{data(ss_policy_defaults)}.
ss_benefits = function(ss_policy,
                       individual_profile,
                       begin_year = 1951,
                       end_year = 2100) {

  date_range = seq(begin_year, end_year)

  # load in policy and individual data for nicer notation
  wage_index = ss_policy$wage_index
  bend_points = ss_policy$bend_points
  pia_percentage = ss_policy$pia_percentage
  cola_increases = ss_policy$cola_increases
  maximum_taxable_income = ss_policy$maximum_taxable_income
  pia_marginal_rates = ss_policy$pia_marginal_rates
  maximum_family_benefit_rates = ss_policy$maximum_family_benefit_rates
  earnings_history = individual_profile$earnings_history
  retirement_age = individual_profile$retirement_age
  retirement_year = individual_profile$retirement_year
  current_years = individual_profile$current_years

  # check inputs
  required_policies        = c("wage_index",
                               "bend_points",
                               "pia_percentage",
                               "cola_increases",
                               "maximum_taxable_income",
                               "pia_marginal_rates",
                               "maximum_family_benefit_rates")
  required_individual_data = c("earnings_history",
                               "retirement_age",
                               "retirement_year",
                               "current_years")
  missing_policies = setdiff(
    required_policies,
    intersect(
      required_policies,
      names(ss_policy)
    )
  )
  missing_individual_data = setdiff(
    required_policies,
    intersect(
      required_policies,
      names(ss_policy)
    )
  )
  if (length(missing_policies) > 0)
    stop(
      "Missing the following policy info: "
      + paste(missing_policies, sep = ", ")
    )
  if (length(missing_individual_data) > 0)
    stop(
      "Missing the following individual info: "
      + paste(missing_individual_data, sep = ", ")
    )
  if (retirement_age < 62)
    stop('Retirement age must be larger than 62.')
  if (length(maximum_family_benefits_rates) != 4)
    stop('There should be 4 marginal rates in the maximum family benefits')
  if (length(pia_marginal_rates) != 3)
    stop('There should be 3 marginal rates in the computation of PIA')

  # fill out everything for balanced dataset
  earnings_history = merge(
    expand.grid(date = date_range),
    earnings_history, all = TRUE
  )[earnings_history$date %in% date_range,]
  earnings_history$wage[is.na(earnings_history$wage)] = 0
  maximum_taxable_income = merge(
    expand.grid(date = date_range),
    maximum_taxable_income, all = TRUE
  )[maximum_taxable_income$date %in% date_range,]
  maximum_taxable_income$cap[is.na(maximum_taxable_income$cap)] = 0

  # find year of eligibility
  # year of eligibility the year turned 62
  # source: https://www.ssa.gov/oact/cola/awifactors.html
  eligibility_year = retirement_year - (retirement_age - 62)

  # cap earnings
  earnings_history_capped = earnings_history
  earnings_history_capped$wage = pmin(
    earnings_history$wage,
    maximum_taxable_income$cap
  )

  # index earnings
  # earnings indexed to two years before eligibility year
  # source: https://www.ssa.gov/oact/cola/awifactors.html
  index_year = eligibility_year - 2
  index_wage = wage_index[date == index_year]$index
  normalized_wage_index = wage_index[date %in% earnings_history_capped$date]
  normalized_wage_index$index = index_wage / normalized_wage_index$index
  normalized_earnings_history = earnings_history_capped
  normalized_earnings_history$wage = normalized_earnings_history$wage*normalized_wage_index$index

  # compute aime
  # pick top 35 years of earnings
  # find average monthly amount, rounded down to the nearest dollar
  # source: https://www.ssa.gov/oact/cola/Benefits.html
  top_35 = tail(sort(normalized_earnings_history$wage), 35)
  if (length(top_35) < 35) {
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
  pia = pia + pia_marginal_rates[1]*(min(eligibility_bend_points$first, aime))
  if (eligibility_bend_points$first < aime)
    pia = pia + pia_marginal_rates[2]*(min(eligibility_bend_points$second, aime)
                                         - eligibility_bend_points$first)

  if (eligibility_bend_points$second < aime)
    pia = pia + pia_marginal_rates[3] * (aime - eligibility_bend_points$second)


  # compute maximum family benefits
  maximum_family_benefits = 0
  maximum_family_benefits = maximum_family_benefits + maximum_family_benefit_rates[1]*min(eligibility_bend_points$third, pia)
  if (eligibility_bend_points$third < pia)
    maximum_family_benefits = maximum_family_benefits + maximum_family_benefit_rates[2] * min(eligibility_bend_points$fourth, pia)

  if (eligibility_bend_points$fourth < pia)
    maximum_family_benefits = maximum_family_benefits + maximum_family_benefit_rates[3] * min(eligibility_bend_points$fifth, pia)

  if (eligibility_bend_points$fifth < pia)
    maximum_family_benefits = maximum_family_benefits + maximum_family_benefit_rates[4] * (pia - eligibility_bend_points$fifth)

  # apply cola for each of the current years
  insurance_benefits = cbind(lapply(current_years, FUN = function(current_year) {
    pia_cola_return = pia
    for (year in seq(eligibility_year, current_year)) {
      pia_cola_return = pia_cola_return * (1 + cola_increases[date == year]$value / 100)
    }
    return(round(
      pia_percentage[date == eligibility_year][[as.character(retirement_age)]]
      * pia_cola_return,
      2
    ))
  }))

  return(list(personal_insurance = insurance_benefits,
              family_insurance = 0.5 * insurance_benefits,
              maximum_family_benefits = maximum_family_benefits))
}
