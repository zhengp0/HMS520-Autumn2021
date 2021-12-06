library("readr")
library("dplyr")
library("data.table")


# Load data --------------------------------------------------------------------
base_url <- "https://static.usafacts.org/public/data/covid-19"
deaths_url <- paste(base_url, "covid_deaths_usafacts.csv", sep = "/")
cases_url <- paste(base_url, "covid_confirmed_usafacts.csv", sep = "/")
population_url <- paste(base_url, "covid_county_population_usafacts.csv",
                        sep = "/")

# specify which package to use
csv_loader <- fread

# load data
deaths <- csv_loader(deaths_url)
cases <- csv_loader(cases_url)
population <- csv_loader(population_url)


# Data cleaning ----------------------------------------------------------------
# Clean column names, change all column names to lower case, and change the date
# column to the format `"year-month-day"`, e.g. `"2021-01-01"` if the date is
# not already in the this format.
names(deaths) <- tolower(names(deaths))
names(cases) <- tolower(names(cases))
names(population) <- tolower(names(population))

# Collect the date column names of both `deaths` and `cases` data frames, and
# save them into the variables `death_dates` and `case_dates`.
death_dates <- names(deaths)[startsWith(names(deaths), "2021")]
case_dates <- names(cases)[startsWith(names(cases), "2021")]

# Aggregate the deaths, cases and population from each county to their state
# using sum, and re-assign them to `deaths`, `cases` and `population`.
deaths <- deaths[, lapply(.SD, sum), by = state, .SDcols = death_dates]
cases <- cases[, lapply(.SD, sum), by = state, .SDcols = case_dates]
population <- population[, list(population = sum(population)), by = state]

# Reshape the `deaths` and `cases` so that each row is from a single state and a
# single date. The date column should be named `"date"` and the count column
# should be named `"deaths"` for `deaths` and `"cases"` for `cases`.
deaths <- melt(
  deaths,
  id.vars = "state",
  measure.vars = death_dates,
  variable.name = "date",
  value.name = "deaths",
  variable.factor = FALSE
)
cases <- melt(
  cases,
  id.vars = "state",
  measure.vars = case_dates,
  variable.name = "date",
  value.name = "cases",
  variable.factor = FALSE
)

# Merge `deaths`, `cases` and `population` into one data frame called `counts`.
# `deaths` and cases` will be merged by matching `state` and date` and the
# `population` should be merged by matching `state`.
counts <- merge(deaths, cases, by = c("state", "date"), all = TRUE)
counts <- merge(counts, population, by = c("state"), all.x = TRUE)


# Data validation and processing -----------------------------------------------
# Order the data by state and date.
counts <- counts[order(state, date)]

# Compute the new deaths and cases for each state, by using the current day's
# count minus the yesterday's count. For the first day in the record we will
# keep the count as the "new" count. For example, if deaths is
# `c(200, 201, 205, 209)` the new deaths should be `c(200, 1, 4, 4)`.
# Column name for new deaths should be `"new_deaths"` and the column name for
# new cases should be `"new_cases"`.
counts[, new_deaths := c(deaths[1], diff(deaths)), by = state]
counts[, new_cases := c(cases[1], diff(cases)), by = state]

# You will notice that some of the new deaths or new cases are negative. Replace
# the negative value by 0 and then re-compute the deaths and cases as the
# cumulative sum of the new deaths and new cases.
counts[new_deaths < 0, new_deaths := 0]
counts[new_cases < 0, new_cases := 0]
counts[, deaths := cumsum(new_deaths), by = state]
counts[, cases := cumsum(new_cases), by = state]

# Compute the infection fatality rate as the ratio between the deaths and cases.
# The column name should be `ifr`.
counts[, ifr := deaths / cases]

# Compute the mortality rate as the ratio between the deaths and population.
# The column name should be `mr`.
counts[, mr := deaths / population]


# Data analysis and results ----------------------------------------------------
# List the top five states with the most deaths, cases.
# List the top five states with the highest infection fatality rate and
# mortality rate.
max_summary <- counts[, lapply(.SD, max), by = state, .SDcols = -c("date")]
top_five_states <- sapply(
  c("deaths", "cases", "ifr", "mr"),
  function(x) max_summary[order(-max_summary[[x]])]$state[1:5],
  USE.NAMES = TRUE
)
