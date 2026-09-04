fileter(pa_income, estimate > 60000)
filter(pa_income, estimate > 60000)
library(tidyverse)
library(tidycensus)

pa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
)

dim(pa_income)
glimpse(pa_income)
head(pa_income, 10)

# Pennsylvania has 67 counties. Does my row count match? Why or why not?

pa_income$GEOID

as.numeric("01001")
# the leading 0 disappears

filter(pa_income, estimate > 60000)

#counties where the margin of error is bigger than 3000
#counties whre the estimate is under 50000

select(pa_income, NAME, estimate, moe)

# Show only GEOID and estimate
