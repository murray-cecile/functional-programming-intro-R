#==========================================================#
# PULL POVERTY DATA 
#   Cecile Murray
#   2021-10-27
#==========================================================#

# packages required
libs <- c(
          "tidyverse",
          "magrittr",
          "here",
          "tidycensus"
)

# load packages quietly
invisible(
  suppressMessages(
    lapply(libs, library, character.only=TRUE)
  )
)

# get Census API key from .Renviron
CENSUS_API_KEY <- Sys.getenv("CENSUS_API_KEY")

# get 1-year ACS poverty estimates by state
get_poverty <- function(year) {
  get_acs(
    "state",
    table = "B17001",
    year = year,
    survey = "acs1"
  ) %>% 
    mutate(
      variable = case_when(
        variable == "B17001_001" ~ "pov_univ",
        variable == "B17001_002" ~ "poor_pop",
        variable == "B17001_003" ~ "poor_male",
        variable == "B17001_017" ~ "poor_female"
      )
    ) %>% 
    filter(!is.na(variable)) %>% 
    select(-moe) %>% 
    pivot_wider(
      names_from = variable,
      values_from = estimate
    ) %>% 
    write_csv(
      str_c("data/poverty_", year, ".csv")
    )
}

# pull for four years
map_df(
  seq(2016, 2019, 1),
  ~ get_poverty(.)
)