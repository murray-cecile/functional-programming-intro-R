#==========================================================#
# INTRO TO FUNCTIONS 
#   Cecile Murray
#   2021-10-27
#==========================================================#

# setup
library(here)
library(tidyverse)

#==========================================================#
# DEFINE A FUNCTION
#==========================================================#

# define a function
square <- function(a) {
  return(a ^ 2)
}

# one way to repeatedly call a function
square(1)
square(2)
square(3)
square(4)


assertthat::assert_that(square(4) == 16)

# define list of numbers to square
numbers_to_square <- seq(0, 5)

#========== LOOP


# use a loop to call the function on each number in the list and print result

save <- NULL

for(x in numbers_to_square) {
  save <- square(x)
}

save


# loops are less abstract, but may not be the best tool:
# - lots of bookkeeping about what you're looping over
# - sometimes slower 

#==========  USING APPLY() FUNCTIONS IN BASE R

# this produces a numeric vector 
sapply(numbers_to_square, function(x) { square(x) })

# this produces a numeric vector (more concise syntax)
sapply(numbers_to_square, square)

# this produces a list 
lapply(numbers_to_square, square)

#========== USING PURRR::MAP()

# this will produce a numeric vector just like sapply
purrr::map_dbl(numbers_to_square, function(x) { square(x) })

# this is the more concise, conventional way to write that
purrr::map_dbl(numbers_to_square, ~ square(.))


#==========================================================#
# USING MAP() WITH DATA FRAMES
#==========================================================#


# read in the data
pov19 <- read_csv("data/poverty_2019.csv")
head(pov19)

#========== SIMPLE EXAMPLE: COLUMN TYPE

# procedural way #1: have to know variable names in advance
class(pov19$GEOID)
class(pov19$NAME)
class(pov19$pov_univ)
# etc

# procedural way #2: hard to read! easy to make copy-paste errors
class(pov19[[colnames(pov19)[[1]]]])
class(pov19[[colnames(pov19)[[2]]]])
class(pov19[[colnames(pov19)[[3]]]])


# we could loop like this: better, still clunky
for(col in colnames(pov19)) {
  print(class(pov19[[col]]))
}

# we could use lapply, but it's also hard to read this code
lapply(
  seq(ncol(pov19)),
  function(x) {class(pov19[[colnames(pov19)[[x]]]])}  
)

# a dataframe is a list of columns, so with purrr::map we can do this:
pov19 %>% 
  map_chr(function(x) { class(x) } )

# more concise syntax, same thing
pov19 %>% 
  map_chr(~ class(.))

#==========================================================#
# MORE COMPLEX EXAMPLE WITH MAP()
#==========================================================#

# more complex operation:
# - read in data
# - reshape to long
# - compute poverty rate
# - reshape back to wide
# and we want to do this for 2016, 2017, 2018, and 2019

create_rate_df <- function(year) {

    # read in the data
    read_csv(
      str_c("data/poverty_", year, ".csv")
    ) %>% 
    # transform it to long
    pivot_longer(
      cols = contains("poor"),
      names_to = "pop_type",
      values_to = "poor_ct"
    ) %>% 
    mutate(
      year = year, # create new variable to store year
      pct_poor = (poor_ct / pov_univ) * 100 # compute pct in poverty
    ) %>% 
    # transform back to wide
    select(-poor_ct) %>% 
    pivot_wider(
      names_from = pop_type,
      values_from = pct_poor
    ) 
  
}

create_rate_df(2019)

# let's do this for 4 years of data!

output <- map_dfr(
  seq(2016, 2019, 1),
  ~ create_rate_df(.)
)

head(output)
distinct(output, year)
