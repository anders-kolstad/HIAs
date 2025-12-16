# I now want to take the variables and normalise them before I can then combine
# them despite them being on different scales.
# I will first normalise by converting into % (not for 7GR-GI).
# Remember the ordinal categories represents frequency ranges
# The data is strongly right skewed, so simply taking the center value of each
# bin will not work

# I will use the lower bound for each bin instead.
# The exception in when the variable is 1, because then the lower bound
# is 0, same as when the variable is 0.
# For these I will set manually a slightly higher value.

convertToPercent <- function(x) {
 x %>%
  mutate(value = case_when(
    # selecting the variables that follow the same 4 step scale
    variable %in% c("7TK", "7SE", "7FA") ~
      case_match(
        value,
        0 ~ 0,
        1 ~ mean(c(0, 1 / 16)) * 100,
        2 ~ 1 / 16 * 100,
        3 ~ 50
      ), # note that it is not possible to get a value of 1
    # selecting the eight step variables
    variable %in% c("PRTK", "PRSL") ~
      case_match(
        value,
        0 ~ 0,
        1 ~ 1.5,
        2 ~ 3,
        3 ~ 6.25,
        4 ~ 12.5,
        5 ~ 25,
        6 ~ 50,
        7 ~ 75
      ),
    .default = value
  ))

}