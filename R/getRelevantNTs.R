getRelevantNTs <- function(x, myVars) {
  
  # these are the mapping units that have the selected variables
  nts <- x %>%
    rowwise() %>%
    mutate(keepers = sum(c_across(
      all_of(myVars))>0, na.rm=T)) |>
    filter(
      keepers >0,
      Ecosystem == "våtmark"
      ) |>
    pull(Nature_type)
}