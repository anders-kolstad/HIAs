# get the path to the internal server folder where
# I store the big files.
# The path depends on the OS.
get_folder_dir <- function(server = "P", folder = "41001581_egenutvikling_anders_kolstad/data/") {
  server <- toupper(server)
  if (!server %in% c("P", "R")) {
    stop("server must be 'P' or 'R'")
  }
  if (.Platform$OS.type == "windows") {
    base <- switch(server,
                   P = "P:/",
                   R = "R:/")
  } else {
    base <- switch(server,
                   P = "/data/P-Prosjekter2/",
                   R = "/data/R/")
  } 
  base <- paste0(base, folder)
  return(base)
}

# extract the relevant naturetypes
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


# Preparing the naturetypes dataset
clean_subset <- function(x, nts, myCodes) {
  x |>
  # keep only wetlands
  filter(
    hovedøkosystem == "våtmark",
    naturtype %in% nts,
    naturtype != "Kalkrik helofyttsump"
  ) |>
  # calculate the areas (m2) of the polygons
  mutate(area = SHAPE |> st_area()) |>
  # the variable codes and values are all in the same column
  separate_rows(ninBeskrivelsesvariable, sep = ",") |>
  separate(
    col = ninBeskrivelsesvariable,
    into = c("NiN_variable_code", "NiN_variable_value"),
    sep = "_",
    remove = F
  ) |>
  mutate(NiN_variable_value = as.numeric(NiN_variable_value)) |>
  filter(NiN_variable_code %in% myCodes) |>
  select(
    id = identifikasjon_lokalId,
    municipality = kommunenummer,
    year = kartleggingsår,
    mosaic = mosaikk,
    quality = lokalitetskvalitet,
    biodiversity = naturmangfold,
    condition = tilstand,
    natureType = naturtype,
    variable = NiN_variable_code,
    value = NiN_variable_value,
    area
  ) 
}

# Plot of the most common naturetypes
f_test_most_common <- function(x) {
  x |>
  as_tibble() |>
  count(natureType, sort=T) |>
  mutate(natureType = fct_reorder(natureType, n)) |>
  ggplot(aes(x = natureType, y = n))+
  geom_col()+
  coord_flip()
}

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

# Plot to see it if worked
test_convertToPercent <- function(x) {
  x %>%
  filter(variable != "7GR-GI") |>
  ggplot() +
  theme_bw() +
  geom_histogram(aes(x = value),
    binwidth = 1,
    color = "orange",
    fill = "orange"
  ) +
  xlab("%") +
  facet_wrap(. ~ variable,
    scales = "free"
  )
}

make_naturetypes_wide <- function(x) {
  x |>
    filter(variable %in% c("7TK", "7SE", "PRTK", "PRSL")) |>
  # Column names starting with a number is problematic, so adding a prefix
  mutate(variable = paste0("var_", variable)) |>
  pivot_wider(
    names_from = "variable",
    values_from = "value",
    id_cols = "id") |>
    as_tibble()
}


# First I will combine 7TK and PRTK, and also 7SE and PRSL.
# Then I taking the sum of 7SE and 7TK (incl the PR.. variables)

make_naturetypes_comb <- function(x) {
  x |>
    mutate(
      TK = if_else(is.na(var_PRTK), var_7TK, var_PRTK),
      SE = if_else(is.na(var_PRSL), var_7SE, var_PRSL)
    ) |>
    rowwise() |>
    mutate(ADSV = sum(c(SE, TK), na.rm = TRUE))
}

f_plot_naturetypes_comb <- function(x) {
  plot_grid(
    x |>
    as_tibble() |>
    count(SE, name = "sum") |>
    ggplot(
      aes(x = factor(SE), y = sum)) +
    geom_bar(
      stat = "identity",
      fill = "grey",
      colour = "black"
    ) +
    theme_bw(base_size = 12) +
    labs(
      x = "7SE or PRSL score",
      y = "Number of localities"
    ),
  x |>
    as_tibble() |>
    count(TK, name = "sum") |>
    ggplot(
      aes(x = factor(TK), y = sum)) +
    geom_bar(
      stat = "identity",
      fill = "grey",
      colour = "black"
    ) +
    theme_bw(base_size = 12) +
    labs(
      x = "7TK or PRTK score",
      y = "Number of localities"
    )
  )
}

f_plot_naturetypes_comb2 <- function(x) {
  x |>
    as_tibble() |>
  count(ADSV, name = "sum") |>
  ggplot(
    aes(x = ADSV, y = sum)) +
  geom_bar(
    stat = "identity",
    fill = "grey",
    colour = "black"
  ) +
  theme_bw(base_size = 12) +
  labs(
    x = "Summed ADVS score",
    y = "Number of localities"
  ) +
  scale_x_continuous(
    labels = scales::label_number(accuracy = 1)
  )
}

# Now I will copy these ADVS-values into the sf object again, 
# keeping things in wide format
make_naturetypes_wide2 <- function(x, y) {
  x |>
    pivot_wider(
      names_from = "variable",
      values_from = "value"
    ) |>
    left_join(y |> select(id, ADSV), by = "id") |>
    select(!c("7TK", "7SE", "PRSL", "PRTK"))
}

head_sf <- function(y) {
  y |> as_tibble() |> head()
}

# Now I normalise the now continuous variables using reference levels
# I will use the same reference levels for all of Norway for ADSV and alien species:
# upper is x100, lower is X0 and threshold is x60.

normalise_plot <- function(x) {
  upper <- 0
  lower <- 100
  threshold <- 10

  # For 7GR-GI I use this
  upper2 <- 1
  lower2 <- 5
  threshold2 <- 2.5 # = observable effect. Value 3 indicates a shift to a new type (grunntype)

  scale1 <- eaTools::ea_normalise(
    data = x,
    vector = "ADSV",
    upper_reference_level = lower,
    lower_reference_level = upper,
    break_point = threshold,
    plot = T,
    reverse = T
  ) +
    labs(x = "ADVS (converted to %)") +
    ylim(0, 1)

  # There is no point yet making this a time series
  # I will assign all the indicator value to the same time (2018-2022)

  # same for 7FA
  scale2 <- eaTools::ea_normalise(
    data = x,
    vector = "7FA",
    upper_reference_level = lower,
    lower_reference_level = upper,
    break_point = threshold,
    plot = T,
    reverse = T
  ) +
    labs(x = "7FA (converted to %)", y = "") +
    ylim(0, 1)
  # The variables are really coarse

  scale3 <- eaTools::ea_normalise(
    data = x,
    vector = "7GR-GI",
    upper_reference_level = lower2,
    lower_reference_level = upper2,
    break_point = threshold2,
    plot = T,
    reverse = T
  ) +
    labs(x = "7GR-GI (original units)", y = "") +
    ylim(0, 1)

  ggarrange(scale1, scale2, scale3, ncol = 3)
}