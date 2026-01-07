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

normalise <- function(x) {
  # Adding scaled indicator values to the dataset
  # Same code as above, but with plot=F.

  upper <- 0
  lower <- 100
  threshold <- 10

  # For 7GR-GI I use this
  upper2 <- 1
  lower2 <- 5
  threshold2 <- 2.5 # = observable effect. Value 3 indicates a shift to a new type (grunntype)


  x$i_ADSV <- eaTools::ea_normalise(
    data = x,
    vector = "ADSV",
    upper_reference_level = lower,
    lower_reference_level = upper,
    break_point = threshold,
    reverse = T
  )

  x$i_alien <- eaTools::ea_normalise(
    data = x,
    vector = "7FA",
    upper_reference_level = lower,
    lower_reference_level = upper,
    break_point = threshold,
    reverse = T
  )

  x$i_ditch <- eaTools::ea_normalise(
    data = x,
    vector = "7GR-GI",
    upper_reference_level = lower2,
    lower_reference_level = upper2,
    break_point = threshold2,
    reverse = T
  )

  return(x)
}

ensure_multipolygons <- function(x) {
  # Cast polygons to multipolygons (keeps multipolygons as multipolygons)
  sf::st_cast(x, "MULTIPOLYGON", warn = FALSE)
}

getMunicipalities <- function(x) {
  x |>
    filter(kommunenummer %in% c(
      "3207", # Nordre Follo
      "3451", # Nord-Aurdal
      "3446" # Gran
    )) |>
    mutate(Municipality = case_when(
      kommunenummer == "3207" ~ "Nordre Follo",
      kommunenummer == "3451" ~ "Nord-Aurdal",
      kommunenummer == "3446" ~ "Gran"
  ))

}

prepPolygons <- function(x,y) {
  out <- x |> 
    st_intersection(y)
}

test_prepPolygons <- function(x) {
  x |>
    as_tibble() |>
    count(municipality,
      sort = TRUE,
      name = "Number of polygons")
}

get_coverage <- function(x, y){
  x |> 
    st_intersection(y)
}

get_world_map <- function(x) {
  ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(x) |>
  filter(admin %in% c("Norway", "Sweden")) |>
  st_make_valid()
}

posMap <- function(x,y,z,z2) {
  inc <- 200000
  myB <- z2
  myB[1:2] <- myB[1:2]-inc 
  myB[3:4] <- myB[3:4]+inc 

  out <- 
    tm_shape(x,
             bbox = myB) +
      tm_polygons() +
    tm_shape(y) +
      tm_polygons(col = "green") +
    tm_shape(z) +
    tm_text(
      text = "Municipality",
      just= "left",
      size = .8,
      xmod = 1,
      ymod = 0
    ) +
    tm_grid(projection = 4326) +
    tm_layout(
      bg.color = "skyblue",
      outer.margins = c(0.01, .02, .02, .02))+
    tm_compass()+
    tm_scale_bar()

 tmap_save(tm = out,
        here::here("images/positionMap.jpg"))

}

wgt_mean <- function (x, weights, sigma.x = NULL, mu = NULL, mu.prior = NULL, n.mu = 50, 
          ...) 
{
  if (n.mu < 3) 
    stop("Number of prior values of theta must be greater than 2")
  if (is.null(mu)) {
    mu = seq(min(x) - sigma.x, max(x) + sigma.x, length = n.mu)
    mu.prior = rep(1/n.mu, n.mu)
  }
  mx = weighted.mean(x, weights)
  quiet = Bolstad.control(...)$quiet
  if (is.null(sigma.x)) {
    sigma.x = sd(x - mx)
    if (!quiet) {
      cat(paste("Standard deviation of the residuals :", 
                signif(sigma.x, 4), "\n", sep = ""))
    }
  }
  else {
    if (sigma.x > 0) {
      if (!quiet) {
        cat(paste("Known standard deviation :", signif(sigma.x, 
                                                       4), "\n", sep = ""))
      }
    }
    else {
      stop("The standard deviation must be greater than zero")
    }
  }
  if (any(mu.prior < 0) | any(mu.prior > 1)) 
    stop("Prior probabilities must be between 0 and 1 inclusive")
  if (round(sum(mu.prior), 7) != 1) {
    warning("The prior probabilities did not sum to 1, therefore the prior has been normalized")
    mu.prior = mu.prior/sum(mu.prior)
  }
  n.mu = length(mu)
  nx = length(x)
  snx = sigma.x^2/nx
  likelihood = exp(-0.5 * (mx - mu)^2/snx)
  posterior = likelihood * mu.prior/sum(likelihood * mu.prior)
  if (Bolstad.control(...)$plot) {
    plot(mu, posterior, ylim = c(0, 1.1 * max(posterior, 
                                              mu.prior)), pch = 20, col = "blue", xlab = expression(mu), 
         ylab = expression(Probabilty(mu)))
    points(mu, mu.prior, pch = 20, col = "red")
    legend("topleft", bty = "n", fill = c("blue", "red"), 
           legend = c("Posterior", "Prior"), cex = 0.7)
  }
  mx = sum(mu * posterior)
  vx = sum((mu - mx)^2 * posterior)
  results = list(name = "mu", param.x = mu, prior = mu.prior, 
                 likelihood = likelihood, posterior = posterior, weighted_mean = mx, 
                 var = vx, cdf = function(x, ...) cumDistFun(x, mu, posterior), 
                 quantileFun = function(probs, ...) qFun(probs, mu, posterior))
  class(results) = "Bolstad"
  invisible(results)
}



# ---- Second_part ----

distance_between_centroids_km <- function(centroids) {
  centroids |>
    sf::st_distance() |>
    max() |>
    units::set_units("km") |>
    units::drop_units() |>
    round()
}

convert_to_vect <- function(x) {
  x  |> sf::as_Spatial()  |> terra::vect() 
}

crop_and_mask <- function(edm,vect){
   edm |> 
     terra::crop(vect) |> 
     terra::mask(vect) |>
     stars::st_as_stars()
}

## Read manually cached mire datasets (stars) and return both stars + terra rasters
#read_mire_manual_cache <- function() {
#  nf_path <- here::here("manuscript", "manual_cache", "mire_stars_nf.RDS")
#  gr_path <- here::here("manuscript", "manual_cache", "mire_stars_gr.RDS")
#  na_path <- here::here("manuscript", "manual_cache", "mire_stars_na.RDS")
#
#  mire_stars_nf <- readRDS(nf_path)
#  mire_stars_gr <- readRDS(gr_path)
#  mire_stars_na <- readRDS(na_path)
#
#  list(
#    stars = list(nf = mire_stars_nf, gr = mire_stars_gr, na = mire_stars_na),
#    terra = list(
#      nf = terra::rast(mire_stars_nf),
#      gr = terra::rast(mire_stars_gr),
#      na = terra::rast(mire_stars_na)
#    ),
#    files = c(nf_path, gr_path, na_path)
#  )
#}

coverage_area_km2 <- function(coverage3) {
  coverage3 |>
    dplyr::group_by(Municipality) |>
    dplyr::summarise(SHAPE = sf::st_union(SHAPE), .groups = "drop") |>
    dplyr::mutate(
      dk_area_km = SHAPE |> sf::st_area(),
      dk_area_km = round(units::drop_units(dk_area_km * 1e-6))
    )
}

mire_area_stats <- function(mire_terra_by_muni) {
  # mire_terra_by_muni: list(nf=SpatRaster, gr=SpatRaster, na=SpatRaster)
  bind_one <- function(x, name) {
    x |>
      terra::global(c("mean", "sum"), na.rm = TRUE) |>
      tibble::add_column(Municipality = name, .before = 1) |>
      dplyr::mutate(
        mirePercent = round(mean * 100, 1),
        mire_km2 = sum / 1e4
      )
  }
  dplyr::bind_rows(
    bind_one(mire_terra_by_muni$nf, "Nordre Follo"),
    bind_one(mire_terra_by_muni$gr, "Gran"),
    bind_one(mire_terra_by_muni$na, "Nord-Aurdal")
  )
}

mire_in_survey_km2 <- function(mire_terra_by_muni, dk2) {
  mask_one <- function(x, muni_name) {
    x |>
      terra::mask(dk2 |> dplyr::filter(Municipality == muni_name)) |>
      terra::global("sum", na.rm = TRUE) |>
      dplyr::mutate(mireInSurvey_km2 = sum / 1e4) |>
      tibble::add_column(Municipality = muni_name, .before = 1)
  }
  dplyr::bind_rows(
    mask_one(mire_terra_by_muni$nf, "Nordre Follo"),
    mask_one(mire_terra_by_muni$gr, "Gran"),
    mask_one(mire_terra_by_muni$na, "Nord-Aurdal")
  )
}

downsize <- function(x, crs, outline){
  x <- x|>
    st_warp(
    cellsize = c(1000, 1000),
    crs = crs,
    use_gdal = TRUE,
    method = "average"
  ) |>
  setNames("infrastructureIndex") |>
  st_transform(crs) |>
  mutate(infrastructureIndex = case_when(
    infrastructureIndex < 1 ~ 0,
    infrastructureIndex < 6 ~ 1,
    infrastructureIndex < 12 ~ 2,
    infrastructureIndex >= 12 ~ 3
  )) |>
  # taking away point in the sea
  st_crop(outline)

x <- eaTools::ea_homogeneous_area(x,
  groups = infrastructureIndex
)
  
return(x)
  
}


#infra_vectorized_cached <- function(path_temp) {
#  readRDS(paste0(path_temp, "infrastructureIndex_discrete_vectorized.rds"))
#}

infra_area_add <- function(infra_vec) {
  infra_vec |>
    dplyr::mutate(
      area = geometry |> sf::st_area(),
      area_km = units::set_units(area, "km^2")
    )
}

infra_area_plot <- function(infra_vec) {
  infra_vec |>
    tibble::as_tibble() |>
    dplyr::group_by(infrastructureIndex) |>
    dplyr::summarise(area_km = units::drop_units(sum(area_km)), .groups = "drop") |>
    ggplot2::ggplot(ggplot2::aes(x = infrastructureIndex, y = area_km)) +
    ggplot2::geom_col()
}

# Prepare 100m infra raster for muni3, discretize and vectorize, then intersect muni3 and compute area stats
infra_muni3_vectorize <- function(infra_big, muni3, outline, myCRS) {
  muni3x <- muni3 |> sf::st_transform(sf::st_crs(infra_big))
  infraMuni3 <- infra_big[muni3x] |>
    sf::st_transform(myCRS) |>
    setNames("infrastructureIndex") |>
    dplyr::mutate(infrastructureIndex = dplyr::case_when(
      infrastructureIndex < 1 ~ 0,
      infrastructureIndex < 6 ~ 1,
      infrastructureIndex < 12 ~ 2,
      infrastructureIndex >= 12 ~ 3
    ))
  rm(muni3x)
  infraMuni3 <- eaTools::ea_homogeneous_area(infraMuni3, groups = infrastructureIndex)
  infraMuni3 |>
    dplyr::mutate(area = geometry |> sf::st_area()) |>
    sf::st_intersection(muni3)
}

infra_muni3_tbl <- function(infraMuni3) {
  infraMuni3 |>
    as.data.frame() |>
    dplyr::mutate(area_HIA_km2 = units::drop_units(area) * 1e-6) |>
    dplyr::group_by(Municipality, infrastructureIndex) |>
    dplyr::summarise(total_area_HIAs_km2 = round(sum(area_HIA_km2)), .groups = "drop")
}

infra_muni3_summary <- function(infraMuni3_tbl) {
  infraMuni3_tbl |>
    dplyr::group_by(Municipality) |>
    dplyr::summarise(
      meanHIA = round(stats::weighted.mean(infrastructureIndex, total_area_HIAs_km2), 2),
      .groups = "drop"
    )
}

infra_dist_plot <- function(infraMuni3_tbl) {

  # this is the HIA area in the municipality overall
# and not the distribution of HIAs for wetlands.
# There is for example very little mires in HIA 3 in Nordre Follo.
  
  
  infraMuni3_tbl |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = infrastructureIndex,
        y = total_area_HIAs_km2,
        fill = factor(infrastructureIndex),
        colour = factor(infrastructureIndex)
      ),
      stat = "identity",
      linewidth = 1.2
    ) +
    cowplot::theme_minimal_hgrid() +
    ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(4, "YlOrBr")) +
    ggplot2::scale_color_manual(values = RColorBrewer::brewer.pal(5, "YlOrBr")[-1]) +
    ggplot2::theme(
      axis.title.x = element_textbox_simple(
        width = NULL,
        padding = margin(4, 4, 4, 4),
        margin = margin(4, 0, 0, 0),
        linetype = 1,
        r = grid::unit(8, "pt"),
        fill = "azure1"
      ),
      axis.title.y = element_textbox_simple(
        width = NULL,
        padding = margin(4, 4, 4, 4),
        margin = margin(4, 0, 0, 0),
        linetype = 1,
        orientation = "left-rotated",
        r = grid::unit(8, "pt"),
        fill = "azure1"
      ),
      strip.background = element_blank(),
      strip.text = element_textbox(
        size = 12,
        color = "white", fill = "#5D729D", box.color = "#4A618C",
        halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
        padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
      )
    ) +
    ggplot2::labs(x = "Homogeneous Impact Areas", y = "Area (km^2)") +
    ggplot2::guides(fill = "none", colour = "none") +
    ggplot2::facet_grid(cols = ggplot2::vars(Municipality))
}

corr_check <- function(naturetypes_norm, infra_vec) {
  sf::st_intersection(naturetypes_norm, infra_vec)
}

validation_plot <- function(corrCheck) {
  corrCheck |>
    tidyr::pivot_longer(
      cols = c(i_ADSV, i_alien, i_ditch),
      values_to = "indicatorValue",
      names_to = "indicator",
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(
      condition = dplyr::case_when(
        indicatorValue < 0.6 ~ "<0.6",
        indicatorValue < 0.8 ~ "0.6 to 0.8",
        indicatorValue < 0.91 ~ "0.8 to 0.9",
        TRUE ~ "0.9 to 1"
      ),
      condition = forcats::fct_reorder(condition, indicatorValue),
      indicator = dplyr::case_when(
        indicator == "i_ADSV" ~ "ADSV",
        indicator == "i_alien" ~ "Alien species",
        indicator == "i_ditch" ~ "Trenching",
        TRUE ~ indicator
      )
    ) |>
    tibble::as_tibble() |>
    dplyr::group_by(indicator, infrastructureIndex, condition) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last") |>
    dplyr::group_by(indicator, infrastructureIndex) |>
    dplyr::mutate(
      lab = round(n / sum(n) * 100),
      lab = dplyr::case_when(lab < 5 ~ NA_character_, TRUE ~ paste0(lab, "%"))
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = infrastructureIndex, y = n, fill = condition)) +
    ggplot2::geom_bar(position = "fill", stat = "identity") +
    ggplot2::geom_text(
      ggplot2::aes(label = lab),
      position = ggplot2::position_fill(vjust = 0.5),
      color = "black",
      vjust = 0.5,
      size = 4
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = -10)),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      strip.text = element_textbox(
      size = 12,
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3))
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend("Indicator values")) +
    ggplot2::xlab("Homogeneous Impact Areas") +
    scale_fill_manual(values = c("#E85437","#FBAF00", "#B5DF73", "#009000")) +
    ggplot2::facet_grid(~indicator)
}

infra_maps <- function(muni3, infraMuni3, terrestrial, ocean, dk2, nature3) {
  infraMuniMap <- tmap::tm_shape(muni3) +
    tmap::tm_borders() +
    tmap::tm_shape(infraMuni3) +
    tmap::tm_polygons(col = "infrastructureIndex", style = "cat", title = "Homogeneous Impact Areas") +
    tmap::tm_layout(legend.outside = TRUE, panel.label.height = 0) +
    tmap::tm_shape(muni3) +
    tmap::tm_borders(lwd = 3, col = "black") +
    tmap::tm_facets(by = "Municipality")

  muniPlot <- tmap::tm_shape(muni3) +
    tmap::tm_borders(lwd = 3, col = "black") +
    tmap::tm_facets(by = "Municipality", ncol = 3) +
    tmap::tm_shape(terrestrial) +
    tmap::tm_fill(col = "lightgreen", alpha = 1) +
    tmap::tm_shape(ocean) +
    tmap::tm_polygons(col = "skyblue", border.col = "black") +
    tmap::tm_shape(dk2) +
    tmap::tm_polygons(col = "grey", alpha = 0.8) +
    tmap::tm_shape(nature3) +
    tmap::tm_polygons(col = "red", border.col = "red", lwd = 3) +
    tmap::tm_add_legend(
      type = "fill",
      labels = c("Land", "Water", "Survey coverage area", "Mapped mires"),
      col = c("lightgreen", "skyblue", "grey", "red")
    ) +
    tmap::tm_scale_bar(position = c("left", "bottom"), text.size = 1) +
    tmap::tm_layout(legend.outside = TRUE)

  list(
    muniPlot = muniPlot,
    infraMuniMap = infraMuniMap,
    methodsMap = tmap::tmap_arrange(muniPlot, infraMuniMap, heights = c(0.6, 0.4), ncol = 1, outer.margins = NULL)
  )
}

map_overview_na <- function(muni3, terrestrial, dk2, nature3) {
  tmap::tm_shape(muni3 |> dplyr::filter(Municipality == "Nord-Aurdal")) +
    tmap::tm_borders(lwd = 3, col = "black") +
    tmap::tm_shape(terrestrial) +
    tmap::tm_fill(col = "lightgreen", alpha = 1) +
    tmap::tm_shape(dk2) +
    tmap::tm_polygons(col = "grey", alpha = 0.8) +
    tmap::tm_shape(nature3) +
    tmap::tm_polygons(col = "red", border.col = "red", lwd = 3) +
    tmap::tm_add_legend(type = "fill", labels = c("Land", "Survey coverage", "Condition data"),
                        col = c("lightgreen", "grey", "red")) +
    tmap::tm_scale_bar(position = c("left", "bottom"), text.size = 1) +
    tmap::tm_layout(legend.position = c("left", "top"))
}

infra_map_na2 <- function(muni3, infraMuni3) {
  tmap::tm_shape(muni3 |> dplyr::filter(Municipality == "Nord-Aurdal")) +
    tmap::tm_borders() +
    tmap::tm_shape(infraMuni3) +
    tmap::tm_polygons(col = "infrastructureIndex", style = "cat", title = "Homogeneous\nImpact\nAreas") +
    tmap::tm_layout(legend.show = TRUE, legend.position = c("left", "top"), legend.text.size = 1.2) +
    tmap::tm_shape(muni3) +
    tmap::tm_borders(lwd = 3, col = "black")
  
}

muni_table <- function(muni3, terrestrial, dk2, nature3, mireArea, mire_in_dk, infraMuni3_summary) {
  nature3_tbl <- nature3 |>
    dplyr::group_by(Municipality) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    tibble::as_tibble()

  muni3 |>
    dplyr::mutate(area_km = round(units::drop_units(sf::st_area(SHAPE) * 1e-6))) |>
    tibble::as_tibble() |>
    dplyr::left_join(terrestrial |> tibble::as_tibble() |> dplyr::select(kommunenummer, t_area_km), by = "kommunenummer") |>
    dplyr::left_join(dk2 |> dplyr::select(Municipality, dk_area_km), by = "Municipality") |>
    dplyr::mutate(dk_percent = round((dk_area_km / t_area_km) * 100)) |>
    dplyr::left_join(nature3_tbl |> dplyr::select(Municipality, n), by = "Municipality") |>
    dplyr::left_join(mireArea |> dplyr::select(Municipality, mirePercent, mire_km2), by = "Municipality") |>
    dplyr::left_join(mire_in_dk |> dplyr::select(Municipality, mireInSurvey_km2), by = "Municipality") |>
    dplyr::mutate(mireInSurvery_percent = round(mireInSurvey_km2 / mire_km2 * 100, 1)) |>
    dplyr::left_join(infraMuni3_summary, by = "Municipality") |>
    dplyr::mutate(mire_km2 = round(mire_km2, 1), meanHIA = round(meanHIA, 1))
}

# --- Empirical Bayes / PD simulation block ---

fit_zoib <- function(x) {
  prop_1 <- mean(x == 1)
  prop_0 <- mean(x == 0)
  x_trunc <- x[x > 0 & x < 1]
  alpha <- MASS::fitdistr(x_trunc, dbeta, start = list(shape1 = 1, shape2 = 1))$estimate
  list(alpha = alpha, prop_1 = prop_1, prop_0 = prop_0)
}

update_distribution <- function(domain_data, national_fit, prior_weight = 10) {
  x <- as.numeric(domain_data)
  n <- length(x)
  prop_1_domain <- mean(x == 1)
  prop_0_domain <- mean(x == 0)

  updated_prop_1 <- (n * prop_1_domain + prior_weight * national_fit$prop_1) / (n + prior_weight)
  updated_prop_0 <- (n * prop_0_domain + prior_weight * national_fit$prop_0) / (n + prior_weight)

  x_trunc <- x[x > 0 & x < 1]

  updated_alpha <- national_fit$alpha
  if (length(x_trunc) > 1) {
    updated_alpha <- tryCatch({
      alpha_domain <- MASS::fitdistr(x_trunc, dbeta, start = list(shape1 = 1, shape2 = 1))$estimate
      (alpha_domain * length(x_trunc) + national_fit$alpha * prior_weight) / (length(x_trunc) + prior_weight)
    }, error = function(e) national_fit$alpha)
  }

  list(alpha = updated_alpha, prop_1 = updated_prop_1, prop_0 = updated_prop_0)
}

simulate_domain <- function(dist, n_sim = 1000) {
  is_1 <- stats::rbinom(n_sim, 1, dist$prop_1)
  is_0 <- stats::rbinom(n_sim, 1, dist$prop_0) * (1 - is_1)
  cont_part <- (1 - is_1 - is_0) * stats::rbeta(n_sim, dist$alpha[1], dist$alpha[2])
  is_1 * 1 + is_0 * 0 + cont_part
}

make_national_data <- function(n = 1000, seed = 123) {
  set.seed(seed)
  tibble::tibble(
    uniform = stats::runif(n, 0, 1),
    normal_centered = stats::rnorm(n, 0.5, 0.2),
    normal_high = stats::rnorm(n, 1, 0.5),
    beta_2_2 = stats::rbeta(n, 2, 2),
    beta_05_3 = stats::rbeta(n, 0.5, 3),
    beta_3_05 = stats::rbeta(n, 3, 0.5)
  ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::case_when(. >= 1 ~ 1, . <= 0 ~ 0, TRUE ~ .))) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "distribution", values_to = "population_sample") |>
    tidyr::nest(population_sample = population_sample)
}

make_domain_data <- function(seed = 123) {
  a <- 2; b <- 5; c <- 10; d <- 30
  set.seed(seed)
  tibble::tibble(
    sample_ID = c(
      "domain_1a","domain_1b","domain_1c","domain_1d",
      "domain_2a","domain_2b","domain_2c","domain_2d",
      "domain_3a","domain_3b","domain_3c","domain_3d",
      "domain_4b","domain_4c","domain_4d",
      "domain_5b","domain_5c","domain_5d",
      "domain_6a","domain_6b","domain_6c","domain_6d"
    ),
    values = list(
      tibble::tibble(sample = rep(1, a)),
      tibble::tibble(sample = rep(1, b)),
      tibble::tibble(sample = rep(1, c)),
      tibble::tibble(sample = rep(1, d)),

      tibble::tibble(sample = seq.int(0.7, 1, length.out = a)),
      tibble::tibble(sample = seq.int(0.7, 1, length.out = b)),
      tibble::tibble(sample = seq.int(0.7, 1, length.out = c)),
      tibble::tibble(sample = seq.int(0.7, 1, length.out = d)),

      tibble::tibble(sample = seq.int(0.1, 0.4, length.out = a)),
      tibble::tibble(sample = seq.int(0.1, 0.4, length.out = b)),
      tibble::tibble(sample = seq.int(0.1, 0.4, length.out = c)),
      tibble::tibble(sample = seq.int(0.1, 0.4, length.out = d)),

      tibble::tibble(sample = sort(pmin(1, pmax(0, stats::rnorm(b, 1, 0.5))))),
      tibble::tibble(sample = sort(pmin(1, pmax(0, stats::rnorm(c, 1, 0.5))))),
      tibble::tibble(sample = sort(pmin(1, pmax(0, stats::rnorm(d, 1, 0.5))))),

      tibble::tibble(sample = sort(pmin(1, pmax(0, stats::rnorm(b, 1, 0.1))))),
      tibble::tibble(sample = sort(pmin(1, pmax(0, stats::rnorm(c, 1, 0.1))))),
      tibble::tibble(sample = sort(pmin(1, pmax(0, stats::rnorm(d, 1, 0.1))))),

      tibble::tibble(sample = rep(0.9, a)),
      tibble::tibble(sample = rep(0.9, b)),
      tibble::tibble(sample = rep(0.9, c)),
      tibble::tibble(sample = rep(0.9, d))
    )
  )
}

add_national_fits <- function(national_data) {
  national_data |>
    dplyr::rowwise() |>
    dplyr::mutate(fit = list(fit_zoib(unlist(population_sample))))
}

combine_domain_national <- function(domain_data, national_data, prior_weight = 10, n_sim = 1000) {
  domain_data |>
    dplyr::cross_join(national_data) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      updated_fit = list(update_distribution(domain_data = unlist(values), national_fit = fit, prior_weight = prior_weight)),
      PDs = list(simulate_domain(updated_fit, n_sim = n_sim)),
      mean = mean(unlist(values)),
      firstQuartile = stats::quantile(unlist(PDs), 0.25),
      thirdQuartile = stats::quantile(unlist(PDs), 0.75),
      lowCI = stats::quantile(unlist(PDs), 0.025),
      highCI = stats::quantile(unlist(PDs), 0.975)
    ) |>
    dplyr::mutate(
      n = stringr::str_sub(sample_ID, -1, -1),
      n = dplyr::case_when(n == "a" ~ 2, n == "b" ~ 5, n == "c" ~ 10, n == "d" ~ 30, TRUE ~ NA_real_),
      scenario = stringr::str_sub(sample_ID, -2, -2),
      scenario = dplyr::case_when(
        scenario == "1" ~ "Only 1's",
        scenario == "2" ~ "High values",
        scenario == "3" ~ "Low values",
        scenario == "4" ~ "Trunc.nor. high SD",
        scenario == "5" ~ "Trunc.nor. low SD",
        scenario == "6" ~ "Single value",
        TRUE ~ scenario
      )
    )
}

plot_population_dist <- function(national_data) {
  national_data |>
    tidyr::unnest(population_sample) |>
    ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = population_sample), binwidth = 0.1) +
    ggplot2::facet_wrap(ggplot2::vars(distribution), scales = "free")
}

plot_domain_dist <- function(domain_data) {
  domain_data |>
    tidyr::unnest(values) |>
    ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = sample), binwidth = 0.1) +
    ggplot2::xlim(-0.2, 1.2) +
    ggplot2::facet_wrap(ggplot2::vars(sample_ID), scales = "free_y")
}

# --- National shapes from corrCheck (used later) ---
fit_df_from_corrcheck <- function(corrCheck) {
  corrCheck |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(cols = dplyr::starts_with("i_"), names_to = "indicator", values_to = "indicatorValue") |>
    tidyr::drop_na(indicatorValue) |>
    dplyr::group_by(indicator, infrastructureIndex) |>
    dplyr::summarise(fit = list(fit_zoib(indicatorValue)), .groups = "drop")
}

mean_per_hia_tbl <- function(nature3, infraMuni3, fit_df, prior_weight = 20, n_sim = 1000) {
  nature3 |>
    sf::st_intersection(infraMuni3) |>
    dplyr::select(Municipality, i_ADSV, i_alien, i_ditch, infrastructureIndex) |>
    dplyr::mutate(area = units::drop_units(sf::st_area(SHAPE))) |>
    tidyr::pivot_longer(cols = c(i_ADSV, i_alien, i_ditch), names_to = "indicator", values_to = "indicatorValue") |>
    dplyr::filter(!is.na(indicatorValue)) |>
    tibble::as_tibble() |>
    tidyr::nest(.by = c(Municipality, infrastructureIndex, indicator)) |>
    dplyr::left_join(fit_df, by = c("indicator", "infrastructureIndex")) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      updated_fit = list(update_distribution(
        domain_data = unlist(data$indicatorValue),
        national_fit = fit,
        prior_weight = prior_weight
      )),
      mean = stats::weighted.mean(x = unlist(data$indicatorValue), w = unlist(data$area)),
      n = nrow(data),
      PDs = list(simulate_domain(updated_fit, n_sim = n_sim)),
      low = stats::quantile(unlist(PDs), 0.025),
      high = stats::quantile(unlist(PDs), 0.975)
    )
}

forest_plot_from_stats <- function(stats_tbl) {
  dotCOLS <- c("grey90","grey70", "grey50", "grey40")
  barCOLS <- c("#fff7bd", "#fecf66", "#f88b22", "#cc4c02")

  stats_tbl |>
    dplyr::mutate(indicator = dplyr::case_when(
      indicator == "i_ADSV" ~ "ADSV",
      indicator == "i_alien" ~ "Alien species",
      indicator == "i_ditch" ~ "Trenching",
      TRUE ~ indicator
    )) |>
    ggplot2::ggplot(ggplot2::aes(x = infrastructureIndex, y = mean, ymin = low, ymax = high)) +
    ggplot2::geom_linerange(ggplot2::aes(colour = factor(infrastructureIndex)), linewidth = 10) +
    ggplot2::geom_point(ggplot2::aes(fill = factor(infrastructureIndex)), size = 3, shape = 21, colour = "white", stroke = 0.5) +
    ggplot2::geom_text(ggplot2::aes(y = low, label = n), nudge_y = -0.1, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = rev(dotCOLS)) +
    ggplot2::scale_color_manual(values = barCOLS) +
    ggplot2::scale_x_discrete(name = "") +
    ggplot2::scale_y_continuous(name = "Indicator values", limits = c(-0.1, 1)) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::labs(fill = "HIA", col = "HIA") +
    ggplot2::facet_grid(Municipality ~ indicator)
}

forest_plot_example <- function(stats_tbl) {
  dotCOLS <- c("grey90","grey70", "grey50", "grey40")
  barCOLS <- c("#fff7bd", "#fecf66", "#f88b22", "#cc4c02")

  stats_tbl |>
    dplyr::filter(indicator == "i_ditch", Municipality == "Nord-Aurdal") |>
    ggplot2::ggplot(ggplot2::aes(x = infrastructureIndex, y = mean, ymin = low, ymax = high,
                                 col = factor(infrastructureIndex), fill = factor(infrastructureIndex))) +
    ggplot2::geom_linerange(linewidth = 10) +
    ggplot2::geom_linerange(linewidth = 1, colour = "black") +
    ggplot2::geom_point(size = 3, shape = 21, colour = "white", stroke = 0.5) +
    ggplot2::scale_fill_manual(values = rev(dotCOLS)) +
    ggplot2::scale_color_manual(values = barCOLS) +
    ggplot2::scale_x_discrete(name = "") +
    ggplot2::scale_y_continuous(name = "Indicator values", limits = c(0, 1)) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    labs(fill = "HIA", col = "HIA")
}

spread_mires_to_edm <- function(mire_stars, infraMuni3, stats_tbl, municipality_name) {
  mire_stars |>
    sf::st_as_sf(merge = TRUE) |>
    dplyr::rename(presence = starts_with("mire_stars")) |>
    dplyr::filter(presence == 1) |>
    sf::st_intersection(infraMuni3 |> dplyr::select(infrastructureIndex)) |>
    dplyr::mutate(area = geometry |> sf::st_area()) |>
    dplyr::left_join(
      stats_tbl |>
        tibble::as_tibble() |>
        dplyr::filter(Municipality == municipality_name) |>
        dplyr::select(mean, PDs, indicator, n, infrastructureIndex, Municipality),
      by = "infrastructureIndex",
      relationship = "many-to-many"
    )
}

spread_na_example_map <- function(spread_na, na, myCRS) {
  from <- c(xmin = 510000, xmax = 515000, ymin = 6741000, ymax = 6746000)
  to <-   c(xmin = 495000, xmax = 520000, ymin = 6765000, ymax = 6790000)
  myCols <- c("#FBAF00", "#B5DF73", "#009000")

  spread_na |>
    dplyr::filter(indicator == "i_ditch") |>
    dplyr::mutate(Trenching = factor(round(mean, 2))) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = Trenching, color = Trenching), show.legend = FALSE) +
    ggplot2::geom_sf(data = na, alpha = 0) +
    ggplot2::scale_fill_manual(values = myCols) +
    ggplot2::scale_color_manual(values = myCols) +
    ggplot2::coord_sf(datum = sf::st_crs(myCRS),
                      xlim = c(494174.8, 537114.7),
                      ylim = c(6737092, 6789676)) +
    ggmagnify::geom_magnify(from = from, to = to, expand = 0, corners = 0.1) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
}

combine_all_eaa <- function(spread_nf, spread_gr, spread_na, stats_tbl) {
  dplyr::bind_rows(spread_nf, spread_gr, spread_na) |>
    tibble::as_tibble() |>
    dplyr::select(-geometry) |>
    tidyr::drop_na() |>
    dplyr::mutate(area = units::drop_units(area)) |>
    tidyr::unnest(PDs) |>
    dplyr::group_by(Municipality, indicator) |>
    dplyr::slice_sample(n = 10000, weight_by = area, replace = TRUE) |>
    dplyr::ungroup() |>
    dplyr::select(Municipality, indicator, PDs, mean, area_EDM = area) |>
    tidyr::nest(data = c(PDs, mean, area_EDM)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      low = stats::quantile(data$PDs, 0.025),
      high = stats::quantile(data$PDs, 0.975),
      meanWeighted = mean(data$mean),
      indicator2 = dplyr::case_when(
        indicator == "i_ADSV" ~ "ADSV",
        indicator == "i_alien" ~ "Alien species",
        indicator == "i_ditch" ~ "Trenching",
        TRUE ~ indicator
      )
    ) |>
    dplyr::left_join(
      stats_tbl |>
        tidyr::unnest(data) |>
        dplyr::group_by(Municipality, indicator) |>
        dplyr::summarise(realSamples = list(indicatorValue), realAreas = list(area), .groups = "drop"),
      by = c("Municipality", "indicator")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(n = length(realSamples))
}

eea_plot <- function(combineAll) {
  combineAll |>
    ggplot2::ggplot() +
    ggplot2::geom_density(
      data = combineAll |> tidyr::unnest(data),
      ggplot2::aes(x = PDs, y = after_stat(scaled)),
      alpha = 0.5,
      fill = "sienna1",
      bounds = c(0, 1)
    ) +
    ggplot2::geom_histogram(
      data = combineAll |> tidyr::unnest(realSamples),
      ggplot2::aes(x = realSamples, y = after_stat(ncount)),
      binwidth = 0.05,
      alpha = 0.8,
      fill = "cornflowerblue"
    ) +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::theme(legend.position = "none", axis.title.y = ggplot2::element_blank()) +
    ggplot2::geom_segment(ggplot2::aes(x = meanWeighted, xend = meanWeighted, y = -0.15, yend = -0.05),
                          linewidth = 3, alpha = 0.7, colour = "grey10") +
    ggplot2::geom_segment(ggplot2::aes(x = low, xend = high, y = -0.1, yend = -0.1),
                          linewidth = 1.2, alpha = 0.7, colour = "grey30") +
    ggplot2::xlim(-0.1, 1.1) +
    ggplot2::labs(x = "Indicator value", y = "Proportion") +
    ggplot2::geom_text(ggplot2::aes(y = 0.7, x = 0.0, label = paste("n = ", n, "\nmean = ", round(meanWeighted, 2))),
                       show.legend = FALSE, hjust = 0) +
    ggplot2::facet_grid(cols = ggplot2::vars(indicator2), rows = ggplot2::vars(Municipality), scales = "free_y")
}

eea_plot2 <- function(combineAll) {

  bin_width <- 0.1
  breaks = seq(0, 1, by = bin_width)
  
  combineAll_area <- combineAll |>
  unnest(data) |>
  mutate(
    sample_bin = cut(
      mean,
      breaks = breaks,
      include.lowest = TRUE,
      labels = head(breaks, -1) + bin_width/2
    ),
    sample_bin = as.numeric(as.character(sample_bin))
  ) |>
  select(Municipality, indicator2, sample_bin, area_EDM) |>
  group_by(Municipality, indicator2, sample_bin) |>
  summarise(area_EDM = sum(area_EDM)) |>
  ungroup() |>
  group_by(Municipality, indicator2) |>
  mutate(area_norm = area_EDM / max(area_EDM, na.rm = TRUE)) |>
  ungroup()
  
  combineAll |>
    ggplot2::ggplot() +
    ggplot2::geom_density(
      data = combineAll |> tidyr::unnest(data),
      ggplot2::aes(x = PDs, y = after_stat(scaled)),
      alpha = 0.5,
      fill = "sienna1",
      bounds = c(0, 1)
    ) +
    ggplot2::geom_col(
      data = combineAll_area,
    aes(x = sample_bin,
        y = area_norm),
    width = 0.1,
    alpha=.8,
    fill = "cornflowerblue"
    ) +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::theme(legend.position = "none", axis.title.y = ggplot2::element_blank()) +
    ggplot2::geom_segment(ggplot2::aes(x = meanWeighted, xend = meanWeighted, y = -0.15, yend = -0.05),
                          linewidth = 3, alpha = 0.7, colour = "grey10") +
    ggplot2::geom_segment(ggplot2::aes(x = low, xend = high, y = -0.1, yend = -0.1),
                          linewidth = 1.2, alpha = 0.7, colour = "grey30") +
    ggplot2::xlim(-0.1, 1.1) +
    labs(x = "Indicator value", y = "Area (relative)")+
    scale_y_continuous(breaks = c(0, 0.5, 1))+
    ggplot2::geom_text(ggplot2::aes(y = 0.7, x = 0.0, label = paste("n = ", n, "\nmean = ", round(meanWeighted, 2))),
                       show.legend = FALSE, hjust = 0) +
    ggplot2::facet_grid(cols = ggplot2::vars(indicator2), rows = ggplot2::vars(Municipality), scales = "free_y")
}

eea_table_kable <- function(combineAll) {
  combineAll |>
    dplyr::ungroup() |>
    dplyr::mutate(`Indicator value` = paste0(round(meanWeighted, 2), " [", round(low, 2), " - ", round(high, 2), "] ")) |>
    dplyr::select(Indicator = indicator2, `Indicator value`) |>
    kableExtra::kbl(table.attr = 'style = "color: black;"', align = "lr") |>
    kableExtra::kable_classic("striped", full_width = FALSE) |>
    kableExtra::row_spec(0, bold = TRUE)
}

indicator_magnify_plot <- function(nature3, na, myCRS) {
  from <- c(xmin = 510000, xmax = 510600, ymin = 6747200, ymax = 6747800)
  to <-   c(xmin = 495000, xmax = 520000, ymin = 6765000, ymax = 6790000)

  nature3 |>
    dplyr::select(i_ditch) |>
    tidyr::drop_na() |>
    dplyr::mutate(Trenching = factor(format(round(i_ditch, 2), 2))) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = na, alpha = 0) +
    ggplot2::geom_sf(ggplot2::aes(fill = Trenching, color = Trenching)) +
    ggplot2::coord_sf(datum = sf::st_crs(myCRS),
                      xlim = c(494174.8, 537114.7),
                      ylim = c(6737092, 6789676)) +
    ggmagnify::geom_magnify(from = from, to = to, expand = 0, corners = 0.1) +
    ggplot2::theme_bw()
}

# Utility: save ggplot to file target
save_plot_tiff <- function(plot, path, width = 18, height = 12, units = "cm", dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(filename = path, plot = plot, width = width, height = height, units = units, dpi = dpi)
  path
}

# Utility: save tmap to file target
save_tmap_tiff <- function(tm, path, width = 18, height = 10, units = "cm", dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmap::tmap_save(tm = tm, filename = path, width = width, height = height, units = units, dpi = dpi)
  path
}


get_path_temp <- function(server = "P", folder = "41201785_okologisk_tilstand_2022_2023/data/cache/") {
  # similar logic as get_folder_dir(), but for the cache directory
  server <- toupper(server)
  if (!server %in% c("P", "R")) stop("server must be 'P' or 'R'")
  if (.Platform$OS.type == "windows") {
    base <- switch(server, P = "P:/", R = "R:/")
  } else {
    base <- switch(server, P = "/data/P-Prosjekter2/", R = "/data/R/")
  }
  paste0(base, folder)
}


ggplot_tiff <- function(plot, path, width = 8, height = 6, dpi = 300){
  ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi)
  path
}