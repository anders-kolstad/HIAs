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