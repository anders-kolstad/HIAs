# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
 library(geotargets)
options(warn=-1)

# Set target options:
tar_option_set(
  packages = c(
    "knitr", "sf",  "tmap", "tmaptools", "stars", "terra",
    "tidyterra", "ggtext", "cowplot",  "units",  "rnaturalearth",
    "rnaturalearthdata", "ggmagnify", "ggridges", "eaTools", "ggpubr",
    "kableExtra", "here", "MASS", "ggh4x", "tidyverse")
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source(files = here::here("R"))

# Pipeline
list(
  tar_target(server_folder, get_folder_dir()),
  tar_target(myCRS, 25832),
    # I already did some work to identify the relevant nature types
    # summary file (https://github.com/NINAnor/ecosystemCondition/blob/main/data/naturetypes/natureType_summary.rds)  
  tar_file_read(naturetypes_summary, here::here("data/natureType_summary.rds"), read = readRDS(file = !!.x)),
  tar_terra_rast(mire_terra, paste0(server_folder, "myrmodell90pros.tif") |> terra::rast()),
  tar_file_read(naturetypes, paste0(server_folder, "Naturtyper_nin_0000_norge_25833_FILEGDB.gdb"), read = sf::st_read(dsn = !!.x, layer = "naturtyper_nin_omr", quiet= T) |> st_transform(myCRS)),
  tar_file_read(coverage, paste0(server_folder, "Naturtyper_nin_0000_norge_25833_FILEGDB.gdb"), read = sf::st_read(dsn = !!.x, layer = "naturtyper_nin_dekning", quiet= T) |> st_transform(myCRS)),
  tar_file_read(outline, here::here("data/outlineOfNorway_EPSG25833.shp"), read = sf::st_read(dsn = !!.x, quiet = TRUE) |> st_transform(myCRS)),
  tar_file_read(muni, here::here("data/Basisdata_0000_Norge_25833_Kommuner_FGDB.gdb"), read = sf::read_sf(dsn = !!.x, layer = "kommune", quiet = TRUE) |> st_transform(myCRS)),
  tar_file_read(infra, paste0(server_folder, "infra_tiff.tif"), read = stars::read_stars(!!.x)),
  tar_target(myVars, c("7TK", "7SE", "PRTK", "PRSL", "7FA", "7GR-GI")),
  tar_target(nts, getRelevantNTs(naturetypes_summary, myVars)),
  tar_target(naturetypes_cs, clean_subset(naturetypes, nts = nts, myCodes = myVars)),
  tar_target(test_most_common, f_test_most_common(naturetypes_cs)),
  tar_target(naturetypes_p, convertToPercent(naturetypes_cs)),
  tar_target(test_naturetypes_p, test_convertToPercent(naturetypes_p)),
  tar_target(naturetypes_wide, make_naturetypes_wide(naturetypes_p)),
  tar_target(naturetypes_comb, make_naturetypes_comb(naturetypes_wide)),
  tar_target(plot_naturetypes_comb, f_plot_naturetypes_comb(naturetypes_comb)),
  tar_target(plot_naturetypes_comb2, f_plot_naturetypes_comb2(naturetypes_comb)),
  tar_target(naturetypes_wide2, make_naturetypes_wide2(naturetypes_p, naturetypes_comb)),
  tar_target(plot_normalised, normalise_plot(naturetypes_wide2)),
  tar_target(naturetypes_norm, normalise(naturetypes_wide2)),
  tar_target(muni_mp, ensure_multipolygons(muni)),
  tar_target(muni3, getMunicipalities(muni_mp)),
  tar_target(nf, dplyr::filter(muni3, kommunenummer == "3207")),
  tar_target(na, dplyr::filter(muni3, kommunenummer == "3451")),
  tar_target(gr, dplyr::filter(muni3, kommunenummer == "3446")),
  tar_target(nature3, prepPolygons(naturetypes_norm, muni3)),
  tar_target(nature3_test, test_prepPolygons(nature3)),
  tar_target(coverage3, get_coverage(coverage, muni3)),
  tar_target(terrestrial_poly, outline |> sf::st_intersection(muni3)),
  tar_target(ocean, muni3 |> sf::st_difference(outline)),
  tar_target(terrestrial, terrestrial_poly |> mutate(area_t = geometry |> st_area(), t_area_km = round(units::drop_units(area_t * 1e-6)))),
  tar_target(world, get_world_map(myCRS)),
  tar_target(centroids, muni3 |> sf::st_centroid()),
  tar_target(myBbox, sf::st_bbox(centroids)),
  tar_target(positionMap, posMap(world, muni3, centroids, myBbox), format = "file"),


   # ---- Second part targets ----
  tar_target(km_distance, distance_between_centroids_km(centroids)),

  tar_terra_vect(nf_vect, convert_to_vect(nf)),
  tar_terra_vect(gr_vect, convert_to_vect(gr)),
  tar_terra_vect(na_vect, convert_to_vect(na)),
  tar_stars(mire_stars_nf, crop_and_mask(mire_terra, nf_vect)),
  tar_stars(mire_stars_gr, crop_and_mask(mire_terra, gr_vect)),
  tar_stars(mire_stars_na, crop_and_mask(mire_terra, na_vect)),
  tar_terra_rast(mire_terra_nf, terra::rast(mire_stars_nf)),
  tar_terra_rast(mire_terra_gr, terra::rast(mire_stars_gr)),
  tar_terra_rast(mire_terra_na, terra::rast(mire_stars_na)),
  tar_target(dk2, coverage_area_km2(coverage3)),
  tar_target(mireArea, mire_area_stats(list(nf = mire_terra_nf, gr = mire_terra_gr, na = mire_terra_na)))
  #tar_target(mire_in_dk, mire_in_survey_km2(list(nf = mire_terra_nf, gr = mire_terra_gr, na = mire_terra_na), dk2)),

  ## Infrastructure (vectorized cache + 100m muni-cropped version)
  #tar_target(path_temp, get_path_temp()),
  #tar_file_read(infra_vec, paste0(path_temp, "infrastructureIndex_discrete_vectorized.rds"), read = readRDS(!!.x)),
  #tar_target(infra_vec2, infra_area_add(infra_vec)),
  #tar_target(infra_area_plot, infra_area_plot(infra_vec2)),
  #tar_target(infra_big, infra),
  #tar_target(infraMuni3, infra_muni3_vectorize(infra_big, muni3, outline, myCRS)),
  #tar_target(infraMuni3_tbl, infra_muni3_tbl(infraMuni3)),
  #tar_target(infraMuni3_summary, infra_muni3_summary(infraMuni3_tbl)),
  #tar_target(infra_dist_plot, infra_dist_plot(infraMuni3_tbl)),
#
  ## Stratification validation
  #tar_target(corrCheck, corr_check(naturetypes_norm, infra_vec)),
  #tar_target(validationPlot, validation_plot(corrCheck)),
#
  ## Methods maps (tmap objects + a TIFF file to include in quarto)
  #tar_target(infra_maps_list, infra_maps(muni3, infraMuni3, terrestrial, ocean, dk2, nature3)),
  #tar_target(methodsMap, infra_maps_list$methodsMap),
  #tar_target(methodsMap_tiff, save_tmap_tiff(methodsMap, here::here("outputs", "fig", "studyLocations.tiff"), dpi = 600), format = "file"),
  #tar_target(NA_overview, map_overview_na(muni3, terrestrial, dk2, nature3)),
  #tar_target(infraMuniMap_NA, infra_map_na(muni3, infraMuni3)),
#
  ## Municipality summary table (data frame)
  #tar_target(muni_tbl, muni_table(muni3, terrestrial, dk2, nature3, mireArea, mire_in_dk, infraMuni3_summary)),
#
  ## National shapes from full data + domain summaries (core results)
  #tar_target(fit_df, fit_df_from_corrcheck(corrCheck)),
  #tar_target(stats_tbl, mean_per_hia_tbl(nature3, infraMuni3, fit_df, prior_weight = 20, n_sim = 1000)),
  #tar_target(forest_plot, forest_plot_from_stats(stats_tbl)),
  #tar_target(forest_plot_ex, forest_plot_example(stats_tbl)),
#
  ## Spread HIA summaries to EDM mires + example map
  #tar_target(spread_nf, spread_mires_to_edm(mire_stars_nf, infraMuni3, stats_tbl, "Nordre Follo")),
  #tar_target(spread_gr, spread_mires_to_edm(mire_stars_gr, infraMuni3, stats_tbl, "Gran")),
  #tar_target(spread_na, spread_mires_to_edm(mire_stars_na, infraMuni3, stats_tbl, "Nord-Aurdal")),
  #tar_target(spread_na_map, spread_na_example_map(spread_na, na, myCRS)),
  #tar_target(spread_na_tiff, save_plot_tiff(spread_na_map, here::here("outputs", "fig", "spread-na.tiff"), dpi = 300), format = "file"),
#
  ## EAA (municipality) aggregation + table/figures
  #tar_target(combineAll, combine_all_eaa(spread_nf, spread_gr, spread_na, stats_tbl)),
  #tar_target(eea_plot, eea_plot(combineAll)),
  #tar_target(eea_plot_tiff, save_plot_tiff(eea_plot, here::here("outputs", "fig", "results.tiff"), dpi = 300), format = "file"),
  #tar_target(EEA_tbl_out, eea_table_kable(combineAll)),
#
  ## Indicator magnify example figure
  #tar_target(indicator_magnify, indicator_magnify_plot(nature3, na, myCRS)),
  #tar_target(indicator_magnify_tiff, save_plot_tiff(indicator_magnify, here::here("outputs", "fig", "indicator-magnify.tiff"), dpi = 300), format = "file"),



  #tar_quarto(all_outputs, "all_outputs.qmd")
)
