# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Some directories

# infrastructure index (i.e. land use intensity index)#
#path_infrastructure <- paste0(my_folder, "infra_tiff.tif")

# field survey, downloaded from https://kartkatalog.geonorge.no/metadata/naturtyper-miljoedirektoratets-instruks/eb48dd19-03da-41e1-afd9-7ebc3079265c
#path_naturetypes <- paste0(my_folder, "Naturtyper_nin_0000_norge_25833_FILEGDB.gdb")

# municipality outline
#path_muni <- here::here("data/Basisdata_0000_Norge_25833_Kommuner_FGDB.gdb")

# path to local caching folder
#path_temp <- paste0(root, "41201785_okologisk_tilstand_2022_2023/data/cache/")


# Set target options:
tar_option_set(
  packages = c(
    "knitr", "sf", "tmap", "tmaptools", "stars", "terra",
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
  tar_file_read(mire_terra, paste0(server_folder, "myrmodell90pros.tif"), read = terra::rast(!!.x)),
  tar_file_read(naturetypes, paste0(server_folder, "Naturtyper_nin_0000_norge_25833_FILEGDB.gdb"), read = sf::st_read(dsn = !!.x, layer = "naturtyper_nin_omr", quiet= T) |> st_transform(myCRS)),
  tar_file_read(coverage, paste0(server_folder, "Naturtyper_nin_0000_norge_25833_FILEGDB.gdb"), read = sf::st_read(dsn = !!.x, layer = "naturtyper_nin_dekning", quiet= T) |> st_transform(myCRS)),
  tar_file_read(outline, here::here("data/outlineOfNorway_EPSG25833.shp"), read = sf::st_read(dsn = !!.x, quiet = TRUE) |> st_transform(myCRS)),
  tar_file_read(muni, here::here("data/Basisdata_0000_Norge_25833_Kommuner_FGDB.gdb"), read = sf::read_sf(dsn = !!.x, layer = "kommune", quiet = TRUE) |> st_transform(myCRS)),
  tar_file_read(infra, paste0(server_folder, "infra_tiff.tif"), read = stars::read_stars(!!.x)),
  tar_target(myVars, c("7TK", "7SE", "PRTK", "PRSL", "7FA", "7GR-GI")),
  tar_target(nts, getRelevantNTs(naturetypes_summary, myVars)),
  tar_target(naturetypes_cs, clean_subset(naturetypes, nts = nts, myCodes = myVars)),
  tar_target(test_most_common, test_most_common(naturetypes_cs)),
  tar_target(naturetypes_p, convertToPercent(naturetypes_cs)),
  tar_target(test_naturetypes_p, test_convertToPercent(naturetypes_p)),

  tar_quarto(all_outputs, "manuscript/all_outputs.qmd")
)
