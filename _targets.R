# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Some directories
source(here::here("R/get_root_NINA.R"))
root <- get_root_NINA()
my_folder <- paste0(root, "41001581_egenutvikling_anders_kolstad/data/")

# Ecosystem delineation map - unfortunately to publicly available
path_mire <- paste0(my_folder, "Myrmodell/myrmodell90pros.tif")

# infrastructure index (i.e. land use intensity index)
path_infrastructure <- paste0(my_folder, "infra_tiff.tif")

# field survey, downloaded from https://kartkatalog.geonorge.no/metadata/naturtyper-miljoedirektoratets-instruks/eb48dd19-03da-41e1-afd9-7ebc3079265c
path_naturetypes <- paste0(my_folder, "Naturtyper_nin_0000_norge_25833_FILEGDB.gdb")

# municipality outline
path_muni <- here::here("data/Basisdata_0000_Norge_25833_Kommuner_FGDB.gdb")

# path to local caching folder
path_temp <- paste0(root, "41201785_okologisk_tilstand_2022_2023/data/cache/")

myCRS <- 25832


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

  tar_file_read(naturetypes_summary, here::here("data/natureType_summary.rds"), read = readRDS(file = !!.x)),
  tar_file_read(mire_terra, path_mire, read = terra::rast(!!.x))

)
