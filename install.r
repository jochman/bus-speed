r <- getOption("repos")
r["CRAN"] <- "https://ftp.cc.uoc.gr/mirrors/CRAN/"
options(repos = r)
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
    "tidyverse",
    "tidytransit",
    "jsonlite",
    "lubridate",
    "hms",
    "sf",
    "ggspatial",
    "gtfstools",
    "sfnetworks",
    "ggimage",
    "imputeTS",
    "BiocManager",
    "installr",
    "devtools"
)

pacman::p_load_gh("thomasp85/gganimate")
