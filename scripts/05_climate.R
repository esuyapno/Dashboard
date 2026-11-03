# 05_climate.R — climate vulnerability and readiness from ND-GAIN
# Before running: download the ND-GAIN country index zip from gain.nd.edu
# (Our Work > Country Index > Download Data) and unzip it into data/ndgain/

library(dplyr)
library(readr)
library(tidyr)

max_age   <- 10
this_year <- as.numeric(format(Sys.Date(), "%Y"))
nd_dir    <- "data/ndgain"
files     <- list.files(nd_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
if (length(files) == 0) stop("No ND-GAIN files found in ", nd_dir, ". Download and unzip them first.")

# ND-GAIN has several files with the same name (e.g. a "trends" version with a
# Value/sign column). Pick the one that has one column per year (1995, 1996, ...).
find_file <- function(name) {
  cands <- files[basename(files) == name & !grepl("__MACOSX|trend", files, ignore.case = TRUE)]
  for (f in cands) {
    cols <- names(read_csv(f, n_max = 0, show_col_types = FALSE))
    if (any(grepl("^[0-9]{4}$", cols))) return(f)
  }
  NA_character_
}

# One row per country, one column per year. Take the latest year with data.
latest <- function(file, measure) {
  read_csv(file, show_col_types = FALSE) |>
    pivot_longer(matches("^[0-9]{4}$"), names_to = "year", values_to = "value") |>
    filter(!is.na(value)) |>
    group_by(iso3 = ISO3) |>
    slice_max(year, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(iso3, measure = measure, value, year = as.integer(year))
}

vuln_file <- find_file("vulnerability.csv")
read_file <- find_file("readiness.csv")
if (is.na(vuln_file) || is.na(read_file)) {
  stop("Could not find the yearly vulnerability.csv / readiness.csv files in ", nd_dir,
       ". Run list.files(\"data/ndgain\", recursive = TRUE) and check the folder.")
}
message("Using: ", vuln_file, " and ", read_file)

climate <- bind_rows(
  latest(vuln_file, "vulnerability"),   # 0-1, higher = more vulnerable
  latest(read_file, "readiness")        # 0-1, higher = more ready
)

# Sector sub-scores of climate vulnerability (shown on the sector tabs)
sector_files <- c(food = "food.csv", water = "water.csv",
                  health = "health.csv", habitat = "habitat.csv")
for (s in names(sector_files)) {
  f <- find_file(sector_files[[s]])
  if (!is.na(f)) climate <- bind_rows(climate, latest(f, paste0("vuln_", s)))
}

# 0-10, where 10 = most at risk (readiness is flipped). Apply the 10-year rule.
climate <- climate |>
  mutate(score    = ifelse(measure == "readiness", (1 - value) * 10, value * 10),
         data_age = this_year - year) |>
  filter(data_age <= max_age)

# Overall climate risk: average of vulnerability and lack of readiness
climate_score <- climate |>
  filter(measure %in% c("vulnerability", "readiness")) |>
  group_by(iso3) |>
  summarise(climate_score = mean(score), climate_year = min(year), .groups = "drop")

write_csv(climate, "data/climate_detail.csv")
write_csv(climate_score, "data/climate.csv")
message("Climate data saved for ", nrow(climate_score), " countries. Measures found: ",
        paste(unique(climate$measure), collapse = ", "))