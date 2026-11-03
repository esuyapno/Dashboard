# 04_structural.R — sector indicators from the World Bank (values only, no scoring)
# Each sector tab shows these values using the sector's own established classification.
# Rule: latest value within the last 10 years; the year is kept and shown on the dashboard.

library(WDI)
library(dplyr)
library(readr)

max_age   <- 10
this_year <- as.numeric(format(Sys.Date(), "%Y"))

indicators <- tribble(
  ~code,               ~sector,       ~side,          ~label,
  "SN.ITK.DEFC.ZS",    "food",        "development",  "Prevalence of undernourishment (%)",
  "SH.STA.WAST.ZS",    "nutrition",   "humanitarian", "Wasting, children under 5 (%)",
  "SH.STA.STNT.ME.ZS", "nutrition",   "development",  "Stunting, children under 5 (%, modelled)",
  "SH.H2O.BASW.ZS",    "wash",        "development",  "At least basic drinking water (%)",
  "SH.STA.BASS.ZS",    "wash",        "development",  "At least basic sanitation (%)",
  "SH.IMM.IDPT",       "health",      "development",  "DTP3 vaccination coverage (%)",
  "SH.UHC.SRVS.CV.XD", "health",      "development",  "UHC service coverage index (0-100)",
  "SE.PRM.UNER.ZS",    "education",   "development",  "Primary-age children out of school (%)",
  "EN.POP.SLUM.UR.ZS", "shelter",     "development",  "Urban population in slums (%)",
  "SP.POP.TOTL",       "population",  "denominator",  "Total population"
)

# Download one indicator at a time, so one broken code doesn't stop the others
fetch_one <- function(code) {
  tryCatch({
    WDI(indicator = code, start = this_year - max_age, end = this_year, extra = TRUE) |>
      filter(!is.na(region), region != "Aggregates") |>
      transmute(iso3 = iso3c, country, year = as.integer(year),
                value = .data[[code]], code = code)
  }, error = function(e) {
    message("Could not download ", code, ": ", conditionMessage(e))
    NULL
  })
}

raw <- bind_rows(lapply(indicators$code, fetch_one))

structural <- raw |>
  filter(!is.na(value)) |>
  group_by(iso3, code) |>
  slice_max(year, n = 1, with_ties = FALSE) |>     # latest year within the window
  ungroup() |>
  left_join(indicators, by = "code") |>
  mutate(data_age = this_year - year) |>
  select(iso3, country, sector, side, code, label, value, year, data_age)

write_csv(structural, "data/structural.csv")

missing_codes <- setdiff(indicators$code, structural$code)
if (length(missing_codes) > 0) {
  message("No data in the last ", max_age, " years for: ", paste(missing_codes, collapse = ", "))
}
message("Saved ", n_distinct(structural$code), " of ", nrow(indicators), " indicators for ",
        n_distinct(structural$iso3), " countries.")
print(count(structural, label, name = "countries"), n = Inf)
