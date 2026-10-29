# 02_fts.R — how much of each country's humanitarian appeal is funded

library(dplyr)
library(readr)

# Open the file. Its second row holds special tags (HXL), so we remove it.
fts_raw <- read_csv("data/fts_requirements_funding_global.csv", show_col_types = FALSE)
fts <- fts_raw[-1, ] |> type_convert()

# Look at the column names first, so we know what we're working with
names(fts)

# Keep this year's appeals and add them up per country
this_year <- as.numeric(format(Sys.Date(), "%Y"))

funding <- fts |>
  filter(year == this_year, !is.na(countryCode), requirements > 0) |>
  group_by(iso3 = countryCode) |>
  summarise(requirements = sum(requirements, na.rm = TRUE),
            funding      = sum(funding, na.rm = TRUE),
            funded_pct   = round(100 * funding / requirements, 1),
            plans        = n(),
            .groups = "drop")

# Put the juice in the fridge
write_csv(funding, "data/funding.csv")
funding |> filter(iso3 %in% c("SDN", "COD", "HTI", "MMR", "UKR"))