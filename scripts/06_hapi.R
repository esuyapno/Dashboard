# 06_hapi.R — humanitarian sector data from HDX HAPI
#  - IPC acute food insecurity: share of population in Phase 3+ (current analysis)
#  - People in need by sector, from humanitarian needs overviews (JIAF)
#
# ONE-TIME SET-UP: HAPI needs a free "app identifier" (your app name and email, encoded).
#   1. In the Console run:  jsonlite::base64_enc("SAAnalyticalDashboard:your-email@example.com")
#   2. Copy the result, then run file.edit("~/.Renviron") and add a line:
#        HAPI_APP_ID=the-text-you-copied
#   3. Save, then Session > Restart R.

library(httr2)
library(dplyr)
library(readr)

app_id <- Sys.getenv("HAPI_APP_ID")
if (app_id == "") stop("HAPI_APP_ID is empty. Follow the set-up steps at the top of this script.")

base <- "https://hapi.humdata.org/api/v2/"

# Fetch every page of a HAPI endpoint as a table
hapi_get <- function(path, ...) {
  out <- list(); offset <- 0
  repeat {
    resp <- request(paste0(base, path)) |>
      req_url_query(app_identifier = app_id, output_format = "csv",
                    limit = 10000, offset = offset, ...) |>
      req_timeout(120) |>
      req_retry(max_tries = 3, retry_on_failure = TRUE) |>
      req_perform()
    d <- read_csv(I(resp_body_string(resp)), col_types = cols(.default = col_character()))
    out[[length(out) + 1]] <- d
    if (nrow(d) < 10000) break
    offset <- offset + 10000
  }
  bind_rows(out) |> type_convert(col_types = cols())
}

# Only analyses whose reference period ended in the last two years
recent_cutoff <- Sys.Date() - 730

# ---- 1. IPC acute food insecurity --------------------------------------------
message("Fetching IPC acute food insecurity...")
fs <- bind_rows(
  hapi_get("food-security-nutrition-poverty/food-security", ipc_phase = "3+",  ipc_type = "current"),
  hapi_get("food-security-nutrition-poverty/food-security", ipc_phase = "all", ipc_type = "current")
) |>
  mutate(ref_end = as.Date(substr(as.character(reference_period_end), 1, 10)),
         population_in_phase = as.numeric(population_in_phase)) |>
  filter(!is.na(ref_end), ref_end >= recent_cutoff)

# Latest analysis per country; use the national figure if present, otherwise add up provinces
ipc <- fs |>
  group_by(iso3 = location_code) |>
  filter(ref_end == max(ref_end)) |>
  filter(admin_level == min(admin_level)) |>
  group_by(iso3, ref_end, admin_level) |>
  summarise(p3   = sum(population_in_phase[ipc_phase == "3+"],  na.rm = TRUE),
            pall = sum(population_in_phase[ipc_phase == "all"], na.rm = TRUE),
            ref_start = min(as.Date(substr(as.character(reference_period_start), 1, 10))),
            .groups = "drop") |>
  filter(pall > 0) |>
  transmute(iso3, ipc3_pct = 100 * p3 / pall, ipc3_people = p3, analysed_pop = pall,
            ipc_from = ref_start, ipc_to = ref_end, ipc_level = admin_level)

write_csv(ipc, "data/ipc.csv")
message("IPC: ", nrow(ipc), " countries with a recent analysis.")

# ---- 2. People in need by sector (JIAF) ----------------------------------------
message("Fetching people in need by sector...")
hn <- hapi_get("affected-people/humanitarian-needs", population_status = "INN", admin_level = 0) |>
  mutate(ref_end = as.Date(substr(as.character(reference_period_end), 1, 10)),
         population = as.numeric(population),
         sname = tolower(coalesce(sector_name, "")))

# Show the sector names HAPI uses, so the matching below can be checked
message("Sector names found:")
print(sort(unique(hn$sector_name)))

sector_of <- function(s) {
  case_when(
    grepl("child|gender|gbv|mine|housing, land|hlp", s)  ~ NA_character_,   # protection sub-areas
    grepl("intersector|inter-sector", s)                  ~ "intersectoral",
    grepl("food", s)                                      ~ "food",
    grepl("nutrition", s)                                 ~ "nutrition",
    grepl("wash|water", s)                                ~ "wash",
    grepl("shelter", s)                                   ~ "shelter",
    grepl("^health", s)                                   ~ "health",
    grepl("education", s)                                 ~ "education",
    grepl("protection", s)                                ~ "protection",
    TRUE                                                  ~ NA_character_
  )
}

needs <- hn |>
  mutate(sector = sector_of(sname)) |>
  filter(!is.na(sector), !is.na(population), !is.na(ref_end), ref_end >= recent_cutoff) |>
  group_by(iso3 = location_code) |>
  filter(ref_end == max(ref_end)) |>                    # latest needs overview per country
  group_by(iso3, sector, ref_end) |>
  summarise(pin = max(population), .groups = "drop")   # the total is the largest figure

write_csv(needs, "data/needs.csv")
message("People in need: ", n_distinct(needs$iso3), " countries, sectors: ",
        paste(sort(unique(needs$sector)), collapse = ", "))
