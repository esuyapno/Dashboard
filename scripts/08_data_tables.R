# 08_data_tables.R — the tables behind the maps, for the "Get the data" page
#
# For each dashboard tab (except Funding) this script makes one table and writes:
#   docs/data/<tab>.csv    first line = source and licence, then the table
#   docs/data/<tab>.xlsx   sheet "Data" (the table) + sheet "Notes" (sources, licences, how to cite)
#   data/tables.rds        everything data.qmd needs to show the tables on screen
#
# ACAPS INFORM Severity scores (licence CC BY-NC-ND 4.0) are shown on screen only.
# They are never written into the download files.
#
# ONE-TIME SET-UP: install.packages("writexl")

library(dplyr)
library(readr)
library(writexl)

out_dir <- "docs/data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
today <- Sys.Date()

# ---- Read the data the other scripts saved ---------------------------------------
read_opt <- function(f, empty) if (file.exists(f)) read_csv(f, show_col_types = FALSE) else empty

structural <- read_csv("data/structural.csv", show_col_types = FALSE)
severity   <- read_csv("data/severity.csv",   show_col_types = FALSE)
ipc   <- read_opt("data/ipc.csv",   tibble(iso3 = character(), ipc3_pct = numeric(),
                                           ipc3_people = numeric(), ipc_to = as.Date(character())))
needs <- read_opt("data/needs.csv", tibble(iso3 = character(), sector = character(),
                                           ref_end = as.Date(character()), pin = numeric()))
climate_detail <- read_opt("data/climate_detail.csv",
                           tibble(iso3 = character(), measure = character(), score = numeric(), year = integer()))
inform_risk <- read_opt("data/inform_risk.csv",
                        tibble(iso3 = character(), risk = numeric(), lcc = numeric(),
                               dev_depr = numeric(), release = character()))
risk_release <- if (nrow(inform_risk) > 0) as.character(inform_risk$release[1]) else "(not loaded)"
ndgain_year  <- if (nrow(climate_detail) > 0) max(climate_detail$year, na.rm = TRUE) else format(today, "%Y")

country_names <- bind_rows(distinct(structural, iso3, country),
                           select(severity, iso3, country)) |>
  distinct(iso3, .keep_all = TRUE)

# ---- Small helpers: each returns iso3, value, year ----------------------------------
ind <- function(cd) structural |> filter(code == cd) |> select(iso3, value, year)

pop <- ind("SP.POP.TOTL") |> select(iso3, pop = value)

# People in need as % of population (needs overviews + World Bank population)
pin_pct <- function(sec) {
  needs |> filter(sector == sec) |> left_join(pop, by = "iso3") |>
    filter(!is.na(pop), pop > 0) |>
    transmute(iso3, value = pmin(100, 100 * pin / pop), year = as.integer(format(as.Date(ref_end), "%Y")))
}
# People in need as a number of people
pin_n <- function(sec) {
  needs |> filter(sector == sec) |>
    transmute(iso3, value = pin, year = as.integer(format(as.Date(ref_end), "%Y")))
}

ipc_pct <- ipc |> transmute(iso3, value = ipc3_pct,    year = as.integer(format(as.Date(ipc_to), "%Y")))
ipc_n   <- ipc |> transmute(iso3, value = ipc3_people, year = as.integer(format(as.Date(ipc_to), "%Y")))

clim <- function(m) climate_detail |> filter(measure == m) |> transmute(iso3, value = score, year)

inform_col <- function(col) inform_risk |> transmute(iso3, value = .data[[col]], year = NA_integer_)

# Join several indicators into one table: Country, ISO3, then each value with its year
build_table <- function(parts) {
  out <- country_names
  for (nm in names(parts)) {
    p <- parts[[nm]] |> select(iso3, value, year)
    names(p) <- c("iso3", nm, paste0(nm, " - year"))
    out <- left_join(out, p, by = "iso3")
  }
  out <- out |>
    filter(if_any(all_of(names(parts)), ~ !is.na(.x))) |>
    mutate(across(all_of(names(parts)), ~ round(.x, 1)))
  
  # Drop columns with no data at all (e.g. the empty "year" columns of INFORM Risk,
  # or an indicator the World Bank has no recent values for) and say so
  empty <- names(out)[sapply(out, function(x) all(is.na(x)))]
  gone  <- intersect(empty, names(parts))
  if (length(gone) > 0) message("  No data for: ", paste(gone, collapse = ", "), " (column left out)")
  out <- out |> select(-all_of(empty), -any_of(paste0(gone, " - year")))
  
  first <- intersect(names(parts), names(out))[1]     # sort by the first indicator that has data
  out |>
    rename(Country = country, ISO3 = iso3) |>
    relocate(Country, ISO3) |>
    arrange(desc(.data[[first]]), Country)
}

# ---- Source and licence texts --------------------------------------------------------
src <- list(
  ipc    = "IPC acute food insecurity, current analyses (Integrated Food Security Phase Classification, via HDX HAPI). Licence: public domain.",
  hno    = "People in need: humanitarian needs overviews (OCHA, via HDX HAPI). Licence: Creative Commons Attribution (CC BY). Share of population calculated by SA Analytical with World Bank population (SP.POP.TOTL).",
  wb     = "World Bank, World Development Indicators. Licence: CC BY 4.0. The original producer of each indicator is named in the Notes.",
  inform = paste0("INFORM Risk ", risk_release, ", European Commission Joint Research Centre. \u00a9 European Union. Licence: CC BY 4.0."),
  ndgain = paste0("Notre Dame Global Adaptation Initiative Country Index (ND-GAIN). (", ndgain_year,
                  "). University of Notre Dame. Free and open access; please cite as shown.")
)

# What each column means (used in the Notes sheet)
about <- c(
  "IPC Phase 3+ (%)"                     = "Share of the analysed population in IPC Phase 3 (Crisis) or above, latest current analysis in the last two years.",
  "IPC Phase 3+ (people)"                = "Number of people in IPC Phase 3 or above.",
  "Undernourishment (%)"                 = "Prevalence of undernourishment, SDG 2.1.1 (FAO, via World Bank SN.ITK.DEFC.ZS).",
  "Wasting (%)"                          = "Wasting, children under 5 (UNICEF/WHO/World Bank, via World Bank SH.STA.WAST.ZS).",
  "Stunting (%)"                         = "Stunting, children under 5, modelled (Joint Malnutrition Estimates, via World Bank SH.STA.STNT.ME.ZS).",
  "Basic drinking water (%)"             = "Population using at least basic drinking water (WHO/UNICEF JMP, via World Bank SH.H2O.BASW.ZS).",
  "Basic sanitation (%)"                 = "Population using at least basic sanitation (WHO/UNICEF JMP, via World Bank SH.STA.BASS.ZS).",
  "Urban slum population (%)"            = "Urban population living in slums, SDG 11.1.1 (UN-Habitat, via World Bank EN.POP.SLUM.UR.ZS).",
  "DTP3 coverage (%)"                    = "DTP3 immunisation coverage among 1-year-olds (WHO/UNICEF, via World Bank SH.IMM.IDPT).",
  "UHC service coverage index"           = "Universal health coverage service coverage index, 0-100 (WHO, via World Bank SH.UHC.SRVS.CV.XD).",
  "Out of school, primary (%)"           = "Primary-age children out of school (UNESCO Institute for Statistics, via World Bank SE.PRM.UNER.ZS).",
  "Development & deprivation (0-10)"     = "INFORM Risk, Development & Deprivation component, as published.",
  "Lack of coping capacity (0-10)"       = "INFORM Risk, Lack of Coping Capacity dimension, as published.",
  "INFORM Risk (0-10)"                   = "INFORM Risk overall score, as published.",
  "Climate vulnerability (0-10)"         = "ND-GAIN vulnerability score (0-1), shown x10.",
  "Lack of readiness (0-10)"             = "ND-GAIN readiness score (0-1), reversed and shown x10, so higher = less ready.",
  "Food (sector, 0-10)"                  = "ND-GAIN food sector vulnerability (0-1), shown x10.",
  "Water (sector, 0-10)"                 = "ND-GAIN water sector vulnerability (0-1), shown x10.",
  "Health (sector, 0-10)"                = "ND-GAIN health sector vulnerability (0-1), shown x10.",
  "Habitat (sector, 0-10)"               = "ND-GAIN human habitat sector vulnerability (0-1), shown x10."
)
pin_about <- function(label) c(
  setNames(paste0("People in need of ", label, " assistance, as a share of the total population."),
           paste0("People in need, ", label, " (%)")),
  setNames(paste0("People in need of ", label, " assistance (humanitarian needs overview)."),
           paste0("People in need, ", label, " (people)"))
)

# ---- The tables, one per tab ------------------------------------------------------------
pin_parts <- function(sec, label) {
  setNames(list(pin_pct(sec), pin_n(sec)),
           c(paste0("People in need, ", label, " (%)"), paste0("People in need, ", label, " (people)")))
}

tabs <- list(
  global = list(
    title = "Global vulnerability map",
    intro = "Crisis indicators and the INFORM Risk Index for every country with data.",
    parts = c(list(`IPC Phase 3+ (%)` = ipc_pct),
              setNames(list(pin_pct("intersectoral")), "People in need, all sectors (%)"),
              list(`Development & deprivation (0-10)` = inform_col("dev_depr"),
                   `Lack of coping capacity (0-10)`   = inform_col("lcc"),
                   `INFORM Risk (0-10)`               = inform_col("risk"))),
    sources = c("ipc", "hno", "inform"),
    about   = c(about["IPC Phase 3+ (%)"],
                c(`People in need, all sectors (%)` = "People in need across all sectors (intersectoral), as a share of the total population."),
                about[c("Development & deprivation (0-10)", "Lack of coping capacity (0-10)", "INFORM Risk (0-10)")]),
    acaps   = TRUE),
  food = list(
    title = "Food security",
    intro = "Acute food insecurity (IPC) and chronic undernourishment.",
    parts = list(`IPC Phase 3+ (%)` = ipc_pct, `IPC Phase 3+ (people)` = ipc_n,
                 `Undernourishment (%)` = ind("SN.ITK.DEFC.ZS")),
    sources = c("ipc", "wb"),
    about   = about[c("IPC Phase 3+ (%)", "IPC Phase 3+ (people)", "Undernourishment (%)")]),
  nutrition = list(
    title = "Nutrition",
    intro = "Acute (wasting) and chronic (stunting) malnutrition in children under 5.",
    parts = list(`Wasting (%)` = ind("SH.STA.WAST.ZS"), `Stunting (%)` = ind("SH.STA.STNT.ME.ZS")),
    sources = "wb",
    about   = about[c("Wasting (%)", "Stunting (%)")]),
  wash = list(
    title = "WASH",
    intro = "People in need of water, sanitation and hygiene assistance, and access to basic services.",
    parts = c(pin_parts("wash", "WASH"),
              list(`Basic drinking water (%)` = ind("SH.H2O.BASW.ZS"),
                   `Basic sanitation (%)`     = ind("SH.STA.BASS.ZS"))),
    sources = c("hno", "wb"),
    about   = c(pin_about("WASH"), about[c("Basic drinking water (%)", "Basic sanitation (%)")])),
  shelter = list(
    title = "Shelter",
    intro = "People in need of shelter assistance, and the urban population living in slums.",
    parts = c(pin_parts("shelter", "shelter"),
              list(`Urban slum population (%)` = ind("EN.POP.SLUM.UR.ZS"))),
    sources = c("hno", "wb"),
    about   = c(pin_about("shelter"), about["Urban slum population (%)"])),
  health = list(
    title = "Health",
    intro = "People in need of health assistance, vaccination coverage and access to health services.",
    parts = c(pin_parts("health", "health"),
              list(`DTP3 coverage (%)`          = ind("SH.IMM.IDPT"),
                   `UHC service coverage index` = ind("SH.UHC.SRVS.CV.XD"))),
    sources = c("hno", "wb"),
    about   = c(pin_about("health"), about[c("DTP3 coverage (%)", "UHC service coverage index")])),
  education = list(
    title = "Education",
    intro = "People in need of education assistance, and primary-age children out of school.",
    parts = c(pin_parts("education", "education"),
              list(`Out of school, primary (%)` = ind("SE.PRM.UNER.ZS"))),
    sources = c("hno", "wb"),
    about   = c(pin_about("education"), about["Out of school, primary (%)"])),
  protection = list(
    title = "Protection",
    intro = "People in need of protection assistance. There is no established development benchmark for protection.",
    parts = pin_parts("protection", "protection"),
    sources = "hno",
    about   = pin_about("protection")),
  climate = list(
    title = "Climate",
    intro = "Climate vulnerability and readiness to adapt (ND-GAIN), with sector vulnerability scores.",
    parts = list(`Climate vulnerability (0-10)` = clim("vulnerability"),
                 `Lack of readiness (0-10)`     = clim("readiness"),
                 `Food (sector, 0-10)`          = clim("vuln_food"),
                 `Water (sector, 0-10)`         = clim("vuln_water"),
                 `Health (sector, 0-10)`        = clim("vuln_health"),
                 `Habitat (sector, 0-10)`       = clim("vuln_habitat")),
    sources = "ndgain",
    about   = about[c("Climate vulnerability (0-10)", "Lack of readiness (0-10)", "Food (sector, 0-10)",
                      "Water (sector, 0-10)", "Health (sector, 0-10)", "Habitat (sector, 0-10)")])
)

# ---- Write the files ------------------------------------------------------------------
acaps_text <- paste("ACAPS INFORM Severity scores are not included in this file (their licence does not",
                    "allow redistribution). They are free from ACAPS: https://www.acaps.org/en/data")

for (key in names(tabs)) {
  t <- tabs[[key]]
  file_table <- build_table(t$parts)                       # no ACAPS data in here
  t$about <- t$about[names(t$about) %in% names(file_table)]  # only explain columns that are in the file
  source_line <- paste0("Source and licence: ", paste(unlist(src[t$sources]), collapse = " "),
                        " Compiled by SA Analytical (dashboard.sa-analytical.com), ", today, ".")
  
  # CSV: the source line first (in quotes, so it stays in one cell in Excel), then the table.
  # The invisible "﻿" at the start makes Excel read accents (e.g. Côte d'Ivoire) correctly.
  csv_path <- file.path(out_dir, paste0(key, ".csv"))
  writeLines(c(paste0("﻿\"", gsub("\"", "'", source_line), "\""),
               format_csv(file_table, na = "")),
             csv_path, useBytes = TRUE)
  
  # Excel: the table, plus a Notes sheet
  notes <- tibble(
    Item = c("Dataset", "Source and licence", "Columns", names(t$about),
             "Year columns", "Missing values", if (isTRUE(t$acaps)) "Not included",
             "Compiled by", "Date", "Disclaimer"),
    Details = c(paste0("Severe Crises. Silent Headlines. - ", t$title),
                paste(unlist(src[t$sources]), collapse = " "),
                "", unname(t$about),
                "The year of the data behind each value. Values are the latest available within the last 10 years (two years for humanitarian analyses).",
                "Empty cells mean no data or no analysis for that country, which does not mean there are no needs.",
                if (isTRUE(t$acaps)) acaps_text,
                "SA Analytical, https://dashboard.sa-analytical.com",
                as.character(today),
                "The data providers are independent of SA Analytical and do not endorse this dataset. Any errors are SA Analytical's own.")
  )
  write_xlsx(list(Data = file_table, Notes = notes), file.path(out_dir, paste0(key, ".xlsx")))
  
  # For the Data page: the same table, plus (Global tab only) the ACAPS score shown on screen
  screen_table <- file_table
  if (isTRUE(t$acaps)) {
    screen_table <- screen_table |>
      left_join(severity |> select(ISO3 = iso3, `Crisis severity (ACAPS, on screen only)` = severity), by = "ISO3") |>
      relocate(`Crisis severity (ACAPS, on screen only)`, .after = ISO3) |>
      arrange(desc(`Crisis severity (ACAPS, on screen only)`), Country)
  }
  tabs[[key]]$screen  <- screen_table
  tabs[[key]]$source  <- source_line
  tabs[[key]]$n       <- nrow(file_table)
  tabs[[key]]$parts   <- NULL
  message(sprintf("  %-11s %4d countries  ->  %s.csv / .xlsx", key, nrow(file_table), key))
}

saveRDS(list(tabs = tabs, created = today, acaps_text = acaps_text), "data/tables.rds")
message("Data files written to ", out_dir, "/")