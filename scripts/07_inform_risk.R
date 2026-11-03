# 07_inform_risk.R — INFORM Risk Index (European Commission JRC)
# Used for the Development map on the Global vulnerability tab.
#
# BEFORE RUNNING (once a year, when JRC publishes a new release):
#   1. Go to the INFORM Risk "Results and data" page on the JRC DRMKC website.
#   2. Download the latest INFORM Risk results file (Excel, .xlsx).
#   3. Save it in your data folder. Keep "INFORM" and "Risk" in the file name,
#      e.g. data/INFORM_Risk_2026.xlsx. Delete older releases from the folder.
#   4. Check the licence and recommended citation shown next to the download.

library(readxl)
library(dplyr)
library(readr)

f <- list.files("data", pattern = "INFORM.*Risk.*\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
if (length(f) == 0) stop("No INFORM Risk Excel file found in the data folder. See the steps at the top of this script.")
if (length(f) > 1) message("Several INFORM Risk files found; using the newest by name: ", sort(f, decreasing = TRUE)[1])
f <- sort(f, decreasing = TRUE)[1]
release <- regmatches(basename(f), regexpr("20[0-9]{2}", basename(f)))
if (length(release) == 0) release <- "release year unknown"

# Find the results table: the sheet whose header row contains ISO3 and a
# "Development & Deprivation" column (the header is often not on the first row)
read_results <- function(sh) {
  top <- suppressMessages(read_excel(f, sheet = sh, col_names = FALSE, n_max = 15))
  hdr <- which(apply(top, 1, function(r) any(toupper(trimws(as.character(r))) == "ISO3", na.rm = TRUE)))
  if (length(hdr) == 0) return(NULL)
  d <- suppressMessages(read_excel(f, sheet = sh, skip = hdr[1] - 1, .name_repair = "unique"))
  if (!any(grepl("development.*deprivation", names(d), ignore.case = TRUE))) return(NULL)
  d
}

tab <- NULL
for (sh in excel_sheets(f)) {
  tab <- read_results(sh)
  if (!is.null(tab)) { message("Using sheet: ", sh); break }
}
if (is.null(tab)) stop("Could not find a sheet with ISO3 and 'Development & Deprivation' columns. ",
                       "Run readxl::excel_sheets() on the file and tell me the sheet names.")

# Pick columns by name (first match), and convert to numbers ("x" = missing)
pick <- function(pattern) {
  hit <- grep(pattern, names(tab), ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) { message("  No column matching: ", pattern); return(rep(NA_real_, nrow(tab))) }
  message("  ", pattern, "  ->  ", hit[1])
  suppressWarnings(as.numeric(as.character(tab[[hit[1]]])))
}

message("Columns used:")
inform_risk <- tibble(
  iso3          = as.character(tab[[grep("^ISO3$", names(tab), ignore.case = TRUE, value = TRUE)[1]]]),
  risk          = pick("^INFORM RISK$"),
  hazard        = pick("^HAZARD & EXPOSURE$"),
  vulnerability = pick("^VULNERABILITY$"),
  lcc           = pick("^LACK OF COPING CAPACITY$"),
  dev_depr      = pick("development.*deprivation"),
  release       = release
) |>
  filter(!is.na(iso3), nchar(iso3) == 3)

write_csv(inform_risk, "data/inform_risk.csv")
message("INFORM Risk ", release, ": ", sum(!is.na(inform_risk$dev_depr)),
        " countries with a Development & Deprivation score.")
