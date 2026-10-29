# 03_inform.R — crisis severity from ACAPS INFORM Severity Index
# Also produces the master list of crisis countries.

library(httr2)
library(dplyr)
library(readr)
library(tidyr)
library(countrycode)

base <- "https://api.acaps.org/api/v1/"

# 1. Get a token using the login saved in .Renviron
get_token <- function() {
  if (Sys.getenv("ACAPS_USER") == "") stop("ACAPS_USER is empty. Check your .Renviron file.")
  for (path in c("token-auth/", "get-auth-token/")) {
    resp <- request(paste0(base, path)) |>
      req_body_form(username = Sys.getenv("ACAPS_USER"),
                    password = Sys.getenv("ACAPS_PASS")) |>
      req_error(is_error = function(r) FALSE) |>
      req_perform()
    if (resp_status(resp) == 200) return(resp_body_json(resp)$token)
  }
  stop("Could not get an ACAPS token. Check your email and password in .Renviron.")
}
token <- get_token()
message("Token received.")

# 2. A helper that fetches every page of an ACAPS endpoint
get_acaps <- function(url) {
  out <- list()
  while (!is.null(url)) {
    resp <- request(url) |>
      req_headers(Authorization = paste("Token", token)) |>
      req_error(is_error = function(r) FALSE) |>
      req_perform()
    if (resp_status(resp) != 200) return(NULL)
    body <- resp_body_json(resp, simplifyVector = TRUE)
    out[[length(out) + 1]] <- body$results
    url <- body$`next`
    Sys.sleep(1)
  }
  bind_rows(out)
}

# Reference month = last complete month (same rule as the GDELT data)
ref <- seq(as.Date(format(Sys.Date(), "%Y-%m-01")), by = "-1 month", length.out = 2)[2]
months_to_try <- paste0(month.abb[as.integer(format(ref, "%m"))], format(ref, "%Y"))

inform <- NULL
for (m in months_to_try) {
  message("Trying ", m, "...")
  inform <- get_acaps(paste0(base, "inform-severity-index/", m, "/"))
  if (!is.null(inform) && nrow(inform) > 0) { inform_month <- m; break }
}
if (is.null(inform)) stop("No INFORM data found for the last 4 months.")
message("Using INFORM Severity for ", inform_month, ": ", nrow(inform), " crises.")

# 4. Find the right columns (names can vary slightly)
print(names(inform))
sev_col    <- grep("severity", names(inform), ignore.case = TRUE, value = TRUE)
sev_col    <- sev_col[!grepl("category|trend|class", sev_col, ignore.case = TRUE)][1]
iso_col    <- intersect(c("iso3", "ISO3", "iso"), names(inform))[1]
crisis_col <- intersect(c("crisis_name", "crisis", "name"), names(inform))[1]
message("Severity column: ", sev_col, " | ISO3 column: ", iso_col, " | Crisis column: ", crisis_col)

# 5. All rows, with ACAPS's own flags
crises <- inform |>
  transmute(crisis        = .data[[crisis_col]],
            iso3          = .data[[iso_col]],
            severity      = suppressWarnings(as.numeric(.data[[sev_col]])),
            country_level = country_level,
            row_type      = individual_aggregated)

if (is.list(crises$iso3)) crises <- unnest_longer(crises, iso3)

# How many individual crises each country has
crisis_counts <- crises |>
  filter(row_type == "Individual") |>
  count(iso3, name = "crises")

# 6. One row per country: ACAPS's official country-level score
severity <- crises |>
  filter(country_level == "Yes", !is.na(severity), !is.na(iso3), iso3 != "") |>
  group_by(iso3) |>
  slice_max(severity, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(iso3, top_crisis = crisis, severity) |>
  left_join(crisis_counts, by = "iso3") |>
  mutate(country = countrycode(iso3, "iso3c", "country.name", warn = FALSE),
         inform_month = inform_month) |>
  arrange(desc(severity))

# 7. Put the juice in the fridge
write_csv(severity, "data/severity.csv")
write_csv(select(severity, iso3, country), "data/countries.csv")

message(nrow(severity), " countries saved.")
head(severity, 15)