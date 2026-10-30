# 01_gdelt.R — news attention for the crisis countries in data/countries.csv
# - Countries come from data/countries.csv (made by 03_inform.R)
# - Search phrases live in data/gdelt_queries.csv (editable)
# - Each month's answers are kept in data/gdelt_cache/<month>/, building a history

library(httr2)
library(dplyr)
library(readr)

# ---- 1. Search phrases, one per country ----------------------------------
countries    <- read_csv("data/countries.csv", show_col_types = FALSE)
queries_file <- "data/gdelt_queries.csv"

# Names that need special handling, so GDELT finds the right country.
# GDELT rejects quoted phrases shorter than about 5 letters, so short names have no quotes.
overrides <- tribble(
  ~iso3, ~query,
  "SDN", '"Sudan" -"South Sudan"',
  "SSD", '"South Sudan"',
  "COD", '("DR Congo" OR "Democratic Republic of the Congo")',
  "COG", '("Republic of the Congo" OR "Congo-Brazzaville")',
  "NER", '"Niger" -"Niger Delta" -"Niger State"',
  "GIN", '"Guinea" -"Guinea-Bissau" -"Papua New Guinea" -"Equatorial Guinea"',
  "GNB", '"Guinea-Bissau"',
  "CIV", "(\"Ivory Coast\" OR \"Cote d'Ivoire\")",
  "MMR", '"Myanmar"',
  "PSE", '("Gaza Strip" OR "West Bank" OR "Palestinian" OR Gaza)',
  "PRK", '"North Korea"',
  "KOR", '"South Korea"',
  "CAF", '"Central African Republic"',
  "TUR", '("Turkey" OR "Turkiye")',
  "GEO", '"Georgia" ("Tbilisi" OR "Georgian")',
  "LAO", 'Laos',
  "IRN", 'Iran',
  "SYR", '"Syria"',
  "RUS", '"Russia"',
  "VEN", '"Venezuela"',
  "BOL", '"Bolivia"'
)

default_query <- function(country) paste0('"', country, '"')

if (file.exists(queries_file)) {
  queries <- read_csv(queries_file, show_col_types = FALSE)
} else {
  queries <- tibble(iso3 = character(), country = character(), query = character())
}

# Add any new countries (e.g. a new crisis appears in INFORM)
new_countries <- anti_join(countries, queries, by = "iso3") |>
  mutate(query = default_query(country)) |>
  rows_update(overrides, by = "iso3", unmatched = "ignore")
queries <- bind_rows(queries, new_countries)

# Fix phrases GDELT would reject: one quoted word under 5 letters -> no quotes
fixed <- queries |>
  mutate(query = case_when(
    iso3 == "PSE" ~ '("Gaza Strip" OR "West Bank" OR "Palestinian" OR Gaza)',
    grepl('^"[^" ]{1,4}"$', query) ~ gsub('"', "", query),
    TRUE ~ query
  ))
if (nrow(new_countries) > 0 || !identical(fixed$query, queries$query)) {
  write_csv(fixed, queries_file)
  message("Updated ", queries_file)
}
queries <- fixed

# Only this month's crisis countries are fetched
run_list <- filter(queries, iso3 %in% countries$iso3)
message("Crisis countries this month: ", nrow(run_list))

# ---- 2. This month's drawer in the memory box ------------------------------
run_month <- format(Sys.Date(), "%Y-%m")
cache_dir <- file.path("data/gdelt_cache", run_month)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 3. Ask GDELT about one country -----------------------------------------
gdelt_volume <- function(query, iso3) {
  cache_file <- file.path(cache_dir, paste0(iso3, ".csv"))
  if (file.exists(cache_file)) return(invisible(TRUE))     # already have it
  
  for (attempt in 1:4) {
    resp <- tryCatch(
      request("https://api.gdeltproject.org/api/v2/doc/doc") |>
        req_url_query(
          query = query,
          mode = "timelinevolraw",
          startdatetime = format(Sys.Date() - 90, "%Y%m%d000000"),
          enddatetime   = format(Sys.Date(), "%Y%m%d000000"),
          format = "json"
        ) |>
        req_timeout(60) |>
        req_retry(max_tries = 3, backoff = ~ 60, retry_on_failure = TRUE) |>
        req_error(is_error = function(r) FALSE) |>
        req_perform(),
      error = function(e) { message("    ", iso3, ": connection failed"); NULL }
    )
    
    if (!is.null(resp)) {
      # Real data: save it and finish
      if (resp_status(resp) == 200 && grepl("json", resp_content_type(resp))) {
        body <- resp_body_json(resp, simplifyVector = TRUE)
        if (length(body$timeline) == 0) {
          message("    ", iso3, ": GDELT found no articles for this search phrase")
          break
        }
        d <- body$timeline$data[[1]]
        d$iso3 <- iso3
        write_csv(d, cache_file)
        Sys.sleep(30)
        return(invisible(TRUE))
      }
      
      # A text message instead of data: show it
      msg <- substr(gsub("\\s+", " ", resp_body_string(resp)), 1, 150)
      message("    ", iso3, " (attempt ", attempt, "): ", msg)
      if (!grepl("limit requests", msg, ignore.case = TRUE)) break   # a different problem: don't retry
    }
    
    Sys.sleep(90)   # GDELT said slow down, or the connection failed: wait, then retry
  }
  
  Sys.sleep(30)
  invisible(FALSE)
}

# ---- 4. Loop through this month's crisis countries --------------------------
for (i in seq_len(nrow(run_list))) {
  message(i, "/", nrow(run_list), "  ", run_list$country[i])
  gdelt_volume(run_list$query[i], run_list$iso3[i])
}

done          <- sub("\\.csv$", "", list.files(cache_dir, pattern = "\\.csv$"))
gdelt_missing <- setdiff(countries$iso3, done)
if (length(gdelt_missing) > 0) {
  message("Still missing ", length(gdelt_missing), " countries: ",
          paste(gdelt_missing, collapse = ", "),
          ". Run the script again later; it will only ask about these.")
}

# ---- 5. Combine every monthly drawer into one history ----------------------
files <- list.files("data/gdelt_cache", pattern = "\\.csv$",
                    recursive = TRUE, full.names = TRUE)
files <- files[grepl("/[0-9]{4}-[0-9]{2}/", files)]     # monthly drawers only

attention <- bind_rows(lapply(files, function(f) {
  read_csv(f, col_types = cols(date = col_character()), show_col_types = FALSE) |>
    mutate(run = basename(dirname(f)))
}))

attention_monthly <- attention |>
  filter(iso3 %in% countries$iso3) |>                   # this month's crisis countries only
  mutate(day = as.Date(substr(date, 1, 8), "%Y%m%d")) |>
  arrange(desc(run)) |>
  distinct(iso3, day, .keep_all = TRUE) |>              # if two drawers have the same day, keep the newest
  mutate(month = format(day, "%Y-%m")) |>
  group_by(iso3, month) |>
  summarise(articles      = sum(value),
            total         = sum(norm),
            share_per_10k = 10000 * articles / total,
            .groups = "drop") |>
  left_join(select(countries, iso3, country), by = "iso3")

write_csv(attention_monthly, "data/attention_monthly.csv")
message("Saved attention for ", n_distinct(attention_monthly$iso3), " of ",
        nrow(countries), " crisis countries.")