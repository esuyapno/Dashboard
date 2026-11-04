# ---------------------------------------------------------------
# update_dashboard.R - run this whenever you want to update the
# live dashboard:  source("update_dashboard.R")
#
# BEFORE RUNNING:
#  - Close Excel
#  - Download a fresh "fts_requirements_funding_global.csv" from HDX
#    and save it in the data folder, replacing the old one
#  - Once a year: download the latest ND-GAIN country index and
#    unzip it into data/ndgain/, replacing the old files
# ---------------------------------------------------------------

# 0. Check the FTS file is fresh BEFORE the long GDELT step
fts_file <- "data/fts_requirements_funding_global.csv"
fts_age  <- as.numeric(Sys.Date() - as.Date(file.mtime(fts_file)))
if (fts_age > 7) {
  stop("The FTS file is ", fts_age, " days old. Download a fresh copy from HDX, ",
       "save it as ", fts_file, ", then run this again.")
}

# 1a. Severity scores + list of crisis countries (must be first)
source("scripts/03_inform.R")

# 1b. Sector indicators (World Bank), climate (ND-GAIN) and humanitarian sector data (HDX HAPI) - quick
source("scripts/04_structural.R")
source("scripts/05_climate.R")
source("scripts/06_hapi.R")
source("scripts/07_inform_risk.R")   # needs the INFORM Risk Excel file in data/ (updated yearly)
source("scripts/08_data_tables.R")   # the CSV/Excel downloads (docs/data/) and the tables for the Data page

# 1c. News attention (slow, about 30-40 minutes)
source("scripts/01_gdelt.R")
if (length(gdelt_missing) > 0) {
  stop("GDELT is still missing ", length(gdelt_missing), " countries. ",
       "Wait an hour, then run source(\"update_dashboard.R\") again - ",
       "it will only fetch the missing ones.")
}

# 1d. Funding
source("scripts/02_fts.R")

# 2. Bake the dashboard into index.html, and the Data page into data.html
quarto::quarto_render("dashboard.qmd")
quarto::quarto_render("data.qmd")

# 3. Put it in the shop window
dir.create("docs", showWarnings = FALSE)
ok <- file.copy(c("index.html", "data.html"), "docs/", overwrite = TRUE)
if (!all(ok)) stop("Could not copy index.html and data.html into docs/. Did both renders work?")

# GitHub Pages settings: only needed if the dashboard is published with GitHub Pages
writeLines("dashboard.sa-analytical.com", "docs/CNAME")
file.create("docs/.nojekyll")

# 4. Send everything to GitHub
gert::git_add(".")
if (nrow(subset(gert::git_status(), staged)) > 0) {
  gert::git_commit(paste("Update dashboard", Sys.Date()))
  gert::git_push()
  message("Sent! The live dashboard updates in 1-2 minutes.")
} else {
  message("Nothing has changed, so nothing was sent.")
}
