# ---------------------------------------------------------------
# update_dashboard.R - run this whenever you want to update the
# live dashboard:  source("update_dashboard.R")
#
# BEFORE RUNNING:
#  - Close Excel
#  - Download a fresh "fts_requirements_funding_global.csv" from HDX
#    and save it in the data folder, replacing the old one
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

# 1b. News attention (slow, about 30-40 minutes)
source("scripts/01_gdelt.R")
if (length(gdelt_missing) > 0) {
  stop("GDELT is still missing ", length(gdelt_missing), " countries. ",
       "Wait an hour, then run source(\"update_dashboard.R\") again - ",
       "it will only fetch the missing ones.")
}

# 1c. Funding
source("scripts/02_fts.R")

# 2. Bake the dashboard (needs embed-resources: true in dashboard.qmd)
quarto::quarto_render("dashboard.qmd")

# 3. Put it in the shop window
dir.create("docs", showWarnings = FALSE)
ok <- file.copy("index.html", "docs/index.html", overwrite = TRUE)
if (!ok) stop("Could not copy index.html into docs/. Did the render work?")

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