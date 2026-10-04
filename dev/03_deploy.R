# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::build()

## Docker ----
## NOTE: the commentary in this section is AI-generated.
## docker/ is HAND-MAINTAINED and built from the repo root (context = repo
## root). Do NOT re-run golem::add_dockerfile_with_renv() without re-applying
## the customisations afterwards: it overwrites Dockerfile / Dockerfile_base,
## re-creates docker/renv.lock (drift source) and rebuilds the tarball into
## docker/. It also needs {dockerfiler}, which is not pinned in renv.lock.
# golem::add_dockerfile_with_renv(
#   lockfile = "renv.lock",
#   output_dir = "docker",
#   port = 3838,
#   from = "ghcr.io/rocker-org/verse:4.5.2"
# )

## If you want to deploy to ShinyProxy
# golem::add_dockerfile_with_renv_shinyproxy()

## Posit ----
## If you want to deploy on Posit related platforms
golem::add_positconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Deploy to Posit Connect or ShinyApps.io ----

## Add/update manifest file (optional; for Git backed deployment on Posit )
rsconnect::writeManifest()

## In command line.
rsconnect::deployApp(
  appName = desc::desc_get_field("Package"),
  appTitle = desc::desc_get_field("Package"),
  appFiles = c(
    # Add any additional files unique to your app here.
    "R/",
    "inst/",
    "data/",
    "NAMESPACE",
    "DESCRIPTION",
    "app.R"
  ),
  appId = rsconnect::deployments(".")$appID,
  lint = FALSE,
  forceUpdate = TRUE
)
