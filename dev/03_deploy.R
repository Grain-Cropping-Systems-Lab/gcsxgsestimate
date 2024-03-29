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

# build check
devtools::build(path = "")

gcsxgsestimate::run_app(DATABASE_URL = "",
                        MAPS_API_KEY = "")

# Deploy

## Docker ----
golem::add_dockerfile_heroku()
#golem::add_dockerfile_with_renv_heroku(output_dir = "deploy")

