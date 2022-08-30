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
devtools::build(path = "C:/Users/Taylor/Desktop")

gcsxgsestimate::run_app(DATABASE_URL = "postgres://u6b45hj5hg8vmd:pa502eeb5448c25379665d7961334d0019d28f1b413f189acf29c8978808a799f@ec2-44-209-196-14.compute-1.amazonaws.com:5432/d48k64vr72n4m0",
                        MAPS_API_KEY = "AIzaSyDprIIkJiGAGD54c9IjmL78MANoUryH2dc")

# Deploy

## Docker ----
golem::add_dockerfile_heroku()
#golem::add_dockerfile_with_renv_heroku(output_dir = "deploy")

