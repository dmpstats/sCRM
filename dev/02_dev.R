# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "thinkr" )
usethis::use_package("bs4Dash")
usethis::use_package("shinipsum")
usethis::use_package("markdown")
usethis::use_package("shinyWidgets")
usethis::use_package("fresh")
usethis::use_package("shinydashboard")
usethis::use_package("shinydashboardPlus")
usethis::use_package("shinyjs")
usethis::use_package("htmltools")
usethis::use_package("bsplus")
usethis::use_package("ggplot2")
usethis::use_package("msm")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("purrr")
usethis::use_package("tibble")
usethis::use_package("rhandsontable")
usethis::use_package("rlang")
usethis::use_package("truncnorm")
usethis::use_package("tools")
usethis::use_package("vroom")
usethis::use_package("shinyvalidate")
usethis::use_package("shinyFeedback")
usethis::use_package("MetBrewer")
usethis::use_package("ggridges")
usethis::use_package("zeallot")
usethis::use_package("ggdist")
usethis::use_package("distributional")
usethis::use_package("patchwork")
usethis::use_package("reactable")
usethis::use_package("reactablefmtr")
usethis::use_package("waiter")
usethis::use_package("sever")
usethis::use_package("flextable")
usethis::use_package("fs")
usethis::use_package("writexl")
usethis::use_package("zip")
usethis::use_package("officer")
usethis::use_package("rmarkdown")

usethis::use_dev_package("stochLAB", remote = "HiDef-Aerial-Surveying/stochLAB")
usethis::use_dev_package("officedown", remote = "davidgohel/officedown")

## Environment reproducibility
#renv::init()
renv::snapshot(type = "explicit")
renv::install()
renv::update()


## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "pnl_wf" ) 
golem::add_module( name = "prob_inputs" )
golem::add_module( name = "prob_inputs_row" )
golem::add_module( name = "trbn_oper" )
golem::add_module( name = "spp_in_wf" )
golem::add_module( name = "fhd_inputs" )
golem::add_module( name = "bird_dens" )
golem::add_module( name = "pnl_spp" )
golem::add_module( name = "monthly_hotab" )
golem::add_module( name = "input_completion" )
golem::add_module( name = "pnl_sim" )
golem::add_module( name = "sim_options" )
golem::add_module( name = "sim_outputs" )
golem::add_module( name = "output_spec" )
golem::add_module( name = "spp_results" )


## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "remove_tbp" )
golem::add_fct( "add_scenarios_btns" )
golem::add_fct( "wf_feat_inputs" )
golem::add_fct( "trbn_cfg_inputs" ) 
golem::add_fct( "input_plots" )
golem::add_fct( "input_qtiles" ) 
golem::add_fct( "hacks" ) 
golem::add_fct( "add_spp_btns" )
golem::add_fct( "drop_from_sltize" )
golem::add_fct( "input_val_rules" )
golem::add_fct( "info_dropdown" )
golem::add_fct( "theme_sCRM" )
golem::add_fct( "intro_guide" )


golem::add_fct("processing", module = "fhd_inputs")
golem::add_fct("processing", module = "bird_dens")
golem::add_fct("plot_collisions", module = "spp_results")
golem::add_fct("plot_collisions", module = "spp_results")
golem::add_fct("summ_collisions", module = "spp_results")
golem::add_fct("export_inputs", module = "sim_outputs")
golem::add_fct("export_outputs", module = "sim_outputs")


golem::add_utils("helpers", module = "sim_options")



golem::add_utils( "helpers" )
golem::add_utils("inline_inputs")




## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "lst_btn_clicked" )
golem::add_js_file( "return_click" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )
golem::add_js_file( "shinyjs_funcs" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("scrm")
usethis::use_vignette("scrm_dev")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
## 
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action() 
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
usethis::use_github_action_check_full() 
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis() 
usethis::use_travis_badge() 

# AppVeyor 
usethis::use_appveyor() 
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")


