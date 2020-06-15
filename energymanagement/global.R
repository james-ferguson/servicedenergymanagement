library(shiny)
library(sortable)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(DBI)
library(pool)
library(config)
library(plotly)
library(lubridate)
library(Rcpp)
library(dplyr)
library(tidyr)
library(crosstalk)
library(DT)
library(leaflet)
library(knitr)
library(kableExtra)
library(leaflet.extras)
devtools::install_github("james-ferguson/servicedenergymanagement")
library(servicedenergymanagement)

sourcerer <- function(){
  #wd <- getwd()
  # setwd(paste0(getwd(), "/R"))
  files.sources = list.files("~/servicedenergymanagement/R", pattern = "\\.R$") #path = "~/vp/R
  #  print(files.sources)
  sapply(paste0("~/servicedenergymanagement/R/",files.sources), source)
  #setwd(wd)
}
#sourcerer()

pool <- get_pool()
onStop(function() { poolClose(pool) })

options("lubridate.week.start" = 1)

moduleServer <- function(id, module) {
  shiny::callModule(module, id)
}



icon_name_for_state <- function(update_status){

  switch (update_status,

          'ND' = "ban",
          'NP' = "battery-empty",
          'NL' = "map-signs",
          'NC' = "phone-slash",
          'PF' = "heartbeat",
          'W' = "coins",
          'NT' = "balance-scale",
          'NW' = 'bolt',
          'ID' ='user-clock',
          'S' = 'piggy-bank',
          "question"
  )

}


