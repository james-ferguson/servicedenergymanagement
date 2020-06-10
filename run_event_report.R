library(servicedenergymanagement)
library(magrittr)
library(tidyverse)
library(DBI)
library(pool)
library(config)
library(knitr)
library(kableExtra)
library(lubridate)
library(DT)
library(plotly)

run_generic_event_report <- function(owner, start_date, end_date = NA, name, utility, intermediary, dir, pool){

  file <- paste(dir, "index.html", sep="/")

  tempReport <- file.path(tempdir(), "temp.Rmd")
  src_file <- system.file("generic_event_report.Rmd", package = "servicedenergymanagement", lib.loc = NULL, mustWork = TRUE)
  file.copy(src_file, tempReport, overwrite = TRUE)

  report_name <- paste0( name, ", from ", start_date, ifelse(is.na(end_date), " ongoing...", paste0(" ~ " , end_date) ))

  event = tibble(owner_id = owner$id, start_date = start_date, end_date = end_date, name = report_name)

  params = list(
    owner = owner,
    event = event,
    utility = utility,
    intermediary = intermediary,
    title = paste(report_name, "<br>impact report for", owner$owner),
    subtitle = paste("by kWIQly on behalf of ", intermediary)
  )

  out <- rmarkdown::render(
    tempReport,
    output_format = rmarkdown::html_document(),
    output_file = file, params = params, envir = new.env(parent = globalenv())
  )

  file.rename(out, file)

  file

}


owner <- dbq("SELECT o.* FROM v1.owner o WHERE id = ?oid;", oid = 211)
dir = tempdir()
out <- run_generic_event_report(owner, start_date = '2020-03-23', end_date = NA, name='Covid-19', utility = 'Gas', intermediary = 'TEC', dir, pool)
pool::poolClose(pool)

browseURL(out, browser = getOption("browser"),  encodeIfNeeded = FALSE)

library(plotly)
pool <- get_pool()
df <- dbq("SELECT mid, dm.ts, dm.power FROM v1.poi_meters pm, v1.poi p, prod.meter_day_48 dm WHERE pm.poi_id = p.id AND p.owner_id = ?oid AND dm.id = mid AND dm.ts > '2020-02-01';", oid = 211)

last_reads <- df %>% group_by(mid) %>% summarise(df = max(ts))


p <- chart(df) %>% add_markers(x=~ts, y=~power, color =~mid)
