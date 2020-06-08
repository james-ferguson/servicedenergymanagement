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

run_event_report <- function(params, o){

  dir <- paste(getwd(),
               paste("output/Event_Report_v1", gsub("-", "_", Sys.Date()), sep="_"),
               paste0("oid_", o$id),
               sep="/")
  dir.create(dir,recursive = TRUE)

  file <- paste(dir, "index.html", sep="/")

  tempReport <- file.path(tempdir(), "event_report.Rmd")

  src_file <- paste(getwd(),"R/event_report.Rmd", sep = "/")

  if(!file.exists(src_file)){
    stop("file path problem")
  }
  file.copy(src_file, tempReport, overwrite = TRUE)

  out <- rmarkdown::render(
    tempReport,
    output_format = html_document(),
    output_file = file, params = params, envir = new.env(parent = globalenv())
  )

  file.rename(out, file)

  file

}


pool <- get_pool()
oid = 211
owner <- q("SELECT o.* FROM v1.owner o WHERE id = ?oid;", oid = oid)
owner_events <-  q("SELECT oe.* FROM v1.owner_event oe WHERE oe.owner_id = ?oid ORDER BY owner_event DESC;", oid = oid)

owner_event_num = 1L # Temporary

e <- owner_events[owner_events$owner_event==owner_event_num,]
if(is.na(e$end_date))
  e$end_date=Sys.Date()

owner = owners[owners$id  == oid,],

params = list(
  owner = owner,
  event = e,
  utility = 'Gas'
interlaken


prepare_content_params <- function(o, e, meter_events, ou_consolidation, ou_time_selected, ob){
  e$name = 'Covid-19 2020'
  e$credit = owner$intermediary
  list(
    event_start = e$start_date,
    event_end = e$end_date,
    set_title =  ,
    owner_name = e$owner_name,
    ae_dist_chart = ae_dist_chart(meter_events),
    a_vs_e_chart = a_vs_e_chart(meter_events, NULL),
    meter_events = meter_events,
    intermediary_mention = paste("by kWIQly on behalf of ",owner$intermediary) ,
    ouehp = ouehp(ou_time_selected) %>% maybe_show_event(e),
    ouhp = ouhp(ou_consolidation, ou_time_selected) %>% maybe_show_event(e),
    oucp = oucp(ou_time_selected) %>% maybe_show_event(e)
  )
}


for(i in seq_along(owner_data$id)){
    o = owner_data[i,]

    print(o)

    owner_event_num = 1L

    owner_events <- read_owner_events_by_owner(o$id, pool)

    e <- owner_events[owner_events$owner_event==owner_event_num,]

    owner_name <- o$owner
    event_name <- paste(e$name, "From", e$start_date, "~",ifelse(is.na(e$end_date), "Ongoing", e$end_date) )
    meter_events <- read_meter_events_by_owner_event(o$id, owner_event_num,  pool) %>%
      mutate(turndown = round(100 * actual / expected,1))

    ou_consolidation <- read_owner_utility_consolidation(o$id, 'Gas', pool)

    ou_time_selected <- ou_consolidation %>% filter_history_by_period("2")

    params <- prepare_content_params(o, e, meter_events, ou_consolidation, ou_time_selected, ob)

    owner_data$rel_file[i] <- run_event_report(params,o)

}
