

meter_table <- function(data){
  DT::datatable(
    data,
    options = list( dom = "Bftipr",
                    buttons = c(I("colvis"), I("copy"), I("csv"), I("excel"), I("pdf")),
                    autoWidth = TRUE,
                    pageLength = 12
    ),
    class = "display",
    #callback = JS("return table;"),
    rownames = FALSE,
    filter = "top",
    escape = TRUE,
    style = 'default',
    width = NULL,
    height = NULL,
    elementId = NULL,
    fillContainer = getOption("DT.fillContainer", NULL),
    autoHideNavigation = getOption("DT.autoHideNavigation", TRUE),
    selection = "single",
    extensions = list(),
    plugins = NULL,
    editable = FALSE
  )
}

#' @export
table_meter_selection_ui <- function(id, width = "100%", height = "auto"){
  ns <- NS(id)
  DT::dataTableOutput(ns("table"), width = "100%", height = height)
}

#' @export
table_meter_selection_server <- function(id, selection_update_status){



  moduleServer(id, function(input, output, session) {

    meters <- reactive({
      all <- req(session$userData$all())

      switch (selection_update_status,
        'W' = all %>% dplyr::filter(update_status == selection_update_status) %>%
          select(mid, mpr, update_status, update_status_prior, update_status_changed,
                 latest_trend, latest_duration, prior_trend, prior_duration, prior_duration, big_duration,
                 n, nnz, recent_zero, power_to) %>%
          arrange(desc(latest_duration), desc(latest_trend)),
        'S' = all %>% dplyr::filter(update_status == selection_update_status) %>%
          select(mid, mpr, update_status, update_status_prior, update_status_changed,
                 latest_trend, latest_duration, prior_trend, prior_duration, prior_duration, big_duration,
                 n, nnz, recent_zero, power_to) %>%
          arrange(desc(latest_duration), latest_trend),
        'NL' = all %>% dplyr::filter(update_status == selection_update_status) %>%
          select(mid, mpr, update_status, update_status_prior, update_status_changed,
                 latest_trend, latest_duration, prior_trend, prior_duration, prior_duration, big_duration,
                 n, nnz, recent_zero, power_to) %>%
          arrange(desc(latest_duration), latest_trend),
         all
      )
      # [1] "mid"                   "mpr"                   "activity"              "update_status"         "update_status_prior"   "update_status_changed"
      # [7] "n"                     "nnz"                   "recent_zero"           "power_to"              "summer_quartile"       "winter_quartile"
      # [13] "convexity"             "spearman"              "madness"               "latest_trend"          "latest_duration"       "prior_trend"
      # [19] "prior_duration"        "prior_duration"             "big_duration"          "poi_id"                "longitude"             "latitude"
      # [25] "oid"                   "owner"                 "utility"               "intermediary_id"
     # col Selection and sort order


    })

    df_table <- reactive({
      m <- req(meters())
      meter_table(m)
    })


    output$table <-  DT::renderDataTable(req(df_table()))

    observeEvent(input$table_rows_selected,{
      df <- meters()
      m <- df[input$table_rows_selected,]
      session$userData$selected_meter(m)
    })

    meters

  })
}
