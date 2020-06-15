
server <- function(session, input, output) {

  filter <- dplyr::filter

  session$allowReconnect(TRUE) # unsure about this
  shiny.info::inspect_btn_server(input)
  output$session_info <- shiny.info::render_session_info()
  shiny.info::toggle_info("Ctrl+Shift+K")

  # Selected Owner from User Permissions and selected utility (in future from URL)-
  # These must be session unique so are handled as session globals  session$userData$oid  and $ownername

  utility_server("u")
  permitted <- permissions_server("permissions")

  oid <- owner_server("owner_table", permitted)

  #  Session Global ####

  # Returns Sytem Meter and Statistics
  # As side effect creates
  #session$userData$owner_utility_meter_days
  #session$userData$owner_consolidation

  # Monitors Session Selected meter
  # session$userData$selected_meter
  # And eactracts history from consolidation
  # session$userData$selected_meter_history

  meters <- data_server('ds')

  # Derive Financial Data from owner_consolidation, filering, period and pricing - ie for Budget Forecast Actuals
  account_period_server("ap")
  workflow_server('wf')
  owner_events_server('events')
  single_meter_display_server('smd')

  # User state overrides ####
  deferral_meters <- reactive(req(oid()) %>% owner_deferrals %>%  filter(unless < Sys.Date())) # Determine whether to temporarily hide a problem  - if in this list and until < Sys.date
  owner_user_states <- reactive(req(oid()) %>% owner_meter_user_states) # Determine whether to override state (until cleared)

  # can be changed with
  # upsert_owner_deferral(oid, mid, until)
  # upsert_meter_user_states(oid, mid, user_state)

  trend_meter_data <- reactive({
    req(meters()) %>%
      select(mid, mpr, update_status, update_status_prior, update_status_changed,
             latest_trend, latest_duration, prior_trend, prior_duration, prior_duration, big_duration,
             n, nnz, recent_zero, power_to)
  })

  kpi_meter_data <- reactive({
    req(meters()) %>%
      select(mid, mpr, update_status, update_status_prior, update_status_changed,
             summer_quartile, winter_quartile, convexity, spearman, madness,
             n, nnz, recent_zero, power_to)
  })

  meter_meta_data <- reactive({
    req(meters()) %>%
      select(mid, mpr, update_status, update_status_prior, update_status_changed,
             poi_id, longitude, latitude, oid, owner, utility, intermediary_id,
             n, nnz, recent_zero, power_to)
  })

  w_server('w', reactive(req(trend_meter_data()) %>% filter(update_status == 'W') %>% arrange(desc(latest_duration), desc(latest_trend))))
  s_server('s', reactive(req(trend_meter_data()) %>% filter(update_status == 'S') %>% arrange(desc(latest_duration), latest_trend)))
  nt_server('nt', reactive(req(trend_meter_data()) %>% filter(update_status == 'NT') %>% arrange(desc(latest_duration), latest_trend)))
  id_server('id', reactive(req(trend_meter_data()) %>% filter(update_status == 'ID') %>% arrange(desc(latest_duration), latest_trend)))


  mdc <- function(sus){
    reactive(req(meter_meta_data()) %>% dplyr::filter(update_status == sus))
  }

  nl_server('nl', mdc('NL'))
  nw_server('nw', mdc('NW'))
  nd_server('nd', mdc('ND'))
  np_server('np', mdc('NP'))
  nc_server('nc', mdc('NC'))
  pf_server('pf', mdc('PF'))
  q_server('q', mdc('QA'))
  qa_server('qa', trend_meter_data)
  kpi_server('kpi', kpi_meter_data)
  wins_server('wins', meters)


  # Tasks ####
  # task_server("tasks")
  #
  # observeEvent(input$js.button_clicked, {
  #   print(input$js.button_clicked)
  #   uid = stringr::str_split(input$js.button_clicked, "-")
  #   name_space = uid[[1]][3] # Assumes namespace "tasks-status_board-
  #   uid = stringr::str_split(name_space, "#")
  #
  #   if(length(uid)>1){
  #     verb = uid[[1]][2]
  #     subject_count = stringr::str_split(uid[[1]][3], "_")
  #
  #     if(verb=="type"){
  #       w <- req(session$userData$work())
  #       session$userData$work_selection(w[w$type==subject_count[[1]][1],])
  #     } else{
  #       print(paste(verb, "subject" , subject_count[[1]][1]))
  #     }
  #   }   else(print(uid[[1]][1]))
  # })


  # # Stats Boxes ####
  #
  # sb_server("sb")
  #
  # # Selected Meter ####
  #
  # observeEvent(  session$userData$selected_meter(), {
  #   x <-  session$userData$selected_meter()
  #   print(paste(x$mid, x$mpr))
  # })
  #
  # output$user_info <- renderPrint({
  #   session$user
  # })
  #
  # # selected_meter_Server("sms")
  #
  # # probably dont need this too non_secifyc
  #
  # stats_selected_meter <- events::single_row_selector_server("stats", meters)
  # observeEvent(stats_selected_meter(),   session$userData$selected_meter(stats_selected_meter()))
  #
  # # Can we bury this ####
  #
  #
  # observeEvent({
  #   event_data("plotly_click", source = "meter_selection_chart")}, {
  #     meters <- req(meters())
  #     ed <- event_data("plotly_click", source = "meter_selection_chart")
  #     session$userData$selected_meter(metersf[meters$mid == ed$key[1], ])
  #   })



}
