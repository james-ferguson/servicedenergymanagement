
server <- function(session, input, output) {

  session$allowReconnect(TRUE) # unsure about this
  shiny.info::inspect_btn_server(input)
  output$session_info_value <- shiny.info::render_session_info()
  shiny.info::toggle_info("Ctrl+Shift+K")

  utility <- utility_server("u")
  permitted <- permissions_server("permissions")
  owner <- single_row_selector_server("owner_table", permitted)
  observeEvent(owner(), js$collapse("owner_selection_box") )



  ap <- account_period_server("ap", owner, utility)

  data_server('ds', owner, utility)

  workflow_server('wf')
  owner_events_server('events', owner, utility, ap)

  single_meter_display_server('smd')

  nd_server('nd')
  np_server('np')
  nc_server('nc')
  pf_server('pf')
  nl_server('nl')
  nw_server('nw')
  w_server('w')#
  s_server('s')
  nt_server('s')
  id_server('id')
  q_server('q')
  qa_server('qa')
  kpi_server('kpi')
  wins_server('wins')


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
  #     meters <- req(session$userData$all())
  #     ed <- event_data("plotly_click", source = "meter_selection_chart")
  #     session$userData$selected_meter(metersf[meters$mid == ed$key[1], ])
  #   })



}
