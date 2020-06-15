

event_choice_UI <- function(id){
  ns <- NS(id)
  selectInput(
    ns("owner_events_choice"),
    "Please Choose Event to Display",
    choices = c(),
    width = '450px'
  )
}

event_choice_server <- function(id){
  moduleServer(id, function(input, output, session) {

    chosen_event <- reactiveVal()

    owner_events <- reactive({
      req(session$userData$oid)
      read_owner_events_by_owner(req(session$userData$oid()))
    })

    observeEvent(owner_events(), {
      oe <- req(owner_events())
      oec = list(oe$owner_event)
      names(oec) <- paste(oe$name, ":", oe$start_date, "~",ifelse(is.na(oe$end_date), "Ongoing", oe$end_date) )
      updateSelectInput(session, inputId ="owner_events_choice", choices = oec)
    },ignoreNULL = TRUE)



    observeEvent({
      owner_events()
      req(input$owner_events_choice)}
      ,{
      oe <- req(owner_events())
      chosen_event(oe[oe$owner_event == input$owner_events_choice,])
    })

    # ####

    event_data <- reactive({
      event = req(chosen_event())
      utility = req(session$userData$utility())
      owner_utility_event_meter_days = filter_meter_events(session$userData$owner_utility_meter_days(), event)
      displayable_consolidation = session$userData$owner_consolidation() %>% filter_history_by_period("2") %>%
        mutate(dow = factor(weekdays(ts), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
      event_consolidation = displayable_consolidation %>% filter_meter_events( event)

      meter_events_summaries = summarise_meter_days(owner_utility_event_meter_days)
      oat_e_plot=selected_period_oat_chart(displayable_consolidation, event_consolidation)
      a_vs_e_plot = meter_events_summaries  %>% a_vs_e_chart(s =  session$userData$selected_meter())
      ae_dist_plot = meter_events_summaries  %>% ae_dist_chart()
      owner_utility_error_history_plot = event_consolidation %>% high_low_forecast_errors() %>% maybe_show_event(event)
      owner_utility_cusum_plot = event_consolidation %>% cumulative_waste_impact_chart() %>% maybe_show_event(event)
      owner_utility_history_plot = event_consolidation %>% event_impact_chart() %>% maybe_show_event(event)
      owner_utility_history_plot_in_context= displayable_consolidation %>% event_impact_chart() %>% maybe_show_event(event)
      owner_name <- session$userData$owner_name()
      e <- list(
        owner_name,
        utility,
        event,
        owner_utility_event_meter_days,
        displayable_consolidation,
        event_consolidation,
        meter_events_summaries,
        a_vs_e_plot,
        ae_dist_plot,
        owner_utility_error_history_plot,
        owner_utility_cusum_plot,
        owner_utility_history_plot,
        owner_utility_history_plot_in_context,
        oat_e_plot
      )

      names(e) <- c(
        "owner_name",
        "utility",
        "event",
        "owner_utility_event_meter_days",
        "displayable_consolidation",
        "event_consolidation",
        "meter_events_summaries",
        "a_vs_e_plot",
        "ae_dist_plot",
        "owner_utility_error_history_plot",
        "owner_utility_cusum_plot",
        "owner_utility_history_plot",
        "owner_utility_history_plot_in_context",
        "oat_e_plot"
      )
      e
    })

    event_data

  })


}
