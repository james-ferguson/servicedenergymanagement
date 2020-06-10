

event_choice_UI <- function(id){
  ns <- NS(id)
  selectInput(
    ns("owner_events_choice"),
    "Please Choose Event to Display",
    choices = c(),
    width = '450px'
  )
}

event_choice_server <- function(id, owner, utility){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {

    chosen_event <- reactiveVal()

    oid <- reactive({
      req(owner)
      owner <- req(owner())
      chosen_event(NULL)
      owner$oid
    })

    # Event Choice Module ####

    owner_events <- reactive(read_owner_events_by_owner(req(oid())))

    observeEvent(owner_events(), {
      oe <- req(owner_events())
      oec = list(oe$owner_event)
      names(oec) <- paste(oe$name, "From", oe$start_date, "~",ifelse(is.na(oe$end_date), "Ongoing", oe$end_date) )
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
      event <- req(chosen_event())

      owner_utility_event_meter_days <- filter_meter_events(session$userData$owner_utility_meter_days(), event)
      displayable_consolidation <- session$userData$owner_consolidation() %>% filter_history_by_period("2") %>%
        mutate(dow = factor(weekdays(ts), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
      event_consolidation <- displayable_consolidation %>% filter_meter_events( event)


      list(
        owner = owner,
        utility = utility,
        event = event,
        owner_utility_event_meter_days = owner_utility_event_meter_days,
        displayable_consolidation = displayable_consolidation,
        event_consolidation = event_consolidation,
        meter_events_summaries = summarise_meter_days(owner_utility_event_meter_days)
      )
    })

    event_data

  })


}
