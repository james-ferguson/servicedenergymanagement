

event_creator_UI <- function(id){
  ns <- NS(id)
  fluidRow(
      column(width = 12, textInput(ns('e_name'), label = "Event Name", value = "", width = '100%', placeholder = 'Brief title with no mention of dates')),
      column(width = 4, checkboxInput(ns('model'), "Event represents 'new - normal'", value = TRUE)),
      column(width = 4, checkboxInput(ns('use_end'), "Event is ongoing", value = TRUE)),
      column(width = 4, checkboxInput(ns('permanent'), "Remember Event", value = FALSE)),
      column(width = 6, dateRangeInput(ns('date_range'), 'Event period (yyyy-mm-dd) end ignored if ongoing', min =Sys.Date()- 1000, start = Sys.Date()- 28, max = Sys.Date(), end=Sys.Date(), weekstart = 1)),
      column(width = 4,offset = 2, p(strong('Create')), actionButton(ns('define_event'), label ='Define New Event', icon = icon('calendar-plus'))
    )
  )
}

event_creator_server <- function(id, owner, utility){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {

    chosen_event <- reactiveVal()

    oid <- reactive({
      req(owner)
      owner <- req(owner())
      owner$oid
    })

    ##### requirements
    #  integer NOT NULL, # Known
    # start_date date, # Define
    # end_date date,# Define or NA
    # name text,# Define
    # owner_event integer NOT NULL, # Calculate
    # expected real,
    # actual real,
    # meter_count integer,
    # model boolean NOT NULL DEFAULT false,  # Define
    # Event Choice Module ####

    owner_events <- reactive(read_owner_events_by_owner(req(oid())))

    observeEvent(owner_events(), {
      oe <- req(owner_events())
      oec = list(oe$owner_event)
      names(oec) <- paste(oe$name, "From", oe$start_date, "~",ifelse(is.na(oe$end_date), "Ongoing", oe$end_date) )
      updateSelectInput(session, inputId ="owner_events_choice", choices = oec)
    },ignoreNULL = TRUE)


    observeEvent(input$permanent,{
      if(input$permanent)
        showModal(modalDialog(title = "Not Yet", p("We havent set up event persistence yet! - For now you get an unsaved event based on new-normal" ), easyClose = TRUE))
    })

    observeEvent(input$model,{
      if(!input$model)
        showModal(modalDialog(title = "Not Yet", p("We can't yet distinguish treatment yet! - For now you get an unsaved event based on new-normal"), easyClose = TRUE))
    })

    event_data <- reactiveVal()

    observeEvent(input$define_event,{

      end <- ifelse(input$use_end,  input$date_range[2], NA)

      event_data(
        tibble::tibble(
          owner_id = req(oid()),
          name = input$e_name,
          start_date = input$date_range[1],
          end_date = end,
          model = input$model
        )
      )

    })

    event_data

  })


}
