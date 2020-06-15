utility_server <- function(id){
  moduleServer(id, function(input, output, session) {
    #  Session Global ####
    session$userData$utility = reactive(input$utility)
  })
}

utility_UI <- function(id){
  ns <- NS(id)
  radioButtons(
    ns("utility"),
    inline = TRUE,
    label="Utility:",
    choices = c("Gas", "Electricity", "Water", "Heat"),
    selected = "Gas",
    width = "100%"
  )
}
