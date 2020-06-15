
qa_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'QA',
    h2("Quality Assurance - meters with a data quality question outstanding"),
    table_meter_selection_ui(ns('meters'))
  )

}

qa_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    meter <- table_meter_selection_server('meters', df)

    observeEvent(meter(), print(meter()))
  })
}
