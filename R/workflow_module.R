
#' @export
workflow_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'WF',
    h2("Workflow"),

    p("Put Search in here"),
    p ("put filters in here"),
    p("put flows in here"),
    p("meter counts in here")
  )

}


#' @export
workflow_server <- function(id){
  moduleServer(id, function(input, output, session) {
  })
}
