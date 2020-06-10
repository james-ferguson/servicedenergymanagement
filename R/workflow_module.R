workflow_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'WF',
    h2("Workflow"),

    p("Put Search in here")

  )

}

workflow_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
  })
}
