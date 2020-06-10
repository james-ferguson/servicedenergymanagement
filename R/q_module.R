
q_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'Q',
    h2("Question")

  )

}

q_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
  })
}
