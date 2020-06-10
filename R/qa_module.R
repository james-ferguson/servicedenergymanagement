
qa_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'QA',
    h2("Quality Assurance")

  )

}

qa_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
  })
}
