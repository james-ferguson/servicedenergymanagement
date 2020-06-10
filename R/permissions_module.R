
user_can_access <- function(email, pool){
  if(is.null(email))
    email <- "james@kwiqly.com"

  owners <- dbq("SELECT o.id as oid, o.owner, o.match_ref FROM v1.owner o;")

  permissions <- dbq("SELECT * FROM v1.user WHERE TRIM(email) = ?email" , email = email)

  if (any(permissions$is_super_user))
    return(owners)
  owners[owners$oid %in% permissions$owner_id,]
}



permissions_server <- function(id){

  moduleServer(id, function(input, output, session) {

    permitted <- reactive({

      permissions <- user_can_access(email = session$user, pool)

      if(nrow(permissions) == 0)
        showModal(modalDialog(title = "Unknown User", p("As we do not recogise your authorisation level we will put you into demo mode."),
                              p("If you are a subscriber when this happens - please contact support."),
                              p("If you just looking and maybe interested please contact us.")
        ))

      if(nrow(permissions) == 1)
        owner(permissions)

      print(paste("permitting", session$user))

      print(permissions[1,])

      permissions

    })

    permitted
  })

}
