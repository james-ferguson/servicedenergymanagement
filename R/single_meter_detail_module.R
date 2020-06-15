


#' @export
single_meter_detail_UI <- function(id){
  ns <- NS(id)
      box(
        title = "Meter Commentary",
        width = 12,
        htmlOutput(ns("meter_detail")),
        solidHeader = TRUE, background = "teal"
      )

}

single_meter_detail_server <- function(id){
  moduleServer(id, function(input, output, session) {

    meter <- reactive(req(session$userData$selected_meter()))


    meter_detail <- reactiveVal()

    viaURL <- reactiveVal(FALSE)

    observeEvent(session$clientData$url_search, {
      search_pairs <- urltools::param_get(urls = session$clientData$url_search,  parameter_names = c("mid", "mpr", "context"))
      md <- NULL
      if(!is.na(search_pairs$mid)){
        md <- read_meter_detail_by_mid(search_pairs$mid) # More efficient / discrete than by mpr
      } else if(!is.na(search_pairs$mpr)){
        md <- read_meter_detail_by_mpr(search_pairs$mpr)
      }
      req(md)
      if(nrow(md)>0 & !is.na(md$mid)){
        viaURL(TRUE)
        meter_detail(md)
      }
    })

    observeEvent(session$userData$selected_meter(),{
      if(!viaURL())
        meter_detail(session$userData$selected_meter())
    })

    mid <- reactive({
      validate(need(meter_detail(), message= "Output requires a meter"))
      md <- meter_detail()
      md$mid
    })

    output$mpr <- renderText({
      validate(need(meter_detail(), message= "Output requires a meter"))
      md <- meter_detail()
      paste(md$mpr, md$owner)

    })

    output$meter_detail <- renderUI({

      md <- req(meter_detail())
      tagList(
        h2(paste(req(session$userData$utility()), "Meter:", md$mpr, md$owner, paste0("[ref:",req(session$userData$oid()),"]"))),
        hr(),
        p(paste("Intermediary:" , md$intermediary, paste0("[ref:",md$intermediary_id,"]"))),

        h3("Data Quality"),
        p(paste("Meter State:", md$activity)),
        p(paste("Number of Days Readings:", md$n, "of which", md$nnz, "were positive")),
        p(paste("Consumption data last received: ", md$power_to)),
        p(paste(md$recent_zero, "Recent Zeroes: ")),

        h3("Performance Trends"),

        p(paste("Most Recent: ", md$latest_duration, "days", if_else(md$latest_trend > 0, "over budget by", "under budget by"),
                round(md$latest_trend * md$latest_duration / 1000,3), "kWh starting ", (as.Date(md$power_to) -md$latest_duration  ))),
        p(paste("Immediately Prior: ", md$prior_duration, "days", if_else(md$prior_trend > 0, "over budget by", "under budget by"),
                round(md$prior_trend * md$prior_duration / 1000,3), "kWh" )),
        p(paste("Major Long Term: ", md$big_duration, "days", if_else(md$big_trend > 0, "over budget by", "under budget by"),
                round(md$big_trend * md$big_duration / 1000,3), "kWh " )),


        h3("Weather Response Statistics"),
        p(paste("Consumption during Summer: ", round(md$summer_quartile/10,1 ), "%")),
        p(paste("Consumption during Winter: ", round(md$winter_quartile/10,1 ), "%")),
        p(paste("Consumption during shoulder periods: ", round(100 - ((md$winter_quartile+md$summer_quartile) /10),1 ), "%")),
        p(paste("The Weather response has a ",
                if_else(abs(md$spearman) > 750,
                        "strong",
                        if_else(abs(md$spearman) < 250,
                                "slight",
                                "moderate")
                        , ),
                "general ", if_else(md$spearman > 0, "cooling", "heating"), "pattern")),


        h3("Observations"),
        p(paste("Mean Absolute Deviation (M.A.D.ness) index of ", md$madness,
                if_else(abs(md$madness) < 150,
                        "is low and excellent, suggesting clear control policyies are sustained",
                        if_else(abs(md$madness) < 250, "is moderate suggesting reasonable control and adequate data quality",
                                "is very poor suggesting either poor data quality, highly erratic control or varying modes of operation.")
                )
        )
        ),
        p(paste("The Weather response is generally: ", if_else(md$convexity < 0, "convex with respect to weather  - it should be concave", "concave with respect to weather - as it should be"))),

        hr(),
        tags$i(paste("kWIQly internal meter ref :", md$mid))
      )
    })
  })
}
