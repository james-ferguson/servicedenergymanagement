


work_list <- function(universe){

  filt <- function(universe, type, case, aka, status_colour, help){

    meters = dplyr::filter(universe, update_status == type)

    n = nrow(meters)

    pc = round(100 * n / nrow(universe), 1)

    icon_name = icon_name_for_state(type)

    tibble::tibble_row (type = type, case = case, meters = list(meters), aka = aka , n = n, percent = pc , status_colour = status_colour, icon_name=icon_name, help = help)
  }

  my_keys <- bind_rows(

    # Service
    #1 Other / Mystery
    filt(universe, "ND", case = 'service',"No Readings",  "danger",  "No AMR data received."),
    filt(universe, "NP", case = 'service',"No Energy",    "danger",  "No Consumption"),

    # User Maintain
    filt(universe, "NL", case = 'user',   "Lost", "danger",  "Location allows us to provide weather services."),
    filt(universe, "NC", case = 'service',"Non Comm.",     "warning",  "Meter recently silent"),
    filt(universe, "PF", case = 'user',   "Possibly Flat", "primary", 'Suspicious Zeroes received recently, can be an AMR faiure'), #7

    #  User Imporve
    filt(universe, "W", case =  'user',    "Overspend", 'danger',  'Excess energy consumption'), # 8
    filt(universe, "NT", case = 'user',    "No Trend", "primary",'Neutral trend - with enhancements possible'), # 9

    #  Self-correct
    filt(universe, "NW", case = 'kWIQly', "No Weather",  "warning",   "Weather source failure."), # 6
    filt(universe, "ID", case = 'user',   "Learning",    "primary",  'System is learning with insufficient data so far.'), #5
    filt(universe, "S", case =  'user',    "Underspend", 'success', 'Reduced energy consumption'), #10

  )

  others <- universe[!universe$update_status %in% my_keys$type,]

  bind_rows(
    tibble::tibble_row (type = "O", case = 'kWIQly',
                        meters =list(others),
                        aka = "Unclassified",
                        n = nrow(others), percent = round(100 * n / nrow(universe), 1), status_colour = 'danger',iconName = icon_name_for_state("O"), help = "Meters not automatically recognised"),
    my_keys
  )
}


data_server <- function(id, owner, utility){

  moduleServer(
    id,
    function(input, output, session) {

      #  Session Global ####
      session$userData$all = reactiveVal()
      session$userData$selected_meter <- reactiveVal()
      session$userData$owner_utility_meter_days <- reactiveVal()
      session$userData$owner_consolidation <- reactiveVal()
      session$userData$selected_meter_history <- reactiveVal()

      # Owner Utility Specific Meters

      observeEvent({
        utility()
        owner()
      },{
        utility <- req(utility())
        o = req(owner())
        meters <- meter_statistics(o$oid, utility)
        session$userData$all(meters)
        session$userData$selected_meter(NULL)
        md <- meter_days_for_owner_utility(o$oid, utility)
        session$userData$owner_utility_meter_days(md)
        session$userData$owner_consolidation(md %>%
          consolidate_owner_utility_meter_days() %>%
            mutate(dow = factor(weekdays(ts), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))))

      })

      observeEvent({
        session$userData$selected_meter()
        session$userData$owner_utility_meter_days()
      },
      {
        meter <- session$userData$selected_meter()



        if(!is.null(meter)){
          data <- req(session$userData$owner_utility_meter_days())

          mh <- data[data$mid == meter$mid,]
          df <- NULL
          if(nrow(mh)>0)
             df <- mutate(mh, dow = factor(weekdays(ts), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                   waste = kw - expected )

          session$userData$selected_meter_history(df)
              shinyjs::show(id= "smd_panel", asis = TRUE)
        } else{
          session$userData$selected_meter_history(NULL)
        }
      })




    }
  )
}
