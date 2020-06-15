
w_UI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = 'W',
    h2("Excessive"),
    helpText("Instructions: @TODO (Does not work yet).
             Select meter from table or 'Triage' colomn below (These are ranked in priority order and can be filtered / printed) -  On selection graphs showing performance will appear."),
    helpText("Decide to : "),
    helpText("- Defer Action - It will disappear from this and other low priority listings for the period selected (default one week)"),
    helpText("- Flag as poor data - It will be transferred to the 'Quality' work flow"),
    helpText("- Adopt for 'Tracking/Action' - It will appear in the second 'Action' and continue to be ranked by default in rate of waste order"),
    helpText("- Decide waste is too trivial to be of concern'"),
    helpText("- Decide waste is due to abnormal circumtances'"),
    helpText("  A - A multi-meter event - see owner events tab"),
    helpText("  B - Explicable event for this meter (eg plant - refurbishment) - in this case decide if:"),
    helpText("      i. - The explanation represent a 'new-normal' - In which case it should affect the threshold for future alarms, and the alarm sensitivity will be reduced during a 'learning-phase'"),
    helpText("      ii. - The explanation represent an exception - to be rectified or that is expected to cease without intervention - in this case it will remain in the 'Tracking / Action' column until the system detects improvement"),
    helpText("Meters placed or automatically postioned in tracking / action will be observed until change occurs (a reveral of the waste of saving pattern) - Therafter they will be archived into the 'Complete' column until a new action state is defined for the meter. Successes will compete in the 'Success' section for particpation in optional case studies."),
    table_meter_selection_ui(ns('meters'))
  )
  }

w_server <- function(id, df){
  moduleServer(id, function(input, output, session) {
    meter <- table_meter_selection_server('meters', df)

    observeEvent(meter(), print(meter()))
  })
}
