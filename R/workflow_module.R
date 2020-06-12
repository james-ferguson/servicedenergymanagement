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

workflow_server <- function(id){ # o oid owner match_ref
  moduleServer(id, function(input, output, session) {
  })
}

# Infrastructure

# "permissions_module.R"
# "chart_utils.R"
# "manipulations.R"
# "utility_module.R"
# "meter_work_module.R"
# "single_row_selector_module.R"
# "sql.R"
# "pool.R"
# "package.R"



# Common Functions - Show Location
# View
# Data Quality
# Rank Priority
# Cancellation
# Defer

# User States

## Triage

### Rejected - Data
### Rejected - Low Impact
### Rejected - Defer
### Rejected - Explain
### Promote

## Accept

## Progress

## Done


# State specific

## AMR Validate / Inform / Complain

# "nc_module.R"
# "nd_module.R"
# "np_module.R
# "pf_module.R"

## kWIQly Validate / Service

# "nw_module.R"
# "q_module.R"

## Client Action

# "nl_module.R"
# "s_module.R"
# "w_module.R"
# "nt_module.R"

## Just Wait

# "id_module.R"

## Quality Service

# "qa_module.R"

# Other

# "event_choice_module.R"
# "owner_events_module.R"
# "wins_module.R"
# "workflow_module.R"
# "kpi_module.R"
# "account_period_module.R"

