
dashtab <- {

  box_collapse_code <- "shinyjs.collapse = function(boxid) { $('#' + boxid).closest('.box').find('[data-widget=collapse]').click(); } "
  tabItem(
    tabName = "dashboard",
    useShinyjs(),
    extendShinyjs(text = box_collapse_code, functions = c("collapse")),
    box(
      id = "owner_selection_box",
      title = tagList(icon("user-secret",p("Client Selection"))),
      width = 12,
      utility_UI("u"),
      single_row_selector_ui("owner_table", height = 625),
      status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE),
    account_period_UI("ap"),
    box(
      tags$head(tags$style(".rightAlign{float:right;}")),
      title = "AMR Situation",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      # stats_box_UI("sb"),
      #transition_stats_UI("sb"),
      status = "success", collapsed = FALSE
    )

  )

}

datatab <- {tabItem(
  tabName = "data",
  h2("Meter Statistics"),
 # events::single_row_selector_ui("stats"),
  verbatimTextOutput("user_info", placeholder = TRUE),
  verbatimTextOutput("credential_info", placeholder = TRUE),
  verbatimTextOutput("userdata")
)}

settingstab <- {tabItem(
  tabName = "SETS",
  h2("Settings"),
  sliderInput("ncd", "Non. Comm. Threshold:", 1, 31, 7),
  h3("Budget"),

  # h3("Events"),
  # textInput("event_name", "Name of Event (max 60 chars)", value = "", placeholder = "E.g. 2021 Covid19 Pandemic"),
  # checkboxInput("open_ended_event", "Is Event Open Ended (Currently)", value = TRUE),
  # dateRangeInput("event_range", "Range of One-Off Event Study"),
  h3("Modelling"),
  numericInput("max_filter", "Treat kW per meter exceeding this value as Noise:", value = 1250, min = 250, max = 5000, step = 250),
  h3("Temperature"),
  checkboxInput("degrees_c_f", "Use Celsius (Unchecked = Fahrenheit)", value = TRUE),
  actionButton("save_settings", "Save / Update Changes"),
  account_period_UI2('a')
)}



body <- {

  dashboardBody(
    tags$head(
      tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),

      tags$head(tags$style(HTML('
         .skin-blue .left-side, .skin-blue .wrapper {
                        background-color: #ecf0f5;
                        }
         '))
      )
    ),

    useShinyjs(),  #start as hidden
    div(
      id = "panelA",
      #shinyjs::hidden(div(id="single_meter_dispay", single_meter_diagnostics_UI("tm"))),
      shinyjs::hidden(div(id="smd_panel",single_meter_display_UI('smd'))),

      tabItems(
        dashtab,
        workflow_UI('wf'),
        nd_UI('nd'),
        np_UI('np'),
        nc_UI('nc'),
        pf_UI('pf'),
        nl_UI('nl'),
        nw_UI('nw'),
        w_UI('w'),
        s_UI('s'),
        id_UI('id'),
        q_UI('q'),
        qa_UI('qa'),
        owner_events_UI('events'),
        kpi_UI('kpi'),
        wins_UI('wins'),
        settingstab
      )),
   # shinyjs::hidden(div(id="panelB",  account_period_charts_UI("ap"))),
   # shinyjs::hidden(div(id="panelC", single_meter_diagnostics_UI("tm")))

  )
}
ui <- dashboardPagePlus(

  header = dashboardHeaderPlus(
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 50px}"),
            tags$style(".main-header .logo {height: 50px}"),
            tags$style(".popover{width:200px}"),
            fixedPanel(top = 10,  right= 35,
                       actionButton('plotBtn', label='',  style="opacity: .80;", icon = icon("chart-area"))), # color: #fff; background-color: #a662e3; border-color: #a153e5",
            bsPopover("plotBtn", title = "Display Charts", content= "Rotate through: <br> Main display,<br>Owner portfolio charts,<br> Selected meter charts", placement = "bottom", trigger = "hover"),
            enable_rightsidebar = TRUE,
            rightSidebarIcon = "gears"
    ),

    title ="kWIQly ~ Powerful, Intelligence fast",titleWidth = 600 ), # Use image in title#,  tags$img(src='logo.svg',  width="25%", align="left", alt =

  sidebar = dashboardSidebar(
    # shiny.info::info_panel(
    #   shiny.info::inspect_btn_ui(),
    #   shiny.info::info_value("test_info_value"),
    #   shiny.info::info_value("session_info_value"),
    #   shiny.info::inspect_btn_ui(),
    shiny.info::busy(position = "bottom right"),
   # width = 220,
   # tags$style(".left-side, .main-sidebar {padding-top: 75px}"),
    sidebarMenu(
      id = "tabs",
      menuItem( "Budget, Forecast & Actual", tabName = "dashboard", icon = icon("dashboard") ),
     # 1. ENERGY PERFORMANCE COEFFICIENT: this is the RATIO of actual to expected consumption. A value of one signifies as-expected behaviour; greater than one suggests adverse performance, and less than one implies improvement. It is therefore potentially useful as a performance indicator in its own right but is better used as a way of adjusting other more traditional performance indicators to remove the distortions caused by the weather, changes in prevailing production levels, etc.. It would normally be used on a weekly cycle or something of that order.
     # 2. PERFORMANCE DEFICIT: this is the DIFFERENCE between actual consumption and what it would have been if the building, process or vehicle had operated at 'yardstick' efficiency (however you choose to set that). Typically the performance deficit is worked out on an annual basis but it can be done at any interval; its purpose is to rank opportunities for improvement when there are a lot to choose from, and for that purpose the deficits are usually converted into cash value. It is superior to ranking by percentage variation because it takes unit price and the scale of consumption into account
     # 3. PARAMETRIC BENCHMARKING: this is a method rather than a numerical indicator. It is an approach to comparing buildings, processes and even vehicles, in which the fixed and variable components of consumption are compared separately. Notably if you have a manufacturing process whose consumption has a straight-line relationship with one driving factor, the gradient of the line is a measure of process efficiency and can be compared with that achieved by comparable installations regardless of their scale. The intercepts may not lend themselves to comparison. On the other hand with delivery vehicles you can separately compare the gradients (which correspond to vehicle fuel economy) and intercepts (which might tell you about driver behaviour in terms of idling).
    hr(),
      menuItem("Meter Workflow",
               sidebarSearchForm(textId = "mpr_search", buttonId = "mpr_search_action" , label = "Meter Search...", icon = shiny::icon("search")),
               p('Overview'),
               menuSubItem('Workflow', tabName = 'WF', icon = shiny::icon('eye'), selected = FALSE),
               p('AMR Service Info.'),
               menuSubItem('No AMR data', tabName = 'ND', icon = shiny::icon(icon_name_for_state('ND')), selected = FALSE),
               menuSubItem('No positive readings', tabName = 'NP', icon = shiny::icon(icon_name_for_state('NP')), selected = FALSE),
               menuSubItem('Non. comms.', tabName = 'NC', icon = shiny::icon(icon_name_for_state('NC')), selected = FALSE),
               menuSubItem('Flat-lining', tabName = 'PF',icon = shiny::icon(icon_name_for_state('PF')), selected = FALSE),
               p('Weather Service Info.'),
               menuSubItem('Location not known', tabName = 'NL', icon = shiny::icon(icon_name_for_state('NL')), selected = FALSE),
               menuSubItem('No weather data', tabName = 'NW', icon = shiny::icon(icon_name_for_state('NW')), selected = FALSE),
               p('Energy Performance'),
               menuSubItem('Excessive', tabName = 'W', icon = shiny::icon(icon_name_for_state('W')), selected = FALSE),
               menuSubItem('Improved', tabName = 'S', icon = shiny::icon(icon_name_for_state('S')), selected = FALSE),
               menuSubItem('On Target', tabName = 'NT', icon = shiny::icon(icon_name_for_state('NT')), selected = FALSE),
               menuSubItem('Learning...', tabName = 'ID', icon = shiny::icon(icon_name_for_state('ID')), selected = FALSE),
               p('Other'),
               menuSubItem('Question', tabName = 'Q', icon = shiny::icon(icon_name_for_state('Q')), selected = FALSE),
               menuSubItem('Quality', tabName = 'QA', icon = shiny::icon('gem'), selected = FALSE),
               icon = icon("eye")),
     hr(),
     menuItem("Comparative KPI",   tabName = 'KPI', icon = icon("award")),
     menuItem("Events",   tabName = 'EVENTS', icon = icon("globe")),
     menuItem("Success",   tabName = 'WINS', icon = icon("glass-martini-alt")),
     hr(),
     menuItem("Settings",   tabName = 'SETS', icon = icon("gear"))
    )
  ),
  # rightsidebar = rightSidebar(
  #   background = "dark",
  #   rightSidebarTabContent(
  #     id = 1,
  #     title = "Tab 1",
  #     icon = "desktop",
  #     active = TRUE,
  #     sliderInput(
  #       "obs",
  #       "Number of observations:",
  #       min = 0, max = 1000, value = 500
  #     )
  #   ),
  #   rightSidebarTabContent(
  #     id = 2,
  #     title = "Tab 2",
  #     textInput("caption", "Caption", "Data Summary")
  #   ),
  #   rightSidebarTabContent(
  #     id = 3,
  #     icon = "paint-brush",
  #     title = "Tab 3",
  #     numericInput("obs", "Observations:", 10, min = 1, max = 100)
  #   ),
  #   title = "Right Sidebar"
  # ),
  body
)
