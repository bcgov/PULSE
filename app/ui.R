# Copyright 2026 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#tags$head(tags$link(rel = "stylesheet", href = "bcgov.css"))

# ---------------------------------------------------------------------------
# Station choices are built from the data rather than hard-coded, so the
# dropdown always matches whatever location IDs the ETL last pulled.
# Requires site_name_lookup (or most_recent) to be loaded in global.R.
# ---------------------------------------------------------------------------

#station_choices <- sort(unique(site_name_lookup$LocationName))

station_choices <- unique(current_reading %>%
                                 filter(!is.na(Label)) %>%
                                 select(LocationName))

station_choices_manual <- unique(current_reading %>%
                            filter(is.na(Label)) %>%
                            select(LocationName))

vbs <- list(
  value_box(
    title = "Date of Last Field Visit",
    value = p(textOutput("fv"), style = "font-size: 70%"),
    showcase = bs_icon("clipboard2-data-fill"),
    theme = "primary",
    max_height = "100px"
  ),
  value_box(
    title = "Current Groundwater Level",
    value = p(textOutput("sgwl"), style = "font-size: 70%"),
    showcase = bs_icon("moisture"),
    theme = "primary",
    max_height = "100px"
  ),
  value_box(
    title = "Battery Voltage",
    value = p(textOutput("battery"), style = "font-size: 70%"),
    showcase = bs_icon("battery-half"),
    theme = "primary",
    max_height = "100px"
  )
)

ts_graph <- navset_card_tab(
  height = "400px",
  full_screen = TRUE,
  title = "Telemetry Timeseries",
  nav_panel(title = "Groundwater Level", plotlyOutput("sgwl_plot")),
  nav_panel(title = "Battery Voltage", plotlyOutput("battery_plot"))
)

current_conditions_table <- card(
  min_height = 400,
  full_screen = TRUE,
  gt_output("current_conditions_table")
)

ui <- page_navbar(
  header = tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "bcgov.css"
  ),

  title = tags$div(
    class = "bcgov-navbar-title",
    tags$img(
      src = "gov3_bc_logo.png",
      class = "bcgov-logo",
      alt = "Government of British Columbia"
    ),
    tags$div(
      class = "bcgov-title-text",
      tags$div("PULSE Report", class = "bcgov-title"),
      tags$div(
        "PGOWN Upkeep, Logistics & Status Evaluator",
        class = "bcgov-subtitle"
      )
    )
  ),

  nav_spacer(),

  nav_panel(
    "Overview Map",
    layout_column_wrap(
      width = 1,
      heights_equal = "row",
      
      card(
        full_screen = TRUE,
        card_body(leafletOutput("map", height = "60vh"))
         ),
      
      #tables nested side by side
      
      layout_column_wrap(
        width = 1/2,
        
        card(
          full_screen = TRUE,
          gt_output("wells_low_bvolt")
          ),
        card(
          full_screen = TRUE,
          gt_output("well_visited_in_183_days")
          ),
      )
    )
  ),

  nav_panel(
    "Station Data",
    layout_sidebar(
      sidebar = sidebar(
        title = "Station Region and Name",
        open = TRUE,
        selectizeInput(
          inputId = "RegionSelect",
          label = "Well Region",
          choices = unique(regions$Region)
        ),
        selectizeInput(
          inputId = "LocationName",
          label = "Site Name",
          choices = NULL #filled in by the server based on region
        )
      ),
      layout_column_wrap(
        width = 1 / 3,
        heights_equal = "row",
        !!!vbs
      ),  
      layout_column_wrap(
        width = 1,
        ts_graph
      ),
      layout_column_wrap(
        width = 1,
        current_conditions_table
      )
    )
  ),

  nav_panel(
    "Approval Status",
    layout_sidebar(
      sidebar = sidebar(
        title = "Field Visit and Data Approvals",
        open = TRUE,
        selectizeInput(
          inputId = "Region",
          label = "Region",
          choices = unique(regions$Region),
          selected = "South Coast"
        )
      ),
      card(plotlyOutput("timeseries_approval_status"))
    )
  ),

  nav_spacer(),
  nav_spacer(),

  footer = tags$footer(
    class = "bcgov-footer",
    tags$div(
      class = "container-fluid",
      tags$div(
        "© Province of British Columbia",
        style = "font-weight: 200;"
      )
    )
  )
)
