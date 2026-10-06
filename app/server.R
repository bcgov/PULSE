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

# ---------------------------------------------------------------------------
# Parameters of interest for this app.
# SGWL = static groundwater level, reported in metres below ground surface
#        (m bgs).
# Bvolt = station battery voltage.
# ---------------------------------------------------------------------------

app_parameters <- c("SGWL", "Bvolt")

# Colours reused across the approval figures
review_levels <- c(
  "Working" = "#fe3200",
  "In Review" = "#ffcc02",
  "Reviewed" = "#b4fec0",
  "Approved" = "#31a926"
)

fv_review_levels <- c(
  "Working" = "#fe0800",
  "In Review" = "#ffb702",
  "Reviewed" = "#bbfeb4",
  "Approved" = "#52a926"
)

# Placeholder used whenever a reactive resolves to zero rows
empty_plot <- function(msg = "Insufficient Data") {
  ggplot() +
    geom_text(aes(x = 1, y = 1, label = msg)) +
    theme_void()
}

server <- function(input, output, session) {
  site_name_rv <- reactive({
    input$LocationName
  })

  timeseries_type <- reactive({
    input$Region
  })

  timeseries_data <- reactive({
    ts_approval |>
      filter(Region == timeseries_type())
  })

  filtered_data <- reactive({
    current_reading |>
      filter(LocationName == site_name_rv()) |>
      filter(Parameter %in% app_parameters) |>
      select(c(
        "LocationName",
        "DateTime",
        "NumericValue1",
        "Parameter",
        "Label",
        "Unit",
        "week_difference",
        "diff_from_monthly_ave"
      ))
  })

  fv_data <- reactive({
    most_recent |>
      filter(LocationName == site_name_rv()) |>
      select(c(
        "LocationName",
        "Date",
        "Current_Date",
        "Most_Recent"
      ))
  })

  telemetry_data <- reactive({
    ts_telemetry |>
      filter(LocationName == site_name_rv()) |>
      filter(Parameter %in% app_parameters)
  })

  station_click <- reactiveVal()

  observeEvent(input$RegionSelect, {
    stations_in_region <- locations_regions %>% filter(Region == input$RegionSelect) %>%
      pull(LocationName)
    
     updateSelectInput(
       session, "LocationName",
       choices = stations_in_region,
       selected = stations_in_region[1]
       )
    
    # NOTE: the observed input and the input read inside must be the same
    # outputId. Change both to match whatever the map is called in ui.R.
    # station_click(input$map_marker_click$id)
    # shiny::updateNavbarPage(
    #   session,
    #   inputId = "Station Data",
    #   selected = "Site Name"
    # )
  })
  
  output$current_conditions_table <-
    render_gt({
      if (nrow(filtered_data()) > 0) {
        current_status <- filtered_data() |>
          ungroup() |>
          mutate(
            Parameter = recode(
              Parameter,
              "SGWL" = "Groundwater Level",
              "Bvolt" = "Battery Voltage"
            )
          ) |>
          select(c(
            Parameter,
            NumericValue1,
            Unit,
            week_difference,
            diff_from_monthly_ave
          )) |>
          gt() |>
          cols_label(
            Parameter = "Parameter",
            NumericValue1 = "Current Value",
            Unit = "Unit",
            week_difference = "Change from<br>7-days Ago",
            diff_from_monthly_ave = "Change from<br>30-day Average",
            .fn = md
          ) |>
          fmt_number(
            columns = matches("diff"),
            decimal = 2,
            force_sign = TRUE
          ) |>
          fmt_number(columns = matches("Value"), decimal = 2) |>
          cols_width(everything() ~ px(120)) |>
          cols_align(
            align = "left",
            columns = everything()
          ) |>
          tab_header(
            title = md(paste0(
              "**Site Telemetry Readings at ",
              site_name_rv(),
              "**"
            ))
          ) |>
          tab_source_note(
            source_note = md(paste("Data Retrieved on", date_to))
          ) |>
          tab_source_note(
            source_note = md(
              "Change columns = current value - previous value (7 days, monthly average)"
            )
          ) |>
          tab_source_note(
            source_note = md(
              "Groundwater level is reported in metres below ground surface (m bgs): a **positive** change means the water table has fallen."
            )
          )

        current_status
      } else {
        gt(tibble(Message = "Data not found."))
      }
    })

  output$wells_low_bvolt <-
    render_gt({
      threshold_values <- map_data |>
        ungroup() |>
        filter(NumericValue1 < 12.4) |>
        filter(Parameter %in% c("Bvolt")) |>
        filter(Label == "Telemetry") |>
        #filter(Parameter == "Bvolt") |>
        select(c(
          LocationName,
          Parameter,
          NumericValue1,
          monthly_mean,
          Unit,
          perc_diff
        )) |>
        arrange(desc(LocationName))

      if (nrow(threshold_values) == 0) {
        return(gt(tibble(
          Message = "No wells have a battery voltage less than 12.4V."
        )))
      }

      threshold_values |>
        gt() |>
        cols_label(
          Parameter = "Parameter",
          LocationName = "Location Name",
          NumericValue1 = "Current Value",
          monthly_mean = "30-day Average",
          Unit = "Unit",
          perc_diff = "Percent Difference from<br>30-day Average (%)",
          .fn = md
        ) |>
        fmt_number(columns = matches("diff"), decimal = 2, force_sign = TRUE) |>
        fmt_number(columns = matches("Value"), decimal = 2) |>
        fmt_number(columns = matches("mean"), decimal = 2) |>
        cols_align(
          align = "center",
          columns = everything()
        ) |>
        tab_header(
          title = md(
            "**Wells With Battery Voltage Less than 12.4V**"
          )
        ) |>
        tab_source_note(
          source_note = md(paste("Data Retrieved on", date_to))
        )
        
    })

  ##
  #new table
  output$well_visited_in_183_days <-
    render_gt({
      threshold_values <- map_data |>
        ungroup() |>
        filter(Most_Recent > 183) |>
        #filter(Parameter %in% c("SGWL")) |>
        #filter(Label == "Telemetry") |>
        #filter(Parameter == "Bvolt") |>
        select(c(
          LocationName,
          Most_Recent
        )) |> unique() |>
        arrange(desc(Most_Recent))
      
      if (nrow(threshold_values) == 0) {
        return(gt(tibble(
          Message = "All wells have been visited at least once in the last 183 days."
        )))
      }
      
      threshold_values |>
        gt() |>
        cols_label(
          #Parameter = "Parameter",
          LocationName = "Location Name",
          Most_Recent = "Days Since Last Field Visit",
          #NumericValue1 = "Current Value",
          #monthly_mean = "30-day Average",
          #Unit = "Unit",
          #perc_diff = "Percent Difference from<br>30-day Average (%)",
          .fn = md
        ) |>
        #fmt_number(columns = matches("diff"), decimal = 2, force_sign = TRUE) |>
        #fmt_number(columns = matches("Value"), decimal = 2) |>
        #fmt_number(columns = matches("mean"), decimal = 2) |>
        cols_align(
          align = "center",
          columns = everything()
        ) |>
        tab_header(
          title = md(
            "**Wells Not Visited in Over 183 Days**"
          )
        ) |>
        tab_source_note(
          source_note = md(paste("Data Retrieved on", date_to))
        )
    })
  
  ##
  
  output$battery <- renderText({
    battery_value <- filtered_data() |>
      ungroup() |>
      filter(Parameter == "Bvolt") |>
      pull(NumericValue1)

    if (length(battery_value) > 0 && !is.na(battery_value[1])) {
      paste0(round(battery_value[1], 2), " V")
    } else {
      "NA"
    }
  })

  output$sgwl <- renderText({
    sgwl_value <- filtered_data() |>
      ungroup() |>
      filter(Parameter == "SGWL") |>
      pull(NumericValue1)

    if (length(sgwl_value) > 0 && !is.na(sgwl_value[1])) {
      paste0(round(sgwl_value[1], 3), " m bgs")
    } else {
      "NA"
    }
  })

  output$fv <- renderText({
    last_visit <- fv_data() |>
      pull(Date)

    days_since <- fv_data() |>
      pull(Most_Recent)

    if (
      length(last_visit) > 0 && !is.na(last_visit[1]) && !is.na(days_since[1])
    ) {
      as.character(paste0(last_visit[1], " (", days_since[1], " days ago)"))
    } else {
      "NA"
    }
  })

  output$sgwl_plot <- renderPlotly({
    sgwl <- telemetry_data() |>
      filter(Parameter == "SGWL")

    if (nrow(sgwl) == 0) {
      return(ggplotly(empty_plot()))
    }

    sgwl <- sgwl |>
      mutate(line_type = "Groundwater Level") |>
      mutate(
        text = paste0(
          "<br>Date: ",
          Timestamp,
          "<br>Parameter: ",
          Parameter,
          "<br>Value: ",
          round(NumericValue1, 3),
          " m bgs",
          "<br>Location Name: ",
          LocationName
        )
      )

    hline_df <- data.frame(
      yintercept = unique(sgwl$monthly_mean),
      line_type = as.factor("30-day Average")
    )

    min_date <- as_datetime(min(sgwl$Date))
    max_date <- as_datetime(max(sgwl$Date))

    g <- ggplot(
      sgwl,
      aes(
        x = as_datetime(Timestamp),
        y = NumericValue1,
        col = Parameter,
        text = text,
        group = 1
      )
    ) +
      labs(subtitle = paste0("Data from: ", min_date, " to ", max_date)) +
      geom_line(aes(linetype = line_type), na.rm = FALSE) +
      geom_hline(
        data = hline_df,
        aes(yintercept = yintercept, linetype = line_type),
        color = "black"
      ) +
      scale_x_datetime(limits = c(min_date, max_date)) +
      # Reversed: depth below ground surface, so shallower water plots higher
      scale_y_reverse(expand = expansion(mult = 0.05)) +
      scale_color_manual(
        labels = c("Groundwater Level"),
        values = c("#3b74b8")
      ) +
      theme_minimal() +
      theme(
        text = element_text(colour = "black", size = 13),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(colour = "grey50"),
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, face = "plain", size = 11)
      ) +
      xlab("Date") +
      ylab("Hourly Mean (m below ground surface)") +
      scale_linetype_manual(
        values = c("Groundwater Level" = "solid", "30-day Average" = "dashed")
      ) +
      labs(linetype = "")

    ggplotly(g, tooltip = "text")
  })

  output$battery_plot <- renderPlotly({
    bvolt <- telemetry_data() |>
      filter(Parameter == "Bvolt")

    if (nrow(bvolt) == 0) {
      return(ggplotly(empty_plot()))
    }

    bvolt <- bvolt |>
      mutate(line_type = "Battery Voltage") |>
      mutate(
        text = paste0(
          "<br>Date: ",
          Timestamp,
          "<br>Parameter: ",
          Parameter,
          "<br>Value: ",
          round(NumericValue1, 2),
          " V",
          "<br>Location Name: ",
          LocationName
        )
      )

    hline_df <- data.frame(
      yintercept = unique(bvolt$monthly_mean),
      line_type = as.factor("30-day Average")
    )

    min_date <- as_datetime(min(bvolt$Date))
    max_date <- as_datetime(max(bvolt$Date))

    b <- ggplot(
      bvolt,
      aes(
        x = as_datetime(Timestamp),
        y = NumericValue1,
        col = Parameter,
        text = text,
        group = 1
      )
    ) +
      labs(subtitle = paste0("Data from: ", min_date, " to ", max_date)) +
      geom_line(aes(linetype = line_type), na.rm = FALSE) +
      geom_hline(
        data = hline_df,
        aes(yintercept = yintercept, linetype = line_type),
        color = "black"
      ) +
      scale_x_datetime(limits = c(min_date, max_date)) +
      scale_y_continuous(expand = expansion(mult = 0.05)) +
      scale_color_manual(
        labels = c("Battery Voltage"),
        values = c("#95d840ff")
      ) +
      theme_minimal() +
      theme(
        text = element_text(colour = "black", size = 13),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(colour = "grey50"),
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0, face = "plain", size = 11)
      ) +
      xlab("Date") +
      ylab("Hourly Mean (V)") +
      scale_linetype_manual(
        values = c("Battery Voltage" = "solid", "30-day Average" = "dashed")
      ) +
      labs(linetype = "")

    ggplotly(b, tooltip = "text")
  })

  # ---------------------------------------------------------------------
  # Approval status timeline.
  # Factored into one builder so the SGWL tab and the parameter-driven tab
  # share the same code instead of the three near-identical copies that
  # existed for Stage / Discharge / selected parameter.
  # ---------------------------------------------------------------------

  approval_timeline <- function(plot_data) {
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(ggplotly(empty_plot("No approval records for this parameter")))
    }

    plot_data <- plot_data |>
      mutate(Date = as.Date(Date)) |>
      arrange(LocationName) |>
      mutate(LocationName = forcats::fct_inorder(LocationName)) |>
      mutate(
        LevelDescription = fct_relevel(
          LevelDescription,
          "Approved",
          "Reviewed",
          "In Review",
          "Working"
        )
      ) |>
      mutate(
        text_ts = paste0(
          "Date: ",
          Date,
          "<br>Location Name: ",
          LocationName,
          "<br>Parameter: ",
          Parameter,
          "<br>Approval Status: ",
          LevelDescription
        )
      )

    plot_sites <- unique(plot_data$LocationName)

    fv_yr <- field_visit_year |>
      filter(LocationName %in% plot_sites) |>
      mutate(
        text_fv = paste0(
          "<b>FIELD VISIT </b>",
          "<br>Date: ",
          FieldVisitDate,
          "<br>Location Name: ",
          LocationName,
          "<br>Approval Status: ",
          LevelDescription
        )
      )

    p <- ggplot(
      plot_data,
      aes(
        x = Date,
        y = fct_rev(LocationName),
        color = LevelDescription,
        group = LocationName,
        text = text_ts
      )
    ) +
      geom_point(size = 7, na.rm = TRUE) +
      geom_point(
        data = fv_yr,
        shape = 21,
        color = "black",
        size = 3,
        aes(
          x = FieldVisitDate,
          y = fct_rev(LocationName),
          fill = LevelDescription,
          text = text_fv
        ),
        inherit.aes = FALSE,
        show.legend = TRUE
      ) +
      scale_color_manual(
        values = review_levels,
        name = "Approval Status"
      ) +
      scale_fill_manual(
        values = fv_review_levels,
        name = ""
      ) +
      xlab("Date") +
      ylab("") +
      labs(
        title = paste0(
          unique(plot_data$Parameter),
          " Timeseries Approval Status: Updated ",
          max(plot_data$Date)
        )
      ) +
      theme_minimal() +
      scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month") +
      theme(
        panel.grid.major = element_line(linewidth = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(linewidth = 0.5, colour = "grey85"),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(vjust = 1, hjust = 1, size = 10, angle = 45),
        axis.title.y = element_text(
          size = 10,
          margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")
        ),
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        legend.background = element_rect(colour = "white"),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA)
      )

    ggplotly(p, tooltip = "text")
  }

  output$approval_plot_sgwl <- renderPlotly({
    approval_timeline(ts_approval |> filter(Region == input$Region))
  })

  output$timeseries_approval_status <- renderPlotly({
    approval_timeline(timeseries_data())
  })

  output$map <- renderLeaflet({
    map_pts <- map_data |>
      select(LocationName, Parameter, LocationIdentifier, NumericValue1, 
             Date, Most_Recent, color, legend, Latitude, Longitude, 
             Elevation, legend_battery, batter_colour, DataSource) %>%
      mutate(
        legend = as.factor(legend),
        legend = fct_relevel(legend, "< 152", "153 - 183", "> 183"),
        
        legend_battery = as.factor(legend_battery),
        legend_battery = fct_relevel(legend_battery, ">12.4V", "12.0 - 12.4V", "<12.0V")
      ) |>
      rename(latitude = Latitude, longitude = Longitude)
    
    map_pts <- map_pts %>% filter(Parameter == 'Bvolt' | is.na(Parameter)) %>% rename(Bvolt = Parameter)
    
    ## start
    # ---- Colours -----------------------------------------------------------
    # Explicit level order so colours always match the legend labels
    # (a factor palette built from `domain` sorts levels alphabetically,
    #  which would scramble green/orange/red here)
    days_levels <- c("< 152", "153 - 183", "> 183")
    batt_levels <- c(">12.4V", "12.0 - 12.4V", "<12.0V")
    cols        <- c("#4fb06d", "#f07857", "#bf2c34")
    
    colorpal         <- colorFactor(cols, levels = days_levels)
    colorpal_battery <- colorFactor(cols, levels = batt_levels)
    
    # ---- Marker icons: circle = Telemetry, square = Non-Telemetry -----------
    make_icons <- function(fill, data_source) {
      size <- 18
      svg <- ifelse(
        data_source == "Telemetry",
        sprintf("<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>
               <circle cx='9' cy='9' r='6.5' fill='%s' stroke='black' stroke-width='3'/>
             </svg>", size, size, fill),
        sprintf("<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>
               <rect x='2.5' y='2.5' width='13' height='13' fill='%s' stroke='black' stroke-width='3'/>
             </svg>", size, size, fill)
      )
      urls <- paste0("data:image/svg+xml;charset=utf-8,",
                     vapply(svg, utils::URLencode, character(1), reserved = TRUE))
      icons(
        iconUrl     = unname(urls),
        iconWidth   = size, iconHeight  = size,
        iconAnchorX = size / 2, iconAnchorY = size / 2   # centre icon on the point
      )
    }
    
    # ---- Popups -------------------------------------------------------------
    popup_base <- function(d) {
      paste0(
        "<strong>", d$LocationName, ": ", d$LocationIdentifier, "</strong>",
        "<br>Elevation: ", d$Elevation, " m",
        "<br>Location Data Type: ", d$DataSource
      )
    }
    
    pts_tel <- map_pts %>% filter(DataSource == "Telemetry")
    
    pts_days_popup <- paste0(
      popup_base(map_pts),
      "<br>Days Since Last Visit: ", map_pts$Most_Recent,
      ifelse(is.na(map_pts$NumericValue1), "",
             paste0("<br>Battery Voltage: ", map_pts$NumericValue1))
    )
    
    pts_batt_popup <- paste0(
      popup_base(pts_tel),
      "<br>Battery Voltage: ", pts_tel$NumericValue1
    )
    
    # ---- Hover labels (HTML, so wrap in htmltools::HTML) -------------------
    labels_days <- lapply(
      paste0(
        "<strong>", map_pts$LocationName, "</strong>",
        "<br>Days Since Last Visit: ", map_pts$Most_Recent,
        ifelse(is.na(map_pts$NumericValue1), "",
               paste0("<br>Battery Voltage: ", map_pts$NumericValue1, " V"))
      ),
      htmltools::HTML
    )
    
    labels_batt <- lapply(
      paste0(
        "<strong>", pts_tel$LocationName, "</strong>",
        "<br>Battery Voltage: ", pts_tel$NumericValue1, " V"
      ),
      htmltools::HTML
    )
    
    # ---- Map ----------------------------------------------------------------
    leaflet() |>
      addTiles() |>
      setView(lat = 51.8, lng = -121.8, zoom = 6) |>
      
      # Layer 1: days since last field visit (all sites)
      addMarkers(
        data    = map_pts,
        lat     = ~latitude,
        lng     = ~longitude,
        layerId = ~LocationName,
        group   = "Most Recent Field Visit",
        icon    = make_icons(colorpal(map_pts$legend), map_pts$DataSource),
        label   = labels_days,
        popup   = pts_days_popup
      ) |>
      
      # Layer 2: battery voltage (telemetry sites only)
      addMarkers(
        data    = pts_tel,
        lat     = ~latitude,
        lng     = ~longitude,
        layerId = ~paste0(LocationName, "_battery"),
        group   = "Battery Voltage",
        icon    = make_icons(colorpal_battery(pts_tel$legend_battery), pts_tel$DataSource),
        label   = labels_batt,
        popup   = pts_batt_popup
      ) |>
      
      # Legends (tagged with CSS classes; shown/hidden by the JS at the end)
      addLegend(
        position = "topright",
        colors   = cols,
        labels   = days_levels,
        title    = "Days Since Last Visit",
        opacity  = 1,
        className = "info legend legend-days"
      ) |>
      addLegend(
        position = "topright",
        colors   = cols,
        labels   = batt_levels,
        title    = "Battery Voltage",
        opacity  = 1,
        className = "info legend legend-battery"
      ) |>
      
      addLayersControl(
        baseGroups = c("Most Recent Field Visit", "Battery Voltage"),  # note: baseGroups (plural)
        options    = layersControlOptions(collapsed = FALSE)
      ) |>
      
      # Shape key (always visible)
      addControl(
        html = "<div style='background:white;padding:6px 8px;border-radius:4px;'>
              <span style='display:inline-block;width:12px;height:12px;border:2px solid black;border-radius:50%;background:#ccc;vertical-align:middle;'></span> Telemetry<br>
              <span style='display:inline-block;width:12px;height:12px;border:2px solid black;background:#ccc;vertical-align:middle;'></span> Non-Telemetry
            </div>",
        position = "bottomright"
      ) |>
      
      # Show only the legend belonging to the selected base layer
      htmlwidgets::onRender("
    function(el, x) {
      var map = this;
      var legends = {
        'Most Recent Field Visit': '.legend-days',
        'Battery Voltage': '.legend-battery'
      };
      function showLegend(name) {
        Object.keys(legends).forEach(function(layer) {
          var lg = el.querySelector(legends[layer]);
          if (lg) lg.style.display = (layer === name) ? '' : 'none';
        });
      }
      showLegend('Most Recent Field Visit');            // initial state
      map.on('baselayerchange', function(e) { showLegend(e.name); });
    }
  ")
    
    ## fin
    
    # colorpal <- colorFactor(
    #   palette = c("#4fb06d", "#f07857", "#bf2c34"),
    #   domain = map_pts$legend
    # )
    # 
    # colorpal_battery <- colorFactor(
    #   palette = c("#4fb06d", "#f07857", "#bf2c34"),
    #   levels = c(">12.4V", "12.0 - 12.4V", "<12.0V")#map_pts$legend_battery
    # )
    # 
    # colorpal_datasource <- colorFactor(
    #   c("dodgerblue", "firebrick"), 
    #   domain = map_pts$DataSource)
    # 
    # #make separate columns for voltage and sgwl
    # #map_pts <- tidyr::pivot_wider(map_pts, names_from = Parameter, values_from = NumericValue1)
    # 
    # #Bryan didn't ask for sgwl so this works but drops all non-telemetry sites
    # map_pts <- map_pts %>% filter(Parameter == 'Bvolt' | is.na(Parameter)) %>% rename(Bvolt = Parameter)
    # 
    # leaflet() |>
    #   addTiles() |>
    #   setView(lat = 51.8, lng = -121.8, zoom = 6) |>
    #   
    #   #days from last field visit
    #   addCircleMarkers(
    #     data = map_pts,
    #     lat = ~latitude,
    #     lng = ~longitude,
    #     layerId = ~LocationName,
    #     color = "black",
    #     group = "Most Recent Field Visit",
    #     fillColor = ~ colorpal(legend),
    #     radius = 4,
    #     weight = 5,
    #     fillOpacity = 1,
    #     label = ~LocationName,
    #     popup = ~ paste(
    #       "<strong>",
    #       LocationName,
    #       ": ",
    #       LocationIdentifier,
    #       "</strong><br>Elevation: ",
    #       Elevation,
    #       " m",
    #       #"<br>Contact Name: ",
    #       #UploadedByUser,
    #       "<br>Days Since Last Visit: ",
    #       Most_Recent,
    #       "<br> Battery Voltage: ",
    #       NumericValue1,
    #       "<br>Location Data Type: ",
    #       DataSource
    #     ),
    #     labelOptions = labelOptions(
    #       noHide = FALSE,
    #       offset = c(0, -12),
    #       opacity = 1,
    #       textsize = "12px",
    #       textOnly = FALSE
    #     )
    #   ) |>
    #   
    #   #battery
    #   addCircleMarkers(
    #     data = map_pts %>% filter(DataSource == "Telemetry"),
    #     lat = ~latitude,
    #     lng = ~longitude,
    #     layerId = ~paste0(LocationName, "_battery"),
    #     color = "black",
    #     group = "Battery Voltage",
    #     fillColor = ~ colorpal_battery(legend_battery),
    #     radius = 4,
    #     weight = 5,
    #     fillOpacity = 1,
    #     label = ~LocationName,
    #     popup = ~ paste(
    #       "<strong>",
    #       LocationName,
    #       ": ",
    #       LocationIdentifier,
    #       "</strong><br>Elevation: ",
    #       Elevation,
    #       " m",
    #       #"<br>Contact Name: ",
    #       #UploadedByUser,
    #       
    #       "<br> Battery Voltage: ",
    #       NumericValue1,
    #       "<br>Location Data Type: ",
    #       DataSource
    #     ),
    #     labelOptions = labelOptions(
    #       noHide = FALSE,
    #       offset = c(0, -12),
    #       opacity = 1,
    #       textsize = "12px",
    #       textOnly = FALSE
    #     )
    #   ) |>
    #   
    #   #location type
    # addCircleMarkers(
    #   data = map_pts,
    #   lat = ~latitude,
    #   lng = ~longitude,
    #   layerId = ~paste0(LocationName, "_type"),
    #   color = "black",
    #   group = "Location Type",
    #   fillColor = ~ colorpal_datasource(DataSource),
    #   radius = 4,
    #   weight = 5,
    #   fillOpacity = 1,
    #   label = ~LocationName,
    #   popup = ~ paste(
    #   "<strong>",
    #   LocationName,
    #   ": ",
    #   LocationIdentifier,
    #   "</strong><br>Elevation: ",
    #   Elevation,
    #   " m",
    #   #"<br>Contact Name: ",
    #   #UploadedByUser,
    #   "<br>Days Since Last Visit: ",
    #   Most_Recent,
    #   "<br>Location Data Type: ",
    #   DataSource
    # ),
    # labelOptions = labelOptions(
    #   noHide = FALSE,
    #   offset = c(0, -12),
    #   opacity = 1,
    #   textsize = "12px",
    #   textOnly = FALSE
    # )
    # ) |> 
    #   ##
    #   
    #   ##
    #   addLegend(
    #     data = map_pts,
    #     position = "topright",
    #     pal = colorpal, values = ~legend,
    #     #values = ~legend,
    #     title = "Days Since Last Visit"
    #   ) |>
    #   #battery legend
    #   addLegend(
    #     data = map_pts %>% filter(DataSource == "Telemetry"),
    #     position = "topright",
    #     pal = colorpal_battery, values = ~legend_battery,
    #     #values = ~legend,
    #     title = "Battery Voltage"
    #   ) |>
    #   addLayersControl(
    #     baseGroup = c("Most Recent Field Visit", "Battery Voltage", 
    #                   "Location Type"),
    #     options = layersControlOptions(collapsed = FALSE)
    #   )
  })
}
