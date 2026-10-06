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

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
#library(envreportutils)
library(leaflet)
library(gt)
library(bsicons)
library(bslib)
library(lubridate)
library(readr)
library(forcats)

#source("00_setup.R")

# Load data
ts_telemetry <- read_csv('data/ts_telemetry.csv')
current_reading <- read_csv('data/current_reading.csv') |>
  mutate(across(where(is.numeric), ~ round(., digits = 2)))
most_recent <- readRDS("data/most_recent_visit_weekly.rds")
field_visit_all <- readRDS('data/field_visits.rds')
map_data <- read_csv('data/map_data.csv')
ts_approval <- readRDS("data/timeseries_approval.rds")
field_visit_year <- readRDS("data/field_visit_year.rds")
site_name_lookup <- readRDS("data/site_name_lookup.rds") #added

#join region
regions <- read.csv('data/pgown_regions.csv')
regions$Obs.Well.. <- paste0('OW', 
      stringr::str_pad(regions$Obs.Well.., width = 3, side = "left", pad = "0"))

ts_approval <- left_join(ts_approval, regions, 
                         by = join_by('LocationIdentifier' == 'Obs.Well..'))

locations_regions <- ts_approval %>% select(c("LocationName",
                                              "LocationIdentifier",
                                              "Region")) %>%
  unique()

# date_from = as_datetime(Sys.Date() - 30)
# date_to = as_datetime(Sys.Date())

date_from = as_datetime(min(ts_telemetry$Date))
date_to = as_datetime(max(ts_telemetry$Date))


theme_lineplots <- theme(
  axis.text.y = element_text(size = 14),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(
    size = 16,
    margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")
  ),
  plot.title = element_text(size = 17, hjust = 0.5),
  plot.margin = unit(c(6, 6, 6, 6), "mm")
)

add_external_resources <- function() {
  tagList(tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "www/bcgov.css"
  ))
}
