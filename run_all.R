library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)
library(xml2)
library(purrr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
#library(envreportutils)
library(stringr)
library(forcats)


# Set-up  -----------------------------------------------------------------

if (!dir.exists('out')) {
  dir.create('out')
}
if (!dir.exists('app/data')) {
  dir.create('app/data')
}

readRenviron(paste0(getwd(), "/.Renviron"))
username = Sys.getenv("APP_USERNAME")
password = Sys.getenv("APP_PASSWORD")

# safety check

# if (Sys.getenv("APP_USERNAME") == "" || Sys.getenv("APP_PASSWORD") == "") {
#   stop("Missing credentials: check GitHub secrets")
# }


#Load list of well IDs, some of these wells are non-telemetry
sites <- read.csv('Active Groundwater Observation Wells.csv', stringsAsFactors = F) |>
  select(label) |> filter(!(label %in% c("OW293", "OW482")))

sites <- as.list(sites$label)
ts_sites <- sites

paramters = 'SGWL'
fv_start_date = "2026-01-01"

# Setting color codes -----------------------------------------------------

# Timeseries approval status

review_levels <- c(
  "Working" = "#fe3200",
  "In Review" = "#ffcc02",
  "Reviewed" = "#b4fec0",
  "Approved" = "#31a926"
)

# Grade codes

grade_codes <- c(
  "0 - Undefined" = "#b4e2ff",
  "151 - Grade A" = "#117412",
  "141 - Grade B" = "#edff61",
  "131 - Grade C" = "#ec9e32",
  "121 - Grade E - Estimated" = "#ff0059",
  "100 - Grade U - Unknown" = "#b5b5b5"
)

# Functions required ------------------------------------------------------

api_req <- function(url, username, password) {
  httr2::request(paste0(
    "https://bcmoe-prod.aquaticinformatics.net/AQUARIUS/Publish/v2/",
    url
  )) |>
    httr2::req_auth_basic(username = username, password = password)
}

# 01_field-visit functions ------------------------------------------------

location_data <- function(location_url, site, username, password) {
  site_specific_url <- paste0(location_url, "?LocationIdentifier=", site)

  req_location <- api_req(
    url = site_specific_url,
    username = username,
    password = password
  )

  resp_location <- req_location |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  location_meta = as_tibble(resp_location[1:12])
  location_ea = as_tibble(resp_location$ExtendedAttributes)
  
  data_source = location_ea$Value[location_ea$Name == 'DATASOURCE_GW']
  
  if (is_null(data_source)) {data_source = "Missing!"}
  
  location_meta$DataSource = data_source
  
  return(location_meta)
}


extract_fv_info_mod <- function(site, username, password) {
  fv_url <- "GetFieldVisitDataByLocation"

  site_specific_url <- paste0(fv_url, "?LocationIdentifier=", site)

  req_fv <- api_req(
    url = site_specific_url,
    username = username,
    password = password
  )

  resp_fv <- req_fv |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    purrr::pluck("FieldVisitData") |>
    tibble::as_tibble() |>
    # unnest(col = Attachments) |>
    tidyr::unnest(
      cols = c(
        #Attachments, #not always present for obs well ex 152, 511
        #ControlConditionActivity,
        Approval,
        InspectionActivity,
        #LevelSurveyActivity, #removed for PGOWN obs well test 401
        CompletedWork
      ),
      names_repair = "universal"
    )
  # pluck('Approval') |>
  # glimpse()

  fv_db <- resp_fv |>
    dplyr::select(
      Identifier,
      LocationIdentifier,
      ApprovalLevel,
      LevelDescription,
      StartTime,
      EndTime#,
      #UploadedByUser #not always present for obs well ex 152
    ) |>
    dplyr::mutate(
      StartTime = lubridate::as_date(StartTime),
      EndTime = lubridate::as_date(EndTime),
      #DateUploaded = lubridate::as_date(DateUploaded),
      Year = as.numeric(lubridate::year(StartTime))
    ) |>
    dplyr::arrange(LocationIdentifier, Identifier, StartTime) |>
    dplyr::group_by(
      Identifier,
      LocationIdentifier,
      ApprovalLevel,
      LevelDescription,
      StartTime,
      EndTime
    ) |>
    dplyr::slice_head() |>
    dplyr::ungroup() |>
    dplyr::group_by(Year) |>
    dplyr::mutate(n_per_year = n())
}


# Retrieving Locations ----------------------------------------------------
# Location data is for both telemetry and non-telemetry wells with the new column
# data source used to tell tele from non-tele wells.

location_url <- "GetLocationData"

all_location_meta = sites |>
  purrr::map(
    \(sites) {
      location_data(
        location_url,
        sites,
        username = username,
        password = password
      )
    },
    .progress = TRUE
  ) |>
  purrr::list_rbind()

site_name_lookup <- all_location_meta |>
  select(LocationName, Identifier, Latitude, Longitude, Elevation, DataSource) |>
  rename(LocationIdentifier = Identifier)

write_rds(site_name_lookup, 'app/data/site_name_lookup.rds')


# Review Status by Station
# this gets meta data for each field visit, both telemetery and non-telemtery wells
# data goes back to the start of time but even still only about 5200 fv for wells
all_fv_info = sites |>
  purrr::map(
    \(sites) {
      extract_fv_info_mod(sites, username = username, password = password)
    },
    .progress = TRUE
  ) |>
  purrr::list_rbind() |>
  left_join(site_name_lookup, by = 'LocationIdentifier')

write_rds(all_fv_info, "app/data/field_visits.rds")


# Extract field visit details for review and approval figures -------------
# Filter to just field visit details after date_from (2026)
date_from = as.Date(fv_start_date)
date_to = as.Date(Sys.Date())

field_visit_year = all_fv_info |>
  filter(StartTime >= date_from) |>
  rename(FieldVisitDate = StartTime) |>
  select(
    Identifier,
    LocationIdentifier,
    LocationName,
    ApprovalLevel,
    LevelDescription,
    FieldVisitDate#,
    #UploadedByUser #missing for some obs wells
  ) |>
  unique()

write_rds(field_visit_year, "app/data/field_visit_year.rds")


# 02_timeseries functions - Timeseries retrieval functions ------------------------------------------

#' Retrieve the approval status and the change in approvals over time by timeseries
#'
#' @param sample_ts The unique ID of a timeseries (several within each site)
#' @param username API username required
#' @param password API password required
#'
#' @returns A dataframe with approval levels and date of approval level change by unique timeseries id
#' @export
#'
#' @examples extract_appr <- retrieve_approvals(
#'   sample_ts = "8a0ed03b7fad4a40904122d3b6d0086f",
#'   "username", "password"
#' )
#'
retrieve_approvals <- function(sample_ts, username, password) {
  approval_url <- paste0(
    "GetApprovalsTransactionList?TimeSeriesUniqueId=",
    sample_ts
  )

  approval_data <- api_req(approval_url, username, password)

  appr_ts_data <- approval_data |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    purrr::pluck("ApprovalsTransactions") |>
    tibble::as_tibble() |>
    dplyr::mutate(UniqueId = sample_ts, .before = 1)

  appr_ts_data
}


#' Retrieve list of unique time-series ids
#'
#' @param site A character site number (format "11AA1111")
#' @param username API username required
#' @param password API password required
#' @param output Preferred format of the output ('list' or 'dataframe').
#' Dataframe provides site number with timeseries id, list does not.
#'
#' @returns A list or dataframe of timeseries ID by site number
#' @export
#'
#' @examples extract_list <- ts_unique_ids(site = "08MH0041", "username", "password", output = "list")
#'
retrieve_ts_ids <- function(
  site,
  parameter,
  published,
  username,
  password,
  output = c("list", "dataframe")
) {
  ts_url <- paste0("GetTimeSeriesUniqueIdList?Parameter=", parameter, 
                   "&LocationIdentifier=", site,
                   "&Publish=", published)

  req_ts <- api_req(ts_url, username, password)

  resp_ts <- req_ts |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    tibble::as_tibble()
  
  if (nrow(resp_ts) > 0) {
  ts_id <- resp_ts |>
    purrr::pluck("TimeSeriesUniqueIds") |>
    dplyr::mutate(site = site)
  } else { #nothing returned
    ts_id <- data.frame('UniqueId'=NULL)
  }
  if (output == "list") {
    ts_id <- as.list(ts_id$UniqueId)
    ts_id
  } else if (output == "dataframe") {
    ts_id
  }
}

ts_meta_data <- function(sample_ts, username, password) {
  ts_info_url = paste0(
    "GetTimeSeriesCorrectedData?TimeSeriesUniqueId=",
    sample_ts
  )

  ts_info_req <- api_req(ts_info_url, username, password)

  ts_info_data <- ts_info_req |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  grades = ts_info_data |>
    pluck("Grades")

  ts_meta = as_tibble(ts_info_data[1:6]) |>
    cbind(grades)
}

#added for speed to replace ts_meta_data
ts_meta_data_lite <- function(site, username, password) {
  ts_info_url = paste0(
    "GetTimeSeriesDescriptionList?LocationIdentifier=",
    site
  )
  
  ts_info_req <- api_req(ts_info_url, username, password)
  
  ts_info_data <- ts_info_req |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
  
  ts_info_data = ts_info_data |>
    pluck("TimeSeriesDescriptions")
  
  #ts_meta = as_tibble(ts_info_data[1:6]) |>
  #  cbind(grades)
}

# Retrieve all unique timeseries ids ---------------------------------------------

# find all unique timeseries ids
# Get information on what each timeseries id references

# all_ts_ids = ts_sites |>
#   purrr::map(
#     \(ts_sites) {
#       retrieve_ts_ids(
#         ts_sites,
#         username = username,
#         password = password,
#         parameter = "SGWL",
#         published = "true",
#         output = "dataframe"
#       )
#     },
#     .progress = TRUE
#   ) |>
#   purrr::list_rbind()

# Added for speed
#gets all the time series meta data which can then be filtered to the parameters
#and labels as needed
all_ts_ids <- ts_sites %>% 
  purrr::map(
    \(ts_sites) {
      ts_meta_data_lite(ts_sites,
                        username = username,
                        password = password)
    },
    .progress = TRUE
    ) %>%
  purrr::list_rbind()

#filter to just those that are needed for PGWON that is working and SGWL and BVOLT
all_ts_ids <- all_ts_ids %>% filter((Parameter == 'SGWL' & Label == "Working") | 
                                    (Parameter == 'Bvolt' & Label != "Logger"))

#gets the unique ts IDs for each 
ts_id = as.list(all_ts_ids$UniqueId)

#do it again for BVOLT not needed anymore
# all_ts_ids_bvolt = ts_sites |>
#   purrr::map(
#     \(ts_sites) {
#       retrieve_ts_ids(
#         ts_sites,
#         username = username,
#         password = password,
#         parameter = "Bvolt",
#         published = "false",
#         output = "dataframe"
#       )
#     },
#     .progress = TRUE
#   ) |>
#   purrr::list_rbind()
# 
# ts_id_bvlot = as.list(all_ts_ids_bvolt$UniqueId)

#join bvolt and sgwl ts_id
#all_ts_ids <- rbind(all_ts_ids, all_ts_ids_bvolt)
#ts_id <- as.list(all_ts_ids$UniqueId)

# sample_ts <- all_ts_ids$UniqueId[[92]]
#
# ts_url = paste0("GetTimeSeriesCorrectedData?TimeSeriesUniqueId=", sample_ts)

## Approval Level ----------------------------------------------------------

all_approval_level = ts_id |>
  purrr::map(
    \(ts_id) {
      retrieve_approvals(ts_id, username = username, password = password)
    },
    .progress = TRUE
  ) |>
  purrr::list_rbind() |>
  left_join(all_ts_ids, by = "UniqueId")

write_rds(all_approval_level, 'out/approval_levels.rds')

## Retrieve grade level and additional meta data-----------------------------------
#VERY SLOW for wells 2598 ts ids
# grade_levels = ts_id |>
#   purrr::map(
#     \(ts_id) ts_meta_data(ts_id, username = username, password = password),
#     .progress = TRUE
#   ) |>
#   purrr::list_rbind() |>
#   left_join(all_ts_ids, by = "UniqueId")
# 
# write_csv(grade_levels, "out/grade_levels.csv")
# 
# all_ts_meta = grade_levels |>
#   select(-c(GradeCode, StartTime, EndTime)) |>
#   unique() |>
#   left_join(all_approval_level, by = c("UniqueId", "site"))
# 
# write_csv(all_ts_meta, 'out/timeseries_info_w_approvals.csv')




# ts_id_info <- all_ts_ids |>
#   rename(LocationIdentifier = site) |>
#   left_join(all_ts_meta, by = c('UniqueId', 'LocationIdentifier')) |>
#   select(c(
#     UniqueId,
#     LocationIdentifier,
#     Parameter,
#     Label,
#     LevelDescription,
#     NumPoints,
#     DateAppliedUtc
#   )) |>
#   mutate(DateAppliedUtc = as_datetime(DateAppliedUtc)) |>
#   distinct() |>
#   arrange(UniqueId, LocationIdentifier, Parameter, DateAppliedUtc)

#write_csv(ts_id_info, 'timeseries_id_info.csv')

# Read in csv. if re-starting here -------------------------------

# all_ts_meta <- read_csv('./out/timeseries_info_w_approvals.csv')
# ts_id_info <- read_csv('timeseries_id_info.csv')

# Timeseries Corrected Data -----------------------------------------------
#HERE
ts_data_ids <- all_approval_level |>
  filter(Parameter == "SGWL" | Parameter == 'Bvolt') |> 
  filter(
    Label == "Working" | Label == "Telemetry" | Label == "Working_Telemetry" 
  ) |>
  distinct() |>
  filter(StartTime > "2000-01-01") |>
  mutate(
    LevelDescription = factor(
      LevelDescription,
      levels = c("Approved", "Reviewed", "In Review", "Working")
    ),
    StartTime = as_datetime(StartTime),
    EndTime = as_datetime(EndTime)
  ) |>
  left_join(site_name_lookup, by = "LocationIdentifier") |>
  mutate(
    EndTime = case_when(EndTime > Sys.Date() ~ StartTime, TRUE ~ EndTime)
  ) |>
  arrange(LocationIdentifier, Parameter, LevelDescription, DateAppliedUtc) |>
  ungroup()

latest_trans = ts_data_ids |>
  group_by(UniqueId, Parameter, Label, LocationIdentifier) |>
  arrange(desc(EndTime)) |>
  slice(1) |>
  mutate(LatestTransaction = EndTime) |>
  select(UniqueId, Parameter, Label, LocationIdentifier, LatestTransaction)

# Set up daily grid to arrange timeseries timeline for figure

date_from = as.Date(fv_start_date)
date_to = as.Date(Sys.Date())
LocationIdentifier = unique(site_name_lookup$LocationIdentifier)
Parameter = c("SGWL", "Bvolt") #change to SGWL

Date <- seq.Date(from = date_from, to = date_to, by = "day")

year_grid = crossing(Date, Parameter, LocationIdentifier) |>
  left_join(site_name_lookup, by = "LocationIdentifier")

ts_data_test <- year_grid |>
  left_join(
    ts_data_ids,
    by = c('Parameter', 'LocationIdentifier', 'LocationName'),
    relationship = "many-to-many"
  ) |>
  left_join(
    latest_trans,
    by = c('UniqueId', 'Parameter', 'Label', 'LocationIdentifier')
  ) |>
  mutate(
    Within_Series = case_when(
      Date <= EndTime & Date >= StartTime ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  mutate(
    Working_Default = case_when(
      Date <= Sys.Date() & Date > LatestTransaction ~ TRUE,
      TRUE ~ FALSE
    ),
    ApprovalLevel = case_when(
      Working_Default == TRUE ~ 800,
      TRUE ~ as.numeric(ApprovalLevel)
    ),
    LevelDescription = case_when(
      Working_Default == TRUE ~ "Working",
      TRUE ~ as.factor(LevelDescription)
    )
  ) |>
  select(
    LocationName,
    LocationIdentifier,
    Parameter,
    Date,
    StartTime,
    EndTime,
    Within_Series,
    Working_Default,
    ApprovalLevel,
    LevelDescription,
    DateAppliedUtc
  ) |>
  filter(Within_Series == TRUE | Working_Default == TRUE) |>
  group_by(Date, LocationName, LocationIdentifier, Parameter) |>
  arrange(desc(ApprovalLevel)) |>
  slice(1) |>
  ungroup() |>
  arrange(LocationName, Parameter, Date, ApprovalLevel) |>
  select(
    LocationName,
    LocationIdentifier,
    Parameter,
    Date,
    ApprovalLevel,
    LevelDescription
  ) |>
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
  )


write_rds(ts_data_test, "app/data/timeseries_approval.rds")

# 03_telemetry functions --------------------------------------------------

# function for pulling individual time-series data sets from specific date range

timeseries_req <- function(sample_id, date_from, date_to, username, password) {
  ts_info_url = paste0(
    "GetTimeSeriesData?TimeSeriesUniqueIds=",
    sample_id,
    "&QueryFrom=",
    date_from,
    "&QueryTo=",
    date_to
  )

  ts_info_req <- api_req(ts_info_url, username, password)

  ts_info_data <- ts_info_req |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  ts_base = ts_info_data |>
    pluck("TimeSeries") |>
    as_tibble()

  ts_points = ts_info_data |>
    pluck("Points") |>
    as_tibble() |>
    bind_cols(ts_base)
}

# Identify shift ids by site
# 
# shift_ids <- function(site_id, username, password) {
#   sh_info_url = paste0(
#     "GetRatingModelDescriptionList?LocationIdentifier=",
#     site_id
#   )
# 
#   sh_info_req <- api_req(sh_info_url, username, password)
# 
#   sh_info_data <- sh_info_req |>
#     httr2::req_perform() |>
#     httr2::resp_body_json(simplifyVector = TRUE)
# 
#   sh_base = sh_info_data |>
#     pluck("RatingModelDescriptions") |>
#     as_tibble()
# }

# function to find most recent shift applied for individual ts

extract_shift <- function(shift_id, username, password, timeseries_id) {
  timeseries_id <- timeseries_id |>
    filter(Identifier == shift_id) |>
    pull(UniqueId)

  if (!is.na(timeseries_id)) {
    sh_applied_url <- paste0(
      "GetRatingModelEffectiveShifts?TimeSeriesUniqueId=",
      timeseries_id,
      "&RatingModelIdentifier=",
      shift_id
    )

    sh_applied_req <- api_req(sh_applied_url, username, password)

    sh_applied_data <- sh_applied_req |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE)

    sh_applied <- sh_applied_data |>
      pluck("EffectiveShifts") |>
      as_tibble() |>
      glimpse() |>
      mutate(Timestamp = lubridate::as_datetime(Timestamp))
    # group_by(Value) |>
    # mutate(
    #   first_occurence = first(Timestamp),
    #   last_occurence = last(Timestamp)
    # ) |>

    if ("Value" %in% colnames(sh_applied)) {
      tol <- 0 # e.g., tol <- 1e-6 for numeric noise

      df_runs <- sh_applied %>%
        arrange(Timestamp) %>%
        mutate(
          # Detect a change (TRUE) when current value differs from previous by more than tol
          changed = case_when(
            is.na(lag(Value)) ~ TRUE, # first row starts a run
            abs(Value - lag(Value)) > tol ~ TRUE,
            TRUE ~ FALSE
          ),
          run_id = cumsum(changed)
        ) %>%
        group_by(run_id) %>%
        summarise(
          Value = first(Value),
          start_ts = first(Timestamp),
          end_ts = last(Timestamp),
          .groups = "drop"
        )

      out <- df_runs |>
        arrange(run_id) |>
        slice_max(run_id) |>
        mutate(
          Identifier = shift_id,
          UniqueId = timeseries_id
        ) |>
        select(Identifier, UniqueId, Value, start_ts) |>
        rename(Timestamp = start_ts)

      out
    } else {
      out = sh_applied |>
        mutate(Value = NA, Identifier = shift_id, UniqueId = timeseries_id) |>
        slice_max(Timestamp)
    }
    return(out)
  }
}

# Part 1: Extracting telemetry data for each site and parameter --------

# list of telemetry parameters of interest
parameters = c("SGWL", "Bvolt") 
# 
# # pull the correct telemetry ids for each station from master list
telemetry_db = all_approval_level |>
  filter(
    Label == "Telemetry" &
      Parameter %in% parameters |
      Label == "Working" & Parameter == "SGWL" 
  ) #|>
# #filter(!(Parameter == "Stage" & Label == "Working" & LocationIdentifier == "08NH0001"))

telemetry_list = all_ts_ids |>
  select(LocationIdentifier, Parameter, Label, UniqueId) |>
  unique() |>
  left_join(site_name_lookup, by = "LocationIdentifier")

telemetry_id = as.list(telemetry_db$UniqueId)

# define dates of interest for app

date_from = Sys.Date() - 30
date_to = Sys.Date()
mid_date = Sys.Date() - 7
cutoff_date = Sys.Date() - 2

dates = c(date_from, date_to, mid_date)


# Retrieve individual time-series data sets from specific date range
# map function over all telemetry ids of interest. ETA 25 mins

#noticed that telemetry_id has 8700 entries but most are duplicate only 411 unique ones
u_telemetry_id <- unique(telemetry_id)

ts_telemetry = u_telemetry_id |>
  purrr::map(
    \(telemetry_id) {
      timeseries_req(telemetry_id, date_from, date_to, username, password)
    },
    .progress = TRUE
  ) |>
  purrr::list_rbind()

# tidy combined telemetry data and calculate monthly means

ts_telemetry = ts_telemetry |>
  mutate(DateTime = ymd_hms(Timestamp)) |>
  group_by(LocationIdentifier, Parameter) |>
  arrange(DateTime) |>
  mutate(monthly_mean = mean(NumericValue1)) |>
  ungroup() |>
  left_join(site_name_lookup, by = "LocationIdentifier") # add station names

# identify value from 7-days prior from current data (to provide a change metric)

week_prior = ts_telemetry |>
  filter(DateTime == mid_date) |>
  group_by(LocationName, LocationIdentifier, Parameter) |>
  slice_head() |>
  rename(WeekPrior = NumericValue1) |>
  select(LocationName, LocationIdentifier, Parameter, WeekPrior, monthly_mean)

# create a grid of all stations and all telemetry metrics to include in app - may change this

all_combinations <- expand_grid(
  LocationName = unique(ts_telemetry$LocationName),
  Parameter = unique(ts_telemetry$Parameter)
)

# get most recent reading at time of data run for current conditions. Some locations
#that have telemetry are delayed like a day so are getting filtered out here

current_reading = ts_telemetry |>
  #filter(DateTime == date_to) |>
  filter(DateTime >= date_to - 3) |>
  group_by(LocationName, LocationIdentifier, Parameter, DataSource, .drop = FALSE) |>
  arrange(DateTime) |>
  slice_max(DateTime) |>
  select(c(LocationName, DateTime, NumericValue1, Parameter, Label, Unit, DataSource)) |>
  unique() |>
  left_join(
    week_prior,
    by = c("LocationName", "LocationIdentifier", "Parameter")
  ) |>
  mutate(
    week_difference = NumericValue1 - WeekPrior,
    diff_from_monthly_ave = NumericValue1 - monthly_mean
  ) |>
  ungroup()

#what about telemetry locations offline for more than 3 days?
offline_telemetry <- ts_telemetry %>% 
  filter(DataSource == "Telemetry", !(LocationIdentifier %in% current_reading$LocationIdentifier)) %>%
  group_by(LocationName, LocationIdentifier, Parameter, DataSource, .drop = FALSE) %>%
  arrange(DateTime) |>
  slice_max(DateTime) |>
  #select(c(LocationName, DateTime, NumericValue1, Parameter, Label, Unit, DataSource)) |>
  unique()

#this gets all locations by name and parameter 
#current_reading <- all_combinations |>
#  left_join(current_reading, by = c('LocationName', 'Parameter'))

current_reading <-site_name_lookup |> 
  left_join(current_reading, by = c('LocationIdentifier', 'LocationName')) |>
  select(-c("DataSource.y")) |>
  rename('DataSource' = 'DataSource.x')

ts_telemetry_out <- ts_telemetry |>
  filter(Parameter == "SGWL" | Parameter == "Bvolt") |>
  select(c(
    'LocationName',
    'Timestamp',
    'DateTime',
    'Parameter',
    'NumericValue1',
    'monthly_mean'
  )) |>
  mutate(Hour = hour(DateTime), Date = date(DateTime)) |>
  group_by(LocationName, Parameter, Hour, Date, monthly_mean) |>
  summarise(NumericValue1 = mean(NumericValue1)) |>
  mutate(Timestamp = ymd_h(paste(Date, Hour)))

write_csv(ts_telemetry_out, 'app/data/ts_telemetry.csv')

#write_csv(ts_telemetry, 'out/ts_telemetry.csv')

write_csv(current_reading, 'app/data/current_reading.csv')

#write_csv(current_reading, 'out/current_reading.csv')

# Part 2: Capturing most recent discharge shift applied ---------------------------

## Extract shift IDs ---------------------------------------

#Not needed for PGOWN
# shift_id_table = ts_sites |>
#   purrr::map(
#     \(ts_sites) shift_ids(ts_sites, username, password),
#     .progress = TRUE
#   ) |>
#   purrr::list_rbind() |>
#   filter(InputParameter == "Stage") |>
#   filter(Label != "GUIC") |>
#   filter(Label != "CLP2.WLRS")


## Find most recent shift applied for telemetry dashboard --------
# 
# tele_shift_id = ts_telemetry |>
#   filter(Parameter == "Stage") |>
#   select(UniqueId, LocationIdentifier, Parameter) |>
#   unique()
# 
# tele_id <- shift_id_table |>
#   left_join(tele_shift_id, by = c('LocationIdentifier')) |>
#   select(Identifier, UniqueId) |>
#   mutate(
#     Identifier = gsub(Identifier, pattern = " ", replacement = "%20"),
#     Identifier = gsub(Identifier, pattern = "@", replacement = "%40")
#   ) |>
#   filter(UniqueId != "5ea7dc42f05e41f3b8b63069fc338efb") # older ts id for Five Mile
# 
# 
# shift_id_list = unique(tele_id$Identifier)


# mapping shift function over all shift ids

# shift_last_applied = shift_id_list |>
#   purrr::map(
#     \(shift_id_list) extract_shift(shift_id_list, username, password, tele_id),
#     .progress = TRUE
#   ) |>
#   purrr::list_rbind() |>
#   left_join(tele_shift_id, by = "UniqueId") |>
#   left_join(site_name_lookup, by = "LocationIdentifier")
# 
# write_rds(shift_last_applied, 'app/data/shift_last_applied.rds')

# Part 3 Field Visit Information -------------------------------------------------
# I added this in for the weekly app - to be separate from the 01_field-visit.R

# all_fv_info = sites |>
#   purrr::map(
#     \(sites) {
#       extract_fv_info_mod(sites, username = username, password = password)
#     },
#     .progress = TRUE
#   ) |>
#   purrr::list_rbind()
#
# all_fv_info = all_fv_info |>
#   left_join(site_name_lookup, by = 'LocationIdentifier')

# Days since last visit

most_recent <- all_fv_info |>
  mutate(Date = as.Date(EndTime, '%d/%m/%Y')) |>
  group_by(LocationIdentifier) |>
  slice(which.max(Date)) |>
  mutate(Current_Date = Sys.Date()) |>
  mutate(Most_Recent = Current_Date - Date) |>
  select(-c(StartTime, EndTime)) |>
  ungroup() |>
  arrange(Most_Recent) |>
  mutate(
    color = case_when(
      Most_Recent > 183 ~ "#bf2c34",
      Most_Recent <= 183 & Most_Recent > 152 ~ "#f07857",
      Most_Recent <= 152 ~ "#4fb06d"
    ),
    legend = case_when(
      color == "#bf2c34" ~ "> 183",
      color == "#f07857" ~ "153 - 183",
      color == "#4fb06d" ~ "< 152"
    )
  ) #|>
  # left_join(
  #   all_location_meta |>
  #     select(LocationName, Latitude, Longitude, ElevationUnits, Elevation),
  #   by = c('LocationName')
  # )

write_rds(most_recent, "app/data/most_recent_visit_weekly.rds")


# Data for map labels -----------------------------------------------------

map_data = current_reading |>
  mutate(
    perc_diff = 100 *
      (diff_from_monthly_ave / ((NumericValue1 + monthly_mean) / 2)),
    exceed_20 = case_when(
      abs(perc_diff) >= 20 ~ TRUE,
      abs(perc_diff) < 20 ~ FALSE
    )
  ) |>
  left_join(
    most_recent,
    by = c('LocationName', 'LocationIdentifier', 'Latitude', 'Longitude', 
           'DataSource', 'Elevation')
  ) |>
  mutate(legend_battery = 
           case_when(Parameter == 'Bvolt' & NumericValue1 >= 12.4 ~ ">12.4V",
                     Parameter == 'Bvolt' & NumericValue1 >= 12.0 & NumericValue1 < 12.4 ~ "12.0 - 12.4V",
                     Parameter == 'Bvolt' & NumericValue1 < 12.0 ~ "<12.0V")) |>
  
  mutate(batter_colour = 
           case_when(Parameter == 'Bvolt' & NumericValue1 >= 12.4 ~ "#4fb06d",
                     Parameter == 'Bvolt' & NumericValue1 >= 12.0 & NumericValue1 < 12.4 ~ "#f07857",
                     Parameter == 'Bvolt' & NumericValue1 < 12.0 ~ "#bf2c34")) 

#all wells are showing up here but those that don't have a recent time series reading are NA


# ) |>
# left_join(
#   fv_meta_data |>
#     select(
#       LocationName,
#       Latitude,
#       Longitude,
#       ElevationUnits,
#       Elevation,
#       color,
#       legend
#     ),
#   by = c('LocationName')
# )

write_csv(map_data, "app/data/map_data.csv")
