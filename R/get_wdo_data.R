############ file: core.R

# Get a response object from Water Data Online API
#
# A flexible user-facing function
get_wdo_response <- function(format, request, ...) {
  wdo_query <- construct_wdo_query(format, request, ...)
  wdo_req <- construct_wdo_req(wdo_query)
  wdo_resp <- httr2::req_perform(wdo_req)
  wdo_resp
}

# Pack query options into a list
# Not user-facing
construct_wdo_query <- function(format, request, ...) {
  wdo_query <- list()
}

# Convert the query list into a httr2 request object
# Not user facing
construct_wdo_req <- function(wdo_query) {
  wdo_req <- make_req(wdo_query)
}

# Get errors from the response body
# No user facing
extract_body_error <- function() {
  body_error
}



########### file: wrappers

convert_wdo_types <- function(wdo_data) {
  wdo_data # with type conversions
}

# Power list requests
# Not user facing
get_wdo_list <- function(request, ..., return_fields = NULL) {
  wdo_list_resp <- get_wdo_response(format = "json", request = request, return_fields = return_fields, ...)
  wdo_list <- #unpack response
  wdo_list <- convert_wdo_types(wdo_list)
  wdo_list
}

# User facing wrappers
# Args not limited
get_station_list <- function() {
  get_wdo_list(request = "getStationList")
}

# Do I actually need this function?
get_parameter_list() <- function() {
  get_wdo_list(request = "getParameterList")
}
get_timeseries_list() <- function() {
  get_wdo_list(request = "getTimeseriesList")
}

# A future function with limited capabilities but a nice interface might be:
# find_stations(station_no, station_name, bbox, return_fields)

get_timeseries_values <- function(ts_id, ..., return_fields = NULL) {
  wdo_ts_resp <- get_wdo_response(
    format = "json",
    request = "getTimeseriesValues",
    ts_id = ts_id,
    ...,
    return_fields = return_fields
  )
  wdo_ts <- # unpack timeseries
  wdo_ts <- convert_wdo_types(wdo_ts)
}

# Explicitly limited args (no dots)
get_timeseries <- function(station_no, ts_name, from, to, timezone, return_fields) {
  ts_id <- get_timeseries_list()$ts_id
  timeseries_id <- get_timeseries_values
}

# get_water_data(station_no = 1234, parameter = "discharge", start_date, end_date, return_fields)

########### file: helpers.R

parameters()
query_fields()
return_fields()
optional_fields()



# KEEP IT SIMPLE, GET IT MADE
