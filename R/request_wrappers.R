# TODO document bbox crs (and test)

#' Get a list of gauging stations
#'
#' `get_station_list` queries the BOM API and returns a table of station
#' details. The list of stations can be narrowed by station number, station
#' name, parameter, or geographic area (bbox).
#'
#' @inherit get_bom_data params return
#'
#' @param station_no Optionally, a character vector of station numbers.
#' @param station_name Optionally, a character vector of station names.
#' @param parametertype_name Optionally, a character vector of parameters. This
#'   can be useful if, for instance, you are only interested in stations which
#'   measure 'Water Course Discharge'. Use [list_parameters()] to see available
#'   options.
#' @param bbox Optionally, a numeric vector of the form c(min_x, min_y, max_x,
#'   max_y) which can be used to get stations in a specific bounding box.
#'
#' @export
#'
#' @examples
#' get_station_list()
get_station_list <- function(station_no = NULL,
                             station_name = NULL,
                             parametertype_name = NULL,
                             bbox = NULL,
                             ...,
                             returnfields = c(
                               "station_no",
                               "station_name",
                               "station_latitude",
                               "station_longitude"
                             )) {
  get_bom_data(
    request = "getStationList",
    station_no = station_no,
    station_name = station_name,
    parametertype_name = parametertype_name,
    bbox = bbox,
    ...,
    returnfields = returnfields
  )
}

#' Get a list of parameters measured at gauging stations
#'
#' `get_parameter_list` queries the BOM API and returns a table of parameters
#' measured at one or more gauging stations. The query can be narrowed to
#' specific parameters, specific stations, or a geographic area.
#'
#' @inherit get_bom_data params return
#' @inherit get_station_list params
#'
#' @export
#'
#' @examples
#' get_parameter_list()
get_parameter_list <- function(station_no = NULL,
                               parametertype_name = NULL,
                               ...,
                               returnfields = c(
                                 "station_no",
                                 "station_name",
                                 "parametertype_name"
                               )) {
  get_bom_data(
    request = "getParameterList",
    station_no = station_no,
    parametertype_name = parametertype_name,
    ...,
    returnfields = returnfields
  )
}

#' Get a list of timeseries available at gauging stations
#'
#' `get_timeseries_list` queries the BOM API and returns a table of timeseries
#' which can be downloaded for one or more gauging stations. The query can be
#' narrowed to specific timeseries, parameters, or stations. If no query filters
#' are specified the API will return an error.
#'
#' @inherit get_bom_data params return
#' @inherit get_station_list params
#'
#' @param ts_id temp
#' @param ts_name temp
#'
#' @export
#'
#' @examples
#' get_timeseries_list(station_no = 609017)
get_timeseries_list <- function(station_no = NULL,
                                parametertype_name = NULL,
                                ts_id = NULL,
                                ts_name = NULL,
                                ...,
                                returnfields = c(
                                  "station_no",
                                  "station_name",
                                  "parametertype_name",
                                  "ts_id",
                                  "ts_name",
                                  "ts_shortname",
                                  "coverage"
                                )) {
  ts_list <- get_bom_data(
    request = "getTimeseriesList",
    station_no = station_no,
    parametertype_name = parametertype_name,
    ts_id = ts_id,
    ts_name = ts_name,
    ...,
    returnfields = returnfields
  )
  # Sort output by parameter for readability
  sort_cols <- c("station_no", "station_name", "parametertype_name", "ts_name")
  dplyr::arrange(ts_list, dplyr::across(dplyr::any_of(sort_cols)))
}

# period = "complete"
# ts_id only really required in metadata if multiple ts_id provided
# get_timeseries_values(ts_id = 83527010, from = "2020-01-01", to = "2020-01-02")
# get_timeseries_values(ts_id = c(169408010, 197867010), from = "2020-01-01", to = "2020-01-05")
# TODO currently it's too fussy about ts_id - other metadata options should be fine too
# TODO don't worry about renaming columns as ts_list_col etc, use use "data" in quotes
# Also, column renaming code is inelegant
# TODO treat timestamp
get_timeseries_values <- function(ts_id,
                                  from = NULL,
                                  to = NULL,
                                  timezone = "UTC",
                                  ...,
                                  md_returnfields = c("ts_id", "station_no"),
                                  returnfields = c("Timestamp", "Value", "Quality Code", "Interpolation Type")) {
  # If multiple timeseries requested, ensure that there is a unique key (ts_id)
  if (!("ts_id" %in% md_returnfields) & length(ts_id) > 1) {
    md_returnfields <- c(md_returnfields, "ts_id")
  }
  resp <- get_bom_response(
    format = "json",
    request = "getTimeseriesValues",
    ts_id = ts_id,
    from = from,
    to = to,
    timezone = timezone,
    ...,
    metadata = "true",
    md_returnfields = md_returnfields,
    returnfields = returnfields
  )
  # Extract timeseries data from response
  ts_data <- tibble::tibble("ts_data" = httr2::resp_body_json(resp))
  ts_data <- tidyr::unnest_wider(ts_data, "ts_data")
  ts <- dplyr::select(ts_data, dplyr::any_of(c(md_returnfields, "data")))
  ts <- tidyr::unnest_longer(ts, "data")
  ts <- tidyr::unnest_wider(ts, "data", names_sep = "_")
  # Apply column names (clumsily)
  columns <- unlist(stringr::str_split(ts_data$columns[[1]], ","))
  names(ts)[stringr::str_detect(names(ts), "^data_\\d+$")] <- columns
  # Convert timestamp character vector to datetime
  ts$Timestamp <- lubridate::as_datetime(ts$Timestamp, tz = timezone)
  ts
}
