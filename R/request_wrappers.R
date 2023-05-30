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
  timeseries_list <- get_bom_data(
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
  dplyr::arrange(timeseries_list, dplyr::across(dplyr::any_of(sort_cols)))
}

# period = "complete"
get_timeseries_values <- function(ts_id,
                                  from = NULL,
                                  to = NULL,
                                  timezone = NULL,
                                  ...,
                                  returnfields = NULL) {
  if (is.null(timezone)) {timezone <- "individual"}
  resp <- get_bom_response(
    format = "json",
    request = "getTimeseriesValues",
    ts_id = ts_id,
    from = from,
    to = to,
    timezone = timezone,
    ...,
    returnfields = returnfields
  )
  resp_body <- httr2::resp_body_json(resp)[[1]]
  ts <- tibble::tibble(ts = resp_body$data)
  ts <- tidyr::unnest_wider(ts, names_sep = "_")
  names(ts) <- unlist(stringr::str_split(resp_body$columns, ","))

  # An arbitrary number of metadata fields are also returned
  # At present, these values are not passed on to the user
  # resp_body[!(names(resp_body) %in% c("columns", "data"))]

  ts
}
