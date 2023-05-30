# TODO document bbox crs (and test)

#' Get a list of gauging stations
#'
#' `get_station_list` queries the BOM API and returns a table of station
#' details. The list of stations can be narrowed by station number, station
#' name, parameter, or geographic area (bbox).
#'
#' @inherit get_bom_data params return
#'
#' @param station_no Optionally, a character vector of station numbers. Accepts an asterix
#'   '*' as a wildcard.
#' @param station_name Optionally, a character vector of station names. Accepts an asterix
#'   '*' as a wildcard.
#' @param parametertype_name Optionally, a character vector of parameters.
#'   Accepts an asterix '*' as a wildcard. This can be useful if, for instance, you are
#'   only interested in stations which measure 'Water Course Discharge'. Use
#'   [list_parameters()] to see available options.
#' @param bbox Optionally, a numeric vector of the form c(min_x, min_y, max_x,
#'   max_y) which can be used to get stations in a specific bounding box.
#' @param returnfields A character vector of columns to include in the returned
#'   tibble. Use [list_query_fields("getStationList")] to see available options.
#'
#' @export
#'
#' @examples
#' get_station_list()
get_station_list <- function(
    station_no = NULL,
    station_name = NULL,
    parametertype_name = NULL,
    bbox = NULL,
    ...,
    returnfields = c("station_no", "station_name", "station_latitude", "station_longitude")) {
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
#' @param station_no, Optionally, a character vector of station numbers.
#' @param returnfields A character vector of columns to include in the returned
#'   tibble. Use [list_query_fields("getParameterList")] to see available
#'   options.
#'
#' @export
#'
#' @examples
#' get_parameter_list()
get_parameter_list <- function(
    station_no = NULL,
    station_name = NULL,
    parametertype_name = NULL,
    ...,
    returnfields = c("station_no", "station_name", "parametertype_name")) {
  get_bom_data(
    request = "getParameterList",
    station_no = station_no,
    station_name = station_name,
    parametertype_name = parametertype_name,
    ...,
    returnfields = returnfields
  )
}
