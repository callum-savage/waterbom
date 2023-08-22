#' Get a list of gauging stations
#'
#' `get_station_list` queries the BOM API and returns a table of station
#' details. The list of stations can be narrowed by station number, station
#' name, parameter, or geographic area (bbox). All filters are optional, however
#' If no filters are applied, information will be returned on ALL stations (~
#' 130,000 rows).
#'
#' @inherit get_bom_data params return
#'
#' @param station_no Optionally, a numeric or character vector of station
#'   numbers.
#' @param station_name Optionally, a character vector of station names.
#' @param parametertype_name Optionally, a character vector of parameters. This
#'   can be useful if, for instance, you are only interested in stations which
#'   measure "Water Course Discharge". Use [list_parameters()] to see available
#'   options.
#' @param bbox Optionally, a numeric vector of the form `c(min_x, min_y, max_x,
#'   max_y)` which can be used to get stations in a specific bounding box.
#'
#' @export
#'
#' @examples
#' get_station_list(bbox = c(136.816078, -32.136483, 138.760658, -30.052863))
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

#' Get a list of parameters measured at each station
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
#' get_parameter_list(station_no = "003203A")
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
