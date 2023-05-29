#' Get a response from the BOM API
#'
#' Make an arbitrary request to the API, and receive the response object. This
#' function is intentionally permissive, meaning that it will allow you to make
#' any request that you like (so long as you specify format and request). No
#' error checking is perfomed on the inputs.
#'
#' @param format Response content format
#' @param request A character string defining the request type to pass on to the
#'   KISTERS API. Use [list_requests()] to see available requests.
#' @param ... Optional key value pairs which can be used to narrow down the
#'   request. Vectors of length greater than one will be collapsed into a comma
#'   separated list. It is possible to use an asterix character (*) as wildcard
#'   in some fields, however this is not recommended as the data naming
#'   conventions can be inconsistent. Use [list_query_fields()] to see available
#'   options.
#'
#' @returns A response object.
#' @seealso [get_bom_data()] for a simplified API interface which is suitable
#'   for most requests returning rectangular data.
#' @export
#'
#' @examples
#' # Get a geojson file of discharge stations
#' get_bom_response(
#'   format = "geojson",
#'   request = "getStationList",
#'   parametertype_name = "Water Course Discharge"
#' )
get_bom_response <- function(format, request, ...) {
  bom_url <- "http://www.bom.gov.au/waterdata/services"
  query <- list(
    service = "kisters",
    type = "QueryServices",
    format = format,
    request = request,
    ...
  )
  req <- httr2::request(bom_url)
  req <- httr2::req_url_query(req, !!!query)
  req <- httr2::req_error(req, body = body_error)
  httr2::req_perform(req)
}

#' Get BOM data
#'
#' @inheritParams get_bom_response
#' @param ... Optional key value pairs passed on to [get_bom_response()].
#'
#' @seealso [get_bom_response()] for more fine-grained control over API
#'   requests.
#' @returns A tibble.
#' @export
#'
#' @examples
#' get_bom_data(request = "getStationList")
get_bom_data <- function(request, ...) {
  resp <- get_bom_response(
    format = "csv",
    request = request,
    ...
  )
  resp_body <- httr2::resp_body_string(resp)
  bom_col_types <- readr::cols(
    station_id = readr::col_integer(),
    station_latitude = readr::col_double(),
    station_longitude = readr::col_double(),
    .default = readr::col_character()
  )
  readr::read_delim(
    resp_body,
    delim = ";",
    col_types = bom_col_types
  )
}

body_error <- function(resp) {
  content_type <- httr2::resp_content_type(resp)
  if (content_type == "application/json") {
    httr2::resp_body_json(resp)$message
  } else if (content_type == "text/xml") {
    xml2::xml_text(httr2::resp_body_xml(resp))
  } else if (content_type == "text/html") {
    xml2::xml_text(httr2::resp_body_html(resp))
  } else {
    "Unknown error response type"
  }
}
