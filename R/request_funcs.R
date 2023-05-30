#' Get a response from the BOM API
#'
#' `get_bom_response` takes an arbitrary API request and returns the response
#' object. Inputs are not error checked, allowing you to make requests which
#' fall outside the scope of [get_bom_data()]. This can be useful if you want to
#' return an unconventional format (e.g. geojson), want make an uncommon
#' request, or just want to interface directly with the API.
#'
#' @param format A string giving the format of the response content. Valid
#'   options will depend on the request. Use [list_formats()] to see available
#'   options.
#' @param request A character string defining the request type to pass on to the
#'   BOM API. Use [list_requests()] to see available options.
#' @param ... Optional named query fields which can be used to narrow the
#'   request. Vectors of length greater than one will be collapsed into a comma
#'   separated list. Use [list_query_fields()] to see available options,
#'   including fields which accept a comma separated list or wildcard.
#'
#' @return A response object.
#' @seealso [get_bom_data()] for a simplified API interface suitable for
#'   requests that return rectangular data.
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
    request = request
  )
  # Collapse query fields into a comma separated list
  query_fields <- purrr::map(list(...), stringr::str_flatten_comma)
  req <- httr2::request(bom_url)
  req <- httr2::req_url_query(req, !!!c(query, query_fields))
  req <- httr2::req_error(req, body = body_error)
  httr2::req_perform(req)
}

#' Get BOM data
#'
#' @inheritParams get_bom_response
#' @param returnfields A character vector of columns to include in the returned
#'   tibble. If not defined, the default columns will be determined by the API.
#'   Use [list_return_fields()] to see available options.
#' @seealso [get_bom_response()] for more fine-grained control over API
#'   requests.
#' @return A tibble with columns determined by `returnfields`.
#' @export
#'
#' @examples
#' get_bom_data(request = "getStationList")
get_bom_data <- function(request, ..., returnfields = NULL) {
  resp <- get_bom_response(
    format = "csv",
    request = request,
    ...,
    returnfields = returnfields
  )
  resp_body <- httr2::resp_body_string(resp)
  bom_data <- readr::read_delim(
    resp_body,
    delim = ";",
    col_types = readr::cols(.default = readr::col_character()),
    na = "",
    progress = FALSE
  )
  # Limited type conversion
  numeric_cols <- c("station_latitude", "station_longitude")
  integer_cols <- c("station_id")
  dplyr::mutate(
    bom_data,
    dplyr::across(dplyr::any_of(numeric_cols), as.numeric),
    dplyr::across(dplyr::any_of(integer_cols), as.integer)
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
