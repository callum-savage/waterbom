# TODO: Make tibble references a link
# TODO: Add details on which requests are supported

#' Get a response from the BOM API
#'
#' `get_bom_response()` takes an arbitrary API request and returns the response
#' object. Inputs are not checked in any way, allowing you to make requests
#' which fall outside the scope of [get_bom_data()]. This can be useful if you
#' want to return an unconventional format (e.g. geojson), or just want to
#' interface directly with the API.
#'
#' @inheritParams get_bom_data
#'
#' @param format A string giving the format of the response content. Valid
#'   options depend on `request`.
#' @return A httr2 response object.
#' @seealso [get_bom_data()] for a simplified API interface which is suited to
#'   requesting rectangular data (including timeseries).
#' @examples
#' # Get a geojson file of discharge stations
#' get_bom_response(
#'   format = "geojson",
#'   request = "getStationList",
#'   parametertype_name = "Water Course Discharge"
#' )
#' @export
get_bom_response <- function(format, request, ...) {
  # Create a request object
  req <- httr2::request("http://www.bom.gov.au/waterdata/services")
  # Append base query fields to the request
  query_base <- list(
    service = "kisters",
    type = "QueryServices",
    format = format,
    request = request
  )
  req <- httr2::req_url_query(req, !!!query_base)
  # Append any additional query fields to the request
  # Any vector arguments must first be concatenated into a single string
  query <- purrr::map(list(...), stringr::str_flatten_comma)
  req <- httr2::req_url_query(req, !!!query)
  # Provide a function to check for errors in the response body
  req <- httr2::req_error(req, body = extract_body_error)
  # Get response
  httr2::req_perform(req)
}

#' Extract body errors
#'
#' Extract error messages from the body of a response object returned by the BOM
#' API. Note that the API usually ignores the requested format when raising an
#' error so not all formats need to be covered.
#'
#' @param resp A response object returned by `get_bom_response()`.
#'
#' @return A string containing an error message.
#'
#' @noRd
extract_body_error <- function(resp) {
  #
  content_type <- httr2::resp_content_type(resp)
  if (content_type == "application/json") {
    httr2::resp_body_json(resp)$message
  } else if (content_type == "text/xml") {
    xml2::xml_text(httr2::resp_body_xml(resp))
  } else if (content_type == "text/html") {
    xml2::xml_text(httr2::resp_body_html(resp))
  } else {
    stringr::str_glue("Body has unknown content type: {content_type}")
  }
}

#' Download a list table from Water Data Online
#'
#' @param request
#' @param returnfields
#' @param ...
#' @return A tibble containing the list data. Columns match `returnfields`.
#'
#' @noRd
get_bom_list <- function(request, returnfields = NULL, ...) {
  # Get response in csv format
  resp <- get_bom_response(
    format = "csv",
    request = request,
    returnfields = returnfields,
    ...
  )
  # Read csv from response body
  resp_body <- httr2::resp_body_string(resp)
  readr::read_delim(
    resp_body,
    delim = ";",
    col_types = readr::cols(.default = readr::col_character()),
    na = "",
    progress = FALSE
  )
}

# TODO warn if zero rows returned for a given timeseries and no metadata
# provided to distinguish between timeseries

#' Download a timeseries from Water Data Online
#'
#' This function makes a 'getTimeseriesValues' request and unpacks the response
#' into a tibble.
#'
#' @noRd
get_bom_timeseries <- function(ts_id, returnfields = NULL, md_returnfields = NULL, ...) {
  resp <- get_bom_response(
    format = "dajson", # data array json
    request = "getTimeseriesValues",
    ...,
    returnfields = returnfields
  )
  resp_body <- httr2::resp_body_json(resp)
  # Unpack json response into a nested tibble with one row per timeseries
  ts1 <- tibble::tibble("resp_body" = resp_body)
  # Expand resp_body into columns (timeseries is still nested)
  ts2 <- tidyr::unnest_wider(ts1, col = "resp_body")
  # Select columns which will be returned
  # The timeseries is returned in a column called "data"
  ts3 <- dplyr::select(ts2, dplyr::any_of(c(md_returnfields, "data")))
  # Unpack each timestep into a row. Empty timeseries result in an empty row.
  ts4 <- tidyr::unnest_longer(ts3, col = "data", keep_empty = TRUE)
  # Expand each timestep into multiple columns
  ts5 <- tidyr::unnest_wider(ts4, col = "data", names_sep = "_")
  # Extract the correct timeseries names
  newnames <- unlist(stringr::str_split(ts2$columns[[1]], ","))
  # Identify the temporary timeseries names (of the form data_1, data_2, etc.)
  oldnames <- stringr::str_subset(names(ts5), "^data_\\d+$")
  # Replace old names with the new
  # TODO check that length(newnames) == length(oldnames)
  dplyr::rename_with(ts5, .fn = \(x) newnames, .cols = dplyr::all_of(oldnames))
}

#' Download data from Water Data Online
#'
#' @param request A character string defining the request type.
#' @param returnfields Optionally, a character vector of columns to include in
#'   the returned `tibble`. If not defined, the API will provide a sensible
#'   default. Use [list_return_fields()] to see available options.
#' @param ... Additional named query parameters which are passed on to the API.
#'   Vectors of length greater than one will be collapsed into comma separated
#'   character strings. Use [list_query_fields()] to see available options.
#' @return A `tibble` containing the returned data.
#' @seealso [get_bom_response()] to get the unmodified API response.
#' @examples
#' get_bom_data(
#'   request = "getStationList",
#'   parametertype_name = "Water Course Level"
#' )
#' @export
get_bom_data <- function(request, returnfields = NULL, ...) {
  # List and timeseries requests need to be unpacked differently
  # TODO use list_requests(supported = TRUE) or similar instead
  list_requests <- c("getStationList", "getParameterList", "getTimeseriesList")
  timeseries_requests <- c("getTimeseriesValues")
  if (request %in% list_requests) {
    bom_data <- get_bom_list(request = request, ...)
  } else if (request %in% timeseries_requests) {
    bom_data <- get_bom_timeseries(...)
  } else {
    # TODO polish this error message
    cli::cli_abort(c("Invalid request: {request}"))
  }
  # Tidy up and return data
  bom_data <- convert_bom_types(bom_data)
  bom_data <- clean_bom_names(bom_data)
  bom_data
}







#' Convert types of returned data
#'
#' All collumns returned by the API are character vectors. Most columns are left
#' as characters, however some columns which have clear natural types (e.g.
#' dates) which make them much easier to work with. This function applies those
#' conversions.
#'
#' @param bom_data A tibble.
#'
#' @return `bom_data` with appropriate type conversions applied.
#'
#' @noRd
convert_bom_types <- function(bom_data) {
  # TODO check that this works with timezones
  # TODO this sould also be applied to values
  numeric_cols <- c("station_latitude", "station_longitude")
  integer_cols <- c("station_id")
  timestamp_cols <- c("Timestamp", "from", "to")
  dplyr::mutate(
    bom_data,
    dplyr::across(dplyr::any_of(numeric_cols), as.numeric),
    dplyr::across(dplyr::any_of(integer_cols), as.integer),
    dplyr::across(dplyr::any_of(timestamp_cols), lubridate::as_datetime)
  )
}

#' Clean the column names of returned data
#'
#' Column names are converted using an explici name lookup (rather than, say
#' `janitor::clean_names()`) so that the reverse process can easily be applied
#' in future functions.
#'
#' @param bom_data A tibble.
#'
#' @return `bom_data` with harmonised column names.
#'
#' @noRd
clean_bom_names <- function(bom_data) {
  name_lookup <- c(
    "timestamp"          = "Timestamp",
    "value"              = "Value",
    "quality_code"       = "Quality Code",
    "interpolation_type" = "Interpolation Type"
  )
  dplyr::rename(bom_data, dplyr::any_of(name_lookup))
}
