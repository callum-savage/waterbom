#' Get BOM data
#'
#' @inheritParams get_bom_response
#' @param request A character string defining the request type to pass on to the
#'   BOM API. Must be one of: `"getStationList"`, `"getParameterList"`, or
#'   `"getTimeseriesList"`.
#' @param returnfields A character vector of columns to include in the returned
#'   tibble. If not defined, the default columns will be determined by the API.
#'   Use [list_return_fields()] to see available options.
#' @seealso [get_bom_response()] for more fine-grained control over API
#'   requests.
#' @return A tibble with columns matching `returnfields`.
#' @export
#'
#' @examples
#' get_bom_data(
#'   request = "getStationList",
#'   parametertype_name = "Water Course Level"
#' )
get_bom_data <- function(request, ...) {
  list_requests <- c("getStationList", "getParameterList", "getTimeseriesList")
  timeseries_requests <- c("getTimeseriesValues")
  # List and timeseries responses are unpacked differently
  if (request %in% list_requests) {
    bom_data <- get_bom_list(request = request, ...)
  } else if (request %in% timeseries_requests) {
    bom_data <- get_bom_timeseries(request = request, ...)
  } else {
    # TODO polish this error message
    # TODO add info on provided request, and what would be valid
    cli::cli_abort(c("Invalid request."))
  }
  # TODO move type conversion to separate function
  # Perform limited type conversion
  numeric_cols <- c("station_latitude", "station_longitude")
  integer_cols <- c("station_id")
  timestamp_cols <- c("Timestamp", "from", "to")
  dplyr::mutate(
    bom_data,
    dplyr::across(dplyr::any_of(numeric_cols), as.numeric),
    dplyr::across(dplyr::any_of(integer_cols), as.integer),
    dplyr::across(dplyr::any_of(timestamp_cols), lubridate::as_datetime)
  )
  # TODO make a function for name cleaning
}

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

# TODO warn if zero rows returned for a given station
# TODO keep empty rows when unnesting
get_bom_timeseries <- function(request, returnfields = NULL, md_returnfields = NULL, ...) {
  resp <- get_bom_response(
    format = "json",
    request = request,
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
  # Unnest timeseries from "data" column
  ts4 <- tidyr::unnest_longer(ts3, col = "data")
  ts5 <- tidyr::unnest_wider(ts4, col = "data", names_sep = "_")
  # The timeseries' column names were returned as metadata
  # Extract the correct timeseries names
  ts_names <- unlist(stringr::str_split(ts2$columns[[1]], ","))
  # Identify the temporary timeseries names (of the form data_1, data_2, etc.)
  ts_oldnames <- stringr::str_subset(names(ts5), "^data_\\d+$")
  # Replace old names with the new
  # TODO check that length(ts_names) == length(ts_oldnames)
  dplyr::rename_with(ts5, .fn = \(x) ts_names, .cols = dplyr::all_of(ts_oldnames))
}

#' Get a response from the BOM API
#'
#' `get_bom_response` takes an arbitrary API request and returns the response
#' object. Inputs are not checked in any way, allowing you to make requests
#' which fall outside the scope of [get_bom_data()] or [get_timeseries()]. This
#' can be useful if you want to return an unconventional format (e.g. geojson),
#' or just want to interface directly with the API.
#'
#' @param format A string giving the format of the response content. Valid
#'   options will depend on the request. Use [list_formats()] to see available
#'   options.
#' @param request A character string defining the request type to pass on to the
#'   BOM API. Use [list_requests()] to see available options.
#' @param ... Optional named query fields which can be used to narrow the
#'   request. Vectors of length greater than one will be collapsed into a comma
#'   separated character string. Use [list_query_fields()] to see available
#'   options, including fields which accept a comma separated list or wildcard.
#'
#' @return A response object.
#' @seealso [get_bom_data()] for a simplified API interface which is suited to
#'   requests for rectangular data.
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

extract_body_error <- function(resp) {
  # httr2 doesn't read error messages in the response body by default. This
  # function extracts body errors an appropriate method for each content type.
  # Note that the API usually ignores the requested format when raising an
  # error, so not all formats need to be covered.
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
