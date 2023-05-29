# TODO make a general purpose response handler which can handle different
# formats
#
# TODO make a general purpose error handler which can extract errors from
# different formats. This may not be necessary, as it seems that csv errors are
# returned as xml. This may be the case for all types except for json.

get_bom_response <- function(format, request, ..., max_tries = 1) {
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
  req <- httr2::req_retry(req, max_tries = max_tries)
  httr2::req_perform(req)
}

get_bom_data <- function(request, ...) {
  resp <- get_bom_response(
    format = "csv",
    request = request,
    ...,
    max_tries = max_tries
  )
  resp_body <- httr2::resp_body_string(resp)
  bom_data <- readr::read_delim(resp_body, delim = ";")
  bom_data
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
