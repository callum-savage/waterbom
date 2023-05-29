get_bom_data <- function(request, ..., format = "json", max_tries = 1) {
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

  resp <- httr2::req_perform(req)
  resp_type <- httr2::resp_content_type(resp)
  if (resp_type == "application/json") {
    resp_body <- httr2::resp_body_json(resp)
    bom_data <- resp_body
  } else if (resp_type == "application/csv") {
    resp_body <- httr2::resp_body_string(resp)
    bom_data <- readr::read_delim(resp_body, delim = ";")
  } else {
    stop("Unkown response type: ", resp_type)
  }
  bom_data
}

body_error <- function(resp) {
  resp_type <- httr2::resp_content_type(resp)
  if (resp_type == "application/json") {
    httr2::resp_body_json(resp)$message
  } else if (resp_type == "text/xml") {
    xml2::xml_text(httr2::resp_body_xml(resp))
  } else {
    message("Unknown response type: ", resp_type)
  }
}
