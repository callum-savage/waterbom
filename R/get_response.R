build_query <- function(...) {
  tibble::lst(...)
}

build_req <- function(format = "json", request, query) {
  query_base <- tibble::lst(
    service = "kisters",
    type = "QueryServices",
    format = format,
    request = request
  )
  req <- httr2::request("http://www.bom.gov.au/waterdata/services")
  req <- httr2::req_url_query(req, !!!query_base)
  req <- httr2::req_url_query(req, !!!query)
  req
}

get_resp <- function(req) {
  resp <- httr2::req_perform(req)
  resp
}

get_query_resp <- function(request, ..., format = "json") {
  query <- build_query(...)
  req <- build_req(format = format, request = request)
  resp <- get_resp(req)
  resp
}
