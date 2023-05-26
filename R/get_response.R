build_query <- function(request, ..., format = "json") {
  query <- list(
    format = format,
    request = request,
    ...
  )
  query
}

build_req <- function(query) {
  req <- httr2::request("http://www.bom.gov.au/waterdata/services")
  req <- httr2::req_url_query(req, service = "kisters", type = "QueryServices")
  req <- httr2::req_url_query(req, !!!query)
  req
}

get_resp <- function(req) {
  resp <- httr2::req_perform(req)
  resp
}

get_query_resp <- function(request, ..., format = "json") {
  query <- build_query(request, ..., format = format)
  req <- build_req(query)
  resp <- get_resp(req)
  resp
}
