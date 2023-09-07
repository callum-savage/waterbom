get_wdo_response <- function(..., .return = c("response", "request", "url", "query")) {
  # TODO make sure .return can be passed in a list
  # TODO document dynamic dots https://rlang.r-lib.org/reference/dyn-dots.html
  # TODO implement arg match for request and query, and maybe others...?
  # TODO implement response caching

  return_type <- rlang::arg_match(.return)

  dots <- rlang::list2(...)

  # TODO make this a `check_dots()` function
  # or perhaps check_query()
  if (length(dots) == 0) {
    rlang::abort("At least one query option must be specified.")
  } else if (!rlang::is_named(dots)) {
    rlang::abort("All query options must be named")
  }

  query <- rlang::list2(
    service = "kisters",
    type = "QueryServices",
    !!!dots
  )

  query <- purrr::map(query, stringr::str_squish)
  query <- purrr::map(query, stringr::str_flatten, collapse = ",")

  req <- httr2::request("http://www.bom.gov.au/waterdata/services") |>
    httr2::req_url_query(!!!query) |>
    httr2::req_error(body = body_error)

  switch(return_type,
    "query" = query,
    "request" = req,
    "url" = httr2::url_parse(req$url),
    "response" = httr2::req_perform(req)
  )
}

body_error <- function(resp) {
  switch(httr2::resp_content_type(resp),
     "text/html" = xml2::xml_text(httr2::resp_body_html(resp)),
     "text/xml" = xml2::xml_text(httr2::resp_body_xml(resp)),
     "application/json" = httr2::resp_body_json(resp)$message,
     stringr::str_glue("Unexpected error content type: {content_type}")
   )
}
