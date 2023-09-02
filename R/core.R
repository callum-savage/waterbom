get_wdo_response <- function(format, request, ..., .return_url = FALSE) {
  # TODO reorder args, format isn't actually a required field
  # It probably shouldn't be explicit in this function
  # (which might make list calls easier)

  # TODO document dynamic dots https://rlang.r-lib.org/reference/dyn-dots.html
  # Pack all query options into a list, with error checking
  wdo_query <- construct_wdo_query(format, request, ...)

  # Use query options to construct a httr2 request object
  wdo_req <- construct_wdo_req(wdo_query)

  # Return the url string if .return_url == TRUE
  if (.return_url) return(wdo_req$url)

  # Get a response from Water Data Online
  httr2::req_perform(wdo_req)
}

construct_wdo_query <- function(format, request, ...) {
  # Package dots into a list
  dots <- rlang::dots_list(
    ...,
    .ignore_empty = "trailing", # allow a trailing comma
    .homonyms = "error", # error if two args have the same name
    .check_assign = TRUE # warn if `<-` used in function call
  )

  # Check that all args have names
  # note: rlang::names2 converts missing names to ""
  if (any(rlang::names2(dots) == "")) {
    rlang::abort(
      "All query options must be named.",
      class = "unnamed_query_options"
    )
  }

  # Assemble all query options into a list
  wdo_query <- rlang::list2(
    service = "kisters",
    type = "QueryServices",
    format = format,
    request = request,
    !!!dots
  )

  # TODO consider removing this whole section as it's probably overkill
  # The API probably does a lot of this for us
  # It may also introduce additional errors

  # Coerce all options to characters and remove any extra white space
  # TODO test if white space actually matters to the API
  # (especially for text fields like staiton name)
  wdo_query <- purrr::map(wdo_query, stringr::str_squish)

  # Remove any duplicate options
  # TODO check if this is actually necessary
  # TODO I don't think the API is case sensitive, so convert all names to the
  # same case before testing uniqueness
  wdo_query <- purrr::map(wdo_query, unique)

  # Concatenate any options with length > 1 into comma separated strings
  purrr::map(wdo_query, stringr::str_flatten, collapse = ",")
}

construct_wdo_req <- function(wdo_query) {
  # Create a request object
  wdo_url <- "http://www.bom.gov.au/waterdata/services"
  wdo_req <- httr2::request(wdo_url)

  # Append query options
  wdo_req <- httr2::req_url_query(wdo_req, !!!wdo_query)

  # In case of an error, check for a message in the response body
  httr2::req_error(wdo_req, body = extract_body_error)
}

extract_body_error <- function(wdo_resp) {
  # Get the format of the error response
  # Note: this doesn't necessarily match format specified in the request
  format <- httr2::resp_content_type(wdo_resp)

  # Extract any messages found in the error response body
  # TODO test that this actually covers all error return types
  if (format == "application/json") {
    body_error <- httr2::resp_body_json(wdo_resp)$message
  } else if (format == "text/xml") {
    body_error <- xml2::xml_text(httr2::resp_body_xml(wdo_resp))
  } else if (format == "text/html") {
    body_error <- xml2::xml_text(httr2::resp_body_html(wdo_resp))
  } else {
    # TODO add a class to this error and find a way to test it
    body_error <- stringr::str_glue("Unexpected error format: {content_type}")
  }

  body_error
}
