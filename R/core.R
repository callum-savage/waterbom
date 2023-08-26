get_wdo_response <- function(format, request, ...) {
  # Pack all query options into a list
  wdo_query <- construct_wdo_query(format, request, ...)

  # Turn query list into a httr2 request object
  wdo_req <- construct_wdo_req(wdo_query)

  # Get a response from Water Data Online
  wdo_resp <- httr2::req_perform(wdo_req)

  # Return the response object
  wdo_resp
}

construct_wdo_query <- function(format, request, ...) {
  # Package dots into a list
  dot_options <- rlang::dots_list(
      ...,
      .ignore_empty = "trailing", # allow a trailing comma
      .homonyms = "error", # error if two args have the same name
      .check_assign = TRUE # warn if `<-` used in function call
    )

  # TODO error if any dots args are unnamed
  # I'm pretty sure they would break the query if left in
  # rlang::is_named() (TRUE for empty list, which is what I want)

  # Concatenate any vector options into comma separated strings
  dot_options <- purrr::map(dot_options, stringr::str_flatten_comma)

  # Assemble all query options into a list
  wdo_query <- rlang::list2(
    service = "kisters",
    type = "QueryServices",
    format = format,
    request = request,
    !!!dot_options
  )

  # TODO check that everything is a character
  # i.e. format and request

  wdo_query
}

construct_wdo_req <- function(wdo_query) {
  # Create a request object
  wdo_url <- "http://www.bom.gov.au/waterdata/services"
  wdo_req <- httr2::request(wdo_url)

  # Append query options
  wdo_req <- httr2::req_url_query(wdo_req, !!!wdo_query)

  # Provide a function to check for error messages in the response body
  # httr2 doesn't pick up on these messages by default
  wdo_req <- httr2::req_error(wdo_req, body = extract_body_error)

  # TODO consider adding in a 'times' argument to make repeated requests

  # Return the request object
  wdo_req
}

extract_body_error <- function(wdo_resp) {
  # Identify the format of the response
  # (json, xml, or html)
  format <- httr2::resp_content_type(wdo_resp)

  # Extract any messages found in the response body
  if (format == "application/json") {
    body_error <- httr2::resp_body_json(wdo_resp)$message
  } else if (format == "text/xml") {
    body_error <- xml2::xml_text(httr2::resp_body_xml(wdo_resp))
  } else if (format == "text/html") {
    body_error <- xml2::xml_text(httr2::resp_body_html(wdo_resp))
  } else {
    # TODO make this error message more informative
    body_error <- stringr::str_glue("Unexpected error format: {content_type}")
  }

  # Return the error message (a string)
  body_error
}
