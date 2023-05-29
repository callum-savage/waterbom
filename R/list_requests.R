# TODO clean up names for internal data
# TODO save internal data with a different name

list_requests <- function() {
  dplyr::select(requests, request, description)
}

list_query_fields <- function(request = NULL) {
  if (!is.null(request)) {
    dplyr::filter(query_fields, request == {{ request }})
  } else {
    query_fields
  }
}

list_return_fields <- function(request = NULL) {
  if (!is.null(request)) {
    dplyr::filter(return_fields, request == {{ request }})
  } else {
    return_fields
  }
}

list_query_fields <- function(request = NULL) {
  if (!is.null(request)) {
    dplyr::filter(optional_fields, request == {{ request }})
  } else {
    optional_fields
  }
}
