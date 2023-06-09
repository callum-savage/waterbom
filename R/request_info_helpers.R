#' List request info
#'
#' @param request Optional request type to filter the response
#'
#' @returns A tibble
#' @export
#'
#' @examples
#' list_requests()
#'
#' list_query_fields("getStationList")
list_requests <- function() {
  dplyr::select(requests, "request", "description")
}

#' @rdname list_requests
#' @export
list_query_fields <- function(request = NULL) {
  if (!is.null(request)) {
    dplyr::filter(query_fields, request == {{ request }})
  } else {
    query_fields
  }
}

#' @rdname list_requests
#' @export
list_return_fields <- function(request = NULL) {
  if (!is.null(request)) {
    dplyr::filter(return_fields, request == {{ request }})
  } else {
    return_fields
  }
}

#' @rdname list_requests
#' @export
list_optional_fields <- function(request = NULL) {
  if (!is.null(request)) {
    dplyr::filter(optional_fields, request == {{ request }})
  } else {
    optional_fields
  }
}

#' @rdname list_requests
#' @export
list_formats <- function(request = NULL) {
  if (!is.null(request)) {
    dplyr::filter(formats, request == {{ request }})
  } else {
    formats
  }
}

#' @rdname list_requests
#' @export
list_parameters <- function() {
  c(
    "Dry Air Temperature",
    "Relative Humidity",
    "Wind Speed",
    "Electrical Conductivity At 25C",
    "Turbidity",
    "pH",
    "Water Temperature",
    "Ground Water Level",
    "Water Course Level",
    "Water Course Discharge",
    "Storage Level",
    "Storage Volume",
    "Rainfall",
    "Evaporation"
  )
}
