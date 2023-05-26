# TODO clean up names for internal data
# TODO save internal data with a different name

list_requests <- function(request_set = "recommended") {
  rlang::arg_match(request_set, c("recommended", "all"))
  request_list <- dplyr::select(requests, Request, Description)
  recommended_requests <- c(
    "getSiteList",
    "getStationList",
    "getParameterList",
    "getTimeseriesList",
    "getTimeseriesValues"
  )
  if (request_set == "recommended") {
    request_list <- dplyr::filter(request_list, Request %in% recommended_requests)
  }
  request_list
}

list_query_fields <- function(request = NULL) {
  query_fields_list <- dplyr::select(request_query_fields, Request, Name)
  if (!is.null(request)) {
    query_fields_list <- dplyr::filter(query_fields_list, Request %in% request)
  }
  query_fields_list
}
