request_info <- function() {
  # TODO only return supported requests by default
  c(wdo_requests$request, "getRequestInfo")
}
