# `getRequestInfo` returns details about the different requests which can be
# made to the WISKI API:
#
#   - query fields
#   - formats
#   - return fields
#   - optional fields
#   - date formates
#   - transformations

# Download the JSON file contining request info and convert it into a tibble

request_info_json <- get_query_resp("getRequestInfo") |>
  httr2::resp_body_json()

request_info <- tibble::tibble(request_info = request_info_json) |>
  tidyr::unnest_wider(request_info) |>
  dplyr::select(Requests) |>
  tidyr::unnest_longer(Requests) |>
  tidyr::unnest_wider(Requests)

# Extract request descriptions

requests <- request_info |>
  dplyr::select(Request, Description, Subdescription)

# Most other information is nested in a consistent structure and can be
# extracted using a helper function

unpack_request_info <- function(request_info, list_col) {
  request_info |>
    dplyr::select(Request, {{ list_col }}) |>
    tidyr::unnest_wider({{ list_col }}) |>
    dplyr::select(!Description) |>
    tidyr::unnest_longer(Content) |>
    tidyr::unnest_wider(Content) |>
    dplyr::select(!any_of("Content_id"))
}

request_query_fields    <- unpack_request_info(request_info, QueryFields)
request_formats         <- unpack_request_info(request_info, Formats)
request_return_fields   <- unpack_request_info(request_info, Returnfields)
request_optional_fields <- unpack_request_info(request_info, Optionalfields)
request_date_formats    <- unpack_request_info(request_info, Dateformats)
request_transformations <- unpack_request_info(request_info, Transformations)

# `request_transformations` still contains nested information. These fields
# (attributes, returnfields, and examples) don't have a uniform structure so
# I'll just convert them to character vectors

unlist_cols <- function(col) {
  purrr::map(col, unlist)
}

request_transformations <- request_transformations |>
  dplyr::mutate(dplyr::across(dplyr::where(is.list), unlist_cols))

# Save data internally

usethis::use_data(
  requests,
  request_query_fields,
  request_formats,
  request_return_fields,
  request_optional_fields,
  request_date_formats,
  request_transformations,
  internal = TRUE
)
