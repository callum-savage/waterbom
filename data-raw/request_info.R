# This script imports the query specification for the Water Data Online API and
# simplifies it into a set of tables. The tables are saved as R/sysdata.rda and
# are available internally, but are not exported.

library(dplyr)
library(tidyr)

# Get request info --------------------------------------------------------

# Request query info from the API

request_info_resp <- get_wdo_response(
  format = "json",
  request = "getRequestInfo"
)

# Unpack the returned json response into a nested tibble

request_info <- request_info_resp |>
  httr2::resp_body_json() |>
  tibble() |>
  unnest_wider(1) |>
  select(Requests) |>
  unnest_longer(Requests) |>
  unnest_wider(Requests)

# Extract tables of query options -----------------------------------------

# Get descriptions for each request type

wdo_requests <- request_info |>
  select(Request, Description, Subdescription) |>
  janitor::clean_names()

# Other query options are nested more deply, and can be extracted using a helper
# function.
#
# `unpack_col` unpacks each nested column into a tidy tibble, and renames the
# column with a provided name

unpack_col <- function(request_info, col, newname) {
  request_info |>
    select(Request, {{ col }}) |>
    unnest_wider({{ col }}) |>
    select(!Description) |>
    unnest_longer(Content) |>
    unnest_wider(Content) |>
    select(!any_of("Content_id")) |>
    mutate(across(any_of("Description"), \(x) na_if(x, ""))) |>
    janitor::clean_names() |>
    rename("{{ newname }}" := name)
}

# Unpack columns into new tibbles, keeping request as a key. Some columns
# require a little extra processing

wdo_formats <- request_info |>
  unpack_col(Formats, format)

wdo_query_fields <- request_info |>
  unpack_col(QueryFields, query_field) |>
  rename(as_list = comma_separated_list) |>
  # Convert yes/no to TRUE/FALSE
  mutate(
    as_wildcard = stringr::str_detect(as_wildcard, "yes"),
    as_list = stringr::str_detect(as_list, "yes")
  )

wdo_optional_fields <- request_info |>
  unpack_col(Optionalfields, optional_field)

wdo_transformations <- request_info |>
  unpack_col(Transformations, transformation) |>
  # attributes, returnfields, and examples are listed more deeply
  mutate(across(where(is.list), ~ purrr::map(.x, unlist)))

wdo_date_formats <- request_info |>
  unpack_col(Dateformats, date_format)

wdo_return_fields <- request_info |>
  unpack_col(Returnfields, return_field)

# Identify content types --------------------------------------------------

# Each format has a returned content type which I need to know to process the
# response objects

# Select one representative request type per format

format_requests <- wdo_formats |>
  add_count(request) |>
  slice_max(n, by = format, with_ties = FALSE)

# Define test parameters

station_no <- "419093"
ts_id <- "343344010"
from <- "2020-01-01"
to <- "2020-01-02"
catchment_id <- 0

# Make a small request for each format and identify the content type. Only four
# requests are needed to cover all content types

get_content_type <- function(request, format) {
  if (request == "getStationList") {
    resp <- get_wdo_response(format, request, station_no = station_no)
  } else if (request == "getTimeseriesValues") {
    resp <- get_wdo_response(format, request, ts_id = ts_id)
  } else if (request == "getGraph") {
    resp <- get_wdo_response(format, request, ts_id = ts_id, from = from, to = to)
  } else if (request == "getCatchmentHierarchy") {
    resp <- get_wdo_response(format, request, catchment_id = 0)
  } else {
    return("Unknown")
  }
  httr2::resp_content_type(resp)
}

content_types <- format_requests |>
  rowwise() |>
  mutate(content_type = get_content_type(request, format)) |>
  distinct(format, content_type)

# Attach format types to wdo_formats

wdo_formats <- left_join(wdo_formats, content_types, join_by("format"))

# TODO identify the default format for each request
# TODO identify the default return fields for each request
# this probably only needs to be done for the core requests, or I could
# rewrite the above somehow

# Save data internally (not exported)

usethis::use_data(
  wdo_formats,
  wdo_query_fields,
  wdo_optional_fields,
  wdo_transformations,
  wdo_date_formats,
  wdo_requests,
  wdo_return_fields,
  internal = TRUE,
  overwrite = TRUE
)
