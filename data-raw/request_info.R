# This script prepares internal package data describing possible requests, their
# parameters, and return fields

library(dplyr)
library(tidyr)

request_info_json <- get_bom_data("getRequestInfo") |>
  httr2::resp_body_json()

request_info <- tibble(request_info = request_info_json) |>
  unnest_wider(request_info) |>
  select(Requests) |>
  unnest_longer(Requests) |>
  unnest_wider(Requests)

# Extract request descriptions

requests <- request_info |>
  select(Request, Description, Subdescription) |>
  janitor::clean_names()

# Most other information is nested in a consistent structure and can be
# extracted using a helper function

unpack_col <- function(request_info, list_col, list_col_name) {
  request_info |>
    select(Request, {{ list_col }}) |>
    unnest_wider({{ list_col }}) |>
    select(!Description) |>
    unnest_longer(Content) |>
    unnest_wider(Content) |>
    select(!any_of("Content_id")) |>
    mutate(across(any_of("Description"), ~ na_if(.x, ""))) |>
    janitor::clean_names() |>
    rename("{{ list_col_name }}" := name)
}

query_fields <- unpack_col(request_info, QueryFields, query_field) |>
  rename(wildcard = as_wildcard) |>
  mutate(across(c(wildcard, comma_separated_list), ~ .x %in% c("yes", "(yes)")))

formats <- unpack_col(request_info, Formats, format)

return_fields <- unpack_col(request_info, Returnfields, return_field)

optional_fields <- unpack_col(request_info, Optionalfields, optional_field)

date_formats <- unpack_col(request_info, Dateformats, date_format)

transformations <- unpack_col(request_info, Transformations, transformation) |>
  mutate(across(where(is.list), ~ purrr::map(.x, unlist)))

# Save data internally. Not all tables are used at present

usethis::use_data(
  requests,
  query_fields,
  # formats,
  return_fields,
  optional_fields,
  # date_formats,
  # transformations,
  internal = TRUE,
  overwrite = TRUE
)
