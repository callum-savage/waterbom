# Prepare tables describing query options for the Water Data Online API

library(dplyr)
library(tidyr)

# Get request information from the API

request_info_resp <- get_wdo_response(
  format = "json",
  request = "getRequestInfo"
)

# Unpack json list into a nested tibble

request_info <- request_info_resp |>
  httr2::resp_body_json() |>
  tibble() |>
  unnest_wider(1) |>
  select(Requests) |>
  unnest_longer(Requests) |>
  unnest_wider(Requests)

# Extract request descriptions

wdo_requests <- request_info |>
  select(Request, Description, Subdescription) |>
  janitor::clean_names()

# Define a helper function for unpacking each column into a separate tibble

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

# Unpack columns into new tibbles, keeping request as a key

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
