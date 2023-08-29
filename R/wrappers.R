convert_wdo_types <- function(wdo_data) {
  # Specify columns to be converted
  numeric_cols <- c("station_latitude", "station_longitude")
  integer_cols <- c("station_id")
  timestamp_cols <- c("Timestamp", "from", "to")

  # Apply conversions to all matched columns
  dplyr::mutate(
    wdo_data,
    dplyr::across(dplyr::any_of(numeric_cols), as.numeric),
    dplyr::across(dplyr::any_of(integer_cols), as.integer),
    dplyr::across(dplyr::any_of(timestamp_cols), lubridate::as_datetime)
  )
}

get_timeseries_values <- function(ts_id,
                                  ...,
                                  from = NULL,
                                  to = NULL,
                                  timezone = "UTC",
                                  md_returnfields = c("ts_id", "station_no"),
                                  returnfields = c("Timestamp", "Value", "Quality Code", "Interpolation Type")) {
  ts_resp <- get_wdo_response(
    format = "dajson",
    request = "getTimeseriesValues",
    ts_id = ts_id,
    from = from,
    to = to,
    timezone = timezone,
    metadata = "true",
    md_returnfields = md_returnfields,
    returnfields = returnfields,
    ...
  )

  ts_body <- httr2::resp_body_json(ts_resp)

  # TODO move unpacking to its own function
  # TODO move column renaming to its own function

  # Unpack json response into a nested tibble with one row per timeseries
  ts1 <- tibble::tibble("ts_body" = ts_body)

  # Expand ts_body into columns (timeseries is still nested)
  ts2 <- tidyr::unnest_wider(ts1, col = "ts_body")

  # Select columns which will be returned
  # The timeseries is returned in a column called "data"
  ts3 <- dplyr::select(ts2, dplyr::any_of(c(md_returnfields, "data")))

  # Unpack each timestep into a row. Empty timeseries result in an empty row.
  ts4 <- tidyr::unnest_longer(ts3, col = "data", keep_empty = TRUE)

  # Expand each timestep into multiple columns
  ts5 <- tidyr::unnest_wider(ts4, col = "data", names_sep = "_")

  # Extract the correct timeseries names
  newnames <- unlist(stringr::str_split(ts2$columns[[1]], ","))

  # Identify the temporary timeseries names (of the form data_1, data_2, etc.)
  oldnames <- stringr::str_subset(names(ts5), "^data_\\d+$")

  # Replace old names with the new
  names(ts5[oldnames]) <- newnames

  # Apply type conversions
  convert_wdo_types(ts5)
}

get_wdo_list <- function(request, ..., returnfields = NULL) {
  # TODO check that request is a list request
  # probably just with a regex

  # Get response in csv format
  wdo_list_resp <- get_wdo_response(format = "csv",
                                    request = request,
                                    returnfields = returnfields,
                                    ...)

  # Extract response body
  wdo_list_body <- httr2::resp_body_string(wdo_list_resp)

  # Convert csv body into a tibble
  wdo_list <- readr::read_delim(
    wdo_list_body,
    delim = ";",
    col_types = readr::cols(.default = "c"),
    progress = FALSE
  )

  # Apply column type conversions
  convert_wdo_types(wdo_list)
}

get_station_list <- function(...,
                             station_no = NULL,
                             station_name = NULL,
                             parametertype_name = "*",
                             bbox = NULL,
                             returnfields = c(
                               "station_no",
                               "station_name",
                               "station_latitude",
                               "station_longitude"
                             )) {
  get_wdo_list(
    request = "getStationList",
    station_no = station_no,
    station_name = station_name,
    parametertype_name = parametertype_name,
    bbox = bbox,
    returnfields = returnfields,
    ...
  )
}

get_parameter_list <- function(...,
                               station_no = NULL,
                               parametertype_name = "*",
                               returnfields = c(
                                 "station_no",
                                 "station_name",
                                 "parametertype_name"
                               )) {
  get_wdo_list(
    request = "getParameterList",
    station_no = station_no,
    parametertype_name = parametertype_name,
    returnfields = returnfields,
    ...
  )
}

get_timeseries_list <- function(...,
                                station_no = NULL,
                                parametertype_name = NULL,
                                ts_id = NULL,
                                ts_name = NULL,
                                returnfields = c(
                                  "station_no",
                                  "station_name",
                                  "parametertype_name",
                                  "ts_id",
                                  "ts_name"
                                )) {
  ts_list <- get_wdo_list(
    request = "getTimeseriesList",
    station_no = station_no,
    parametertype_name = parametertype_name,
    ts_id = ts_id,
    ts_name = ts_name,
    returnfields = returnfields,
    ...
  )

  # Sort output by station, parameter, and ts_id
  # TODO: It would be better to sort by ts_id descending
  sort_cols <- c("station_id", "station_no", "station_name",
                 "parametertype_name", "ts_id")
  dplyr::arrange(ts_list, dplyr::pick(dplyr::any_of(sort_cols)))
}
