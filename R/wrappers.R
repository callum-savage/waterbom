convert_wdo_types <- function(wdo_data) {
  # TODO test that this converts datetimes correctly

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


unpack_ts <- function(ts_json, md_returnfields) {
  # Identify metadata values
  ts_metadata <- ts_json[md_returnfields]

  # Identify column names of the returned timeseries
  ts_colnames <- stringr::str_split_1(ts_json$columns, ",")

  # Convert the timeseries json into a tibble
  ts_rows <- tibble::tibble("ts_col" = ts_json$data)
  ts <- tidyr::unnest_wider(ts_rows, ts_col, names_sep = "_")

  # Apply the correct column names to the timeseries
  names(ts) <- ts_colnames

  # Return the timeseries, with metadata columns as a key
  tibble::tibble(!!!ts_metadata, ts)
}


get_timeseries_values <- function(ts_id,
                                  ...,
                                  from = NULL,
                                  to = NULL,
                                  timezone = "UTC",
                                  md_returnfields = c("ts_id", "station_no"),
                                  returnfields = c("Timestamp", "Value", "Quality Code", "Interpolation Type")) {
  ts_resp <- get_wdo_response(
    format = "dajson", # data array json
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
  # TODO consider allowing the return of a nested tibble
  ts_resp_body <- httr2::resp_body_json(ts_resp)

  # Unpack each timeseries separately into a list of tibbles
  ts_list <- purrr::map(ts_resp_body, unpack_ts, md_returnfields)

  # Combine all timeseries into a single tibble
  ts <- purrr::list_rbind(ts_list)

  # Apply type conversions
  convert_wdo_types(ts)
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
  # TODO identify sensible defaults for an output similar to WDO
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
  # TODO identify sensible defaults
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
  # TODO identify sensible defaults
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
