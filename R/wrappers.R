convert_wdo_types <- function(wdo_data) {
  # Specify columns to be converted
  dbl_cols <- c("station_latitude", "station_longitude", "Value")
  int_cols <- c("station_id")
  dttm_cols <- c("Timestamp", "from", "to")
  fct_cols <- c("Quality Code", "Interpolation Type")
  # Make sure everything else is a character
  chr_cols <- setdiff(names(wdo_data), c(dbl_cols, int_cols, dttm_cols, fct_cols))

  # Apply conversions to all matched columns
  dplyr::mutate(
    wdo_data,
    dplyr::across(dplyr::any_of(dbl_cols), as.numeric),
    dplyr::across(dplyr::any_of(int_cols), as.integer),
    dplyr::across(dplyr::any_of(dttm_cols), lubridate::as_datetime),
    dplyr::across(dplyr::any_of(fct_cols), forcats::as_factor),
    dplyr::across(dplyr::all_of(chr_cols), as.character)
  )
}

get_wdo_list <- function(request, ..., .return = c("response", "request", "url", "query", "dry_run")) {
  # This function supports 'getList' requests which can return a csv
  # Not all of these requests are useful, but they will work
  list_requests <- c(
    "getStationList",
    "getParameterList",
    "getTimeseriesList",
    "getSiteList",
    "getParameterTypeList",
    "getTimeseriesTypeList",
    "getGroupList",
    "getCatchmentList",
    "getRiverList",
    "getGraphTemplateList"
  )
  rlang::arg_match(request, list_requests)

  # Get response as a csv
  query_options <- rlang::list2(
    format = "csv",
    request = request,
    ...,
    .return = .return
  )
  wdo_list_resp <- get_wdo_response(!!!query_options)
  wdo_list_body <- httr2::resp_body_string(wdo_list_resp)

  wdo_list <- readr::read_delim(
    wdo_list_body,
    delim = ";",
    col_types = readr::cols(.default = "c"),
    progress = FALSE
  )

  # Tidy up output
  wdo_list <- dplyr::distinct(wdo_list)
  convert_wdo_types(wdo_list)
}

# wdo_query_fields |> dplyr::filter(request == "getStationList")
# bbox uses 'global' crs (see crs in wdo_optional_fields)
# Need to think about handling of custom attributes
get_station_list <- function(...,
                             station_no = NULL,
                             station_id = NULL,
                             station_name = NULL,
                             parametertype_name = "*",
                             bbox = NULL,
                             returnfields = c(
                               "station_id",
                               "station_no",
                               "station_name",
                               "station_latitude",
                               "station_longitude",
                               "parametertype_name"
                             )) {

  get_wdo_list(
    request = "getStationList",
    station_no = station_no,
    station_name = station_name,
    parametertype_name = parametertype_name,
    bbox = bbox,
    returnfields = returnfields,
    ...,
    flatten = "true" # Ensure that only one row is returned per station id
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
  sort_cols <- c("station_id", "station_no", "station_name",
                 "parametertype_name", "ts_id")
  dplyr::arrange(ts_list, dplyr::pick(dplyr::any_of(sort_cols)))
}

unpack_timeseries <- function(ts_json, md_returnfields) {
  # Identify metadata values
  ts_metadata <- ts_json[md_returnfields]

  # Identify column names of the returned timeseries
  ts_colnames <- stringr::str_split_1(ts_json$columns, ",")

  # Convert the timeseries json into a tibble
  if (length(ts_json$data) == 0) {
    # Construct an empty tibble when no data returned
    ts <- tibble::tibble(!!!rlang::set_names(ts_colnames), .rows = 0)
  } else {
    # Else unpack the json list
    ts_rows <- tibble::tibble("ts_col" = ts_json$data)
    ts <- tidyr::unnest_wider(ts_rows, "ts_col", names_sep = "_")
    ts <- rlang::set_names(ts, ts_colnames)
  }

  # Return the timeseries, with metadata columns as a key
  tibble::tibble(!!!ts_metadata, ts)
}

get_timeseries_values <- function(ts_id,
                                  ...,
                                  from = NULL,
                                  to = NULL,
                                  timezone = "UTC",
                                  md_returnfields = c("ts_id", "station_no"),
                                  returnfields = c(
                                    "Timestamp", "Value", "Quality Code",
                                    "Interpolation Type")) {
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
  ts_resp_body <- httr2::resp_body_json(ts_resp)

  # Unpack each timeseries separately into a list of tibbles
  ts_list <- purrr::map(ts_resp_body, unpack_timeseries, md_returnfields)

  # Combine all timeseries into a single tibble
  ts <- purrr::list_rbind(ts_list)

  # Apply type conversions
  convert_wdo_types(ts)
}
