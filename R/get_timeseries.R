#' Get a list of timeseries available at gauging stations
#'
#' `get_timeseries_list` queries the BOM API and returns a table of timeseries
#' which can be downloaded for one or more gauging stations. The query can be
#' narrowed to specific timeseries, parameters, or stations. If no query filters
#' are specified the API will return an error.
#'
#' @inherit get_bom_data params return
#' @inherit get_station_list params
#'
#' @param ts_id Optionally, a specific time series. The id specifies both the
#'   time series product and the station
#' @param ts_name Optionally, the name of a specific time series
#'
#' @export
#'
#' @examples
#' get_timeseries_list(station_no = 609017)
get_timeseries_list <- function(station_no = NULL,
                                parametertype_name = NULL,
                                ts_id = NULL,
                                ts_name = NULL,
                                ...,
                                returnfields = c(
                                  "station_no",
                                  "station_name",
                                  "parametertype_name",
                                  "ts_id",
                                  "ts_name"
                                )) {
  ts_list <- get_bom_data(
    request = "getTimeseriesList",
    station_no = station_no,
    parametertype_name = parametertype_name,
    ts_id = ts_id,
    ts_name = ts_name,
    ...,
    returnfields = returnfields
  )
  # Sort output by parameter for readability
  sort_cols <- c("station_no", "station_name", "parametertype_name", "ts_name")
  ts_list <- dplyr::arrange(ts_list, dplyr::across(dplyr::any_of(sort_cols)))
  # Convert datetimes
  datetime_cols <- c("from", "to")
  dplyr::mutate(
    ts_list,
    dplyr::across(dplyr::any_of(datetime_cols), lubridate::as_datetime)
  )
}

# period = "complete"
# ts_id only really required in metadata if multiple ts_id provided
# get_timeseries_values(ts_id = 83527010, from = "2020-01-01", to = "2020-01-02")
# get_timeseries_values(ts_id = c(169408010, 197867010), from = "2020-01-01", to = "2020-01-05")
# TODO currently it's too fussy about ts_id - other metadata options should be fine too
# TODO don't worry about renaming columns as ts_list_col etc, use use "data" in quotes
# Also, column renaming code is inelegant

# TODO test interplay between to and from
# TODO document timestamp formats
# TODO test and document timezones

#' Download a specific timeseries
#'
#' @inherit get_bom_data params return
#'
#' @param ts_id One or more timeseries ids. Each `ts_id` identifies both the
#'   time series product and the station. Use ['get_timeseries_list'] to find a
#'   timeseries id.
#' @param from,to Optionally, timestamps defining the the start and end points
#'   of the requested timeseries. `from` and `to` default to the begining and
#'   end of the time series respectively. If no time is specified (e.g.
#'   YYYY-MM-DD format) then values will be returned for the whole day.
#' @param timezone The timezone in which to return time stamps. Defaults to UTC.
#'   Note that stations record measurements in non-daylight savings local time.
#' @param md_returnfields Optionally, additional metadata fields to include in
#'   the response. This is important when requesting multiple time series as the
#'   columns will otherwise be identical.
#'
#' @return A tibble with columns matching `md_returnfields` + `returnfields`.
#' @export
#'
#' @examples
#' get_timeseries_values(ts_id = 83527010, from = "2021-01-01", to = "2021-01-07")
get_timeseries_values <- function(ts_id,
                                  from = NULL,
                                  to = NULL,
                                  timezone = "UTC",
                                  ...,
                                  md_returnfields = c("ts_id", "station_no"),
                                  returnfields = c("Timestamp", "Value", "Quality Code", "Interpolation Type")) {
  resp <- get_bom_response(
    format = "json",
    request = "getTimeseriesValues",
    ts_id = ts_id,
    from = from,
    to = to,
    timezone = timezone,
    ...,
    metadata = "true",
    md_returnfields = md_returnfields,
    returnfields = returnfields
  )
  # Extract timeseries data from response
  ts_data <- tibble::tibble("ts_data" = httr2::resp_body_json(resp))
  ts_data <- tidyr::unnest_wider(ts_data, "ts_data")
  ts <- dplyr::select(ts_data, dplyr::any_of(c(md_returnfields, "data")))
  ts <- tidyr::unnest_longer(ts, "data")
  ts <- tidyr::unnest_wider(ts, "data", names_sep = "_")
  # Apply column names (clumsily)
  columns <- unlist(stringr::str_split(ts_data$columns[[1]], ","))
  names(ts)[stringr::str_detect(names(ts), "^data_\\d+$")] <- columns
  # Convert timestamp character vector to datetime
  ts$Timestamp <- lubridate::as_datetime(ts$Timestamp, tz = timezone)
  ts
}

#' Download a timeseries for a specific station
#'
#' @param station_no
#' @param ts_name
#' @param parametertype_name
#' @param from
#' @param to
#' @param timezone
#' @param ...
#' @param returnfields
#'
#' @return
#' @export
#'
#' @examples
get_timeseries <- function(station_no = NULL,
                           ts_name = NULL,
                           parametertype_name = NULL,
                           from = NULL,
                           to = NULL,
                           timezone = "UTC",
                           ...,
                           returnfields = c("Timestamp", "Value", "Quality Code", "Interpolation Type")
                           ) {
  ts_list <- get_timeseries_list(
      station_no = station_no,
      parametertype_name = parametertype_name,
      ts_name = ts_name,
      returnfields = c("station_no", "parametertype_name", "ts_id", "ts_name")
    )

  get_timeseries_values(
    ts_id = ts_list$ts_id,
    from = from,
    to = to,
    timezone = timezone,
    ...,
    returnfields = c("Timestamp", "Value", "Quality Code", "Interpolation Type"),
    md_returnfields = c("station_no", "ts_name", "parametertype_name")
  )
}

# station_no = c("425004", "423005")
# from = "2020-01-01"
# to = "2020-01-31"
# ts_name = "DMQaQc.Merged.AsStored.1"
# parametertype_name = "Water Course Level"
#
# get_timeseries_list()

get_timeseries(
  station_no = c("425004", "423005"),
  from = "2020-01-01",
  to = "2020-01-31",
  ts_name = "DMQaQc.Merged.AsStored.1",
  parametertype_name = "Water Course Level"
)
