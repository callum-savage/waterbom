# If you already know ts_id, use get_timeseries_values
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
    #...,
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

# get_timeseries(
#   station_no = c("425004", "423005"),
#   from = "2020-01-01",
#   to = "2020-01-31",
#   ts_name = "DMQaQc.Merged.AsStored.1",
#   parametertype_name = "Water Course Level"
# )
