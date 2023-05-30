get_station_list <- function(station_no = NULL,
                             station_name = NULL,
                             parametertype_name = NULL,
                             bbox = NULL,
                             ...,
                             returnfields = c("station_no",
                                              "station_name",
                                              "station_id",
                                              "station_latitude",
                                              "station_longitude")) {
  get_bom_data(
    request = "getStationList",
    station_no = station_no,
    station_name = station_name,
    parametertype_name = parametertype_name,
    bbox = bbox,
    ...,
    returnfields = returnfields
  )
}

get_parameter_list <- function(station_no = NULL,
                               station_name = NULL,
                               parametertype_name = NULL,
                               ...,
                               returnfields = c("station_no",
                                                "station_name",
                                                "station_id",
                                                "parametertype_id",
                                                "parametertype_name")) {
  get_bom_data(
    request = "getParameterList",
    station_no = station_no,
    station_name = station_name,
    parametertype_name = parametertype_name,
    ...,
    returnfields = returnfields
  )
}

