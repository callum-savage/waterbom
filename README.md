# waterbom (under construction).

`waterbom` is an R package for downloading water data from [Water Data Online](http://www.bom.gov.au/waterdata/), a data portal maintained by the Australian Bureau of Meteorology (BOM).

With `waterbom` you can:

-   download data from multiple stations in a single request
-   filter stations by name, location, data owner, and more
-   access time series which aren't available on Water Data Online
-   embed data download into reproducible data analysis scripts

## Design

Queries are made using key-value-pair requests. Most (but not all) of the error checking is left to the API, meaning that you can make bad requests if you want.

The main user-facing functions are:

-   `get_station_list()`
-   `get_parameter_list()`
-   `get_timeseries_list()`
-   `get_timeseries()`

There are two additional functions which may also be useful in some cases:

-   `get_bom_data()` allows you to make an arbitrary data request.
-   `get_bom_response()` returns the response object with no processing. This can be useful for non-data API requests.
