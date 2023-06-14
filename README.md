# waterbom

`waterbom` is an R package for downloading water data compiled by the Australian Bureau of Meteorology (BOM).

While the BOM makes much of their water data available on the excellent [Water Data Online](http://www.bom.gov.au/waterdata/) (WDO), it can be difficult to download data in a way which is efficient and reproducible. `waterbom` aims to fix this issue.

With `waterbom` you can:

-   get data from multiple stations in a single request
-   filter stations by name, location, owner, measured variables, and more
-   access additional time series, including flood warning data
-   ensure that your data analysis is reproducible

I recommend that you use WDO to find the stations you're interested in, and then use `waterbom` for actually downloading the timeseries data.

## Design

The main user-facing functions are:

-   `get_station_list()`
-   `get_parameter_list()`
-   `get_timeseries_list()`
-   `get_timeseries()`

These functions always return a `tibble`, with default columns selected to match the WDO interface. Columns are not renamed despite some unfortunate naming conventions used by the API. This is to ensure consistency between requests.

There are two additional functions which may be useful to advanced users:

-   `get_bom_data()` allows you to make an arbitrary data request.
-   `get_bom_response()` is an advanced function which returns the response object with no processing. This can be useful for non-data API requests.

## Queries

Queries are made using key-value-pair requests. Most (but not all) of the error checking is left to the API, meaning that you can make bad requests if you want. The function documentation covers common use cases, however some query options are undocumented.

## Typical usage
