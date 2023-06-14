# waterbom

`waterbom` is an R package for downloading water data compiled by the Australian Bureau of Meteorology (BOM).

The BOM makes much of their water data available on [Water Data Online](http://www.bom.gov.au/waterdata/) (WDO), however `waterbom` exposes additional functionality:

-   Download data from multiple stations in a single request.
-   Filter stations by name, location, owner, measured variables, and more.
-   Access additional time series, including flood warning data.
-   Ensure that your data analysis is reproducible.

## Design

The main user-facing functions are:

-   `get_station_list`
-   `get_parameter_list`
-   `get_timeseries_list`
-   `get_timeseries`

These functions always return a tibble, with default columns selected to match the WDO interface. Columns are not renamed, despite some unfortunate naming conventions. This is to ensure consistency between requests.

There are two more exported functions which may be useful to advanced users:

-   `get_bom_data` allows you to make an arbitrary data request.
-   `get_bom_response` is an advanced function which returns the response object with no processing. This can be useful for non-data API requests.

## Queries

Queries are made using key-value-pair requests. Most (but not all) of the error checking is left to the API, meaning that you can make bad requests if you want. The function documentation covers common use cases, however some query options are undocumented.

## Typical usage
