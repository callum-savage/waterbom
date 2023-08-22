# waterbom (under construction).

`waterbom` is an R package for downloading water data from [Water Data Online](http://www.bom.gov.au/waterdata/), a data portal maintained by the Australian Bureau of Meteorology (BOM).

With `waterbom` you can:

-   download data from multiple stations in a single request
-   filter stations by name, location, data owner, and more
-   access time series which aren't available on WDO, including flood warning data
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

## Roadmap

### Basic functionality

-   [ ] Identify supported package scope:
    -   [x] Requests
    -   [x] Formats
    -   [ ] Query fields (and optional fields)
    -   [ ] Return fields
-   [ ] Reach test coverage for supported scope
-   [ ] Document function options and capabilities
-   [ ] Provide a list object containing the service capabilities (recommended and all) (as get_request_info)
-   [ ] Create a vignette demonstrating typical usage
-   [ ] Implement time and date formats (including only start and end)
-   [ ] Provide an option for returning the query (in list form and request)
-   [ ] Provide shortcuts for data owner, state, regulation, and time series limits (and potentially other selected optional fields)

### Enhancements

-   [ ] Implement optional fields in full
-   [ ] Match the WDO interface in terms of stations and parameters returned by default
-   [ ] Allow users to filter by data owner, state, and water regulation
-   [ ] Include tools for interpreting data quality and interpolation codes
-   [ ] Create a function for downloading station info listing parameters at the same time
-   [ ] Allow users to provide a shapefile for querying stations (turn it into a bbox and then filter the returned results)
-   [ ] Allow users to make wildcard requests across all text fields
-   [ ] Write a function which takes a tibble and extracts a query object
-   [ ] Submit waterbom to CRAN
-   [ ] Provide plotting functions
-   [ ] Provide a link for downloading rating curve data
