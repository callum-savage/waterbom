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

## Todo

-   [ ] Basic functionality (all 'get\_' functions)
    -   [ ] Functions return data as provided by the API
    -   [ ] All arguments are documented
    -   [ ] Package passess checks without errors/comments/notes
-   [ ] Additional functionality
    -   [ ] Remove duplicates from requests (e.g. duplicate station numbers)
    -   [ ] Include tools for interpreting data quality and interpolation codes
    -   [ ] Make sure return fields always matche the request
        -   For instance, 'coverage' currently creates 'to' and 'from' in the response
        -   Optional fields do all sorts of weird things
    -   [ ] Provide shortcuts for data owner, state, regulation, and time series limits (and potentially other selected optional fields)
    -   [ ] Provide a function to widen parameter list
    -   [ ] Limit query scope to useful fields
-   [ ] Error checking
    -   [ ] Unify type conversions into a single function
    -   [ ] Consolidate error checks into a single function
    -   [ ] Warn if user is going to create a cross join (e.g. parametertype and object type can result in cross joins)
-   [ ] Documentation
    -   [ ] Add examples to all functions
    -   [ ] Document date formats
    -   [ ] Document supported scope
        -   [ ] Requests
        -   [ ] Formats
        -   [ ] Query fields
        -   [ ] Optional fields
        -   [ ] Return fields
    -   [ ] Document wildcards
    -   [ ] Create a function to return service capabilities
        -   [ ] Replace the existing `list_requests()` etc. functions
    -   [ ] Demonstrate typical usage in README
    -   [ ] Write a vignette demonstrating typical usage
-   [ ] Testing
-   [ ] Match the WDO interface
    -   [ ] Only return active stations, or similar, by default
-   [ ] Possible enhancements
    -   [ ] Consolidate optional fields and query fields for users
    -   [ ] Provide an option for returning the query (in list form and request)
    -   [ ] Allow users to filter by data owner, state, or water regulation
    -   [ ] Allow users to provide a shapefile for querying stations (turn it into a bbox and then filter the returned results)
    -   [ ] Make a general wildcard test function which is used across multiple fields (for `get_station_list`)
    -   [ ] Provide plotting functions
    -   [ ] Allow tibbles to be passed as a query object
        -   e.g. The response from `get_station_info`
    -   [ ] Tidy up function args to be snake_case, even when API differs
-   [ ] Submit to CRAN
