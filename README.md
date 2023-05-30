# waterbom

`waterbom` lets you download water data compiled by the Australian Bureau of Meteorology. Requests are made to the WISKI API using key-value pairs.

## Design

-   The list of arguments used in the API request is a `query`.
-   Request functions start with `get_`, e.g. `get_timeseries()`
-   Every request function **must** have the argument `request` e.g. \`"getStationInfo"
-   `get_bom_data` always returns a tibble. Perhaps format should always be csv? Lower level functions can be used to get raw responses
-   waterbom leaves most of the error checking to the API and is permissive by design (i.e. you can make bad requests if you want)
-   Functions are stored in `request_funcs`, `request_wrappers`, or `ts_wrappers`

## Development Roadmap

-   [x] Expose the API to arbitrary queries
-   [x] Return a tidy dataset for valid queries
-   [x] Return an informative error messge (including WISKI messages) for invalid queries
-   [ ] Document the full set requests available to the user
-   [ ] Provide a function to convert dataset responses into queries
-   [ ] Create wrapper functions for common requests
