# waterbom

`waterbom` lets you download water data compiled by the Australian Bureau of Meteorology. Requests are made to the WISKI API using key-value pairs.

## Design

-   The list of arguments used in the API request is a `query`.
-   Request functions start with `get_`, e.g. `get_timeseries()`

## Development Roadmap

-   [ ] Expose the API to arbitrary queries made using a query constructor function
-   [ ] Return a tidy dataset for valid queries
-   [ ] Return an informative error messge (including WISKI messages) for invalid queries
-   [ ] Document the full set requests available to the user
-   [ ] Provide a function to convert dataset responses into queries
-   [ ] Create wrapper functions for common requests
