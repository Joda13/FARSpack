
# FARS

The goal of fars.packageR is to read a csv-file with data from the US National Highway Traffic Safety
  Administrations Fatality Analysis Reporting System (FARS) into a tibble data frame
  FARS is a nationwide census providing the American public yearly data
  (available data: 2013 - 2015) regarding fatal injuries suffered in motor vehicle traffic crashes
  
  
## Date

Nov 16, 2020

## Installation

You can install the released version of fars.packageR here:
``` r
install.packages("fars.packageR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(fars.packageR)

## Visualization of a state map (id = 13) showing places (coordinates) of accidents in a specific year (year = 2013)
fars_map_state(13,2013)

```

