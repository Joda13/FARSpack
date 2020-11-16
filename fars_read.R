#' Reading and converting in FARS data into data frame
#'
#' The function reads a csv-file with data from the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System (FARS) into a tibble data frame
#' FARS is a nationwide census providing the American public yearly data
#' (available data: 2013 - 2015) regarding fatal injuries suffered in motor vehicle traffic crashes

#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df

#' @param filename A character string giving the name of the file

#' @return A data frame read from the csv-file, if file doesnt exist an error

#' @examples
#' ex_year <- 2013
#' data <- ex_year %>%
#'   make_filename %>%
#'   fars_read
#' head(data)
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Creation of csv-file name
#'
#' The function needs a year as input and produces a valid FARS filename.
#' It will not check whether the file actually exists, or if the year is reasonable.
#'
#' @param year A character string/integer giving the year of the input data
#'
#' @return A string with the data file name for a specific year
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reading of FARS years and months
#'
#' @param years A vector (of integers) giving a list of years
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr "%>%"
#'
#' @return A data frame including values in data by month or NULL if the year is invalid
#'
#' @examples
#' fars_read_years(c(2013, 2014))
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summary of FARS data
#'
#' The function summarizes accidents data by month and year over multiple input files
#'
#' @param years A vector (of integers) giving a list of years to summarize
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @return A tibble data frame including number of accidents summarized by month and year
#'
#' @examples
#' fars_summarize_years(c(2013, 2014))
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Map of accidents
#'
#' Visualization of a state map showing places (coordinates) of accidents in a specific year
#'
#' If the state number "state.num" is invalid "invalid STATE number: <state number>" is returned
#'
#' If there are no accidents for the state in this year "no accidents to plot" is returned
#'
#' Longitude (latitude) in plot is limited to 900 (90)
#'
#' @param state.num An integer giving the state code
#' @param year A character string/integer giving the year of the input data
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @return NULL
#'
#' @examples
#' fars_map_state(13,2014)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
