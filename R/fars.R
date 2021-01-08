#'Read csv file and return a tibble
#'
#'This function takes a cvs file, checks that it exists, and reads it, returning a tibble.
#'
#'It uses reader::read_csv and any warning messages from readr::read_csv will be suppressed
#'but the function will issue an error if the file doesn't exist
#'
#'@param filename The name of the file to read
#'
#'@return A tibble of the data in the file
#'
#'@importFrom readr read_csv
#'@importFrom tibble as_tibble
#'
#'@examples
#'fars_read(make_filename("2013"))
#'
#'@export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  tibble::as_tibble(data)
}

#'Create a string to use as a filename for bz2 compressed csv file for accident data in the Fatality Analysis Reporting System.
#'
#'This function takes the year and returns a string in the format accident_<year>.csv.bz2
#'
#'@param Year to use a suffix in the filename
#'
#'@return String in format accident_<year>/csv/bz2
#'
#'@examples
#'make_filename("2013")
#'
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  system.file("extdata", sprintf("accident_%d.csv.bz2", year), package="MSDRFars", mustWork=TRUE)
}

#'Reads data from the bz compressed accidents files that correspond to the list of years passed as a parameter.
#
#'Returns a list of tibbles with data for each month for each year requested.
#'Prints a warning if a requested year isn't found in the available data files from the Fatality Analysis Reporting System
#'
#'
#'@param years vector or list of years
#'
#'@return list of tibbles with data for each year from the file accidents_<year>.csv.bz2,
#'plus attributes for the data, such as the state, county and others'
#'
#'@importFrom dplyr mutate select
#'@importFrom rlang .data
#'
#'@examples
#'fars_read_years(c("2013","2015"))
#'
#'@export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(.data$MONTH, .data$year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
      return(NULL)
    })
  })
}

#'This function returns the total number of accidents by month and year
#'
#'This function takes in a list of years for which data should be summarized.
#'
#'@inheritParams fars_read_years
#'
#'@return Tibble with the summarized data by month with result for each year in its own column
#'
#'@importFrom dplyr group_by summarize n
#'@importFrom tidyr spread
#'@importFrom magrittr "%>%"
#'@importFrom rlang .data
#'
#'@examples
#'fars_summarize_years(c("2013","2015"))
#'
#'@export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(.data$year, .data$MONTH) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::spread(.data$year, n)
}


#'This function plots the locations of accidents for a given state in a given year on that state's map.
#'
#'It will issue a warning if the state number doesn't exist or if there is no data available for the requested
#'state and year
#'
#'
#'@param state.num The state number to plot data for
#'@param year The year to plot data for
#'
#'@return Nothing. Outputs a plot of the locations of accidents on a state map for a given year
#'
#'@importFrom dplyr filter
#'@importFrom maps map
#'@importFrom graphics points
#'
#'@examples
#'fars_map_state(1, "2013")
#'
#'@export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, data$STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(data.sub$LATITUDE, na.rm = TRUE),
              xlim = range(data.sub$LONGITUD, na.rm = TRUE))
    graphics::points(data.sub$LONGITUD, data.sub$LATITUDE, pch = 46)
  })
}
