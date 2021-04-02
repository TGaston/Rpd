#' Read data from csv file
#' @param filename A character string containing the name of the csv file to be read,
#'    which must be at the CURRENT directory.
#'
#' @return If the file exists, this function returns a data frame table. If the file doesn't
#'    exist it raises an error message.
#' @examples
#' \dontrun{
#' fars_read("accident_2014.csv.bz2") # file in current working directory
#' fars_read("data123.csv") # Error: file 'data123.csv' does not exist.
#' }
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


#' Generate the name of the file
#' @details This function uses the year (input) to generate the file name for the Fatality
#'    Analysis Reporting System (FARS) data file. It converts the year into an integer and
#'    then uses the \code{sprintf()} function to format the file name.
#'
#' @param year An integer or string indicating the year that generate the data file.
#' @examples
#' make_filename(2014) #generates "accident_2014.csv.bz2"
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Reads data for multiple years
#' @param years A vector of years.
#'
#' @return A list of FARS data for the years in the parameter \code{years}.
#' @examples
#' fars_read_years(c(2014, 2015, 2016))
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


#' Summarize data data for multiple years
#'
#' @details This function produces a "tidy" data frame of FARS data for multiple years. Data
#'    is summarized by each month of each year with each year in its own column. This function
#'    uses \code{\link{fars_read_years}} function to generate a list of month and year.
#'
#' @param years A vector of years.
#'
#' @return A dataframe with monthly number of accidents in each row and years as colunms.
#' @examples
#' fars_summarize_years(c(2014, 2015))
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plot the location of accidents in a given state and year
#'
#' @details This function takes the state number and a year as inputs and calls the supporting
#'    functions \code{make_filename()} and \code{fars_read()}.
#'
#' @param state.num An integer that represents the state number.
#' @param year An integer or string indicating the year.
#'
#' @return Plots a map of the state, where the fatalities are the dots on the map.
#' @examples
#' fars_map_state(56, 2013)
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
