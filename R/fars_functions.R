#' Read FARS data from single file
#'
#' @param filename Path of CSV containing FARS data
#'
#' @return A data frame containing data loaded from indicated filename.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples
#' \dontrun{
#' fars_read("dummy.csv")
#' }
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Generates filename
#'
#' @param year year to be indicated in filename
#'
#' @return A character string for the filename containing the year provided.
#'
#' @examples make_filename(2012)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' read FARS data from a vector containing the years
#'
#' @param years integer vector containing the years of FARS data to read
#'
#' @return a dataframe containing month and year of data
#' @export
#' @importFrom dplyr mutate_ select_
#' @importFrom magrittr %>%
#' @details This function fails if any single year provided is missing from the data.
#'
#' @examples fars_read_years(c(2012, 2013))
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat, year = ~year) %>%
        dplyr::select_(~MONTH, ~year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarise FARS data by year and month
#'
#' @inheritParams fars_read_years
#'
#' @return A data frame containing the year, month and count of data in each corresponding year/month
#' @importFrom dplyr bind_rows group_by_ summarize_
#' @importFrom tidyr spread_
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \donttest{
#' test <- fars_summarize_years(2013)
#' }
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_(~year, ~MONTH) %>%
    dplyr::summarize_(n = ~n()) %>%
    tidyr::spread_(~year, ~n)
}

#' Plots accidents for a given state and year
#'
#' @param state.num State number for plot
#' @param year Accident year for plot
#'
#' @return Nothing returned, produces a side effect of a plot of given US state and location of accident(s) for given year
#' @importFrom dplyr filter_
#' @importFrom graphics points
#' @importFrom maps map
#' @details This function will also not plot accidents where their longitudes are more than 900 or latitudes are more than 90.
#' @export
#'
#' @examples
#' \dontrun{
#' fars_map_state(50, 2012)
#' }
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~STATE == ~state.num)
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
