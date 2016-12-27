#' Load CSV file into a tibble
#'
#' This function reads CSV file defined by \code{filename}, a main argument of the function,
#' and returns a tibble (data.frame).
#'
#' @param filename A character string giving the path and name of the CSV file
#'
#' @return This function returns a tibble (data.frame) of the input file
#'
#' @note This function will give error if the file path is incorrect or file does not exist
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' fars_read("./data/accident_2013.csv.bz2")
#' accident_2014 <- fars_read("./data/accident_2014.csv.bz2")
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

#' Create a filename by the year of interest
#'
#' This function created a filename as accident_<YEAR>.csv.bz2, where <YEAR> is substituted
#' by the function input \code{year}.
#'
#' @param year A numeric value of the year. It should correspond to the available yearly records
#' from US National Highway Traffic Safety Administration's Fatality Analysis Reporting System.
#'
#' @return This function returns a character sting of the filename as accident_<YEAR>.csv.bz2,
#' where <YEAR> is substituted by the input argument.
#'
#' @note This function will give error if the input \code{year} is non-numeric or non-integer.
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Get months and years from the respective record year
#'
#' This function accepts a vector or list of years \code{years}, where the records from US
#' National Highway Traffic Safety Administration's Fatality Analysis Reporting System should
#' be available. It returns a list of the data.frames with two columns (MONTH, year).
#'
#' @param years A vector or list (numeric or integer) with the years of accident records.
#'
#' @return This function returns a list of tibbles (data.frames) from the respective years of
#' the records. This function returns a warning and NULL if the file(s) does not exist.
#'
#' @note This function will give error if the files are not located in the working directory.
#'
#' @importFrom dplyr mutate select
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(2013:2015)
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

#' Calculate a number of accidents per month and year
#'
#' This function accepts a vector or list of years \code{years}, where the records from US
#' National Highway Traffic Safety Administration's Fatality Analysis Reporting System should
#' be available. It calculates a number of accidents per month and year and returns a tibble
#' (data.frame) with months in rows and years in columns.
#'
#' @param years A vector or list (numeric or integer) with the years of accident records.
#'
#' @return This function returns a tibble (data.frame) with number of accidents per month and year
#' from the indicated years of records.
#'
#' @note This function will give error if the files are not located in the working directory or
#' if the input \code{year} is non-numeric or non-integer.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' fars_summarize_years(2013)
#' fars_summarize_years(2013:2015)
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Draws the accidents on the US map
#'
#' This function creates a plot with all incidents in specific US state (defined by \code{state.num})
#' per indicaed \code{year}. The \code{year} should correspond to the recorded analysis from US
#' National Highway Traffic Safety Administration's Fatality Analysis Reporting System.
#'
#' @param state.num A numeric or integer value of US state as defined in the FARS data.
#' @param year A numeric or integer value for the year of accident record.
#'
#' @return This function returns a plot with the accidents on US map. The accidents are selected
#' based on the input argument \code{state.num} and \code{year}.
#'
#' @note This function will give error if the file with indicated argument \code{year} is not
#' located in the working directory. It also gives error if the \code{state.num} is non-numeric
#' or non-integer value or does not correspond to any US state as defined in the FARS data.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' fars_map_state(1, 2013)
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
