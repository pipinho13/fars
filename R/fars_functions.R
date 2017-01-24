#' Read a file using readr::read_csv suppressing its output
#'
#' This function reads csv files using the read_csv function from readr.
#' Its messages and progress bar is suppressed.
#' Entering a non-existent file
#' while raise an error.
#' @param filename (character) The file to load
#' @return A tbl_df
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Generate filenames for specific years corresponding to the naming scheme of the FARS
#'
#' This function generates one or more filenames corresponding to the naming scheme of the
#' US National Highway Traffic Safety Administration's Fatality Analysis
#' Reporting System.
#' @param year (numeric) The years for which the respective filenames should be generated
#' @return A character vector of filenames ending in .csv.bz2
#' @examples
#' make_filename(c(2013, 2014))
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Extract month numbers from FARS files of specific years
#'
#' This function will try to read in standard named FARS files from the
#' working directory using fars_read() and extract the month number of every observation.
#' Years for which no correspondingly named file can be found will raise an error.
#' @param years (numeric) A vector of years for which the month numbers should be returned
#' @return A tbl_df with columns MONTH and year
#' @importFrom dplyr %>%
#' @export
fars_read_years <- function(years) {
    lapply(years, function(year) {
        file <- make_filename(year)
        tryCatch({
            dat <- fars_read(file)
            dplyr::mutate_(dat, year = ~ year) %>%
                dplyr::select_(~ MONTH, ~ year)
        }, error = function(e) {
            warning("invalid year: ", year)
            return(NULL)
        })
    })
}

#' Get the number of observations per month from a FARS file
#'
#' A FARS file with standard naming is expected to be found in the working
#' directory.
#' @param years (numeric) A vector of years
#' @return A tbl_df with column MONTH and columns corresponding to years with
#' the number of observations per month per year.
#' @importFrom dplyr %>%
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by_(~ year, ~ MONTH) %>%
                dplyr::summarize_(n = ~ n()) %>%
                tidyr::spread_(key_col = "year", value_col = "n")
}

#' Draw a map of accidents of a specific state during a specific year
#'
#' Not suitable for plotting multiple years or multiple states. FARS files with
#' standard naming are expected to be found in the working directory. States
#' that can not be found in a file or state.num year combinations without
#' accidents will raise errors.
#' @param state.num (numeric) The state number
#' @param year (numeric) The year
#' @return NULL
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~ STATE == state.num)
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

