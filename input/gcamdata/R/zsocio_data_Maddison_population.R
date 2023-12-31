# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_data_Maddison_population
#'
#' Dedicated data chunk to read \code{Maddison_population.csv} file.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Maddison_population}.
#' @details The \code{Maddison_population.csv} file has a blank column, which
#' generates a warning message in the general purpose \code{\link{load_csv_files}}
#' that we'd rather not suppress universally. We also need to skip a goofy initial couple of lines.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select
#' @author BBL
module_socio_data_Maddison_population <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(NULL)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("Maddison_population"))
  } else if(command == driver.MAKE) {

    deleteme <- year <- value <- Country <- NULL # silence package check

    fqfn <- find_csv_file("socioeconomics/Maddison_population", optional = FALSE, quiet = TRUE)
    cn <- c("Country", as.character(c(1500, 1600, 1700, 1820:2008)), "deleteme", "2030")
    ct <- paste0("c", paste(rep("d", length(cn) - 1), collapse = ""))

    readr::read_csv(fqfn, comment = COMMENT_CHAR, col_names = cn, col_types = ct, skip = 2) %>%
      select(-deleteme) %>%
      gather_years %>%
      # Remove all the blanks and "Total..." lines
      filter(!(substr(Country, 1, 5) == "Total" & Country != "Total Former USSR"),
             !is.na(Country)) %>%
      mutate(year = as.integer(year)) %>%
      add_title("Angus Maddison historical population by nation from 1500") %>%
      add_units("people") %>%
      add_comments(paste("Read from", gsub("^.*extdata", "extdata", fqfn))) %>%
      add_legacy_name("Maddison_population") %>%
      add_flags(FLAG_NO_OUTPUT) ->
      Maddison_population

    return_data(Maddison_population)
  } else {
    stop("Unknown command")
  }
}
