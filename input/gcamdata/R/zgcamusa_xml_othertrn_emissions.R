# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_othertrn_emissions_xml
#'
#' Construct XML data structure for \code{othertrn_emissions_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{othertrn_emissions_USA.xml}. The corresponding file in the
#' original data system was \code{batch_othertrn_emissions_USA_xml.R} (gcamusa XML).
module_gcamusa_othertrn_emissions_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L276.nonghg_othertrn_tech_coeff_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "othertrn_emissions_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L276.nonghg_othertrn_tech_coeff_USA <- get_data(all_data, "L276.nonghg_othertrn_tech_coeff_USA")

    # ===================================================

    # Produce outputs
    create_xml("othertrn_emissions_USA.xml") %>%
      add_xml_data(L276.nonghg_othertrn_tech_coeff_USA, "TrnInputEmissCoeff") %>%
      add_precursors("L276.nonghg_othertrn_tech_coeff_USA") ->
      othertrn_emissions_USA.xml

    return_data(othertrn_emissions_USA.xml)
  } else {
    stop("Unknown command")
  }
}
