# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_resources_SSP_xml
#'
#' Construct XML data structure for all the \code{resources_SSP.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resources_SSP1.xml}, \code{resources_SSP2.xml}, \code{resources_SSP3.xml}, \code{resources_SSP4.xml}, \code{resources_SSP5.xml}.
module_energy_resources_SSP_xml <- function(command, ...) {

  SSP_nums <- 1:5

  if(command == driver.DECLARE_INPUTS) {
    return(c(paste0("L210.RsrcTechChange_SSP", SSP_nums),
             paste0("L210.RsrcEnvironCost_SSP", SSP_nums)))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resources_SSP1.xml",
             XML = "resources_SSP2.xml",
             XML = "resources_SSP3.xml",
             XML = "resources_SSP4.xml",
             XML = "resources_SSP5.xml"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    L210.RsrcEnvironCost_SSP1 <- L210.RsrcEnvironCost_SSP2 <- L210.RsrcEnvironCost_SSP3 <-
      L210.RsrcEnvironCost_SSP4 <- L210.RsrcEnvironCost_SSP5 <- L210.RsrcTechChange_SSP1 <-
      L210.RsrcTechChange_SSP2 <- L210.RsrcTechChange_SSP3 <-  L210.RsrcTechChange_SSP4 <-
      L210.RsrcTechChange_SSP5 <- resources_SSP1.xml <- resources_SSP2.xml <- resources_SSP3.xml <-
      resources_SSP4.xml <- resources_SSP5.xml <- NULL

    all_data <- list(...)[[1]]

    for(i in SSP_nums) {
      # Load required inputs
      L210.RsrcTechChange_SSP <- get_data(all_data, paste0("L210.RsrcTechChange_SSP", i))
      L210.RsrcEnvironCost_SSP <- get_data(all_data, paste0("L210.RsrcEnvironCost_SSP", i))

      xmlfn <- paste0("resources_SSP", i, '.xml')

      create_xml(xmlfn) %>%
        add_node_equiv_xml("subresource") %>%
        add_xml_data(L210.RsrcEnvironCost_SSP, "ResReserveTechCost") %>%
        add_xml_data(L210.RsrcTechChange_SSP, "RsrcTechChange") %>%
        add_precursors(paste0("L210.RsrcTechChange_SSP", i), paste0("L210.RsrcEnvironCost_SSP", i)) ->
        xml_obj

      # Assign output to output name
      assign(xmlfn, xml_obj)
    }

    return_data(resources_SSP1.xml, resources_SSP2.xml, resources_SSP3.xml, resources_SSP4.xml, resources_SSP5.xml)
  } else {
    stop("Unknown command")
  }
}
