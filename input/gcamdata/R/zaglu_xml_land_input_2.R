# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_land_input_2_xml
#'
#' Construct XML data structure for \code{land_input_2.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{land_input_2.xml}. The corresponding file in the
#' original data system was \code{batch_land_input_2.xml.R} (aglu XML).
module_aglu_land_input_2_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L222.LN2_Logit",
              "L222.LN2_HistUnmgdAllocation",
              "L222.LN2_UnmgdAllocation",
              "L222.LN2_HistMgdAllocation",
              "L222.LN2_MgdAllocation",
              "L222.LN2_UnmgdCarbon",
              "L222.LN2_MgdCarbon"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "land_input_2.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L222.LN2_Logit <- get_data(all_data, "L222.LN2_Logit")
    L222.LN2_HistUnmgdAllocation <- get_data(all_data, "L222.LN2_HistUnmgdAllocation")
    L222.LN2_UnmgdAllocation <- get_data(all_data, "L222.LN2_UnmgdAllocation")
    L222.LN2_HistMgdAllocation <- get_data(all_data, "L222.LN2_HistMgdAllocation")
    L222.LN2_MgdAllocation <- get_data(all_data, "L222.LN2_MgdAllocation")
    L222.LN2_UnmgdCarbon <- get_data(all_data, "L222.LN2_UnmgdCarbon")
    L222.LN2_MgdCarbon <- get_data(all_data, "L222.LN2_MgdCarbon")

    # ===================================================

    # Produce outputs
    create_xml("land_input_2.xml") %>%
      add_logit_tables_xml(L222.LN2_Logit, "LN2_Logit") %>%
      add_xml_data(L222.LN2_HistUnmgdAllocation, "LN2_HistUnmgdAllocation") %>%
      add_xml_data(L222.LN2_UnmgdAllocation, "LN2_UnmgdAllocation") %>%
      add_xml_data(L222.LN2_HistMgdAllocation, "LN2_HistMgdAllocation") %>%
      add_xml_data(L222.LN2_MgdAllocation, "LN2_MgdAllocation") %>%
      add_xml_data(L222.LN2_UnmgdCarbon, "LN2_UnmgdCarbon") %>%
      add_xml_data(L222.LN2_MgdCarbon, "LN2_MgdCarbon") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L222.LN2_Logit", "L222.LN2_HistUnmgdAllocation", "L222.LN2_UnmgdAllocation", "L222.LN2_HistMgdAllocation", "L222.LN2_MgdAllocation", "L222.LN2_UnmgdCarbon", "L222.LN2_MgdCarbon") ->
      land_input_2.xml

    return_data(land_input_2.xml)
  } else {
    stop("Unknown command")
  }
}
