# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_Cstorage_xml
#'
#' Construct XML data structure for \code{Cstorage_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Cstorage_USA.xml}. The corresponding file in the
#' original data system was \code{batch_Cstorage_USA_xml.R} (gcamusa XML).
module_gcamusa_Cstorage_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L261.DeleteRsrc_USAC",
             "L261.DeleteSubsector_USAC",
             "L261.Rsrc_FERC",
             "L261.RsrcCurves_FERC",
             "L261.Supplysector_C_USA",
             "L261.SubsectorLogit_C_USA",
             "L261.SubsectorShrwtFllt_C_USA",
             "L261.StubTech_C_USA",
             "L261.StubTechMarket_C_USA",
             "L261.ResTechShrwt_C_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "Cstorage_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L261.DeleteRsrc_USAC <- get_data(all_data, "L261.DeleteRsrc_USAC")
    L261.DeleteSubsector_USAC <- get_data(all_data, "L261.DeleteSubsector_USAC")
    L261.Rsrc_FERC <- get_data(all_data, "L261.Rsrc_FERC")
    L261.RsrcCurves_FERC <- get_data(all_data, "L261.RsrcCurves_FERC")
    L261.Supplysector_C_USA <- get_data(all_data, "L261.Supplysector_C_USA")
    L261.SubsectorLogit_C_USA <- get_data(all_data, "L261.SubsectorLogit_C_USA")
    L261.SubsectorShrwtFllt_C_USA <- get_data(all_data, "L261.SubsectorShrwtFllt_C_USA")
    L261.StubTech_C_USA <- get_data(all_data, "L261.StubTech_C_USA")
    L261.StubTechMarket_C_USA <- get_data(all_data, "L261.StubTechMarket_C_USA")
    L261.ResTechShrwt_C_USA <- get_data(all_data, "L261.ResTechShrwt_C_USA")

    # ===================================================

    # Produce outputs
    create_xml("Cstorage_USA.xml") %>%
      add_xml_data(L261.DeleteRsrc_USAC, "DeleteRsrc") %>%
      add_xml_data(L261.DeleteSubsector_USAC, "DeleteSubsector") %>%
      add_xml_data(L261.Rsrc_FERC, "Rsrc") %>%
      add_xml_data(L261.RsrcCurves_FERC, "RsrcCurves") %>%
      add_xml_data(L261.ResTechShrwt_C_USA, "ResTechShrwt") %>%
      add_logit_tables_xml(L261.Supplysector_C_USA, "Supplysector") %>%
      add_logit_tables_xml(L261.SubsectorLogit_C_USA, "SubsectorLogit") %>%
      add_xml_data(L261.SubsectorShrwtFllt_C_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L261.StubTech_C_USA, "StubTech") %>%
      add_xml_data(L261.StubTechMarket_C_USA, "StubTechMarket") %>%
      add_precursors("L261.DeleteRsrc_USAC", "L261.DeleteSubsector_USAC", "L261.Rsrc_FERC", "L261.RsrcCurves_FERC", "L261.Supplysector_C_USA", "L261.SubsectorLogit_C_USA", "L261.SubsectorShrwtFllt_C_USA", "L261.StubTech_C_USA", "L261.StubTechMarket_C_USA", "L261.ResTechShrwt_C_USA") ->
      Cstorage_USA.xml

    return_data(Cstorage_USA.xml)
  } else {
    stop("Unknown command")
  }
}
