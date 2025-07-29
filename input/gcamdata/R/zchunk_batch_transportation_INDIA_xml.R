#' module_batch_transportation_India_xml
#'
#' Construct XML data structure for \code{transportation_India_new.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_India_new.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_India_xml.R} (gcamIndia XML).
#' @author Malyan_Ankur_CEEW
module_batch_transportation_India_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.DeleteSupplysector_Indiatrn",
             "L254.DeleteFinalDemand_Indiatrn",
             "L254.Supplysector_trn_India",
             "L254.FinalEnergyKeyword_trn_India",
             "L254.tranSubsectorLogit_India",
             "L254.tranSubsectorShrwtFllt_India",
             "L254.tranSubsectorInterp_India",
             "L254.tranSubsectorSpeed_India",
             "L254.tranSubsectorSpeed_passthru_India",
             "L254.tranSubsectorSpeed_noVOTT_India",
             "L254.tranSubsectorSpeed_nonmotor_India",
             "L254.tranSubsectorVOTT_India",
             "L254.StubTranTech_India",
             FILE = "gcam-india/A54.StubTranTech_passthru_India",
             "L254.StubTranTech_nonmotor_India",
             "L254.StubTranTechLoadFactor_India",
             "L254.StubTranTechCost_India",
             "L254.StubTranTechCoef_India",
             "L254.PerCapitaBased_trn_India",
             "L254.PriceElasticity_trn_India",
             "L254.IncomeElasticity_trn_India",
             "L254.StubTranTechCalInput_India",
             "L254.StubTranTechProd_nonmotor_India",
             "L254.StubTranTechCalInput_passthru_India",
             "L254.BaseService_trn_India",
             "L254.GlobalTechShrwt_passthru_India",
             "L254.GlobalTechShrwt_nonmotor_India",
             "L254.GlobalTechCoef_passthru_India",
             "L254.GlobalRenewTech_nonmotor_India",
             "L254.GlobalTranTechCost_India",
             "L254.GlobalTranTechCoef_India",
             "L254.GlobalTranTechInterp_India",
             "L254.GlobalTranTechShrwt_India",
             "L254.GlobalTranTechSCurve_India"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transportation_India_new.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.DeleteSupplysector_Indiatrn <- get_data(all_data, "L254.DeleteSupplysector_Indiatrn")
    L254.DeleteFinalDemand_Indiatrn <- get_data(all_data, "L254.DeleteFinalDemand_Indiatrn")
    L254.Supplysector_trn_India <- get_data(all_data, "L254.Supplysector_trn_India")
    L254.FinalEnergyKeyword_trn_India <- get_data(all_data, "L254.FinalEnergyKeyword_trn_India")
    L254.tranSubsectorLogit_India <- get_data(all_data, "L254.tranSubsectorLogit_India")
    L254.tranSubsectorShrwtFllt_India <- get_data(all_data, "L254.tranSubsectorShrwtFllt_India")
    L254.tranSubsectorInterp_India <- get_data(all_data, "L254.tranSubsectorInterp_India")
    L254.tranSubsectorSpeed_India <- get_data(all_data, "L254.tranSubsectorSpeed_India")
    L254.tranSubsectorSpeed_passthru_India <- get_data(all_data, "L254.tranSubsectorSpeed_passthru_India")
    L254.tranSubsectorSpeed_noVOTT_India <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT_India")
    L254.tranSubsectorSpeed_nonmotor_India <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor_India")
    L254.tranSubsectorVOTT_India <- get_data(all_data, "L254.tranSubsectorVOTT_India")
    L254.StubTranTech_India <- get_data(all_data, "L254.StubTranTech_India")
    A54.StubTranTech_passthru_India <- get_data(all_data, "gcam-india/A54.StubTranTech_passthru_India")
    L254.StubTranTech_nonmotor_India <- get_data(all_data, "L254.StubTranTech_nonmotor_India")
    L254.StubTranTechLoadFactor_India <- get_data(all_data, "L254.StubTranTechLoadFactor_India")
    L254.StubTranTechCost_India <- get_data(all_data, "L254.StubTranTechCost_India")
    L254.StubTranTechCoef_India <- get_data(all_data, "L254.StubTranTechCoef_India")
    L254.PerCapitaBased_trn_India <- get_data(all_data, "L254.PerCapitaBased_trn_India")
    L254.PriceElasticity_trn_India <- get_data(all_data, "L254.PriceElasticity_trn_India")
    L254.IncomeElasticity_trn_India <- get_data(all_data, "L254.IncomeElasticity_trn_India")
    L254.StubTranTechCalInput_India <- get_data(all_data, "L254.StubTranTechCalInput_India")
    L254.StubTranTechProd_nonmotor_India <- get_data(all_data, "L254.StubTranTechProd_nonmotor_India")
    L254.StubTranTechCalInput_passthru_India <- get_data(all_data, "L254.StubTranTechCalInput_passthru_India")
    L254.BaseService_trn_India <- get_data(all_data, "L254.BaseService_trn_India")
    L254.GlobalTechShrwt_passthru_India <- get_data(all_data, "L254.GlobalTechShrwt_passthru_India")
    L254.GlobalTechShrwt_nonmotor_India <- get_data(all_data, "L254.GlobalTechShrwt_nonmotor_India")
    L254.GlobalTechCoef_passthru_India <- get_data(all_data, "L254.GlobalTechCoef_passthru_India")
    L254.GlobalRenewTech_nonmotor_India <- get_data(all_data, "L254.GlobalRenewTech_nonmotor_India")
    L254.GlobalTranTechInterp_India <- get_data(all_data, "L254.GlobalTranTechInterp_India")
    L254.GlobalTranTechShrwt_India <- get_data(all_data, "L254.GlobalTranTechShrwt_India")
    L254.GlobalTranTechSCurve_India <- get_data(all_data, "L254.GlobalTranTechSCurve_India")
    L254.GlobalTranTechCost_India <- get_data(all_data, "L254.GlobalTranTechCost_India")
    L254.GlobalTranTechCoef_India <- get_data(all_data, "L254.GlobalTranTechCoef_India")

        # ===================================================

    # Produce outputs
    create_xml("transportation_India_new.xml") %>%
      add_xml_data(L254.DeleteSupplysector_Indiatrn, "DeleteSupplysector") %>%
      add_xml_data(L254.DeleteFinalDemand_Indiatrn, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L254.Supplysector_trn_India, "Supplysector") %>%
      add_xml_data(L254.FinalEnergyKeyword_trn_India, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L254.tranSubsectorLogit_India, "tranSubsectorLogit", "tranSubsector") %>%
      add_xml_data(L254.tranSubsectorShrwtFllt_India, "tranSubsectorShrwtFllt") %>%
      add_xml_data(L254.tranSubsectorInterp_India, "tranSubsectorInterp") %>%
      add_xml_data(L254.tranSubsectorSpeed_India, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_passthru_India, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_noVOTT_India, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_nonmotor_India, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorVOTT_India, "tranSubsectorVOTT") %>%
      add_xml_data(L254.StubTranTech_India, "StubTranTech") %>%
      add_xml_data(A54.StubTranTech_passthru_India, "StubTranTech") %>%
      add_xml_data(L254.StubTranTech_nonmotor_India, "StubTranTech") %>%
      add_xml_data(L254.StubTranTechLoadFactor_India, "StubTranTechLoadFactor") %>%
      add_xml_data(L254.StubTranTechCost_India, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_India, "StubTranTechCoef") %>%
      add_xml_data(L254.PerCapitaBased_trn_India, "PerCapitaBased") %>%
      add_xml_data(L254.PriceElasticity_trn_India, "PriceElasticity") %>%
      add_xml_data(L254.IncomeElasticity_trn_India, "IncomeElasticity") %>%
      add_xml_data(L254.StubTranTechCalInput_India, "StubTranTechCalInput") %>%
      add_xml_data(L254.StubTranTechProd_nonmotor_India, "StubTranTechProd") %>%
      add_xml_data(L254.StubTranTechCalInput_passthru_India, "StubTranTechCalInput") %>%
      add_xml_data(L254.BaseService_trn_India, "BaseService") %>%
      add_xml_data(L254.GlobalTechShrwt_passthru_India, "GlobalTechShrwt") %>%
      add_xml_data(L254.GlobalTechShrwt_nonmotor_India, "GlobalTechShrwt") %>%
      add_xml_data(L254.GlobalTechCoef_passthru_India, "GlobalTechCoef") %>%
      add_xml_data(L254.GlobalRenewTech_nonmotor_India, "GlobalRenewTech") %>%
      add_xml_data(L254.GlobalTranTechInterp_India, "GlobalTranTechInterp") %>%
      add_xml_data(L254.GlobalTranTechShrwt_India, "GlobalTranTechShrwt") %>%
      add_xml_data(L254.GlobalTranTechSCurve_India, "GlobalTranTechSCurve") %>%
      add_xml_data(L254.GlobalTranTechCost_India, "GlobalTechCost") %>%
      add_xml_data(L254.GlobalTranTechCoef_India, "GlobalTechCoef") %>%
      add_precursors("L254.DeleteSupplysector_Indiatrn",
                     "L254.DeleteFinalDemand_Indiatrn",
                     "L254.Supplysector_trn_India",
                     "L254.FinalEnergyKeyword_trn_India",
                     "L254.tranSubsectorLogit_India",
                     "L254.tranSubsectorShrwtFllt_India",
                     "L254.tranSubsectorInterp_India",
                     "L254.tranSubsectorSpeed_India",
                     "L254.tranSubsectorSpeed_passthru_India",
                     "L254.tranSubsectorSpeed_noVOTT_India",
                     "L254.tranSubsectorSpeed_nonmotor_India",
                     "L254.tranSubsectorVOTT_India",
                     "L254.StubTranTech_India",
                     "gcam-india/A54.StubTranTech_passthru_India",
                     "L254.StubTranTech_nonmotor_India",
                     "L254.StubTranTechLoadFactor_India",
                     "L254.StubTranTechCost_India",
                     "L254.StubTranTechCoef_India",
                     "L254.PerCapitaBased_trn_India",
                     "L254.PriceElasticity_trn_India",
                     "L254.IncomeElasticity_trn_India",
                     "L254.StubTranTechCalInput_India",
                     "L254.StubTranTechProd_nonmotor_India",
                     "L254.StubTranTechCalInput_passthru_India",
                     "L254.BaseService_trn_India",
                     "L254.GlobalTechShrwt_passthru_India",
                     "L254.GlobalTechShrwt_nonmotor_India",
                     "L254.GlobalTechCoef_passthru_India",
                     "L254.GlobalRenewTech_nonmotor_India",
                     "L254.GlobalTranTechCost_India",
                     "L254.GlobalTranTechCoef_India",
                     "L254.GlobalTranTechInterp_India",
                     "L254.GlobalTranTechShrwt_India",
                     "L254.GlobalTranTechSCurve_India") ->
      transportation_India_new.xml

    return_data(transportation_India_new.xml)
  } else {
    stop("Unknown command")
  }
}
