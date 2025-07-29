#' module_india_L254.transportation
#'
#' Generates GCAM-India model inputs for transportation sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L254.DeleteSupplysector_Indiatrn}, \code{L254.DeleteFinalDemand_Indiatrn},
#' \code{L254.Supplysector_trn_India}, \code{L254.FinalEnergyKeyword_trn_India}, \code{A54.india_tranSubsectorLogit_India},
#' \code{L254.tranSubsectorShrwtFllt_India}, \code{L254.tranSubsectorInterp_India}, \code{L254.tranSubsectorSpeed_India},
#' \code{L254.tranSubsectorSpeed_passthru_India}, \code{L254.tranSubsectorSpeed_noVOTT_India},
#' \code{L254.tranSubsectorSpeed_nonmotor_India}, \code{L254.tranSubsectorVOTT_India}, \code{L254.tranSubsectorFuelPref_India},
#' \code{L254.StubTranTech_India}, \code{L254.StubTranTech_nonmotor_India},
#' \code{L254.StubTranTechLoadFactor_India}, \code{L254.StubTranTechCost_India}, \code{L254.StubTranTechCoef_India},
#' \code{L254.PerCapitaBased_trn_India}, \code{L254.PriceElasticity_trn_India}, \code{L254.IncomeElasticity_trn_India},
#' \code{L254.StubTranTechCalInput_India}, \code{L254.StubTranTechProd_nonmotor_India}, \code{L254.StubTranTechCalInput_passthru_India},
#' \code{L254.BaseService_trn_India}.
#' The corresponding file in the original data system was \code{L254.transportation_India.R} (gcam-india level2).
#' @details This chunk generates input files for transportation sector with generic information for supplysector,
#' subsector and technologies, as well as calibrated inputs and outputs by the US states.
#' @note The transportation structure is heavily nested. The GCAM structure of sector/subsector/technology only
#' allows two levels of nesting within any sector, but a technology of one sector (e.g., trn_pass) can consume the
#' output of another "sector" (e.g., trn_pass_road) that is really just used to represent lower nesting levels of
#' that first, or parent, sector. In the transportation sector, each lower-level nesting "sector" is named by
#' appending a string to the parent sector. So, \code{trn_pass} contains \code{trn_pass_road} which has
#' \code{trn_pass_road_LDV} which has \code{trn_pass_road_LDV_4W}. Each of the links between any two of those sectors
#' is done with a pass-through technology within the parent sector that consumes the output of the child sector.
#' The technology is called a "pass-through" because it (generally) only consumes the output of the child "sector"
#' without making any changes to it. There's an additional complication in the transportation sector, that the
#' pass-through technologies are normal, standard GCAM technologies, not "tranTechnologies" which have different
#' parameters read in, and perform a bunch of hard-wired unit conversions between inputs and outputs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author Malyan_Ankur_CEEW @updated Chetna Arora_CEEW 2023
module_india_L254.transportation <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/UCD_techs_india",
             FILE = "energy/A54.globaltech_nonmotor",
             FILE = "energy/A54.globaltech_passthru",
             FILE = "energy/A54.sector",
             "L254.Supplysector_trn",
             "L254.FinalEnergyKeyword_trn",
             FILE = "gcam-india/A54.finalenergykeyword_trn",
             FILE = "gcam-india/A54.additionsupplypass_trn",
             FILE = "gcam-india/A54.india_tranSubsectorLogit",
             FILE = "gcam-india/A54.india_tranSubsectorShrwtFllt",
             FILE = "gcam-india/A54.india_tranSubsectorInterp",
             FILE = "gcam-india/A54.india_tranSubsectorSpeed",
             FILE = "gcam-india/A54.india_tranSubsectorVOTT",
             FILE = "gcam-india/A54.india_StubTranTech",
             FILE = "gcam-india/A54.StubTranTech_passthru_India",
             FILE = "gcam-india/A54.india_StubTranTechLoadFactor",
             FILE = "gcam-india/A54.india_StubTranTechCost",
             FILE = "gcam-india/A54.india_StubTranTechCoef",
             FILE = "gcam-india/A54.india_globaltranTech_shrwt",
             FILE = "gcam-india/A54.india_globaltranTech_interp",
             FILE = "gcam-india/A54.india_globaltranTech_retire",
             FILE = "gcam-india/A54.india_tranSubsectorSpeed_noVOTT",
             FILE = "gcam-india/A54.india_tranSubsectorSpeed_passthru",
             "L254.tranSubsectorSpeed_nonmotor",
             "L254.StubTech_nonmotor",
             "L254.PerCapitaBased_trn",
             "L254.PriceElasticity_trn",
             "L254.IncomeElasticity_trn",
             "L154.india_in_EJ_trn_India_F",
             "L154.out_mpkm_R_trn_nonmotor_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
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
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package notes
    #region <- supplysector <- technology <- minicam.energy.input <- year <- value <- coefficient <-
      #UCD_fuel <- UCD_sector <- UCD_technology <- calibrated.value <- coefficient <- loadFactor <-
      #size.class <- state <- tranSubsector <- tranTechnology <- calibrated.value <- stub.technology <-
      #output <- output_agg <- output_cum <- share.weight <- subs.share.weight <- share.weight.year <-
      #tech.share.weight <- calOutputValue <- energy.final.demand <- base.service <- year.fillout <-
      #fuelprefElasticity <- income.elasticity <- . <- NULL

    # Load required inputs
    UCD_techs_india <- get_data(all_data, "gcam-india/UCD_techs_india") # Mapping file of transportation technology from the UC Davis report (Mishra et al. 2013)
    A54.globaltech_nonmotor <- get_data(all_data, "energy/A54.globaltech_nonmotor")
    A54.globaltech_passthru <- get_data(all_data, "energy/A54.globaltech_passthru")
    A54.sector <- get_data(all_data, "energy/A54.sector")
    L254.Supplysector_trn <- get_data(all_data, "L254.Supplysector_trn")
    A54.additionsupplypass_trn <- get_data(all_data, "gcam-india/A54.additionsupplypass_trn")
    L254.FinalEnergyKeyword_trn <- get_data(all_data, "L254.FinalEnergyKeyword_trn")
    A54.india_tranSubsectorLogit <- get_data(all_data, "gcam-india/A54.india_tranSubsectorLogit")
    A54.india_tranSubsectorShrwtFllt <- get_data(all_data, "gcam-india/A54.india_tranSubsectorShrwtFllt")
    A54.india_tranSubsectorInterp <- get_data(all_data, "gcam-india/A54.india_tranSubsectorInterp")
    A54.india_tranSubsectorSpeed <- get_data(all_data, "gcam-india/A54.india_tranSubsectorSpeed")
    A54.india_tranSubsectorVOTT <- get_data(all_data, "gcam-india/A54.india_tranSubsectorVOTT")
    A54.india_StubTranTech <- get_data(all_data, "gcam-india/A54.india_StubTranTech")
    A54.india_StubTranTechLoadFactor <- get_data(all_data, "gcam-india/A54.india_StubTranTechLoadFactor")
    A54.india_StubTranTechCost <- get_data(all_data, "gcam-india/A54.india_StubTranTechCost")
    A54.india_StubTranTechCoef <- get_data(all_data, "gcam-india/A54.india_StubTranTechCoef")
    A54.india_tranSubsectorSpeed_passthru <- get_data(all_data, "gcam-india/A54.india_tranSubsectorSpeed_passthru")
    A54.india_tranSubsectorSpeed_noVOTT <- get_data(all_data, "gcam-india/A54.india_tranSubsectorSpeed_noVOTT")
    L254.tranSubsectorSpeed_nonmotor <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor")
    A54.StubTranTech_passthru_India <- get_data(all_data, "gcam-india/A54.StubTranTech_passthru_India")
    A54.finalenergykeyword_trn <- get_data(all_data,"gcam-india/A54.finalenergykeyword_trn")
    L254.StubTech_nonmotor <- get_data(all_data, "L254.StubTech_nonmotor")
    L254.PerCapitaBased_trn <- get_data(all_data, "L254.PerCapitaBased_trn")
    L254.PriceElasticity_trn <- get_data(all_data, "L254.PriceElasticity_trn")
    L254.IncomeElasticity_trn <- get_data(all_data, "L254.IncomeElasticity_trn")
    L154.india_in_EJ_trn_India_F <- get_data(all_data, "L154.india_in_EJ_trn_India_F")
    L154.out_mpkm_R_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_R_trn_nonmotor_Yh")
    A54.india_globaltranTech_shrwt <- get_data(all_data, "gcam-india/A54.india_globaltranTech_shrwt")
    A54.india_globaltranTech_interp <- get_data(all_data, "gcam-india/A54.india_globaltranTech_interp")
    A54.india_globaltranTech_retire <- get_data(all_data, "gcam-india/A54.india_globaltranTech_retire")

    # Need to delete the transportation sector in the India region (energy-final-demands and supplysectors)
    # L254.DeleteSupplysector_Indiatrn: Delete transportation supplysectors of the India region
    gcam.india_REGION <- "India"
    gcamindia.DIGITS_TRNUSA_DEFAULT <- 1
    gcam.INDIA_CODE   <- 17

    L254.Supplysector_trn %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == gcam.india_REGION) %>%
      select(region, supplysector) ->
      L254.DeleteSupplysector_Indiatrn

    L254.PerCapitaBased_trn %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == gcam.india_REGION) %>%
      select(LEVEL2_DATA_NAMES[["EnergyFinalDemand"]]) ->
      L254.DeleteFinalDemand_Indiatrn

    # Process tables at the India region level for new structure
    # All tables for which processing is identical are done by a function.
    # This applies to the supplysectors, subsectors, and stub tech characteristics of the states.
    process_India_restructure <- function(data) {
      state <- region <- grid_region <- subsector <- market.name <-
        minicam.energy.input <- NULL  # silence package check notes

      data_new <- data %>%
        filter(region == gcam.india_REGION) %>%
        mutate (region = region)
    data_new
    }

    process_India_restructure (L254.Supplysector_trn) -> L254.Supplysector_trn_India
    process_India_restructure(L254.FinalEnergyKeyword_trn) -> L254.FinalEnergyKeyword_trn_India
    process_India_restructure(A54.india_tranSubsectorLogit) -> L254.tranSubsectorLogit_India
    process_India_restructure(A54.india_tranSubsectorShrwtFllt) -> L254.tranSubsectorShrwtFllt_India
    process_India_restructure(A54.india_tranSubsectorInterp) -> L254.tranSubsectorInterp_India
    process_India_restructure(A54.india_tranSubsectorSpeed) %>%
      unique()-> L254.tranSubsectorSpeed_India
    process_India_restructure(A54.india_tranSubsectorSpeed_passthru) -> L254.tranSubsectorSpeed_passthru_India
    process_India_restructure(A54.india_tranSubsectorSpeed_noVOTT) -> L254.tranSubsectorSpeed_noVOTT_India
    process_India_restructure(L254.tranSubsectorSpeed_nonmotor) -> L254.tranSubsectorSpeed_nonmotor_India
    process_India_restructure(A54.india_tranSubsectorVOTT) -> L254.tranSubsectorVOTT_India
    process_India_restructure(A54.india_StubTranTech) -> L254.StubTranTech_India
    process_India_restructure(L254.StubTech_nonmotor) -> L254.StubTranTech_nonmotor_India
    process_India_restructure(A54.india_StubTranTechLoadFactor) -> L254.StubTranTechLoadFactor_India
    process_India_restructure(A54.india_StubTranTechCost) -> L254.StubTranTechCost_India
    A54.india_StubTranTechCoef %>%
      mutate(coefficient = round(coefficient, digits = 0)) %>%
      process_India_restructure ->
      L254.StubTranTechCoef_India
    process_India_restructure(L254.PerCapitaBased_trn) -> L254.PerCapitaBased_trn_India
    process_India_restructure(L254.PriceElasticity_trn) -> L254.PriceElasticity_trn_India
    process_India_restructure(L254.IncomeElasticity_trn) -> L254.IncomeElasticity_trn_India

    L254.Supplysector_trn_India <- L254.Supplysector_trn_India %>%
      bind_rows(A54.additionsupplypass_trn)

    L254.FinalEnergyKeyword_trn_India <- L254.FinalEnergyKeyword_trn_India %>%
      bind_rows(A54.finalenergykeyword_trn)

    # Calibration
    # L254.StubTranTechCalInput_India: calibrated energy consumption by all technologies
    L254.StubTranTechCalInput_India <- L154.india_in_EJ_trn_India_F %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calibrated.value = round(value, digits = energy.DIGITS_CALOUTPUT),
             region = State) %>%
      left_join_error_no_match(select(UCD_techs_india, UCD_sector, mode, size.class, UCD_technology, UCD_fuel,
                                      supplysector, tranSubsector, stub.technology = tranTechnology, minicam.energy.input),
                               by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel")) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTech"]], year, minicam.energy.input, calibrated.value) %>%
      mutate(share.weight.year = year, subsector = tranSubsector, calOutputValue = calibrated.value) %>%
      set_subsector_shrwt() %>%
      mutate(tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTechCalInput"]])


    # Non-motorized technologies
    # L254.StubTranTechProd_nonmotor_India: service output of non-motorized transportation technologies
     L154.out_mpkm_R_trn_nonmotor_Yh %>%
        filter(GCAM_region_ID == gcam.INDIA_CODE) %>%
        mutate (state = "India") %>%
        select(state, mode, year, value) %>%
        mutate(year = as.integer(year)) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digits = energy.DIGITS_MPKM),
             region = state, tranSubsector = mode) %>%
      left_join_error_no_match(A54.globaltech_nonmotor, by = "tranSubsector") %>%
      mutate(stub.technology = technology) %>%
      # There is no need to match shareweights to the calOutputValue because no region should ever have a 0 here
      select(LEVEL2_DATA_NAMES[["StubTranTech"]], year, calOutputValue) ->
      L254.StubTranTechProd_nonmotor_India



    # L254.StubTranTechCalInput_passthru_India: calibrated input of passthrough technologies
    # trn_pass, trn_pass_road, trn_pass_road_LDV, trn_freight

    # The transportation structure is heavily nested.
    # The GCAM structure of sector/subsector/technology only allows two levels of nesting within any sector,
    # but a technology of one sector (e.g., trn_pass) can consume the output of another "sector" (e.g., trn_pass_road)
    # that is really just used to represent lower nesting levels of that first, or parent, sector. In the
    # transportation sector, each lower-level nesting "sector" is named by appending a string to the parent sector.
    # So, trn_pass contains trn_pass_road which has trn_pass_road_LDV which has trn_pass_road_LDV_4W. Each of the links
    # between any two of those sectors is done with a pass-through technology within the parent sector that consumes
    # the output of the child sector. The technology is called a "pass-through" because it (generally) only consumes
    # the output of the child "sector" without making any changes to it. There's an additional complication in the
    # transportation sector: the pass-through technologies are normal, standard GCAM technologies, not "tranTechnologies"
    # which have different parameters read in, and perform a bunch of hard-wired unit conversions between inputs and outputs.

    # First, need to calculate the service output for all tranTechnologies
    # calInput * loadFactor * unit_conversion / (coef * unit conversion)
     L254.StubTranTechCoef_India[is.na(L254.StubTranTechCoef_India)] <- 0

    L254.StubTranTechOutput_India <- L254.StubTranTechCalInput_India %>%
      left_join_error_no_match(L254.StubTranTechLoadFactor_India,
                               by = c("region", "supplysector", "tranSubsector", "stub.technology", "year")) %>%
      left_join_error_no_match(L254.StubTranTechCoef_India,
                               by = c("region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.energy.input")) %>%
      mutate(output = round(calibrated.value * loadFactor * CONV_EJ_GJ / (coefficient * CONV_BTU_KJ),
                            digits = gcamindia.DIGITS_TRNUSA_DEFAULT))

    # The next step is to calculate the aggregated outputs by supplysector
    # Outputs of certain supplysectors are inputs for the passthrough technologies
    L254.StubTranTechOutput_India %>%
      group_by(region, year, supplysector) %>%
      summarise(output_agg = sum(output)) %>%
      ungroup() ->
      L254.StubTranTechOutput_India_agg

    # Write all possible pass-through technologies to all regions
    L254.StubTranTechCalInput_passthru_India_cum <- A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      mutate (region = "India") %>%
      select(region, supplysector, tranSubsector, stub.technology = technology, year, minicam.energy.input) %>%
      # Subset only the passthrough technologies that are applicable in each region
      semi_join(A54.StubTranTech_passthru_India,
                by = c("region", "supplysector", "tranSubsector", "stub.technology")) %>%
      # Match in outputs of supplysectors that are inputs for the passthrough technologies
      left_join(L254.StubTranTechOutput_India_agg,
                by = c("region", "year", "minicam.energy.input" = "supplysector")) %>%
      # Some of the technologies are sub-totals, assign zero value now, will be calculated below
      replace_na(list(output_agg = 0)) %>%
      # Arrange input sectors so that sub-total sector is behind the subsectors
      arrange(desc(minicam.energy.input)) %>%
      group_by(region, year) %>%
      # Calculate the cumulative for sub-total sector
      mutate(output_cum = cumsum(output_agg)) %>%
      ungroup()


    # Prepare a list of the supplysector in the passthrough input table to filter the sub-total sectors
    LIST_supplysector <- unique(L254.StubTranTechCalInput_passthru_India_cum$supplysector)

    L254.StubTranTechCalInput_passthru_India_cum %>%
      # Use the cumulative value for sub-total sectors
      mutate(calibrated.value = if_else(minicam.energy.input %in% LIST_supplysector,
                                        output_cum, output_agg)) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calibrated.value > 0, 1, 0),
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTechCalInput"]]) ->
      L254.StubTranTechCalInput_passthru_India

    # L254.BaseService_trn_India: base-year service output of transportation final demand
    L254.StubTranTechOutput_India %>%
      select(LEVEL2_DATA_NAMES[["StubTranTech"]], year, base.service = output) %>%
      bind_rows(L254.StubTranTechProd_nonmotor_India %>%
                  select(LEVEL2_DATA_NAMES[["StubTranTech"]], year, base.service = calOutputValue)) %>%
      left_join_error_no_match(select(A54.sector, supplysector, energy.final.demand), by = "supplysector") %>%
      group_by(region, energy.final.demand, year) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup ->
      L254.BaseService_trn_India


    ##################Global Technology Database############################

    # L254.GlobalTechShrwt_passthru: Shareweights of global transportation sector technologies (not tranTechnologies)
    A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], share.weight) ->
      L254.GlobalTechShrwt_passthru_India

    # L254.GlobalTechShrwt_nonmotor: Shareweights of non-motorized global transportation sector technologies (not tranTechnologies)
    A54.globaltech_nonmotor %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], share.weight) ->
      L254.GlobalTechShrwt_nonmotor_India

     # L254.GlobalTechCoef_passthru: Coefficients of global transportation sector technologies (not tranTechnologies)
    A54.globaltech_passthru %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L254.GlobalTechCoef_passthru_India

    # L254.GlobalRenewTech_nonmotor: Renewable inputs to non-motorized transportation technologies
    A54.globaltech_nonmotor %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalRenewTech"]]) ->
      L254.GlobalRenewTech_nonmotor_India

    # L254.GlobalTranTechInterp: Shareweight interpolation of global tranTechnologies
    A54.india_globaltranTech_interp %>%
      mutate(supplysector = supplysector) %>%  # create new tibble, stripping attributes
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechInterp"]]) ->
      L254.GlobalTranTechInterp_India

    # L254.GlobalTranTechShrwt: Shareweights of global tranTechnologies
    A54.india_globaltranTech_shrwt %>%
      gather_years %>%
      # Expand table to include all model years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, tranSubsector, tranTechnology)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, tranSubsector, tranTechnology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2),
             share.weight = round(share.weight, energy.DIGITS_SHRWT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechShrwt"]]) ->
      L254.GlobalTranTechShrwt_India # OUTPUT

    # L254.GlobalTranTechSCurve: Retirement of global tranTechnologies
    # A54.globaltranTech_retire reports transportation technology retirement parameters. Only applies to vintaged technologies
    A54.india_globaltranTech_retire %>%
      set_years() %>%
      filter(year < max(year)) %>%
      mutate(year = as.numeric(year)) ->
      L254.GlobalTranTechSCurve_ind

    # Copy the final year forward to all future time periods
    L254.GlobalTranTechSCurve_MAX_YEAR_ind <- max(L254.GlobalTranTechSCurve_ind$year)

    A54.india_globaltranTech_retire %>%
      set_years() %>%
      filter(year == max(year)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(year > L254.GlobalTranTechSCurve_MAX_YEAR_ind) %>%
      bind_rows(L254.GlobalTranTechSCurve_ind) %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTranTechSCurve"]]) ->
      L254.GlobalTranTechSCurve_India # OUTPUT


    #L254.GlobalTranTechCost_India
    L254.GlobalTranTechCost_India <- A54.india_StubTranTechCost %>%
      select(-region) %>% unique() %>%
      mutate(supplysector = supplysector) %>%  # create new tibble, stripping attributes
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector, technology = stub.technology) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])

     #L254.GlobalTranTechCoef_India
    L254.GlobalTranTechCoef_India <- A54.india_StubTranTechCoef %>%
      select(-region) %>% unique() %>%
      mutate(supplysector = supplysector) %>%  # create new tibble, stripping attributes
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = tranSubsector, technology = stub.technology) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]])

    # Produce outputs

    ##Global Database

    L254.GlobalTechShrwt_passthru_India %>%
      #add_title("Global Tech Share-weight Pass-thru") %>%
      add_units("NA") %>%
      add_comments("NA") %>%
      add_legacy_name("L254.GlobalTechShrwt_passthru_India") %>%
      add_precursors("energy/A54.globaltech_passthru") ->
      L254.GlobalTechShrwt_passthru_India

    L254.GlobalTechShrwt_nonmotor_India %>%
      #add_title("Global Tech Share-weight non-motor") %>%
      add_units("NA") %>%
      add_comments("NA") %>%
      add_legacy_name("L254.GlobalTechShrwt_nonmotor_India") %>%
      add_precursors("energy/A54.globaltech_nonmotor") ->
      L254.GlobalTechShrwt_nonmotor_India

    L254.GlobalTechCoef_passthru_India %>%
      #add_title("Global Tech Coeff Pass thru") %>%
      add_units("NA") %>%
      add_comments("NA") %>%
      add_legacy_name("L254.GlobalTechCoef_passthru_India") %>%
      add_precursors("energy/A54.globaltech_passthru") ->
      L254.GlobalTechCoef_passthru_India

    L254.GlobalRenewTech_nonmotor_India %>%
      #add_title("Renew tech non motor") %>%
      add_units("NA") %>%
      add_comments("NA") %>%
      add_legacy_name("L254.GlobalRenewTech_nonmotor_India") %>%
      add_precursors("energy/A54.globaltech_nonmotor") ->
      L254.GlobalRenewTech_nonmotor_India

    L254.GlobalTranTechInterp_India %>%
      #add_title("Tran Tech Interpolations") %>%
      add_units("NA") %>%
      add_comments("NA") %>%
      add_legacy_name("L254.GlobalTranTechInterp_India") %>%
      add_precursors("gcam-india/A54.india_globaltranTech_interp") ->
      L254.GlobalTranTechInterp_India

    L254.GlobalTranTechShrwt_India %>%
      #add_title("Tran Tech Share-weights") %>%
      add_units("NA") %>%
      add_comments("NA") %>%
      add_legacy_name("L254.GlobalTranTechShrwt_India") %>%
      add_precursors("gcam-india/A54.india_globaltranTech_shrwt") ->
      L254.GlobalTranTechShrwt_India

    L254.GlobalTranTechSCurve_India %>%
      #add_title("Tech S Curve for 4W anad Truck") %>%
      add_units("NA") %>%
      add_comments("NA") %>%
      add_legacy_name("L254.GlobalTranTechSCurve_India") %>%
      add_precursors("gcam-india/A54.india_globaltranTech_retire") ->
      L254.GlobalTranTechSCurve_India

    L254.GlobalTranTechCost_India %>%
      #add_title("Tech S Curve for 4W anad Truck") %>%
      add_units("NA") %>%
      add_comments("NA") %>%
      add_legacy_name("L254.GlobalTranTechCost_India") %>%
      add_precursors("gcam-india/A54.india_StubTranTechCost") ->
      L254.GlobalTranTechCost_India

    L254.GlobalTranTechCoef_India %>%
      #add_title("Tech S Curve for 4W anad Truck") %>%
      add_units("NA") %>%
      add_comments("NA") %>%
      add_legacy_name("L254.GlobalTranTechCoef_India") %>%
      add_precursors("gcam-india/A54.india_StubTranTechCoef") ->
      L254.GlobalTranTechCoef_India


    ##Main Outputs

      L254.DeleteSupplysector_Indiatrn %>%
      #add_title("delete transport for India full region") %>%
      add_units("NA") %>%
      add_comments("Delete transportation supply sectors of the full India region") %>%
      add_precursors("L254.Supplysector_trn") ->
       L254.DeleteSupplysector_Indiatrn

    L254.Supplysector_trn_India %>%
      #add_title("Supply sector information for transportation sector in the US states") %>%
      add_units("Unitless") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("L254.Supplysector_trn","A54.additionsupplypass_trn") ->
      L254.Supplysector_trn_India

    L254.DeleteFinalDemand_Indiatrn %>%
      #add_title("Delete energy final demand sectors of the full India region") %>%
      add_units("NA") %>%
      add_comments("Delete energy final demand sectors of the full India region") %>%
      add_precursors("L254.PerCapitaBased_trn") ->
      L254.DeleteFinalDemand_Indiatrn

    L254.FinalEnergyKeyword_trn_India %>%
      #add_title("Supply sector final energy keywords for transportation sector in the US states") %>%
      add_units("NA") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("L254.FinalEnergyKeyword_trn","A54.finalenergykeyword_trn") ->
      L254.FinalEnergyKeyword_trn_India

    L254.tranSubsectorLogit_India %>%
      #add_title("Subsector logit exponents of transportation sector in India") %>%
      add_units("Unitless") %>%
      add_legacy_name("L254.tranSubsectorLogit_India") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("gcam-india/A54.india_tranSubsectorLogit") ->
      L254.tranSubsectorLogit_India

    L254.tranSubsectorShrwtFllt_India %>%
      #add_title("Subsector shareweights of transportation sector in India") %>%
      add_units("Unitless") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_legacy_name("L254.tranSubsectorShrwtFllt_India") %>%
      add_precursors("gcam-india/A54.india_tranSubsectorShrwtFllt") ->
      L254.tranSubsectorShrwtFllt_India

    L254.tranSubsectorInterp_India %>%
      #add_title("Temporal subsector shareweight interpolation of transportation sector in India") %>%
      add_units("Unitless") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("gcam-india/A54.india_tranSubsectorInterp") ->
    L254.tranSubsectorInterp_India

    L254.tranSubsectorSpeed_India %>%
      #add_title("Speeds of transportation modes (not including pass-through sectors) in India") %>%
      add_units("km / hr") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("gcam-india/A54.india_tranSubsectorSpeed") ->
      L254.tranSubsectorSpeed_India

    L254.tranSubsectorSpeed_passthru_India %>%
      #add_title("Speeds of pass-through transportation subsectors in India") %>%
      add_units("km / hr") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("gcam-india/A54.india_tranSubsectorSpeed_passthru") ->
      L254.tranSubsectorSpeed_passthru_India

    L254.tranSubsectorSpeed_noVOTT_India %>%
      #add_title("Speeds of transportation subsectors whose time value is not considered in India") %>%
      add_units("km / hr") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("gcam-india/A54.india_tranSubsectorSpeed_noVOTT") ->
      L254.tranSubsectorSpeed_noVOTT_India

    L254.tranSubsectorSpeed_nonmotor_India %>%
      #add_title("Speeds of non-motorized transportation subsectors in India") %>%
      add_units("km / hr") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("L254.tranSubsectorSpeed_nonmotor") ->
      L254.tranSubsectorSpeed_nonmotor_India

    L254.tranSubsectorVOTT_India %>%
      #add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("gcam-india/A54.india_tranSubsectorVOTT") ->
      L254.tranSubsectorVOTT_India


    L254.StubTranTech_India %>%
      #add_title("Transportation stub technologies in India") %>%
      add_units("NA") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("gcam-india/A54.india_StubTranTech") ->
      L254.StubTranTech_India

    L254.StubTranTech_nonmotor_India %>%
      #add_title("Transportation stub technologies for non-motorized subsectors in the US states") %>%
      add_units("NA") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("L254.StubTech_nonmotor") ->
      L254.StubTranTech_nonmotor_India

    L254.StubTranTechLoadFactor_India %>%
      #add_title("Load factors of transportation stub technologies in the US states") %>%
      add_units("person/vehicle and tonnes/vehicle") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("gcam-india/A54.india_StubTranTechLoadFactor") ->
      L254.StubTranTechLoadFactor_India

    L254.StubTranTechCost_India %>%
      #add_title("Costs of transportation stub technologies in the US states") %>%
      add_units("$1990USD / vkm") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("gcam-india/A54.india_StubTranTechCost") ->
      L254.StubTranTechCost_India

    L254.StubTranTechCoef_India %>%
      #add_title("Coefficients of transportation stub technologies in the US states") %>%
      add_units("BTU / vkm") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_comments("Re-set electricity consumed at the state markets") %>%
      add_precursors("gcam-india/A54.india_StubTranTechCoef") ->
      L254.StubTranTechCoef_India

    L254.PerCapitaBased_trn_India %>%
      #add_title("Per-capita based flag for transportation final demand in the US states") %>%
      add_units("NA") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("L254.PerCapitaBased_trn") ->
      L254.PerCapitaBased_trn_India

    L254.PriceElasticity_trn_India %>%
      #add_title("Price elasticity of transportation final demand in the US states") %>%
      add_units("Unitless") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("L254.PriceElasticity_trn") ->
      L254.PriceElasticity_trn_India

    L254.IncomeElasticity_trn_India %>%
      #add_title("Income elasticity of transportation final demand in the US states") %>%
      add_units("Unitless") %>%
      add_comments("The same India region values are repeated for each state") %>%
      add_precursors("L254.IncomeElasticity_trn") ->
      L254.IncomeElasticity_trn_India

    L254.StubTranTechCalInput_India %>%
      #add_title("Calibrated energy consumption by all transportation stub technologies in the US states") %>%
      add_units("EJ") %>%
      add_comments("Set calibration values for those technologies that do not exist in some base years as zero") %>%
      same_precursors_as("L254.StubTranTechCoef_India") %>%
      add_precursors("L154.india_in_EJ_trn_India_F",
                     "gcam-india/UCD_techs_india") ->
      L254.StubTranTechCalInput_India

    L254.StubTranTechProd_nonmotor_India %>%
      #add_title("Calibrated service output of non-motorized transportation technologies in the US states") %>%
      add_units("Million pass-km") %>%
      add_comments("Not match shareweights to the calOutputValue because no region should ever have a zero here") %>%
      add_precursors("L154.out_mpkm_R_trn_nonmotor_Yh",
                     "energy/A54.globaltech_nonmotor") ->
      L254.StubTranTechProd_nonmotor_India

    L254.StubTranTechCalInput_passthru_India %>%
      #add_title("Calibrated energy consumption of transportation passthrough technologies in the US states") %>%
      add_units("EJ") %>%
      add_comments("Use outputs of the supplysectors that are inputs for passthrough technologies") %>%
      add_comments("Outputs of all motorized technologies are calculated as calInput * loadFactor / coefficient") %>%
      same_precursors_as("L254.StubTranTechCalInput_India") %>%
      same_precursors_as("L254.StubTranTechLoadFactor_India") %>%
      same_precursors_as("L254.StubTranTechCoef_India") %>%
      same_precursors_as("A54.StubTranTech_passthru_India") %>%
      add_precursors("energy/A54.globaltech_passthru") ->
      L254.StubTranTechCalInput_passthru_India

    L254.BaseService_trn_India %>%
      #add_title("Base-year service output of transportation final demand") %>%
      add_units("Million pass-km and million ton-km") %>%
      add_comments("Service outputs of all motorized technologies are calculated as calInput * loadFactor / coefficient") %>%
      add_comments("Combine with service output of non-motorized transportation technologies") %>%
      same_precursors_as("L254.StubTranTechCalInput_India") %>%
      same_precursors_as("L254.StubTranTechLoadFactor_India") %>%
      same_precursors_as("L254.StubTranTechCoef_India") %>%
      same_precursors_as("L254.StubTranTechProd_nonmotor_India") %>%
      add_precursors("energy/A54.sector") ->
      L254.BaseService_trn_India

    return_data(L254.DeleteSupplysector_Indiatrn, L254.DeleteFinalDemand_Indiatrn,
                L254.Supplysector_trn_India,
                L254.FinalEnergyKeyword_trn_India,
                L254.tranSubsectorLogit_India,
                L254.tranSubsectorShrwtFllt_India,
                L254.tranSubsectorInterp_India,
                L254.tranSubsectorSpeed_India,
                L254.tranSubsectorSpeed_passthru_India,
                L254.tranSubsectorSpeed_noVOTT_India,
                L254.tranSubsectorSpeed_nonmotor_India,
                L254.tranSubsectorVOTT_India,
                L254.StubTranTech_India,
                L254.StubTranTech_nonmotor_India,
                L254.StubTranTechLoadFactor_India,
                L254.StubTranTechCost_India,
                L254.StubTranTechCoef_India,
                L254.PerCapitaBased_trn_India,
                L254.PriceElasticity_trn_India,
                L254.IncomeElasticity_trn_India,
                L254.StubTranTechCalInput_India, L254.StubTranTechProd_nonmotor_India,
                L254.StubTranTechCalInput_passthru_India, L254.BaseService_trn_India,
                L254.GlobalTechShrwt_passthru_India,
                L254.GlobalTechShrwt_nonmotor_India,
                L254.GlobalTechCoef_passthru_India,
                L254.GlobalRenewTech_nonmotor_India,
                L254.GlobalTranTechCost_India,
                L254.GlobalTranTechCoef_India,
                L254.GlobalTranTechInterp_India,
                L254.GlobalTranTechShrwt_India,
                L254.GlobalTranTechSCurve_India)
  } else {
    stop("Unknown command")
  }
}
