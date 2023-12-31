# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L102.ghg_en_S_T_Y
#'
#' Calculates CH4 and N2O emission factors derived from EPA GHG inventory and GCAM energy balances for the US in 2005.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.ghg_tgej_USA_en_Sepa_F_2005}. The corresponding file in the
#' original data system was \code{L102.ghg_en_USA_S_T_Y.R} (emissions level1).
#' @details Divides CH4 and N2O emissions from EPA GHG inventory by GCAM energy sector activity to get emissions factors for a single historical year 2005 in the US.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter if_else group_by left_join mutate select summarize summarize_if
#' @author HCM April 2017
module_gcamusa_L102.ghg_en_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/mappings/IEA_flow_sector",
             FILE = "energy/mappings/IEA_product_fuel",
             FILE = "emissions/mappings/EPA_ghg_tech",
             FILE = "emissions/mappings/GCAM_sector_tech",
             FILE = "emissions/mappings/GCAM_sector_tech_Revised",
             "L101.in_EJ_R_en_Si_F_Yh",
             FILE = "emissions/EPA_FCCC_GHG_2005"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.ghg_tgej_USA_en_Sepa_F_2005"))
  } else if(command == driver.MAKE) {

    year <- energy <- sector <- fuel <- technology <- GCAM_region_ID <- CO2 <-
      . <- EPA_agg_sector <- EPA_agg_sector <- EPA_agg_fuel_ghg <- CH4 <-
      N2O <- ch4_em_factor <- n2o_em_factor <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    IEA_flow_sector <- get_data(all_data, "energy/mappings/IEA_flow_sector")
    IEA_product_fuel <- get_data(all_data, "energy/mappings/IEA_product_fuel")
    EPA_ghg_tech <- get_data(all_data, "emissions/mappings/EPA_ghg_tech")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")

    if (energy.TRAN_UCD_MODE == "rev.mode"){
      GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech_Revised")

    }


    get_data(all_data, "L101.in_EJ_R_en_Si_F_Yh") %>%
      gather_years(value_col = "energy") ->
      L101.in_EJ_R_en_Si_F_Yh
    EPA_FCCC_GHG_2005 <- get_data(all_data, "emissions/EPA_FCCC_GHG_2005")

    # Convert EPA GHG emissions inventory to Tg and aggregate by sector and fuel
    EPA_FCCC_GHG_2005 %>% # start from EPA GHG
      left_join(EPA_ghg_tech, by = "Source_Category") %>% # define category
      select(-CO2) %>% # non-CO2 only
      replace_na(list(CH4 = 0, N2O = 0)) %>%
      # Primary fossil production (coal, oil, gas) emissions does not exist in the EPA
      # inventory.  We will leave a place holder for now which will get scaled
      # to match EDGAR downstream.
      mutate(CH4 = if_else(sector %in% c("coal", "oil_gas"), 1, CH4),
             N2O = if_else(sector %in% c("coal", "oil_gas"), 1, N2O)) %>%
      group_by(sector, fuel) %>%
      summarize(CH4 = sum(CH4) * CONV_GG_TG,
                N2O = sum(N2O) * CONV_GG_TG) %>% # sum by sector and fuel and change units
      filter(!is.na(sector), !is.na(fuel)) %>% # delete NA sectors and fuel
      ungroup() ->
      L102.ghg_tg_USA_en_Sepa_F_2005 # GHG balance in 2005

    # organize energy balances in USA 2005
    L101.in_EJ_R_en_Si_F_Yh %>% # start from energy balances
      filter(GCAM_region_ID == gcam.USA_CODE, year == 2005) %>% # 2005 USA data only
      select(-GCAM_region_ID, -year) %>%
      left_join(select(GCAM_sector_tech, sector, fuel, technology, EPA_agg_sector, EPA_agg_fuel_ghg),
                by = c("sector", "fuel", "technology")) %>% # assign aggregate sector and fuel names
      group_by(EPA_agg_sector, EPA_agg_fuel_ghg) %>%
      summarize_if(is.numeric, sum) -> # sum by aggregate sector and fuel
      L102.in_EJ_USA_en_Sepa_F_2005 # energy balance in 2005

    # combine emissions and energy to get emission factors
    L102.ghg_tg_USA_en_Sepa_F_2005 %>%
      left_join(L102.in_EJ_USA_en_Sepa_F_2005, by = c("sector" = "EPA_agg_sector", "fuel" = "EPA_agg_fuel_ghg")) %>% # energy data and emission data joined
      mutate(ch4_em_factor = CH4 / energy, # emission factor calculated
             n2o_em_factor = N2O / energy) %>% # emission factor calculated
      select(-CH4, -N2O, -energy) %>% # delete orignal data
      arrange(fuel) %>%
      mutate(ch4_em_factor = if_else(is.na(ch4_em_factor) | is.infinite(ch4_em_factor), 0, ch4_em_factor), # set NA and INF to zero
             n2o_em_factor = if_else(is.na(n2o_em_factor) | is.infinite(n2o_em_factor), 0, n2o_em_factor)) -> # set NA and INF to zero
      L102.ghg_tgej_USA_en_Sepa_F_2005

    # Produce outputs
    L102.ghg_tgej_USA_en_Sepa_F_2005 %>%
      add_title("GHG emissions factors for the USA energy sector by sector / fuel / 2005") %>%
      add_units("Tg/EJ") %>%
      add_comments("CH4 and N2O emission factors derived from EPA GHG inventory and GCAM energy balances for the US in 2005") %>%
      add_legacy_name("L102.ghg_tgej_USA_en_Sepa_F_2005") %>%
      add_precursors("common/iso_GCAM_regID",
                     "energy/mappings/IEA_flow_sector",
                     "energy/mappings/IEA_product_fuel",
                     "emissions/mappings/EPA_ghg_tech",
                     "emissions/mappings/GCAM_sector_tech",
                     "emissions/mappings/GCAM_sector_tech_Revised",
                     "emissions/EPA_FCCC_GHG_2005",
                     "L101.in_EJ_R_en_Si_F_Yh") ->
      L102.ghg_tgej_USA_en_Sepa_F_2005

    return_data(L102.ghg_tgej_USA_en_Sepa_F_2005)
  } else {
    stop("Unknown command")
  }
}
