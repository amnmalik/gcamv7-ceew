#' module_gcamindia_L244.building_INDIA
#' adapted for GCAM6/7 Chetna Arora 2023
#' Creates GCAM-INDIA building output files for writing to xml.


module_gcamindia_L244.building_INDIA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A44.gcam_consumer",
             FILE = "energy/A44.sector",
             FILE = "gcam-india/A44.india_state_calibrated_techs_bld",
             FILE = "gcam-india/A44.india_state_bld_shell_conductance",
             FILE = "gcam-india/A44.india_state_demandFn_flsp",
             FILE = "gcam-india/A44.india_state_demandFn_serv",
             FILE = "gcam-india/A44.india_state_gcam_consumer",
             FILE = "gcam-india/A44.india_state_satiation_flsp",
             FILE = "gcam-india/A44.india_state_sector",
             FILE = "gcam-india/A44.india_state_subsector_interp",
             FILE = "gcam-india/A44.india_state_subsector_logit",
             FILE = "gcam-india/A44.india_state_subsector_shrwt",
             FILE = "gcam-india/A44.india_state_globaltech_cost",
             FILE = "gcam-india/A44.india_state_globaltech_eff",
             FILE = "gcam-india/A44.india_state_globaltech_eff_avg",
             FILE = "gcam-india/A44.india_state_globaltech_shares",
             FILE = "gcam-india/A44.india_state_globaltech_intgains",
             FILE = "gcam-india/A44.india_state_globaltech_retirement",
             FILE = "gcam-india/A44.india_state_globaltech_shrwt",
             FILE = "gcam-india/A44.india_state_globaltech_interp",
             FILE = "gcam-india/A44.india_state_demand_satiation_mult",
             FILE = "gcam-india/A44.india_state_subregional_income_share",
             FILE = "gcam-india/A44.india_state_subregional_pop_share",
             "L144.india_state_flsp_bm2_res",
             "L144.india_state_flsp_bm2_comm",
             "L144.india_state_in_EJ_comm_F_U_Y",
             "L144.india_state_in_EJ_res_F_U_Y",
             "L143.HDDCDD_scen_ctry_Y",
             "L100.india_state_pop_ruralurban",
             "L100.india_state_pcGDP_thous90usd_ruralurban",
             "L100.india_state_pcGDP_thous90usd_ruralurban"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L244.india_state_DeleteConsumer_bld",
             "L244.india_state_DeleteSupplysector_bld",
             "L244.SubregionalShares_gcamindia",
             "L244.PriceExp_IntGains_gcamindia",
             "L244.india_state_Floorspace_gcamindia",
             "L244.DemandFunction_serv_gcamindia",
             "L244.DemandFunction_flsp_gcamindia",
             "L244.india_state_Satiation_flsp",
             "L244.india_state_SatiationAdder",
             "L244.india_state_ThermalBaseService",
             "L244.india_state_GenericBaseService",
             "L244.india_state_ThermalServiceSatiation",
             "L244.india_state_GenericServiceSatiation",
             "L244.india_state_Intgains_scalar",
             "L244.india_state_ShellConductance_bld",
             "L244.india_state_Supplysector_bld",
             "L244.india_state_FinalEnergyKeyword_bld",
             "L244.india_state_SubsectorShrwt_bld",
             "L244.india_state_SubsectorShrwtFllt_bld",
             "L244.india_state_SubsectorInterp_bld",
             "L244.india_state_SubsectorInterpTo_bld",
             "L244.india_state_SubsectorLogit_bld",
             "L244.india_state_StubTech_bld",
             "L244.india_state_StubTechCalInput_bld",
             "L244.india_state_StubTechMarket_bld",
             "L244.india_state_GlobalTechIntGainOutputRatio",
             "L244.india_state_GlobalTechInterpTo_bld",
             "L244.india_state_GlobalTechEff_bld",
             "L244.india_state_GlobalTechShrwt_bld",
             "L244.india_state_GlobalTechCost_bld",
             "L244.india_state_GlobalTechSCurve_bld",
             "L244.india_state_HDDCDD_HadCM3"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    GCM <- Scen <- base.building.size <- base.service <- calibrated.value <- comm <-
      degree.days <- efficiency <- efficiency_tech1 <- efficiency_tech2 <- fuel <-
      gcam.consumer <- grid_region <- half_life_new <- half_life_stock <- input.cost <-
      input.ratio <- internal.gains.market.name <- internal.gains.output.ratio <-
      internal.gains.scalar <- market.name <- minicam.energy.input <- multiplier <-
      object <- pcFlsp_mm2 <- pcGDP <- pcflsp_mm2cap <- pop <- region <- resid <-
      satiation.adder <- satiation.level <- sector <- sector.name <- service <- share <-
      share.weight <- share_tech1 <- share_tech2 <- share_type <- state <- steepness_new <-
      steepness_stock <- stockavg <- subsector <- subsector.name <- supplysector <-
      tech_type <- technology <- technology1 <- technology2 <-
      thermal.building.service.input <- to.value <- value <- year <- year.fillout <- . <-
      pop_year <- Sector <- pop_share <- growth <- flsp_growth <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A44.gcam_consumer_en <- get_data(all_data, "energy/A44.gcam_consumer")
    A44.sector_en <- get_data(all_data, "energy/A44.sector")
    A44.india_state_calibrated_techs_bld <- get_data(all_data, "gcam-india/A44.india_state_calibrated_techs_bld")
    A44.india_state_bld_shell_conductance <- get_data(all_data, "gcam-india/A44.india_state_bld_shell_conductance")
    A44.india_state_demandFn_flsp <- get_data(all_data, "gcam-india/A44.india_state_demandFn_flsp")
    A44.india_state_demandFn_serv <- get_data(all_data, "gcam-india/A44.india_state_demandFn_serv")
    A44.india_state_gcam_consumer <- get_data(all_data, "gcam-india/A44.india_state_gcam_consumer")
    A44.india_state_satiation_flsp <- get_data(all_data, "gcam-india/A44.india_state_satiation_flsp")
    A44.india_state_sector <- get_data(all_data, "gcam-india/A44.india_state_sector")
    A44.india_state_subsector_interp <- get_data(all_data, "gcam-india/A44.india_state_subsector_interp")
    A44.india_state_subsector_logit <- get_data(all_data, "gcam-india/A44.india_state_subsector_logit")
    A44.india_state_subsector_shrwt <- get_data(all_data, "gcam-india/A44.india_state_subsector_shrwt")
    A44.india_state_globaltech_cost <- get_data(all_data, "gcam-india/A44.india_state_globaltech_cost")
    A44.india_state_globaltech_eff <- get_data(all_data, "gcam-india/A44.india_state_globaltech_eff") %>%
      gather_years()
    A44.india_state_globaltech_eff_avg <- get_data(all_data, "gcam-india/A44.india_state_globaltech_eff_avg")
    A44.india_state_globaltech_shares <- get_data(all_data, "gcam-india/A44.india_state_globaltech_shares")
    A44.india_state_globaltech_intgains <- get_data(all_data, "gcam-india/A44.india_state_globaltech_intgains")
    A44.india_state_globaltech_retirement <- get_data(all_data, "gcam-india/A44.india_state_globaltech_retirement")
    A44.india_state_globaltech_shrwt <- get_data(all_data, "gcam-india/A44.india_state_globaltech_shrwt")
    A44.india_state_globaltech_interp <- get_data(all_data, "gcam-india/A44.india_state_globaltech_interp")
    A44.india_state_demand_satiation_mult <- get_data(all_data, "gcam-india/A44.india_state_demand_satiation_mult")
    L144.india_state_flsp_bm2_res <- get_data(all_data, "L144.india_state_flsp_bm2_res")
    L144.india_state_flsp_bm2_comm <- get_data(all_data, "L144.india_state_flsp_bm2_comm")
    L144.india_state_in_EJ_comm_F_U_Y <- get_data(all_data, "L144.india_state_in_EJ_comm_F_U_Y")
    L144.india_state_in_EJ_res_F_U_Y <- get_data(all_data, "L144.india_state_in_EJ_res_F_U_Y")
    L143.HDDCDD_scen_ctry_Y <- get_data(all_data, "L143.HDDCDD_scen_ctry_Y")
    A44.india_state_subregional_pop_share <- get_data(all_data, "gcam-india/A44.india_state_subregional_pop_share")
    A44.india_state_subregional_income_share <- get_data(all_data, "gcam-india/A44.india_state_subregional_income_share")
    L100.india_state_pop_ruralurban <- get_data(all_data, "L100.india_state_pop_ruralurban")
    L100.india_state_pcGDP_thous90usd_ruralurban <- get_data(all_data, "L100.india_state_pcGDP_thous90usd_ruralurban")

    # ===================================================
    # Data Processing

    # Note: Building energy demands and floorspace are calculated endogenously - these are undergoing review
    # per-capita demand = (satiation.level - satiation.adder) * (1 - exp( -log2 / satiation.impedance * Demand.Driver)) + satiation.adder)
    #
    # floorspace = (satiation.level - satiation.adder) *
    # [1 - exp{(-ln(2) * per-capita-GDP/satiation.impedance) * (energy_cost/base_energy_cost)^price_effect_exponent}] + satiation.adder
    #
    # satiation.level: maximum per-capita demand that can be achieved
    # satiation.adder: value that allow the starting position of any region to be set along the demand function
    # satiation.impedance: shape parameter


    # Need to delete the buildings sector in the India region (gcam.consumers and supplysectors)
    L244.india_state_DeleteConsumer_bld <- tibble(region = gcam.INDIA_REGION, gcam.consumer = A44.gcam_consumer_en$gcam.consumer)
    L244.india_state_DeleteSupplysector_bld <- tibble(region = gcam.INDIA_REGION, supplysector = A44.sector_en$supplysector)

    # L244.SubregionalShares_gcamindia: subregional population and income shares (not currently used)
    A44.india_state_subregional_pop_share <- A44.india_state_subregional_pop_share %>%
      select (region, gcam.consumer, year = pop.year.fillout, subregional.population.share)

    A44.india_state_subregional_income_share <- A44.india_state_subregional_income_share %>%
      select (region, gcam.consumer, year = inc.year.fillout, subregional.income.share)

    L244.SubregionalShares_gcamindia <- A44.india_state_subregional_pop_share %>%
      left_join(A44.india_state_subregional_income_share, by = c("region", "gcam.consumer", "year")) %>%
      mutate(pop.year.fillout = year, inc.year.fillout = year) %>%
      select(LEVEL2_DATA_NAMES[["SubregionalShares"]])


    # L244.PriceExp_IntGains_gcamindia: price exponent on floorspace and naming of internal gains trial markets
    L244.PriceExp_IntGains_gcamindia <- A44.india_state_gcam_consumer %>%
      mutate (price.exp.year.fillout = '1975') %>%
      mutate (region = 'India') %>%
      select (LEVEL2_DATA_NAMES[["PriceExp_IntGains"]])

    # L244.india_state_Floorspace_gcamindia: base year floorspace
    # Keep all historical years for now - these are needed in calculating satiation adders later on

    # Residential floorspace
    L244.india_state_Floorspace_resid <- L144.india_state_flsp_bm2_res %>%
      rename(base.building.size = value,
             region = state,
             gcam.consumer = sector) %>%
      mutate(base.building.size = round(base.building.size, energy.DIGITS_FLOORSPACE)) %>%
      left_join_error_no_match(A44.india_state_gcam_consumer, by = "gcam.consumer") %>%
      select(LEVEL2_DATA_NAMES[["Floorspace"]])

      # Commercial floorspace
    L244.india_state_Floorspace_comm <- L144.india_state_flsp_bm2_comm %>%
      rename(base.building.size = value,
             region = state,
             gcam.consumer = sector) %>%
      mutate(base.building.size = round(base.building.size, energy.DIGITS_FLOORSPACE)) %>%
      left_join_error_no_match(A44.india_state_gcam_consumer, by = "gcam.consumer") %>%
      select(LEVEL2_DATA_NAMES[["Floorspace"]])

    L244.india_state_Floorspace_full <- bind_rows(L244.india_state_Floorspace_resid, L244.india_state_Floorspace_comm)

    # Final output only has base years
    L244.india_state_Floorspace_gcamindia <- filter(L244.india_state_Floorspace_full, year %in% MODEL_BASE_YEARS)

    # L244.DemandFunction_serv_gcamindia and L244.DemandFunction_flsp_gcamindia: demand function types
    L244.DemandFunction_serv_gcamindia <- A44.india_state_demandFn_serv %>%
      mutate (region = 'India') %>%
      select (LEVEL2_DATA_NAMES[["DemandFunction_serv"]])

    L244.DemandFunction_flsp_gcamindia <- A44.india_state_demandFn_flsp %>%
      mutate (region = 'India') %>%
      select (LEVEL2_DATA_NAMES[["DemandFunction_flsp"]])

    # L244.india_state_Satiation_flsp: Satiation levels assumed for floorspace
    L244.india_state_Satiation_flsp <- A44.india_state_satiation_flsp %>%
      gather(gcam.consumer, value, 'resid rural', 'resid urban', 'comm') %>%
      rename(region = state) %>%
      # Need to make sure that the satiation level is greater than the floorspace in the final base year
      left_join_error_no_match(L244.india_state_Floorspace_gcamindia %>%
                                 filter(year == max(MODEL_BASE_YEARS)), by = c("region", "gcam.consumer")) %>%
      left_join_error_no_match(L100.india_state_pop_ruralurban, by = c("region", "gcam.consumer" = "sector", "year")) %>%
      mutate(year = as.integer(year),
             # value.y = population
             pcflsp_mm2cap = base.building.size / pop,
             # Satiation level = must be greater than the observed value in the final calibration year, so if observed value is
             # greater than calculated, multiply observed by 1.001
             satiation.level = round(pmax(value * CONV_THOUS_BIL, pcflsp_mm2cap * 1.001), energy.DIGITS_SATIATION_ADDER)) %>%
      left_join_error_no_match(A44.india_state_gcam_consumer, by = c("gcam.consumer", "nodeInput", "building.node.input")) %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], "satiation.level")


    # L244.india_state_SatiationAdder: Satiation adders in floorspace demand function
    # Required for shaping the future floorspace growth trajectories in each region
    # Match in the per-capita GDP, total floorspace, and population (for calculating per-capita floorspace)

    # We will filter GDP to energy.SATIATION_YEAR, but this may be greater than the historical years present
    # under timeshift conditions. So we adjust energy.SATIATION_YEAR
    energy.SATIATION_YEAR <- min(max(MODEL_BASE_YEARS), energy.SATIATION_YEAR)

    L244.india_state_SatiationAdder <- L244.india_state_Satiation_flsp %>%
      # Add per capita GDP
      left_join_error_no_match(L100.india_state_pcGDP_thous90usd_ruralurban %>%
                                 filter(year == energy.SATIATION_YEAR), by = c("region", "gcam.consumer" = "sector")) %>%
      # Add floorspace
      left_join_error_no_match(L244.india_state_Floorspace_full %>%
                                 filter(year == max(MODEL_BASE_YEARS)), by = c("region", "gcam.consumer","year")) %>%
      left_join_error_no_match(L100.india_state_pop_ruralurban, by = c("region", "year", "gcam.consumer" = "sector")) %>%
      mutate(pcFlsp_mm2 = base.building.size / pop,
             # Calculate the satiation adders
             satiation.adder = round(satiation.level - (exp((log(2) * pcGDP) / (energy.GDP_MID_SATIATION)) * (satiation.level - pcFlsp_mm2)),
               energy.DIGITS_SATIATION_ADDER),
             # The satiation adder (million square meters of floorspace per person) needs to be less than the per-capita demand in the final calibration year
             satiation.adder = if_else(satiation.adder > pcFlsp_mm2, pcFlsp_mm2 * 0.999, satiation.adder)) %>%
      rename (nodeInput = nodeInput.x, building.node.input =building.node.input.x) %>%
      select(LEVEL2_DATA_NAMES[["SatiationAdder"]])

    # Heating and cooling degree days (thermal services only)
    # First, separate the thermal from the generic services. Generic services will be assumed to produce
    # internal gain energy, so anything in the internal gains assumptions table will be assumed generic
    generic_services <- unique(A44.india_state_globaltech_intgains$supplysector)
    thermal_services <- setdiff(unique(A44.india_state_sector$supplysector), generic_services)

    # L244.HDDCDD: Heating and cooling degree days by scenario
    L244.india_state_HDDCDD_scen <- L143.HDDCDD_scen_ctry_Y %>%
      rename(region = state,
             degree.days = value) %>%
      filter(iso == "ind") %>%
      select (region = country, scen=SRES, GCM, variable, year, degree.days)

    L244.india_state_HDDCDD_normal <- L244.india_state_HDDCDD_scen %>%
      group_by(variable) %>%
      summarise(degree.days = mean(degree.days)) %>%
      ungroup() %>%
      mutate (region = 'India') %>%
      select (region, variable, year, degree.days)

    # Subset the heating and cooling services, separately
    heating_services <- thermal_services[grepl("heating", thermal_services)]
    cooling_services <- thermal_services[grepl("cooling", thermal_services)]

    L244.india_state_HDDCDD_temp <- tidyr::crossing(region = 'India', thermal.building.service.input = thermal_services) %>%
      # Add in gcam.consumer
      left_join_error_no_match(A44.india_state_calibrated_techs_bld %>%
                                 select(service, gcam.consumer = sector) %>%
                                 distinct(), by = c("thermal.building.service.input" = "service")) %>%
      # Add in nodeInput and building.node.input
      left_join_error_no_match(A44.india_state_gcam_consumer, by = "gcam.consumer") %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], thermal.building.service.input) %>%
      # Add in model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Add HDD/CDD so that we can join with L244.HDDCDD_scen_state, remove at end
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      # Add in degree days
      # L244.HDDCDD_scen_state has multiple scenarios, rows in this tbl_df are intended to be duplicated for each scenario
      # left_join_error_no_match throws an error when rows are duplicated (as intended), so left_join is used
      left_join(L244.india_state_HDDCDD_scen, by = c("region", "variable", "year")) %>%
      mutate(degree.days = round(degree.days, energy.DIGITS_HDDCDD))


    L244.india_state_HDDCDD_HadCM3 <- L244.india_state_HDDCDD_temp %>%
      filter(scen == "B1", GCM == "HadCM3") %>%
      select(-scen, -GCM, -variable) %>%
      select(LEVEL2_DATA_NAMES[["HDDCDD"]])

    # L244.india_state_ShellConductance_bld: Shell conductance (inverse of shell efficiency)
    L244.india_state_ShellConductance_bld <- A44.india_state_bld_shell_conductance %>%
      # Convert to long form
      gather_years() %>%
      # Interpolate to model years
      complete(gcam.consumer, year = c(year, MODEL_YEARS)) %>%
      group_by(gcam.consumer) %>%
      mutate(value = round(approx_fun(year, value), energy.DIGITS_EFFICIENCY)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # Add nodeInput and building.node.input
      left_join_error_no_match(A44.india_state_gcam_consumer, by = "gcam.consumer") %>%
      mutate(floor.to.surface.ratio = energy.FLOOR_TO_SURFACE_RATIO,
             shell.year = year) %>%
      # Rename columns
      rename(shell.conductance = value) %>%
      mutate (region = 'India') %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])

    # The remainder of the building-level parameters require information about the output of each service, which we do not have yet

    # L244.Supplysector_bld: Supplysector info for buildings

    L244.india_state_Supplysector_bld <- A44.india_state_sector %>%
      mutate (logit.year.fillout = '1975') %>%
      mutate (region = 'India') %>%
      select (LEVEL2_DATA_NAMES[["Supplysector"]],  LOGIT_TYPE_COLNAME)

    # L244.FinalEnergyKeyword_bld: Supply sector keywords for detailed building sector
    L244.india_state_FinalEnergyKeyword_bld <- A44.india_state_sector %>%
      mutate (region = 'India') %>%
      select (LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]])

    # L244.SubsectorLogit_bld: Subsector logit exponents of building sector
    L244.india_state_SubsectorLogit_bld <- A44.india_state_subsector_logit %>%
      mutate (logit.year.fillout = '1975') %>%
      mutate (region = 'India') %>%
      select (LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)

    # L244.SubsectorShrwt_bld and L244.SubsectorShrwtFllt_bld: Subsector shareweights of building sector
    if(any(!is.na(A44.india_state_subsector_shrwt$year))) {
      L244.india_state_SubsectorShrwt_bld <- A44.india_state_subsector_shrwt %>%
        filter(!is.na(year)) %>% mutate (region = 'India') %>%
        select(LEVEL2_DATA_NAMES[["SubsectorShrwt"]])
    }

    if(any(!is.na(A44.india_state_subsector_shrwt$year.fillout))) {
      L244.india_state_SubsectorShrwtFllt_bld <- A44.india_state_subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>% mutate (region = 'India') %>%
        select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])
    }

    if(any(!is.na(A44.india_state_subsector_interp$to.value))) {
      L244.india_state_SubsectorInterp_bld <- A44.india_state_subsector_interp %>%
        filter(!is.na(to.value)) %>% mutate (region = 'India') %>%
        select(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]])
    }

    if(any(is.na(A44.india_state_subsector_interp$to.value))) {
      L244.india_state_SubsectorInterpTo_bld <- A44.india_state_subsector_interp %>%
        filter(is.na(to.value)) %>% mutate (region = 'India') %>%
        select(LEVEL2_DATA_NAMES[["SubsectorInterp"]])
    }


    # L244.india_state_StubTech_bld: Identification of stub technologies for buildings
    L244.india_state_StubTech_bld <- A44.india_state_globaltech_eff %>%
      select(supplysector, subsector, technology) %>%
      distinct() %>%
      mutate (region = 'India') %>%
      select(LEVEL2_DATA_NAMES[["Tech"]]) %>%
      rename(stub.technology = technology)

    # L244.india_state_GlobalTechEff_bld: Assumed efficiencies (all years) of buildings technologies
    L244.india_state_end_use_eff <- A44.india_state_globaltech_eff %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT))

    # Note - this code assumes that base-year efficiences are identical (should fix to copy over to make sure)
    L244.india_state_GlobalTechEff_bld <- L244.india_state_end_use_eff %>%
      rename(sector.name = supplysector,
             subsector.name = subsector, efficiency = value) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]])

    # L244.india_state_StubTechMarket_bld: Specify market names for fuel inputs to all technologies in each state
    L244.india_state_StubTechMarket_bld <- L244.india_state_end_use_eff %>%
      mutate(market.name = gcam.INDIA_REGION) %>%
      rename(stub.technology = technology) %>%
      mutate (region = 'India') %>%
      select(LEVEL2_DATA_NAMES[["StubTechMarket"]])

    # L244.StubTechCalInput_bld: Calibrated energy consumption by buildings technologies
    # Combine residential and commercial energy data
    L244.india_state_in_EJ_R_bld_serv_F_Yh <- bind_rows(L144.india_state_in_EJ_res_F_U_Y, L144.india_state_in_EJ_comm_F_U_Y) %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      rename(supplysector = service) %>%
      # Add subsector and energy.input
      left_join_error_no_match(A44.india_state_calibrated_techs_bld %>%
                                 select(sector, supplysector, fuel, subsector, minicam.energy.input) %>%
                                 distinct(), by = c("sector", "supplysector", "fuel")) %>%
      select(region = state, supplysector, subsector, minicam.energy.input, year, calibrated.value)

    # Shares allocated to partitioned technologies need to be computed first using efficiencies
    L244.india_state_globaltech_eff_prt <- A44.india_state_globaltech_eff %>%
      semi_join(A44.india_state_globaltech_eff_avg, by = c("supplysector", "subsector")) %>%
      filter(year == gcamindia.EFFICIENCY_PARTITION_YEAR) %>%
      select(supplysector, subsector, technology, efficiency = value)

    # Calculate technology shares using efficiency values

    L244.india_state_globaltech_shares <- A44.india_state_globaltech_eff_avg %>%
      # Adding specific technology efficiency to stock average efficiency
      left_join_error_no_match(L244.india_state_globaltech_eff_prt, by = c("supplysector", "subsector", "technology1" = "technology")) %>%
      rename(efficiency_tech1 = efficiency) %>%
      left_join_error_no_match(L244.india_state_globaltech_eff_prt, by = c("supplysector", "subsector", "technology2" = "technology")) %>%
      rename(efficiency_tech2 = efficiency) %>%
      # Calculate technology shares using stock average efficiency and individual technology efficiencies
      # Equation can be derived by solving following system of equations:
      # stockavg = efficiency_tech1 * share_tech1 + efficiency_tech2 * share_tech2
      # share_tech1 + share_tech2 = 1
      mutate(share_tech1 = (stockavg - efficiency_tech2) / (efficiency_tech1 - efficiency_tech2),
             share_tech2 = 1 - share_tech1) %>%
      # Keep only same names as A44.india_state_globaltech_shares and bind with A44.india_state_globaltech_shares
      select(names(A44.india_state_globaltech_shares)) %>%
      #bind_rows(A44.india_state_globaltech_shares) %>%
      # Clunky, but we want only one technology and share value, currently have technology1, technology2, share1, share2
      gather(share_type, share, share_tech1, share_tech2)%>%
      gather(tech_type, technology, technology1, technology2) %>%
      # Filter for same technology and share number, then remove tech_type and share_type columns
      filter(substr(tech_type, nchar(tech_type), nchar(tech_type)) == substr(share_type, nchar(share_type), nchar(share_type))) %>%
      select(-tech_type, -share_type)

    # For calibration table, start with global tech efficiency table, repeat by states, and match in tech shares.
    L244.india_state_StubTechCalInput_bld <- L244.india_state_GlobalTechEff_bld %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
      # Using left_join because we don't have shares for all technologies, NAs will be set to 1
      left_join(L244.india_state_globaltech_shares, by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      replace_na(list(share = 1)) %>%
      # Add energy by state/service/fuel
      left_join_error_no_match(L244.india_state_in_EJ_R_bld_serv_F_Yh, by = c("supplysector", "subsector", "minicam.energy.input", "year")) %>%
      # calibrated.value = energy * share
      mutate(calibrated.value = round(share * calibrated.value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             calOutputValue = calibrated.value) %>%
      # Set subsector and technology shareweights
      set_subsector_shrwt() %>%
      mutate(tech.share.weight =  if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]])

    # L244.india_state_GlobalTechShrwt_bld: Default shareweights for global building technologies
    L244.india_state_GlobalTechShrwt_bld <- A44.india_state_globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], share.weight)

    # L244.india_state_GlobalTechInterpTo_bld: Technology shareweight interpolation (selected techs only)
    L244.india_state_GlobalTechInterpTo_bld <- A44.india_state_globaltech_interp %>%
      set_years() %>%
      mutate(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInterpTo"]])

    # L244.GlobalTechCost_bld: Non-fuel costs of global building technologies
    L244.india_state_GlobalTechCost_bld <- A44.india_state_globaltech_cost %>%
      gather_years(value_col = "input.cost") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(input.cost = approx_fun(year, input.cost)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])

    # L244.india_state_GlobalTechSCurve_bld: Retirement rates for building technologies
    L244.india_state_GlobalTechSCurve_bld <- L244.india_state_GlobalTechCost_bld %>%
      filter(year %in% c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS),
             sector.name %in% A44.india_state_globaltech_retirement$supplysector) %>%
      # Add lifetimes and steepness
      left_join_error_no_match(A44.india_state_globaltech_retirement, by = c("sector.name" = "supplysector", "subsector.name" = "subsector", "technology" = "technology")) %>%
      # Set steepness/halflife values to stock for base years, new for future years
      mutate(steepness = if_else(year == max(MODEL_BASE_YEARS), steepness_stock, steepness_new),
             half.life = if_else(year == max(MODEL_BASE_YEARS), half_life_stock, half_life_new)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechSCurve"]])

    # L244.india_state_GlobalTechIntGainOutputRatio: Output ratios of internal gain energy from non-thermal building services
    A44.india_state_calibrated_techs_bld_consumer <- A44.india_state_calibrated_techs_bld %>%
      select(gcam.consumer = sector, supplysector) %>%
      distinct()

    L244.india_state_GlobalTechIntGainOutputRatio <- A44.india_state_globaltech_intgains %>%
      repeat_add_columns(tibble(year = MODEL_YEARS))%>%
      # Add gcam.consumer (sector)
      left_join_error_no_match(A44.india_state_calibrated_techs_bld_consumer, by = "supplysector") %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      # Add internal.gains.market.name
      left_join_error_no_match(A44.india_state_gcam_consumer, by = "gcam.consumer") %>%
      # Add efficiency
      left_join_error_no_match(L244.india_state_GlobalTechEff_bld,
                               by = c("sector.name", "subsector.name", "technology", "year")) %>%
      mutate(internal.gains.output.ratio = round(input.ratio / efficiency, energy.DIGITS_EFFICIENCY)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], internal.gains.output.ratio, internal.gains.market.name)

    # L244.GenericBaseService and L244.ThermalBaseService: Base year output of buildings services (per unit floorspace)
    # Base-service: Multiply energy consumption by efficiency for each technology, and aggregate by service
    L244.india_state_base_service <- L244.india_state_StubTechCalInput_bld %>%
      # Add in efficiency by technology
      left_join_error_no_match(L244.india_state_GlobalTechEff_bld,
                               by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                      "stub.technology" = "technology", "year", "minicam.energy.input")) %>%
      # Calculate base.service = calibrated.value(energy) * efficiency
      mutate(base.service = round(calibrated.value * efficiency, energy.DIGITS_CALOUTPUT)) %>%
      # Aggregate base service by service (supplysector)
      group_by(region, supplysector, year) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() %>%
      # Add gcam.consumer (sector)
      left_join_error_no_match(A44.india_state_calibrated_techs_bld_consumer, by = "supplysector") %>%
      # Add nodeInput and building.node.input
      left_join_error_no_match(A44.india_state_gcam_consumer, by = "gcam.consumer")

    # Separate thermal and generic services into separate tables with different ID strings
    L244.india_state_GenericBaseService <- L244.india_state_base_service %>%
      filter(supplysector %in% generic_services) %>%
      rename(building.service.input = supplysector) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseService"]])

    L244.india_state_ThermalBaseService <- L244.india_state_base_service %>%
      filter(supplysector %in% thermal_services) %>%
      rename(thermal.building.service.input = supplysector) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseService"]])

    # L244.india_state_GenericServiceSatiation: Satiation levels assumed for non-thermal building services
    # Just multiply the base-service by an exogenous multiplier
    L244.india_state_GenericServiceSatiation <- L244.india_state_GenericBaseService %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # Add floorspace
      left_join_error_no_match(L244.india_state_Floorspace_gcamindia, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      # Add multiplier
      left_join_error_no_match(A44.india_state_demand_satiation_mult, by = c("building.service.input" = "supplysector")) %>%
      # Satiation level = service per floorspace * multiplier
      mutate(satiation.level = round(base.service / base.building.size * multiplier, energy.DIGITS_COEFFICIENT)) %>%
      #mutate(satiation.level = 90) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]])

    # L244.ThermalServiceSatiation: Satiation levels assumed for thermal building services
    L244.india_state_ThermalServiceSatiation <- L244.india_state_ThermalBaseService %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # Add floorspace
      left_join_error_no_match(L244.india_state_Floorspace_gcamindia, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      # Add multiplier
      left_join_error_no_match(A44.india_state_demand_satiation_mult, by = c("thermal.building.service.input" = "supplysector")) %>%
      # Satiation level = service per floorspace * multiplier
      mutate(satiation.level = round(base.service / base.building.size * multiplier, energy.DIGITS_COEFFICIENT)) %>%
      #mutate(satiation.level = 90) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]])

    # L244.Intgains_scalar: Scalers relating internal gain energy to increased/reduced cooling/heating demands
    variable <- c("HDD", "CDD")
    scalar <- c(energy.INTERNAL_GAINS_SCALAR_INDIA_H, energy.INTERNAL_GAINS_SCALAR_INDIA_C)
    DDnorm <- c(gcamindia.BASE_HDD_INDIA, gcamindia.BASE_CDD_INDIA)
    India.base.scalar <- tibble(variable, scalar, DDnorm)
    threshold_HDD <- 500

    L244.india_state_Intgains_scalar <- L244.india_state_ThermalServiceSatiation %>%
      # Assign HDD or CDD
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      # Add DDnorm & scalar
      left_join_error_no_match(India.base.scalar, by = "variable") %>%
      # Add degree days
      left_join(L244.india_state_HDDCDD_normal, by = c("region", "variable")) %>%
      mutate(internal.gains.scalar = round(scalar * degree.days / DDnorm, energy.DIGITS_HDDCDD)) %>%
             # Prevent very warm places from having negative heating demands, using exogenous threshold
             #internal.gains.scalar = if_else(variable == "HDD" & degree.days < threshold_HDD, 0, internal.gains.scalar)) %>%
      select(LEVEL2_DATA_NAMES[["Intgains_scalar"]])

    # ===================================================
    # Produce outputs
    L244.india_state_DeleteConsumer_bld %>%
      add_title("Deletes building sector in India region to rewrite with gcam-india data") %>%
      add_units("NA") %>%
      add_comments("gcam.consumer column from A44.india_state_gcam_consumer") %>%
      add_legacy_name("L244.india_state_DeleteConsumer_bld") %>%
      add_precursors("energy/A44.gcam_consumer") ->
      L244.india_state_DeleteConsumer_bld

    L244.india_state_DeleteSupplysector_bld %>%
      add_title("Deletes building sector in India region to rewrite with gcam-india data") %>%
      add_units("NA") %>%
      add_comments("supplysector column from A44.india_state_sector") %>%
      add_legacy_name("L244.india_state_DeleteSupplysector_bld") %>%
      add_precursors("energy/A44.sector") ->
      L244.india_state_DeleteSupplysector_bld

    L244.SubregionalShares_gcamindia %>%
      add_title("Subregional population and income shares") %>%
      add_units("Unitless") %>%
      add_comments("Default values used for years and shares") %>%
      add_legacy_name("L244.SubregionalShares") %>%
      add_precursors("gcam-india/A44.india_state_gcam_consumer", "gcam-india/A44.india_state_subregional_pop_share",
                     "gcam-india/A44.india_state_subregional_income_share") ->
      L244.SubregionalShares_gcamindia

    L244.PriceExp_IntGains_gcamindia %>%
      add_title("Price exponent on floorspace and naming of internal gains trial markets") %>%
      add_units("Unitless") %>%
      add_comments("A44.india_state_gcam_consumer written to all states") %>%
      add_legacy_name("L244.PriceExp_IntGains") %>%
      add_precursors("gcam-india/A44.india_state_gcam_consumer") ->
      L244.PriceExp_IntGains_gcamindia

    L244.india_state_Floorspace_gcamindia %>%
      add_title("base year floorspace") %>%
      add_units("billion m2") %>%
      add_comments("Data from L144.india_state_flsp_bm2_res and L144.india_state_flsp_bm2_comm") %>%
      add_legacy_name("L244.Floorspace") %>%
      add_precursors("L144.india_state_flsp_bm2_res", "L144.india_state_flsp_bm2_comm", "gcam-india/A44.india_state_gcam_consumer") ->
      L244.india_state_Floorspace_gcamindia

    L244.DemandFunction_serv_gcamindia %>%
      add_title("Service demand function types") %>%
      add_units("NA") %>%
      add_comments("A44.india_state_demandFn_serv written to all states") %>%
      add_legacy_name("L244.DemandFunction_serv") %>%
      add_precursors("gcam-india/A44.india_state_demandFn_serv") ->
      L244.DemandFunction_serv_gcamindia

    L244.DemandFunction_flsp_gcamindia %>%
      add_title("Floorspace demand function types") %>%
      add_units("NA") %>%
      add_comments("A44.india_state_demandFn_flsp written to all states") %>%
      add_legacy_name("L244.DemandFunction_flsp") %>%
      add_precursors("gcam-india/A44.india_state_demandFn_flsp") ->
      L244.DemandFunction_flsp_gcamindia

    L244.india_state_Satiation_flsp %>%
      add_title("Satiation levels assumed for floorspace") %>%
      add_units("million m2 / person") %>%
      add_comments("Values from A44.india_state_satiation_flsp or L244.india_state_Floorspace_gcamindia/L100.india_state_pop_ruralurban") %>%
      add_comments("Whichever is larger") %>%
      add_legacy_name("L244.Satiation_flsp") %>%
      add_precursors("gcam-india/A44.india_state_satiation_flsp", "gcam-india/A44.india_state_gcam_consumer", "L100.india_state_pop_ruralurban",
                     "L144.india_state_flsp_bm2_res", "L144.india_state_flsp_bm2_comm") ->
      L244.india_state_Satiation_flsp

    L244.india_state_SatiationAdder %>%
      add_title("Satiation adders in floorspace demand function") %>%
      add_units("million m2 / person") %>%
      add_comments("Calculated with function dependent on satiation level; per capita floorspace; and per capita GDP") %>%
      add_legacy_name("L244.SatiationAdder") %>%
      add_precursors("gcam-india/A44.india_state_satiation_flsp", "gcam-india/A44.india_state_gcam_consumer", "L100.india_state_pop_ruralurban",
                     "L144.india_state_flsp_bm2_res", "L144.india_state_flsp_bm2_comm", "L100.india_state_pcGDP_thous90usd_ruralurban") ->
      L244.india_state_SatiationAdder

    L244.india_state_ThermalBaseService %>%
      add_title("Base year output of thermal buildings services") %>%
      add_units("EJ per unit floorspace") %>%
      add_comments("Multiplied energy consumption by efficiency for each technology, then aggregated by service") %>%
      add_legacy_name("L244.ThermalBaseService") %>%
      add_precursors("L144.india_state_in_EJ_res_F_U_Y", "L144.india_state_in_EJ_comm_F_U_Y", "gcam-india/A44.india_state_calibrated_techs_bld",
                     "gcam-india/A44.india_state_globaltech_eff", "gcam-india/A44.india_state_globaltech_eff_avg", "gcam-india/A44.india_state_globaltech_shares",
                     "gcam-india/A44.india_state_gcam_consumer") ->
      L244.india_state_ThermalBaseService

    L244.india_state_GenericBaseService %>%
      add_title("Base year output of generic buildings services") %>%
      add_units("EJ per unit floorspace") %>%
      add_comments("Multiplied energy consumption by efficiency for each technology, then aggregated by service") %>%
      add_legacy_name("L244.GenericBaseService") %>%
      add_precursors("L144.india_state_in_EJ_res_F_U_Y", "L144.india_state_in_EJ_comm_F_U_Y", "gcam-india/A44.india_state_calibrated_techs_bld",
                     "gcam-india/A44.india_state_globaltech_eff", "gcam-india/A44.india_state_globaltech_eff_avg", "gcam-india/A44.india_state_globaltech_shares",
                     "gcam-india/A44.india_state_gcam_consumer") ->
      L244.india_state_GenericBaseService

    L244.india_state_GenericServiceSatiation %>%
      add_title("Satiation levels assumed for non-thermal building services") %>%
      add_units("EJ/billion m2 floorspace") %>%
      add_comments("Satiation level = base service / floorspace * exogenous multiplier") %>%
      add_legacy_name("L244.GenericServiceSatiation") %>%
      add_precursors("L144.india_state_in_EJ_res_F_U_Y", "L144.india_state_in_EJ_comm_F_U_Y", "gcam-india/A44.india_state_calibrated_techs_bld",
                     "gcam-india/A44.india_state_globaltech_eff", "gcam-india/A44.india_state_globaltech_eff_avg", "gcam-india/A44.india_state_globaltech_shares",
                     "gcam-india/A44.india_state_gcam_consumer", "L144.india_state_flsp_bm2_res", "L144.india_state_flsp_bm2_comm",
                     "gcam-india/A44.india_state_demand_satiation_mult") ->
      L244.india_state_GenericServiceSatiation

    L244.india_state_ThermalServiceSatiation %>%
      add_title("Satiation levels assumed for thermal building services") %>%
      add_units("EJ/billion m2 floorspace") %>%
      add_comments("Satiation level = base service / floorspace * exogenous multiplier") %>%
      add_legacy_name("L244.ThermalServiceSatiation") %>%
      add_precursors("L144.india_state_in_EJ_res_F_U_Y", "L144.india_state_in_EJ_comm_F_U_Y", "gcam-india/A44.india_state_calibrated_techs_bld",
                     "gcam-india/A44.india_state_globaltech_eff", "gcam-india/A44.india_state_globaltech_eff_avg", "gcam-india/A44.india_state_globaltech_shares",
                     "gcam-india/A44.india_state_gcam_consumer", "L144.india_state_flsp_bm2_res", "L144.india_state_flsp_bm2_comm",
                     "gcam-india/A44.india_state_demand_satiation_mult") ->
      L244.india_state_ThermalServiceSatiation

    L244.india_state_Intgains_scalar %>%
      add_title("Scalers relating internal gain energy to increased/reduced cooling/heating demands") %>%
      add_units("Unitless") %>%
      add_comments("internal.gains.scalar = exogenous scalar * degree.days / exogenous degree day norm") %>%
      add_legacy_name("L244.Intgains_scalar") %>%
      add_precursors("L144.india_state_in_EJ_res_F_U_Y", "L144.india_state_in_EJ_comm_F_U_Y", "gcam-india/A44.india_state_calibrated_techs_bld",
                     "gcam-india/A44.india_state_globaltech_eff", "gcam-india/A44.india_state_globaltech_eff_avg", "gcam-india/A44.india_state_globaltech_shares",
                     "gcam-india/A44.india_state_gcam_consumer", "L144.india_state_flsp_bm2_res", "L144.india_state_flsp_bm2_comm",
                     "gcam-india/A44.india_state_demand_satiation_mult", "L143.HDDCDD_scen_ctry_Y") ->
      L244.india_state_Intgains_scalar

    L244.india_state_ShellConductance_bld %>%
      add_title("Shell conductance (inverse of shell efficiency) by state") %>%
      add_units("Unitless") %>%
      add_comments("values from A44.india_state_bld_shell_conductance") %>%
      add_legacy_name("L244.ShellConductance_bld") %>%
      add_precursors("gcam-india/A44.india_state_bld_shell_conductance", "gcam-india/A44.india_state_gcam_consumer") ->
      L244.india_state_ShellConductance_bld

    L244.india_state_Supplysector_bld %>%
      add_title("Supplysector info for buildings") %>%
      add_units("Unitless") %>%
      add_comments("A44.india_state_sector written to all states") %>%
      add_legacy_name("L244.Supplysector_bld") %>%
      add_precursors("gcam-india/A44.india_state_sector") ->
      L244.india_state_Supplysector_bld

    L244.india_state_FinalEnergyKeyword_bld %>%
      add_title("Supply sector keywords for detailed building sector") %>%
      add_units("NA") %>%
      add_comments("A44.india_state_sector written to all states") %>%
      add_legacy_name("L244.FinalEnergyKeyword_bld") %>%
      add_precursors("gcam-india/A44.india_state_sector") ->
      L244.india_state_FinalEnergyKeyword_bld

    if(exists("L244.india_state_SubsectorShrwt_bld")) {
      L244.india_state_SubsectorShrwt_bld %>%
        add_title("Subsector shareweights of building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.india_state_subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwt_bld") %>%
        add_precursors("gcam-india/A44.india_state_subsector_shrwt") ->
        L244.india_state_SubsectorShrwt_bld
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorShrwt_bld") ->
        L244.india_state_SubsectorShrwt_bld
    }

    if(exists("L244.india_state_SubsectorShrwtFllt_bld")) {
      L244.india_state_SubsectorShrwtFllt_bld %>%
        add_title("Subsector shareweights of building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.india_state_subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld") %>%
        add_precursors("gcam-india/A44.india_state_subsector_shrwt") ->
        L244.india_state_SubsectorShrwtFllt_bld
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld") ->
        L244.india_state_SubsectorShrwtFllt_bld
    }


    if(exists("L244.india_state_SubsectorInterp_bld")) {
      L244.india_state_SubsectorInterp_bld %>%
        add_title("Subsector shareweight interpolation of building sector") %>%
        add_units("NA") %>%
        add_comments("A44.india_state_subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterp_bld") %>%
        add_precursors("gcam-india/A44.india_state_subsector_interp") ->
        L244.india_state_SubsectorInterp_bld
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorInterp_bld") ->
        L244.india_state_SubsectorInterp_bld
    }

    if(exists("L244.india_state_SubsectorInterpTo_bld")) {
      L244.india_state_SubsectorInterpTo_bld %>%
        add_title("Subsector shareweight interpolation of building sector") %>%
        add_units("NA") %>%
        add_comments("A44.india_state_subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterpTo_bld") %>%
        add_precursors("gcam-india/A44.india_state_subsector_interp") ->
        L244.india_state_SubsectorInterpTo_bld
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorInterpTo_bld") ->
        L244.india_state_SubsectorInterpTo_bld
    }

    L244.india_state_SubsectorLogit_bld %>%
      add_title("Subsector logit exponents of building sector") %>%
      add_units("Unitless") %>%
      add_comments("A44.india_state_subsector_logit written to all states") %>%
      add_legacy_name("L244.SubsectorLogit_bld") %>%
      add_precursors("gcam-india/A44.india_state_subsector_logit") ->
      L244.india_state_SubsectorLogit_bld

    L244.india_state_StubTech_bld %>%
      add_title("Identification of stub technologies for buildings") %>%
      add_units("NA") %>%
      add_comments("A44.india_state_globaltech_eff written to all states") %>%
      add_legacy_name("L244.StubTech_bld") %>%
      add_precursors("gcam-india/A44.india_state_globaltech_eff") ->
      L244.india_state_StubTech_bld

    L244.india_state_StubTechCalInput_bld %>%
      add_title("Calibrated energy consumption and share weights by buildings technologies") %>%
      add_units("calibrated.value: EJ/yr; shareweights: Unitless") %>%
      add_comments("Energy consumption multiplied by shares to get calibrated energy") %>%
      add_comments("Shares calculated using efficiency averages") %>%
      add_legacy_name("L244.StubTechCalInput_bld") %>%
      add_precursors("L144.india_state_in_EJ_res_F_U_Y", "L144.india_state_in_EJ_comm_F_U_Y", "gcam-india/A44.india_state_calibrated_techs_bld",
                     "gcam-india/A44.india_state_globaltech_eff", "gcam-india/A44.india_state_globaltech_eff_avg", "gcam-india/A44.india_state_globaltech_shares") ->
      L244.india_state_StubTechCalInput_bld

    L244.india_state_StubTechMarket_bld %>%
      add_title("market names for fuel inputs to all technologies in each state") %>%
      add_units("NA") %>%
      add_comments("Categories from A44.india_state_globaltech_eff written to all states") %>%
      add_comments("Market set to states for electricity") %>%
      add_legacy_name("L244.india_state_StubTechMarket_bld") %>%
      add_precursors("gcam-india/A44.india_state_globaltech_eff") ->
      L244.india_state_StubTechMarket_bld

    L244.india_state_GlobalTechIntGainOutputRatio %>%
      add_title("Output ratios of internal gain energy from non-thermal building services") %>%
      add_units("Unitless") %>%
      add_comments("internal.gains.output.ratio = input.ratio from A44.india_state_globaltech_intgains divided by efficiency from L244.india_state_GlobalTechEff_bld") %>%
      add_legacy_name("L244.india_state_GlobalTechIntGainOutputRatio") %>%
      add_precursors("gcam-india/A44.india_state_globaltech_intgains", "gcam-india/A44.india_state_calibrated_techs_bld",
                     "gcam-india/A44.india_state_gcam_consumer", "gcam-india/A44.india_state_globaltech_eff") ->
      L244.india_state_GlobalTechIntGainOutputRatio

    L244.india_state_GlobalTechInterpTo_bld %>%
      add_title("Technology shareweight interpolation") %>%
      add_units("NA") %>%
      add_comments("Directly from A44.india_state_globaltech_interp") %>%
      add_legacy_name("L244.india_state_GlobalTechInterpTo_bld") %>%
      add_precursors("gcam-india/A44.india_state_globaltech_interp") ->
      L244.india_state_GlobalTechInterpTo_bld

    L244.india_state_GlobalTechEff_bld %>%
      add_title("Assumed efficiencies (all years) of buildings technologies") %>%
      add_units("Unitless") %>%
      add_comments("Values from A44.india_state_globaltech_eff") %>%
      add_legacy_name("L244.india_state_GlobalTechEff_bld") %>%
      add_precursors("gcam-india/A44.india_state_globaltech_eff") ->
      L244.india_state_GlobalTechEff_bld

    L244.india_state_GlobalTechShrwt_bld %>%
      add_title("Default shareweights for global building technologies") %>%
      add_units("Unitless") %>%
      add_comments("Values interpolated to model years from A44.india_state_globaltech_shrwt") %>%
      add_legacy_name("L244.GlobalTechShrwt_bld") %>%
      add_precursors("gcam-india/A44.india_state_globaltech_shrwt") ->
      L244.india_state_GlobalTechShrwt_bld

    L244.india_state_GlobalTechCost_bld %>%
      add_title("Non-fuel costs of global building technologies") %>%
      add_units("1975$/GJ") %>%
      add_comments("Values from A44.india_state_globaltech_cost") %>%
      add_legacy_name("L244.GlobalTechCost_bld") %>%
      add_precursors("gcam-india/A44.india_state_globaltech_cost") ->
      L244.india_state_GlobalTechCost_bld

    L244.india_state_GlobalTechSCurve_bld %>%
      add_title("Retirement rates for building technologies") %>%
      add_units("lifetime/half.life = years") %>%
      add_comments("Lifetime, steepness, and half.life from A44.india_state_globaltech_retirement") %>%
      add_legacy_name("L244.india_state_GlobalTechSCurve_bld") %>%
      add_precursors("gcam-india/A44.india_state_globaltech_cost", "gcam-india/A44.india_state_globaltech_retirement") ->
      L244.india_state_GlobalTechSCurve_bld

    L244.india_state_HDDCDD_HadCM3 %>%
      add_title("Heating and Cooling Degree Days by State - constant at historical levels") %>%
      add_units("Fahrenheit Degree Days") %>%
      add_comments("L143.HDDCDD_scen_ctry_Y assigned to GCAM subsectors") %>%
      add_precursors("L143.HDDCDD_scen_ctry_Y", "gcam-india/A44.india_state_sector",
                     "gcam-india/A44.india_state_calibrated_techs_bld", "gcam-india/A44.india_state_gcam_consumer") ->
      L244.india_state_HDDCDD_HadCM3

   return_data(L244.india_state_DeleteConsumer_bld,
                L244.india_state_DeleteSupplysector_bld,
                L244.SubregionalShares_gcamindia,
                L244.PriceExp_IntGains_gcamindia,
                L244.india_state_Floorspace_gcamindia,
                L244.DemandFunction_serv_gcamindia,
                L244.DemandFunction_flsp_gcamindia,
                L244.india_state_Satiation_flsp,
                L244.india_state_SatiationAdder,
                L244.india_state_ThermalBaseService,
                L244.india_state_GenericBaseService,
                L244.india_state_ThermalServiceSatiation,
                L244.india_state_GenericServiceSatiation,
                L244.india_state_Intgains_scalar,
                L244.india_state_ShellConductance_bld,
                L244.india_state_Supplysector_bld,
                L244.india_state_FinalEnergyKeyword_bld,
                L244.india_state_SubsectorShrwt_bld,
                L244.india_state_SubsectorShrwtFllt_bld,
                L244.india_state_SubsectorInterp_bld,
                L244.india_state_SubsectorInterpTo_bld,
                L244.india_state_SubsectorLogit_bld,
                L244.india_state_StubTech_bld,
                L244.india_state_StubTechCalInput_bld,
                L244.india_state_StubTechMarket_bld,
                L244.india_state_GlobalTechIntGainOutputRatio,
                L244.india_state_GlobalTechInterpTo_bld,
                L244.india_state_GlobalTechEff_bld,
                L244.india_state_GlobalTechShrwt_bld,
                L244.india_state_GlobalTechCost_bld,
                L244.india_state_GlobalTechSCurve_bld,
               L244.india_state_HDDCDD_HadCM3)
  } else {
    stop("Unknown command")
  }
}
