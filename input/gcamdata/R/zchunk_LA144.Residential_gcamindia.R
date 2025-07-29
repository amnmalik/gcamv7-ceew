#' module_gcamindia_LA144.Residential

#' Calculate residential floorspace by state and residential energy consumption by state/fuel/end use.

#' @details Calculate residential floorspace by state and residential energy consumption by state/fuel/end use.

module_gcamindia_LA144.Residential <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/A44.india_state_pcflsp_m2_res_rural",
             FILE = "gcam-india/A44.india_state_pcflsp_m2_res_urban",
             FILE = "gcam-india/A44.india_state_in_EJ_res_F_spaceheating_Y",
             FILE = "gcam-india/A44.india_state_in_EJ_res_F_spacecooling_Y",
             FILE = "gcam-india/A44.india_state_in_EJ_res_F_cooking_Y",
             FILE = "gcam-india/A44.india_state_in_EJ_res_F_lighting_Y",
             FILE = "gcam-india/A44.india_state_in_EJ_res_F_apploth_Y",
             "L100.india_state_pop_ruralurban",
             "L142.india_state_in_EJ_resid_F"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.india_state_flsp_bm2_res",
             "L144.india_state_in_EJ_res_F_U_Y"))

  } else if(command == driver.MAKE) {

    # Silence package checks
    year <- value <- state <- year <- variable <- value.x <- value.y <- variable <- pcflsp_m2 <- scaler <-
      pcflsp_m2.x <- pcflsp_m2.y <- conv_9_13 <- sector <- fuel <- service <- DIVISION <- val_1993 <- conv <- val_1990 <-
      Fuel <- Service <- fuel_sum <- share <- service.x <- Sector <- flspc_2010 <- EIA_sector <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A44.india_state_pcflsp_m2_res_rural <- get_data(all_data, "gcam-india/A44.india_state_pcflsp_m2_res_rural") %>%
      gather_years
    A44.india_state_pcflsp_m2_res_urban <- get_data(all_data, "gcam-india/A44.india_state_pcflsp_m2_res_urban") %>%
      gather_years
      A44.india_state_in_EJ_res_F_spaceheating_Y <- get_data(all_data, "gcam-india/A44.india_state_in_EJ_res_F_spaceheating_Y")%>%
      gather_years
    A44.india_state_in_EJ_res_F_spacecooling_Y <- get_data(all_data, "gcam-india/A44.india_state_in_EJ_res_F_spacecooling_Y")%>%
      gather_years
    A44.india_state_in_EJ_res_F_cooking_Y <- get_data(all_data,"gcam-india/A44.india_state_in_EJ_res_F_cooking_Y") %>%
      gather_years
    A44.india_state_in_EJ_res_F_lighting_Y <- get_data(all_data,"gcam-india/A44.india_state_in_EJ_res_F_lighting_Y") %>%
      gather_years
    A44.india_state_in_EJ_res_F_apploth_Y <- get_data(all_data, "gcam-india/A44.india_state_in_EJ_res_F_apploth_Y") %>%
      gather_years
    L100.india_state_pop_ruralurban <- get_data(all_data, "L100.india_state_pop_ruralurban")
    L142.india_state_in_EJ_resid_F <- get_data(all_data, "L142.india_state_in_EJ_resid_F")

    # ===================================================
    ##To calculate the floorspace in indian states- first step is to multiply the per
    # Expand to states: multiply per-capita floorspace with the respective population
    L144.india_state_flsp_bm2_res_rural <- L100.india_state_pop_ruralurban %>% filter (sector %in% 'resid rural') %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(A44.india_state_pcflsp_m2_res_rural, filter (year %in% MODEL_BASE_YEARS),
                               by = c("year")) %>%
      rename (pcflsp_m2 = value) %>%
      mutate(value = pop * pcflsp_m2 * 1000 / CONV_BM2_M2) %>%
      select(state, sector, year, value)
    #for urban
    L144.india_state_flsp_bm2_res_urban <- L100.india_state_pop_ruralurban %>% filter (sector %in% 'resid urban')%>%      filter(year %in% HISTORICAL_YEARS) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(A44.india_state_pcflsp_m2_res_urban, filter (year %in% MODEL_BASE_YEARS),
                               by = c("year")) %>%
      rename (pcflsp_m2 = value) %>%
      mutate(value = pop * pcflsp_m2 * 1000 / CONV_BM2_M2) %>%
      select(state, sector, year, value)


     L144.india_state_flsp_bm2_res <- bind_rows(L144.india_state_flsp_bm2_res_rural, L144.india_state_flsp_bm2_res_urban)

    # ===================================================

     # Assembling unscaled energy consumption by state, fuel, and service
     L144.india_state_in_EJ_res_F_U_Y_unscaled <- bind_rows(A44.india_state_in_EJ_res_F_spaceheating_Y,
                                                            A44.india_state_in_EJ_res_F_spacecooling_Y,
                                                            A44.india_state_in_EJ_res_F_cooking_Y,
                                                            A44.india_state_in_EJ_res_F_lighting_Y,
                                                            A44.india_state_in_EJ_res_F_apploth_Y)

      # Calculating shares of energy consumption by each service, within each state and fuel
     L144.india_state_in_EJ_resrural_F_Y_unscaled <- L144.india_state_in_EJ_res_F_U_Y_unscaled %>%
       filter(sector == 'resid rural') %>%
       group_by(fuel, year) %>%
       summarise(value = sum(value)) %>%
       ungroup()

     # Calculating shares of energy consumption by each service, within each state and fuel
     L144.india_state_in_EJ_resurban_F_Y_unscaled <- L144.india_state_in_EJ_res_F_U_Y_unscaled %>%
       filter(sector == 'resid urban') %>%
       group_by(fuel, year) %>%
       summarise(value = sum(value)) %>%
       ungroup()


     L144.india_state_in_EJ_resrural_F_U_Y_unscaled <- L144.india_state_in_EJ_res_F_U_Y_unscaled %>%
       filter (sector == 'resid rural')

     L144.india_state_in_EJ_resurban_F_U_Y_unscaled <- L144.india_state_in_EJ_res_F_U_Y_unscaled %>%
       filter (sector == 'resid urban')

     # Calculating scaler from RECS data and multiply by L142.in_EJ_state_bld_F data to get final estimates
     #Rural
     L144.india_state_in_EJ_rural_F_U_Y <- L144.india_state_in_EJ_resrural_F_U_Y_unscaled %>%
       left_join_error_no_match(L144.india_state_in_EJ_resrural_F_Y_unscaled, by = c("fuel", "year")) %>%
       mutate(value = value.x / value.y) %>%
       select(-value.x, -value.y)

     L144.india_state_in_EJ_rural_F_U_Y[is.na(L144.india_state_in_EJ_rural_F_U_Y)] <- 0

     L144.india_state_in_EJ_rural_F_U_Y <- L144.india_state_in_EJ_rural_F_U_Y %>%
       left_join_keep_first_only(L142.india_state_in_EJ_resid_F, by = c("sector","fuel", "year")) %>%
       mutate(value = value.x * value.y) %>%
       select(state, sector, fuel, service, year, value)

    #Urban
     L144.india_state_in_EJ_urban_F_U_Y <- L144.india_state_in_EJ_resurban_F_U_Y_unscaled %>%
       left_join_error_no_match(L144.india_state_in_EJ_resurban_F_Y_unscaled, by = c("fuel", "year")) %>%
       mutate(value = value.x / value.y) %>%
       select(-value.x, -value.y)

     L144.india_state_in_EJ_urban_F_U_Y[is.na(L144.india_state_in_EJ_urban_F_U_Y)] <- 0

     L144.india_state_in_EJ_urban_F_U_Y <- L144.india_state_in_EJ_urban_F_U_Y %>%
       left_join_keep_first_only(L142.india_state_in_EJ_resid_F, by = c("sector","fuel", "year")) %>%
       mutate(value = value.x * value.y) %>%
       select(state, sector, fuel, service, year, value)

     L144.india_state_in_EJ_res_F_U_Y <- L144.india_state_in_EJ_rural_F_U_Y %>%
       bind_rows(L144.india_state_in_EJ_urban_F_U_Y) %>%
       replace_na(list(value = 0))

# ==================================================
     # Produce outputs
    L144.india_state_flsp_bm2_res %>%
      add_title("Residential floorspace by state") %>%
      add_units("billion m2") %>%
      add_comments("RECS data interpolated and downscaled to state based on population ratios") %>%
      add_legacy_name("L144.india_state_flsp_bm2_res") %>%
      add_precursors("L100.india_state_pop_ruralurban",
                     "gcam-india/A44.india_state_pcflsp_m2_res_rural",
                     "gcam-india/A44.india_state_pcflsp_m2_res_urban") ->
      L144.india_state_flsp_bm2_res

    L144.india_state_in_EJ_res_F_U_Y %>%
      add_title("Residential energy consumption by state/fuel/end use") %>%
      add_units("EJ/yr") %>%
      add_comments("bottom-up approach adopted for calculating the energy demand by each sector") %>%
      add_legacy_name("L144.india_state_in_EJ_res_F_U_Y") %>%
      add_precursors("gcam-india/A44.india_state_in_EJ_res_F_spaceheating_Y",
                     "gcam-india/A44.india_state_in_EJ_res_F_spacecooling_Y",
                     "gcam-india/A44.india_state_in_EJ_res_F_cooking_Y",
                     "gcam-india/A44.india_state_in_EJ_res_F_lighting_Y",
                     "gcam-india/A44.india_state_in_EJ_res_F_apploth_Y",
                     "L142.india_state_in_EJ_resid_F") ->
      L144.india_state_in_EJ_res_F_U_Y

    return_data(L144.india_state_flsp_bm2_res,L144.india_state_in_EJ_res_F_U_Y)
  } else {
    stop("Unknown command")
  }
}

