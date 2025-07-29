#' module_gcamindia_LA144.Commercial
#'
#' Calculates commercial floorspace by state and energy consumption by state/fuel/end use
#'
#' @author PNK February 2020

module_gcamindia_LA144.Commercial <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/A10.SE_PopH_Census_1961_2011",
             FILE = "gcam-india/A44.india_state_pcflsp_m2_comm",
             FILE = "gcam-india/A44.india_state_in_EJ_comm_F_all_services_Y",
             "L142.india_state_in_EJ_comm_F"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.india_state_flsp_bm2_comm",
             "L144.india_state_in_EJ_comm_F_U_Y"))

  } else if(command == driver.MAKE) {

    # Silence package checks
  REGION <- DIVISION <- state <- year <- value <- setNames <- . <- pcflsp_m2 <- pcflsp_m2.x <- pcflsp_m2.y <-
  variable <- scaler <- sector <- value <- fuel <- share <- efficiency <- service <- value.x <- value.y <-
  Year <- unit <- value_EJ <- pre <- post <- state_EJ <- AEO_target <- initial <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A10.SE_PopH_Census_1961_2011 <- get_data(all_data, "gcam-india/A10.SE_PopH_Census_1961_2011") %>%
      gather_years
    A44.india_state_pcflsp_m2_comm <- get_data(all_data, "gcam-india/A44.india_state_pcflsp_m2_comm") %>%
      gather_years
    A44.india_state_in_EJ_comm_F_all_services_Y <- get_data(all_data, "gcam-india/A44.india_state_in_EJ_comm_F_all_services_Y") %>%
      gather_years
    L142.india_state_in_EJ_comm_F <- get_data(all_data, "L142.india_state_in_EJ_comm_F")

    # ===================================================
    # ===================================================
    ##To calculate the floorspace in indian states- first step is to multiply the per
    # Expand to states: multiply per-capita floorspace with the respective population
    L144.india_state_flsp_bm2_comm <- A10.SE_PopH_Census_1961_2011 %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      mutate(sector = "comm") %>%
      left_join_error_no_match(A44.india_state_pcflsp_m2_comm, by = c("year", "state")) %>%
      rename (pcflsp_m2 = value.y) %>%
      mutate(value = value.x * pcflsp_m2 / CONV_BM2_M2) %>%
      select(state, sector, year, value)

    #   Renaming the commercial all services file
     L144.india_state_in_EJ_comm_F_U_Y_unscaled <- A44.india_state_in_EJ_comm_F_all_services_Y

     # Calculating shares of energy consumption by each service, within each state and fuel
     L144.india_state_in_EJ_comm_F_Y_unscaled <- L144.india_state_in_EJ_comm_F_U_Y_unscaled %>%
       group_by(state, fuel, year) %>%
       summarise(value = sum(value)) %>%
       ungroup()

     # Calculating scaler from RECS data and multiply by L142.in_EJ_state_bld_F data to get final estimates
     #Creating the multiplier
     L144.india_state_in_EJ_comm_F_U_Y <- L144.india_state_in_EJ_comm_F_U_Y_unscaled %>%
       left_join_error_no_match(L144.india_state_in_EJ_comm_F_Y_unscaled, by = c("state", "fuel", "year")) %>%
       mutate(value = value.x / value.y,
              sector = "comm") %>%
       select(-value.x, -value.y)

 #removing any NA values and setting them to zero
     L144.india_state_in_EJ_comm_F_U_Y[is.na(L144.india_state_in_EJ_comm_F_U_Y)] <- 0


  L144.india_state_in_EJ_comm_F_U_Y <- L144.india_state_in_EJ_comm_F_U_Y %>%
   left_join_error_no_match(L142.india_state_in_EJ_comm_F, by = c("fuel", "year")) %>%
    mutate(value = value.x * value.y) %>%
    mutate(sector = sector.x) %>%
    select(state, sector, fuel, service, year, value)

    # ===================================================

    # Produce outputs
    L144.india_state_flsp_bm2_comm %>%
      add_title("Residential floorspace by state") %>%
      add_units("billion m2") %>%
      add_comments("RECS data interpolated and downscaled to state based on population ratios") %>%
      add_legacy_name("L144.india_state_flsp_bm2_comm") %>%
      add_precursors("gcam-india/A10.SE_PopH_Census_1961_2011",
                     "gcam-india/A44.india_state_pcflsp_m2_comm") ->
      L144.india_state_flsp_bm2_comm

    L144.india_state_in_EJ_comm_F_U_Y %>%
      add_title("Residential energy consumption by state/fuel/end use") %>%
      add_units("EJ/yr") %>%
      add_comments("bottom-up approach adopted for calculating the energy demand by each sector") %>%
      add_legacy_name("L144.india_state_in_EJ_comm_F_U_Y") %>%
      add_precursors("gcam-india/A44.india_state_in_EJ_comm_F_all_services_Y",
                     "L142.india_state_in_EJ_comm_F") ->
      L144.india_state_in_EJ_comm_F_U_Y

    return_data(L144.india_state_flsp_bm2_comm,L144.india_state_in_EJ_comm_F_U_Y)
  } else {
    stop("Unknown command")
  }
}
