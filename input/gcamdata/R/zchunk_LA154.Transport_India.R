#' module_india_LA154.Transport
#'
#' Change the data structure for transport sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L154.india_in_EJ_trn_India_F}, \code{L154.in_EJ_trn_India_F}. The corresponding file in the
#' original data system was \code{LA154.Transport.R} (gcam-usa level1).
#' @details Transportation energy data was downscaled in proportion to GHG Platform state-level transportation energy data
#' @details Transportation nonmotor data was downscaled in proportion to state population
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author Malyan_Ankur_CEEW
#'
module_india_LA154.Transport <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/trnIndia_GHG_mapping",
             FILE = "gcam-india/A10.EB_trn_SecFuel_mtoe",
             FILE = "gcam-india/A54.india_core_changed",
             FILE = "gcam-india/A52.India_transport_structure_complete",
             "L131.in_EJ_R_Senduse_F_Yh"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L154.india_in_EJ_trn_India_F",
             "L154.in_EJ_trn_India_F"))

  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    trnIndia_GHG_mapping <- get_data(all_data, "gcam-india/trnIndia_GHG_mapping")
    A10.EB_trn_SecFuel_mtoe <- get_data(all_data, "gcam-india/A10.EB_trn_SecFuel_mtoe")
    A54.india_core_changed <- get_data(all_data, "gcam-india/A54.india_core_changed")
    #L154.out_mpkm_R_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_R_trn_nonmotor_Yh")
    #L102.India_Population <- get_data(all_data, "L102.India_Population")
    A52.India_transport_structure_complete <- get_data(all_data, "gcam-india/A52.India_transport_structure_complete")
    L131.in_EJ_R_Senduse_F_Yh <- get_data(all_data, "L131.in_EJ_R_Senduse_F_Yh")
    # ===================================================

      # Silence package notes
      #GCAM_region_ID <- UCD_sector <- mode <- size.class <- UCD_technology <- UCD_fuel <- fuel <- GHG_Platform_fuels <-
      #  year <- value <- GHG_Platform_sectors <- . <- fuel_sector <- state <- sector <- value_state <- value_national <-
        #value_share <- value_mode <- NULL


    #Transport sector fuels to filter
    Transportation_Fuels <- c("Coal", "Electric", "Liquids", "NG", "FCEV", "BEV","Hydrogen")

    #gcam region code
    gcam.india_REGION <- "India"
    gcam.INDIA_CODE   <- 17

    #Modifying India level structure with rows
    Transportation_energy_consumption <- A54.india_core_changed %>%
        filter(year %in% HISTORICAL_YEARS, GCAM_region_ID == gcam.INDIA_CODE) %>%
      bind_rows(A52.India_transport_structure_complete) %>%
      mutate(size.class = "All") %>%
      filter(UCD_technology %in% Transportation_Fuels) %>%
      group_by(GCAM_region_ID, UCD_sector, mode, UCD_technology, UCD_fuel, fuel, year) %>%
      mutate(value = sum(value)) %>%
      arrange(mode) %>%
      ungroup() %>%
      unique() %>%
      left_join_error_no_match(trnIndia_GHG_mapping, by = c("fuel", "mode", "UCD_sector"))

    #Estimating shares across sectors and fuels
    India_shares <- A10.EB_trn_SecFuel_mtoe %>%
      gather_years %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(GHG_Platform_fuels, GHG_Platform_sectors, year) %>%
      mutate(share = value/sum(value)) %>%
      replace_na(list(value_share = 0))%>%
      select(State, GHG_Platform_fuels, GHG_Platform_sectors, year, share) %>%
      ungroup()

    India_shares[is.na(India_shares)] <- 0


      # Creating file for transport setcor and fuels
    L154.india_in_EJ_trn_India_F <- India_shares %>%
        left_join_keep_first_only(Transportation_energy_consumption, by = c("GHG_Platform_fuels", "GHG_Platform_sectors", "year")) %>%
        mutate(value = value * share) %>%
      select(State, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, fuel, year, value)


      # Creating aggregate by fuel and name the sector output
      L154.in_EJ_trn_India_F <- L154.india_in_EJ_trn_India_F %>%
        group_by(State, fuel, year) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(sector = "transportation") %>% # Adding a column named "sector" with "transportation" as the entries
        select(State, sector, fuel, year, value)


    # ===================================================

      L154.india_in_EJ_trn_India_F %>%
      add_title("Transportation energy consumption") %>%
      add_units("EJ") %>%
      add_comments("Transportation energy consumption data using GHG Platform energy data") %>%
      add_legacy_name("L154.india_in_EJ_trn_India_F") %>%
      add_precursors("gcam-india/A54.india_core_changed", "gcam-india/trnIndia_GHG_mapping", "gcam-india/A10.EB_trn_SecFuel_mtoe",
                     "gcam-india/A52.India_transport_structure_complete") ->
        L154.india_in_EJ_trn_India_F


      L154.in_EJ_trn_India_F %>%
      add_title("Transportation energy consumption") %>%
      add_units("EJ") %>%
      add_comments("Transportation energy consumption was aggregated by fuel, and the sector was named transportation") %>%
      add_legacy_name("L154.in_EJ_trn_India_F") %>%
      add_precursors("gcam-india/A54.india_core_changed", "gcam-india/trnIndia_GHG_mapping", "gcam-india/A10.EB_trn_SecFuel_mtoe") ->
        L154.in_EJ_trn_India_F

    return_data(L154.india_in_EJ_trn_India_F, L154.in_EJ_trn_India_F)
  } else {
    stop("Unknown command")
  }
}
