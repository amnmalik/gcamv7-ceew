# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L261.carbon_storage
#'
#' Generates GCAM-USA input files of carbon storage resource supply curves, shareweights, technology coefficients and costs, and other carbon storage information.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L261.DeleteRsrc_USAC}, \code{L261.DeleteSubsector_USAC}, \code{L261.Rsrc_FERC}, \code{L261.RsrcCurves_FERC}, \code{L261.Supplysector_C_USA},
#' \code{L261.SubsectorLogit_C_USA}, \code{L261.SubsectorShrwtFllt_C_USA}, \code{L261.StubTech_C_USA}, \code{L261.StubTechMarket_C_USA}, \code{L261.ResTechShrwt_C_USA}. The corresponding file in the
#' original data system was \code{L261.carbon_storage_USA.R} (gcam-usa level2).
#' @details This chunk generates input files of carbon storage resource supply curves by the US grid regions, and input files of logit, shareweights, and
#' technology information of carbon storage by the US states.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange distinct filter mutate one_of select semi_join
#' @author RC Nov 2017
module_gcamusa_L261.carbon_storage <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L161.Cstorage_FERC",
             "L261.Rsrc",
             "L261.Supplysector_C",
             "L261.SubsectorLogit_C",
             "L261.SubsectorShrwtFllt_C",
             "L261.StubTech_C",
             "L261.GlobalTechCoef_C"))
  } else if(command == driver.DECLARE_OUTPUTS) {
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
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- state <- stub.technology <- subresource <- subsector <- grid_region <- market.name <- minicam.energy.input <-
      Cost_1990USDtC <- MtC <- available <- coefficient <- resource <- extractioncost <- grade <-  NULL # Silence package notes

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L161.Cstorage_FERC <- get_data(all_data, "L161.Cstorage_FERC", strip_attributes = TRUE)
    L261.Rsrc <- get_data(all_data, "L261.Rsrc", strip_attributes = TRUE)
    L261.Supplysector_C <- get_data(all_data, "L261.Supplysector_C", strip_attributes = TRUE)
    L261.SubsectorLogit_C <- get_data(all_data, "L261.SubsectorLogit_C", strip_attributes = TRUE)
    L261.SubsectorShrwtFllt_C <- get_data(all_data, "L261.SubsectorShrwtFllt_C", strip_attributes = TRUE)
    L261.StubTech_C <- get_data(all_data, "L261.StubTech_C", strip_attributes = TRUE)
    L261.GlobalTechCoef_C <- get_data(all_data, "L261.GlobalTechCoef_C")

    # Create a vector of FERC grid regions with non-zero storage curves
    # Will use this list to filter out FERC grid regions with zero storage below
    L161.Cstorage_FERC %>%
      select(grid_region) %>%
      unique %>%
      arrange %>%
      unlist ->
      C_grid_regions

    # Create a vector of FERC grid regions with zero storage curve
    # States in these grid regions will be excluded from the onshare carbon storage subsector in the relevant input files below
    states_subregions %>%
      select(grid_region) %>%
      filter(!grid_region %in% C_grid_regions) %>%
      unique %>%
      unlist ->
      noC_grid_regions

    # L261.DeleteRsrc_USAC: delete onshore carbon storage in the USA region
    # Carbon storage onshore resources are modeled at the grid level
    L261.Rsrc %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == gcam.USA_REGION) %>%
      select(region, resource) ->
      L261.DeleteRsrc_USAC

    # L261.DeleteSubsector_USAC: delete onshore carbon storage subsector of carbon storage sector in the USA region
    # NOTE: leaving the offshore here so that the USA hydrogen sector has a carbon storage market
    L261.SubsectorShrwtFllt_C %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == gcam.USA_REGION) %>%
      semi_join(L261.Rsrc, by = c("subsector" = "resource")) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["Subsector"]]))) ->
      L261.DeleteSubsector_USAC

    # Create a vector of grid level onshare carbon storage subsector with zero storage curve
    # States in these grid regions will be excluded from the onshare carbon storage subsector in the relevant input files below
    grid_Cstorage_nonexist <- paste(noC_grid_regions, L261.DeleteRsrc_USAC$resource[1])

    # L261.Rsrc_FERC: onshore storage in the FERC regions
    L261.Rsrc %>%
      filter(region == gcam.USA_REGION) %>%
      select(-region) %>%
      # Onshore storage only in the FERC regions with non-zero storage curves
      repeat_add_columns(tibble(region = C_grid_regions)) %>%
      mutate(market = region) ->
      L261.Rsrc_FERC

   # L261.RsrcCurves_FERC: onshore storage supply curves in the FERC regions
    L161.Cstorage_FERC %>%
      mutate(region = grid_region,
             resource = L261.Rsrc_FERC$resource[1],
             subresource = L261.Rsrc_FERC$resource[1],
             available = round(MtC, digits = energy.DIGITS_RESOURCE),
             extractioncost = round(Cost_1990USDtC, digits = energy.DIGITS_COST)) %>%
      select(region, resource, subresource, grade, available, extractioncost) ->
      L261.RsrcCurves_FERC

    # L261.Supplysector_C_USA: supplysector information in the states
    L261.Supplysector_C %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)) ->
      L261.Supplysector_C_USA

    # L261.SubsectorLogit_C_USA: subsector logit information in the states
    L261.SubsectorLogit_C %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
      # Drop the states where no carbon storage resources may exist at the grid level
      filter(!(paste(grid_region, subsector) %in% grid_Cstorage_nonexist),
             # Drop offshore carbon storage for states without ocean coastline
             grepl("onshore", subsector) | (region %in% gcamusa.COASTAL_STATES & grepl("offshore", subsector))) ->
      L261.SubsectorLogit_C_USA

    # L261.SubsectorShrwtFllt_C_USA: subsector shareweight information in the states
    L261.SubsectorShrwtFllt_C %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]])) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
      # Drop the states where no carbon storage resources may exist at the grid level
      filter(!paste(grid_region, subsector) %in% grid_Cstorage_nonexist,
             # Drop offshore carbon storage for states without ocean coastline
             grepl("onshore", subsector) | (region %in% gcamusa.COASTAL_STATES & grepl("offshore", subsector))) ->
      L261.SubsectorShrwtFllt_C_USA

    # L261.StubTech_C_USA: stub technology information for the states
    L261.StubTech_C %>%
      filter(region == gcam.USA_REGION) %>%
      write_to_all_states(c(LEVEL2_DATA_NAMES[["StubTech"]])) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = c("region" = "state")) %>%
      # Drop the states where no carbon storage resources may exist at the grid level
      filter(!paste(grid_region, stub.technology) %in% grid_Cstorage_nonexist,
             # Drop offshore carbon storage for states without ocean coastline
             grepl("onshore", subsector) | (region %in% gcamusa.COASTAL_STATES & grepl("offshore", subsector))) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["StubTech"]])))->
      L261.StubTech_C_USA

    # L261.StubTechMarket_C_USA: stub technology market information for the states
    L261.StubTech_C_USA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(select(L261.GlobalTechCoef_C, -coefficient),
                               by = c("supplysector" = "sector.name", "subsector" = "subsector.name", "stub.technology" = "technology", "year")) %>%
      # Use the grid region markets
      left_join_error_no_match(select(states_subregions, state, market.name = grid_region), by = c("region" = "state")) %>%
      # Replace offshore carbon storage with the USA market
      mutate(market.name = replace(market.name, !minicam.energy.input %in% L261.Rsrc_FERC$resource, gcam.USA_REGION)) ->
      L261.StubTechMarket_C_USA

    L261.RsrcCurves_FERC %>%
      select(region, resource = resource, subresource) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1.0) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L261.ResTechShrwt_C_USA

    # Produce outputs
    L261.DeleteRsrc_USAC %>%
      add_title("Delete onshore carbon storage in the USA region") %>%
      add_units("NA") %>%
      add_comments("Carbon storage onshore resources are modeled by grid regions") %>%
      add_legacy_name("L261.DeleteRsrc_USAC") %>%
      add_precursors("L261.Rsrc") ->
      L261.DeleteRsrc_USAC

    L261.DeleteSubsector_USAC %>%
      add_title("Delete onshore carbon storage subsector of carbon storage sector in the USA region") %>%
      add_units("NA") %>%
      add_comments("Keep the offshore here so that the USA hydrogen sector has a carbon storage market") %>%
      add_legacy_name("L261.DeleteSubsector_USAC") %>%
      add_precursors("L261.SubsectorShrwtFllt_C",
                     "L261.Rsrc") ->
      L261.DeleteSubsector_USAC

    L261.Rsrc_FERC %>%
      add_title("Onshore storage in the FERC regions") %>%
      add_units("NA") %>%
      add_comments("Onshore storage are modeled only in the FERC regions with non-zero storage curves") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.Rsrc_FERC") %>%
      add_precursors("L161.Cstorage_FERC",
                     "L261.Rsrc") ->
      L261.Rsrc_FERC

    L261.RsrcCurves_FERC %>%
      add_title("Onshore storage supply curves in the FERC regions") %>%
      add_units("MtC and 1990USDtC") %>%
      add_comments("Onshore carbon storage availability and extraction costs in each FERC region") %>%
      add_legacy_name("L261.RsrcCurves_FERC") %>%
      add_precursors("L161.Cstorage_FERC") %>%
      same_precursors_as("L261.Rsrc_FERC") ->
      L261.RsrcCurves_FERC

    L261.Supplysector_C_USA %>%
      add_title("Supplysector information in the US states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_legacy_name("L261.Supplysector_C_USA") %>%
      add_precursors("L261.Supplysector_C") ->
      L261.Supplysector_C_USA

    L261.SubsectorLogit_C_USA %>%
      add_title("Subsector logit information in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States where no carbon storage resources may exist at the grid level are dropped") %>%
      add_legacy_name("L261.SubsectorLogit_C_USA") %>%
      add_precursors("L161.Cstorage_FERC",
                     "gcam-usa/states_subregions",
                     "L261.SubsectorLogit_C") ->
      L261.SubsectorLogit_C_USA

    L261.SubsectorShrwtFllt_C_USA %>%
      add_title("Subsector shareweight information in the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States where no carbon storage resources may exist at the grid level are dropped") %>%
      add_legacy_name("L261.SubsectorShrwtFllt_C_USA") %>%
      add_precursors("L161.Cstorage_FERC",
                     "gcam-usa/states_subregions",
                     "L261.SubsectorShrwtFllt_C") ->
      L261.SubsectorShrwtFllt_C_USA

    L261.StubTech_C_USA %>%
      add_title("Stub technology information for the states") %>%
      add_units("Unitless") %>%
      add_comments("The same USA region values are repeated for each state") %>%
      add_comments("States where no carbon storage resources may exist at the grid level are dropped") %>%
      add_legacy_name("L261.StubTech_C_USA") %>%
      add_precursors("L161.Cstorage_FERC",
                     "gcam-usa/states_subregions",
                     "L261.StubTech_C") ->
      L261.StubTech_C_USA

    L261.StubTechMarket_C_USA %>%
      add_title("Stub technology market information for the states") %>%
      add_units("Unitless") %>%
      add_comments("Onshore carbon storage is from the grid region markets") %>%
      add_comments("Offshore carbon storage is from the USA market") %>%
      add_legacy_name("L261.StubTechMarket_C_USA") %>%
      add_precursors("L261.GlobalTechCoef_C",
                     "L261.Rsrc_FERC") %>%
      same_precursors_as("L261.StubTech_C_USA") ->
      L261.StubTechMarket_C_USA

    L261.ResTechShrwt_C_USA %>%
      add_title("Technology share-weights for the carbon storage resource") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      same_precursors_as(L261.RsrcCurves_FERC) ->
      L261.ResTechShrwt_C_USA

    return_data(L261.DeleteRsrc_USAC, L261.DeleteSubsector_USAC, L261.Rsrc_FERC,
                L261.RsrcCurves_FERC, L261.Supplysector_C_USA, L261.SubsectorLogit_C_USA,
                L261.SubsectorShrwtFllt_C_USA, L261.StubTech_C_USA, L261.StubTechMarket_C_USA,
                L261.ResTechShrwt_C_USA)
  } else {
    stop("Unknown command")
  }
}
