# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L122.LC_R_Cropland_Yh_GLU
#'
#' Integrate disparate data sources for land cover and harvested area from FAO, Monfreda, and Hyde in
#' order to calculate cropland cover by specific crop type, other arable land, harvested:cropped ratio, economic
#' yield (i.e., production per unit cropland area per year, rather than production per unit area per harvest), and
#' "extra" cropland that needs to be taken from other land use types. All data tables are written out
#' at the GCAM region-GLU level for each historical year, including carbon cycle model spinup years.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L122.ag_HA_bm2_R_Y_GLU_AnnualCHF}, \code{L122.ag_EcYield_kgm2_R_C_Y_GLU},
#'                        \code{L122.LC_bm2_R_OtherArableLand_Yh_GLU}, \code{L122.LC_bm2_R_ExtraCropLand_Yh_GLU},
#'                        \code{L122.LC_bm2_R_HarvCropLand_C_Yh_GLU}, \code{L122.LC_bm2_R_HarvCropLand_Yh_GLU}.
#' The corresponding file in the original data system was \code{LB122.LC_R_Cropland_Yh_GLU.R} (aglu level1).
#' @details First, Hyde cropland cover is aggregated to the region-GLU level in each year, and supplemented with Monfreda
#' harvested area information when necessary. Second, FAO fallowland, cropland, and harvested area are aggregated from
#' the country level to the region level, and used to calculate the "residual" cropland in each land use region:
#' total cropland (Hyde) - known fallow (FAO) - harvested crops (FAO/Monfreda).
#' The harvested area : cropland ratio (HA:CL) is computed as (sum of harvested area) / (cropland - fallow land).
#' Where HA:CL < 1, HA:CL is set to 1, and the balance (the "residual" above) is added to OtherArableLand.
#' Where HA:CL > 3, HA:CL is set to 3, and additional cropland (ExtraCropLand) is written out, to be taken from
#' other land use types (determined in different code chunks).
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join arrange bind_rows distinct filter if_else group_by left_join mutate select summarise summarise_all
#' @importFrom tidyr replace_na
#' @author ACS April 2017
module_aglu_L122.LC_R_Cropland_Yh_GLU <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      "L100.FAO_fallowland_kha",
      "L100.FAO_CL_kha",
      "L100.FAO_harv_CL_kha",
      "L101.ag_HA_bm2_R_C_Y_GLU",
      "L101.ag_Prod_Mt_R_C_Y_GLU",
      "L120.LC_bm2_R_LT_Yh_GLU")

  MODULE_OUTPUTS <-
    c("L122.ag_HA_bm2_R_Y_GLU_AnnualCHF",
      "L122.ag_EcYield_kgm2_R_C_Y_GLU",
      "L122.LC_bm2_R_OtherArableLand_Yh_GLU",
      "L122.LC_bm2_R_ExtraCropLand_Yh_GLU",
      "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
      "L122.LC_bm2_R_HarvCropLand_Yh_GLU")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    Land_Type <- year <- . <- GCAM_region_ID <- GLU <- GCAM_commodity <-
      value <- iso <- countries <- cropland <- fallow <- fallow_frac <- cropped <-
      cropped_frac <- uncropped_frac <- nonharvested_frac <- value.x <-
      value.y <- Land_Type.y <- Land_Type.x <- GCAM_subsector <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # Line 36 in original file
    # take a subset of the land cover table L120.LC_bm2_R_LT_YH_GLU: cropland, and only in aglu historical years
    L120.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type == "Cropland", year %in% aglu.AGLU_HISTORICAL_YEARS) ->
      # store in table Land Cover in bm2 by Region, Year and GLU for Cropland.
      L122.LC_bm2_R_CropLand_Y_GLU

    # Lines 40-45 in original file
    # The harvested area/production tables L103.ag.X (from Monfreda) may have R_GLUs not in the cropland table
    # L122.LC_bm2_R_CropLand_Y_GLU (from Hyde), and vice versa.
    # Fill out the cropland table to include all R_GLUs in Monfreda:
    L122.LC_bm2_R_CropLand_Y_GLU %>%
      # find the region-GLU combos in the Monfreda harvested area table L101.ag_HA_bm2_R_C_Y_GLU NOT contained
      # in the L122.LC_bm2_R_CropLand_Y_GLU land cover table:
      anti_join(L101.ag_HA_bm2_R_C_Y_GLU[,c("GCAM_region_ID", "GLU", "year")], .,
                by = c("GCAM_region_ID", "GLU", "year")) %>%
      # save only the unique combinations:
      unique() %>%
      # arrange so rows occur in more sensible order:
      arrange(GCAM_region_ID, GLU, year) %>%
      # add values of 0 everywhere for now:
      mutate(value = 0,
             # add a Land_Type identifier:
             Land_Type = "Cropland") %>%
      # add these rows to the original Land Cover table:
      bind_rows(L122.LC_bm2_R_CropLand_Y_GLU) ->
      # save as the Land C over table:
      L122.LC_bm2_R_CropLand_Y_GLU

    # Calculating the average percent of cropland that is fallow in each region
    # using the average cropland, fallow land, and land in temporary crops from FAO RESOURCESTAT
    # The time series is unreliable, so using the average of all available years between 2008 and 2012
    # (and applying to all historical years)
    # And continue with calculating average crop land, fallow land, land in temporary crops:
    # compile the ratio of "temporary crops" to total arable land in each region
    # make table with fallow land compared to total arable land
    # FAO fallow land data, L100.FAO_fallowland_kha, is appended to the FAO cropland data table. Quantities are aggregated
    # from iso to GCAM region and the fraction of fallowland is calculated.
    #
    # The old data sytem uses match to append FAO fallow land data to FAO cropland data by iso. However, Ethiopia,
    # Sudan, and Belgium-Luxembourg have all split into two distinct countries and FAO produces data for each.
    # Specifically, there are two rows with iso = "eth" and two rows with iso = "sdn" and two rows with iso = "bel" in some FAO data.
    # Because match only pulls the first value when there are multiple potential matches, both "eth" cropland rows get the first
    # "eth" fallowland row, rather than their correct value. The same occurs for the two "sdn" and "bel" rows. The solution is simple:
    # joining by iso and countries (or country.codes) instead of simply iso. Alternatively, aggregating to the iso level in each
    # FAO table (L100.FAO_X) BEFORE doing further joins and calculations should also lead to the correct answer.

    # Most (if not all) of what described above has been fixed in earlier steps
    # Take the FAO cropland table, L100.FAO_CL_kha:
    L100.FAO_CL_kha %>%
      # only include data in the right fallow land year range
      filter(year %in% aglu.FALLOW_YEARS) %>%
      # keep only the iso country and the value for each:
      select(iso, area_code, cropland = value, year) %>%
      # append in fallow land data in aglu.FALLOW_YEARS from FAO, L100.FAO_fallowland_kha, keeping NA values:
      left_join(L100.FAO_fallowland_kha, by = c("iso", "area_code", "year")) %>%
      # rename value to fallow and remove NAs:
      rename(fallow = value) %>%
      na.omit() %>%
      select(GCAM_region_ID, cropland, fallow) %>%
      ungroup() %>%
      # aggregate cropland and fallow values to the GCAM region level:
      group_by(GCAM_region_ID) %>%
      summarise_all(sum) %>%
      # calculate the fraction of total land in each GCAM region that is fallow:
      mutate(fallow_frac = fallow / cropland) ->
      # store in a table of cropland, fallow information by region:
      L122.cropland_fallow_R

    # Lines 55-74 in original file
    # Calculating the average percent of cropland that is not in active crop rotations in each region
    # make table with cropped land compared to total arable land
    # FAO harvested area data, L100.FAO_harv_CL_kha, is appended to the FAO cropland data table. Quantities are aggregated
    # from iso to GCAM region and the fraction of cropped land is calculated.
    #
    # These lines of code also pull FAO data that is susceptible to the double labeling in Ethiopia, Sudan, and Belgium.
    # The same match error occurs as above, and we fix in the same way:

    # Take the FAO cropland table, L100.FAO_CL_kha:
    L100.FAO_CL_kha %>%
      # only include data in the right fallow land year range
      filter(year %in% aglu.FALLOW_YEARS) %>%
      # keep only the iso country and the value for each:
      select(iso, area_code, cropland = value, year) %>%
      # append in cropped land data in aglu.FALLOW_YEARS from FAO, L100.FAO_harv_CL_kha, keeping NA values:
      left_join(L100.FAO_harv_CL_kha, by = c("iso", "area_code", "year")) %>%
      # rename value to cropped and remove NAs:
      rename(cropped = value) %>%
      na.omit() %>%
      # remove all columns that are not GCAM_region_ID, cropland, and cropped values:
      select(GCAM_region_ID, cropland, cropped) %>%
      ungroup() %>%
      # aggregate cropland and cropped values to the GCAM region level:
      group_by(GCAM_region_ID) %>%
      summarise_all(sum) %>%
      # calculate the fraction of total land in each GCAM region that is cropped:
      mutate(cropped_frac = cropped/cropland) ->
      # store in a table of cropland, cropped information by region:
      L122.cropland_cropped_R

    # Lines 76-89 in original file
    # calculate the average amount of cropland that is not in production as the fallow land fraction,
    # where available, or else the non-cropped cropland
    # based on availability, using (1) fallow land fraction, (2) fraction of cropland not in crop rotations, or (3) 0
    # Calculating cropland not in production is done via a series of nested if statements.

    # take the unique GCAM regions
    tibble::tibble(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID)) %>%
      # sort the ids:
      arrange(GCAM_region_ID) %>%
      # add a land type identifier for each region:
      mutate(Land_Type = "Cropland",
             # join in fallow land table, L122.cropland_fallow_R, to get the fallow_frac for each region,
             # maintaining NA's for use in determining data availability to calculate nonharvested_frac:
             fallow_frac = left_join(., L122.cropland_fallow_R, by = "GCAM_region_ID")[['fallow_frac']],
             # join in cropped land table, L122.cropland_cropped_R, to get the cropped_frac for each region, and
             # use to calculate the uncropped fraction, maintaining NA's to set to 0 later:
             uncropped_frac = 1 - left_join(., L122.cropland_cropped_R, by = "GCAM_region_ID")[['cropped_frac']],
             # calculate the nonharvested fraction of land via nested if statements
             #   based on availability, using (1) fallow land fraction, (2) land not in crop rotations, or (3) 0
             #   This step determines (1) versus (2):
             #                           If there is no available fallow land data
             nonharvested_frac = if_else(is.na(fallow_frac),
                                         # use the uncropped fraction
                                         uncropped_frac,
                                         # otherwise, use the available fallow land data:
                                         fallow_frac)) %>%
      #   This step determines (3) when (1) and (2) are not available:
      replace_na(list(nonharvested_frac = 0)) ->
      # store in a table of nonharvested cropland fractions by region:
      L122.nonharvested_cropland_R


    # Lines 91-99 in original file
    # applying regional average fallow fractions to all GLUs within each region
    # making two tables: the fallow land table and then, using the fallow land table, the available cropland table

    # take cropland table with region, landtype and glu information for each year:
    L122.LC_bm2_R_CropLand_Y_GLU %>%
      # join in the non harvested fraction for each region:
      #   Note that there is no information for GCAM region 30 in L122.LC_bm2_R_CropLand_Y_GLU, even though
      #   a nonharvested fraction (of 0) is calculated in L122.nonharvested_cropland_R.
      left_join(L122.nonharvested_cropland_R, by = c("GCAM_region_ID", "Land_Type")) %>%
      # drop unnecessary columns that come from the join:
      select(-fallow_frac, -uncropped_frac) %>%
      # use the nonharvested_frac and the cropland area information (value) for each Region-GLU-Year to calculate
      # the amouunt of fallowland in that Region-GLU-Year = value * nonharvested_frac
      mutate(value = value * nonharvested_frac,
             # update the Land_Type identifier to reflect that this is now FallowLand, not Cropland
             Land_Type = "FallowLand") %>%
      # drop nonharvested_frac since no longer necessary:
      select(-nonharvested_frac) ->
      # store in a table of fallowland land cover in bm2 for each region-glu-year
      L122.LC_bm2_R_FallowLand_Y_GLU

    # Use the just made fallow land table to calculate a table of Available CropLand in each region-glu-year
    # Available cropland = totalcropland from L122.LC_bm2_R_CropLand_Y_GLU - fallowland from L122.LC_bm2_R_FallowLand_Y_GLU
    # Take the table of total cropland in each region-GLU-year:
    L122.LC_bm2_R_CropLand_Y_GLU %>%
      # join in the land that is fallow in each region-glu-year
      # absolutely should match up and it's a problem at this point if they don't, so left_join_error_no_match:
      left_join_error_no_match(L122.LC_bm2_R_FallowLand_Y_GLU, by = c("GCAM_region_ID", "GLU", "year")) %>%
      # value.x = value from Cropland table, the total amount of cropland in the region-glu-year
      # value.y = value from FallowLand table, the amount of fallowland in the region-glu-year
      # therefore, available cropland value = value.x-value.y:
      mutate(value = value.x-value.y) %>%
      # remove value.x, value.y and Land_Type.y as now unnecessary:
      select(-value.x, -value.y, -Land_Type.y) %>%
      # rename Land_Type.x to Land_Type:
      rename(Land_Type = Land_Type.x) ->
      # store in a table of available cropland:
      L122.LC_bm2_R_AvailableCropLand_Y_GLU



    # Calculate the harvested to cropped land ratio for all crops, by region, year, and GLU
    # applying minimum and maximum harvested:cropped ratios
    # Maximum harvested:cropped ratio may cause cropland to expand
    # This additional cropland will need to be balanced by a deduction from other land types later on, so it is tracked below
    # take harvested area by region-glu-year:


    # Aggregate FAO harvested area in GLU, distinguished by Perennial or not
    # We will calculate harvest frequency for Annual as Perennial has a frequency of 1
    L101.ag_HA_bm2_R_C_Y_GLU %>%
      mutate(type = if_else(grepl("Tree", GCAM_subsector), "Perennial", "Annual")) %>%
      group_by(GCAM_region_ID, type, GLU, year) %>%
      summarise(value = sum(value)) %>% ungroup() %>%
      spread(type, value) %>%
      replace_na(list(Perennial = 0)) ->
      L122.ag_HA_bm2_R_Y_GLU

    aglu.MAX_HA_TO_CROPLAND_Annual = 2.5
    # Note that two regions GCAM_region_ID == 3 & GLU == "GLU087") | (GCAM_region_ID == 30 & GLU == "GLU078"
    # had issues in LB124 "Increase in cropland exceeds available unmanaged land"
    # So a larger CHF is used here
    aglu.MAX_HA_TO_CROPLAND_Annual_SpecialAdjust = 3.5
    aglu.MIN_HA_TO_CROPLAND_Annual = 1

    L122.ag_HA_bm2_R_Y_GLU %>%
      # join the available cropland by region-glu-year
      left_join_error_no_match(L122.LC_bm2_R_AvailableCropLand_Y_GLU %>%
                                 spread(Land_Type, value),
                               by = c("GCAM_region_ID", "GLU", "year")) %>%
      mutate(Cropland_min = Annual / aglu.MAX_HA_TO_CROPLAND_Annual + Perennial,
             Cropland_min = if_else(
                (GCAM_region_ID == 3 & GLU == "GLU087") | (GCAM_region_ID == 30 & GLU == "GLU078"),
                Annual / aglu.MAX_HA_TO_CROPLAND_Annual_SpecialAdjust + Perennial, Cropland_min))  %>%
      # Update cropland
      mutate(Cropland = pmax(Cropland_min, Cropland),
             AnnualCropHarvestFrequency = Annual / (Cropland - Perennial),
             AnnualCropHarvestFrequency = pmax(aglu.MIN_HA_TO_CROPLAND_Annual, AnnualCropHarvestFrequency)) ->
      L122.ag_HA_bm2_R_Y_GLU_AnnualCHF


    # FAO harvested area is adjusted to cover here ----
    # Need to separate tree crops for the adjustment

    # Lines 116-126 in original file
    # The ag_HA_to_CropLand ratio is assumed to be a property of the region and GLU (not specific to individual crops)
    # Calculate cropland requirements of each crop as harvested area divided by regional ag_HA_to_CropLand ratio
    # Take input harvested area by region-commodity-glu-year:
    L101.ag_HA_bm2_R_C_Y_GLU %>%
        mutate(type = if_else(grepl("Tree", GCAM_subsector), "Perennial", "Annual")) %>%
      # join the harvested area to cropland ratios, L122.ag_HA_bm2_R_Y_GLU_AnnualCHF:
      left_join_error_no_match(L122.ag_HA_bm2_R_Y_GLU_AnnualCHF %>%
                                 select(GCAM_region_ID, GLU, year, CropHarvestFrequency = AnnualCropHarvestFrequency),
                               by = c("GCAM_region_ID", "GLU", "year") ) %>%
      # replace CropHarvestFrequency for Perennial with 1
      mutate(CropHarvestFrequency = if_else(type == "Perennial", 1, CropHarvestFrequency),
             # calculate the harvested cropland for each commodity as value = value.x/value.y:
             value = value / CropHarvestFrequency,
             # add a Land_Type identifier:
             Land_Type = "HarvCropLand") ->
      # store in a table of Harvested Cropland by region-commodity-glu-year:
      L122.LC_bm2_R_HarvCropLand_C_Y_GLU

    # Lines 125:128 in original file
    # Aggregate across crops to get harvested cropland area, by region/GLU/year
    # The harvested cropland area by region-commodity-glu-year table, L122.LC_bm2_R_HarvCropLand_C_Y_GLU,
    # is used for other calculations below. That is why this is not part of the previous pipeline
    # Take harvested cropland area by region-commodity-glu-year:
    L122.LC_bm2_R_HarvCropLand_C_Y_GLU %>%
      group_by(GCAM_region_ID, GLU, Land_Type, year) %>%
      summarise(value = sum(value)) ->
      # store in a table of Harvested cropland by region-glu-year:
      L122.LC_bm2_R_HarvCropLand_Y_GLU


    # Update "yield" wrt land cover accordingly ----
    ## Not used later as it is updated again later ----
    # Lines 130-136 in original file
    # Calculate economic yield by each crop as production divided by cropland. Write out this preliminary table.
    # Production by region-glu-commodity-year comes from input data L101.ag_Prod_Mt_R_C_Y_GLU.
    # Cropland area by region-glu-commodity-year was calculated as table L122.LC_bm2_R_HarvCropLand_C_Y_GLU
    # Take cropland by region-glu-commodity-year:
    L122.LC_bm2_R_HarvCropLand_C_Y_GLU %>%
      # join the production by region-glu-commodity-year information:
      left_join_error_no_match(L101.ag_Prod_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "year")) %>%
      # value.x = the cropland area by region-commodity-glu-year from L122.LC_bm2_R_HarvCropLand_C_Y_GLU
      # value.y = production by region-commodity-glu-year from L101.ag_Prod_Mt_R_C_Y_GLU
      # Calculate yield by region-commodity-glu-year as value = value.y/value.x:
      mutate(value = value.y / value.x) %>%
      select(-value.x, -value.y, -Land_Type) %>%
      # set any NA values to 0 yield:
      replace_na(list(value = 0)) ->
      # store in a table of Economic Yield by region-commodity-glu-year:
      L122.ag_EcYield_kgm2_R_C_Y_GLU


    # Calculating OtherArableLand:

    # Lines 138:148 in original file
    # The minimum threshold on HA:CL means that some cropland in Hyde will not be assigned to a crop. This is mapped to "other arable land"
    # The maximum threshold on HA:CL means that cropland in Hyde in some ag regions may be less than cropland in GCAM
    # In the latter case, land needs to be mapped to cropland from other uses.
    # The first step in executing the three above lines of comment is to calculate the residual land cover that is
    # cropland (may be positive or negative): residual = cropland - fallow land - harvested area by crop type
    # Take harvested cropland by region-glu-year:
    L122.LC_bm2_R_HarvCropLand_Y_GLU %>%
      # update the landtype to be residual cropland
      ungroup() %>% mutate(Land_Type = "ResidualCropLand") %>%
      # Join in the available cropland by region-glu-year
      left_join_error_no_match(L122.LC_bm2_R_AvailableCropLand_Y_GLU, by = c("GCAM_region_ID", "GLU", "year")) %>%
      # value.x = region-glu-year harvested cropland from L122.LC_bm2_R_HarvCropLand_Y_GLU
      # value.y = region-glu-year available cropland from L122.LC_bm2_R_AvailableCropLand_Y_GLU
      # Calculate residual cropland as harvested-available, value = value.x-value.y:
      mutate(value = value.x - value.y) %>%
      # remove unnecessary columns:
      select(-value.x, -value.y, -Land_Type.y) %>%
      rename(Land_Type = Land_Type.x) ->
      # store in a table of Residual Cropland by region-glu-year
      L122.LC_bm2_R_ResidualCropLand_Y_GLU

    # second step to execute max/min comments above is to process the residuals
    # Lines 150-161 in original file
    # Calculating unused cropland; this is added with fallow land to calculate other arable land
    # Where residuals are negative, this is "unused" cropland that will be mapped to other arable land
    # Take the residual cropland by region-glu-year:
    L122.LC_bm2_R_ResidualCropLand_Y_GLU %>%
      # update the Land_Type identifier
      ungroup() %>%
      mutate(Land_Type = "UnusedCropLand",
             # positive residuals cannot be remapped to OtherArableLand. Therefore, they do not count as UnusedCropLand
             # and region-glu-years with positive residual have 0 UnusedCropLand:
             value = if_else(value > 0, 0, value),
             # negative residuals ARE UnusedCropLand. -1 * the negative residual is the amount of UnusuedCropLand that
             # can be mapped to OtherArable Land. Calculate UnusedCropLand:
             value = -1 * value) ->
      # store in a table of UnusedCropLand by region-glu-year
      L122.LC_bm2_R_UnusedCropLand_Y_GLU

    # Calculating extra cropland; this will be balanced by a deduction from unmanaged lands
    # Where residuals are positive, this is "extra" land that will later be deducted from other categories.
    # Take the residual cropland by region-glu-year:
    L122.LC_bm2_R_ResidualCropLand_Y_GLU %>%
      # update the Land_Type identifier
      ungroup() %>% mutate(Land_Type = "ExtraCropLand") %>%
      # negative residuals do not count as ExtraCropLand. Therefore, any negative values in value are
      # remapped to 0 to represent 0 ExtraCropLand:
      mutate(value = if_else(value < 0, 0, value)) ->
      # store in a table of ExtraCropLand by region-glu-year:
      L122.LC_bm2_R_ExtraCropLand_Y_GLU

    # Lines 163-178 in original file
    # Calculating other arable land: known fallow plus discrepancy between sum of harvested area and cropland
    # Assigning all cropland to other arable land wherever harvested area is zero
    # If there are any land use regions with 0 harvested area (Monfreda) but positive cropland cover (Hyde), these are missing
    # values in the above table, and all of this cropland should be assigned to other arable land.
    #
    # Take FallowLand by region-GLU-year:
    L122.LC_bm2_R_FallowLand_Y_GLU %>%
      # update the Land_Type identifier
      ungroup() %>% mutate(Land_Type = "OtherArableLand") %>%
      # join the table of unused cropland by region-glu-year, L122.LC_bm2_UnusedCropLand_Y_GLU
      # NAs retained as in old DS:
      left_join(L122.LC_bm2_R_UnusedCropLand_Y_GLU, by = c("GCAM_region_ID", "GLU", "year")) %>%
      # value.x = fallow land in region-glu-year from L122.LC_bm2_R_FallowLand_Y_GLU
      # value.y = unused land in region-glu-year from L122.LC_bm2_R_FallowLand_Y_GLU
      # OtherArableLand = fallow + unused <=> value = value.x + value.y:
      mutate(value = value.x + value.y) %>%
      # remove unnecessary columns:
      select(-value.x, -value.y, -Land_Type.y) %>%
      rename(Land_Type = Land_Type.x) %>%
      # join the cropland info so that can handle NA's
      left_join(L122.LC_bm2_R_CropLand_Y_GLU, by = c("GCAM_region_ID", "GLU", "year")) %>%
      # value.x = OtherArableLand value
      # value.y = Cropland value
      # Assigning cropland to other arable land wherever harvested area is zero
      # If there are any land use regions with 0 harvested area (Monfreda) but positive cropland cover (Hyde), these are missing
      # values in the above table, and all of this cropland should be assigned to other arable land.
      # for NA values in OtherArableLand (value.x), replace with the cropland value (value.y):
      mutate(value = if_else(is.na(value.x), value.y, value.x)) %>%
      # remove unnecessary columns
      select(-value.x, -value.y, -Land_Type.y) %>%
      rename(Land_Type = Land_Type.x) ->
      # store in a table of OtherArableLand by region-glu-year:
      L122.LC_bm2_R_OtherArableLand_Y_GLU


    # Land use history
    # Lines 181 - 200 in original file
    # Cropland quantities prior to the first AGLU historical year, for spinup of simple carbon cycle model
    # NOTE: Simply assigning this to other arable land
    # This method differs from prior versions of GCAM, where 1971 land cover quantities were rolled back on the basis of the cropland ratios
    # between each land history year and 1971. The problem with this method is that zero values in 1971 can not match non-zero values in prior years.
    # The current method instead will have apparent land use change in going from the land history years to the model base years, but due to equal
    # soil carbon contents, and similar/small vegetative carbon contents, the net emissions signal should be negligible.
    # First, make a table with cropland in the pre-aglu years
    L120.LC_bm2_R_LT_Yh_GLU %>%
      ungroup %>%
      # only save the years in pre-AGLU years:
      filter(year %in% aglu.PREAGLU_YEARS) %>% ungroup() %>%
      # insure that there is cropland for each GCAM region-glu that appear L122.LC_bm2_R_CropLand_Y_GLU:
      tidyr::complete(Land_Type = unique(L122.LC_bm2_R_CropLand_Y_GLU[['Land_Type']]),
                      tidyr::nesting(GCAM_region_ID, GLU, year), fill = list(value = NA)) %>%
      unique() %>%
      # join this historical cropland information to the region-glu-landtypes of L122.LC_bm2_R_CropLand_Y_GLU, preserving
      # NAs as in old DS:
      left_join(
        unique(select(L122.LC_bm2_R_CropLand_Y_GLU, GCAM_region_ID, GLU,
                      Land_Type)),
        .,
        by = c("GCAM_region_ID", "GLU", "Land_Type")) %>%
      # missing values are overwritten to 0:
      replace_na(list(value = 0)) %>%
      # bind to the table of OtherArableLand information by region-glu-year:
      bind_rows(L122.LC_bm2_R_OtherArableLand_Y_GLU, .) %>%
      # update the landtype identifier
      mutate(Land_Type = "OtherArableLand") ->
      # store in a table of OtherArableLand by region-glu-year, including historical years:
      L122.LC_bm2_R_OtherArableLand_Yh_GLU


    # Lines 202-205 in original file
    # All other cropland uses are zero in the pre-aglu years
    # ExtraCropLand is expanded to include historical years in each region-glu, with a value of 0.
    # Take ExtraCropLand by region-glu-year:
    L122.LC_bm2_R_ExtraCropLand_Y_GLU %>%
      # expand to include history years and fill in those values to be 0:
      tidyr::complete(year = c(aglu.PREAGLU_YEARS, aglu.AGLU_HISTORICAL_YEARS),
                      nesting(GCAM_region_ID, GLU, Land_Type),
                      fill = list(value = 0)) ->
      # store in a table of ExtraCropland by region-glu-year, including historical years:
      L122.LC_bm2_R_ExtraCropLand_Yh_GLU


    # Lines 207-210 in original file
    # Harvested cropland history
    # HarvCropLand is expanded to include historical years for each region-glu-commodity, with a value of 0.
    # Take HarvCropLand by region-commodity-glu-year:
    L122.LC_bm2_R_HarvCropLand_C_Y_GLU %>%
      ungroup %>%
      # expand to include history years and fill in those values to be 0:
      tidyr::complete(year = c(aglu.PREAGLU_YEARS, aglu.AGLU_HISTORICAL_YEARS),
                      nesting(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, Land_Type),
                      fill = list(value = 0)) ->
      # store in a table of HarvCropland by region-commodity-glu-year, including historical years:
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU

    # Lines 212-217 in original file
    # Combine crop types to get land use history by all harvested cropland.
    # Use the table of HarvCropland by region-commodity-glu-year, including historical years, L122.LC_bm2_R_HarvCropLand_C_Yh_GLU,
    # and aggregate across commodity to arrive at a table of All Harvested Cropland by region-glu-year, including historical years.
    # Take HarvCropland by region-commodity-glu-year:
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      # remove commodity because that is what aggregate over:
      select(-GCAM_commodity) %>%
      # group by region-glu-year to aggregate:
      group_by(GCAM_region_ID, GLU, Land_Type, year) %>%
      # aggregate:
      summarise(value = sum(value)) ->
      # store in a table of all HarvCropland by region-glu-year, including historical years:
      L122.LC_bm2_R_HarvCropLand_Yh_GLU

    # remove the now-unneeded Land_Type identifier from HarvCropland by region-commodity-glu-year:
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      select(-Land_Type) ->
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU


    # Force-Convert years and GCAM_region_IDs in some problematic outputs to integers:
    L122.LC_bm2_R_ExtraCropLand_Yh_GLU %>%
      ungroup() %>%
      mutate(year = as.integer(year),
             GCAM_region_ID = as.integer(GCAM_region_ID)) ->
      L122.LC_bm2_R_ExtraCropLand_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      ungroup() %>%
      mutate(year = as.integer(year),
             GCAM_region_ID = as.integer(GCAM_region_ID)) ->
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_Yh_GLU %>%
      ungroup() %>%
      mutate(year = as.integer(year),
             GCAM_region_ID = as.integer(GCAM_region_ID)) ->
      L122.LC_bm2_R_HarvCropLand_Yh_GLU

    # Produce outputs
    L122.ag_HA_bm2_R_Y_GLU_AnnualCHF %>%
      add_title("Harvested area to cropland for annual crops ratio by GCAM region / year / GLU") %>%
      add_units("Unitless ratio") %>%
      add_comments("FAO harvested area, fallow area and land cover area, with Monfreda harvested area and Hyde") %>%
      add_comments("land cover data are used to calculate harvested area:cropland for every GCAM region-GLU") %>%
      add_legacy_name("L122.ag_HA_bm2_R_Y_GLU_AnnualCHF") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L100.FAO_harv_CL_kha",
                     "L101.ag_HA_bm2_R_C_Y_GLU",
                     "L120.LC_bm2_R_LT_Yh_GLU") ->
      L122.ag_HA_bm2_R_Y_GLU_AnnualCHF

    L122.ag_EcYield_kgm2_R_C_Y_GLU %>%
      add_title("Economic yield by GCAM region / commodity / year / GLU") %>%
      add_units("kilogram per meter squared (kg.m2)") %>%
      add_comments("Monfreda agricultural production data is combined with aggregated harvested area data at the") %>%
      add_comments("region-GLU-commodity level to calculate economic yield.") %>%
      add_legacy_name("L122.ag_EcYield_kgm2_R_C_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L100.FAO_harv_CL_kha",
                     "L101.ag_HA_bm2_R_C_Y_GLU",
                     "L101.ag_Prod_Mt_R_C_Y_GLU",
                     "L120.LC_bm2_R_LT_Yh_GLU") ->
      L122.ag_EcYield_kgm2_R_C_Y_GLU

    L122.LC_bm2_R_OtherArableLand_Yh_GLU %>%
      add_title("Other arable land cover by GCAM region / year / GLU") %>%
      add_units("billion meters squared (bm2)") %>%
      add_comments("Other Arable Land cover is calculated at the region-GLU level by using FAO fallow land data and ") %>%
      add_comments("adding the difference Harvested Cropland - Available Cropland. ") %>%
      add_legacy_name("L122.LC_bm2_R_OtherArableLand_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L100.FAO_harv_CL_kha",
                     "L101.ag_HA_bm2_R_C_Y_GLU",
                     "L120.LC_bm2_R_LT_Yh_GLU") ->
      L122.LC_bm2_R_OtherArableLand_Yh_GLU

    L122.LC_bm2_R_ExtraCropLand_Yh_GLU %>%
      add_title("Extra cropland cover by GCAM region / year / GLU") %>%
      add_units("billion meters squared (bm2)") %>%
      add_comments("Any land at the region-GLU level for which Harvested Cropland - Available Cropland is positive, ") %>%
      add_comments("making it extra.") %>%
      add_legacy_name("L122.LC_bm2_R_ExtraCropLand_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L100.FAO_harv_CL_kha",
                     "L101.ag_HA_bm2_R_C_Y_GLU",
                     "L120.LC_bm2_R_LT_Yh_GLU") ->
      L122.LC_bm2_R_ExtraCropLand_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      add_title("Harvested cropland cover by GCAM region / commodity / year / GLU") %>%
      add_units("billion meters squared (bm2)") %>%
      add_comments("Harvested cropland at the region-GLU-commodity level is calculated by dividing Monfreda harvested area") %>%
      add_comments("values by the HarvestedArea:CropLand ratio calculated from Monfreda, FAO, and Hyde land cover and ") %>%
      add_comments("harvested area data.") %>%
      add_legacy_name("L122.LC_bm2_R_HarvCropLand_C_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L100.FAO_harv_CL_kha",
                     "L101.ag_HA_bm2_R_C_Y_GLU",
                     "L120.LC_bm2_R_LT_Yh_GLU") ->
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_Yh_GLU %>%
      add_title("Harvested cropland cover by GCAM region / year / GLU") %>%
      add_units("billion meters squared (bm2)") %>%
      add_comments("Harvested cropland at the region-GLU-commodity level is calculated by dividing Monfreda harvested area") %>%
      add_comments("values by the HarvestedArea:CropLand ratio calculated from Monfreda, FAO, and Hyde land cover and ") %>%
      add_comments("harvested area data. This commodity information is then aggregated to the region-GLU level.") %>%
      add_legacy_name("L122.LC_bm2_R_HarvCropLand_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L100.FAO_harv_CL_kha",
                     "L101.ag_HA_bm2_R_C_Y_GLU",
                     "L120.LC_bm2_R_LT_Yh_GLU") ->
      L122.LC_bm2_R_HarvCropLand_Yh_GLU

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
