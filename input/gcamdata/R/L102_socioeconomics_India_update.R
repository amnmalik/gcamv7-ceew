#' module_socio_India_update
#'
#' To update the data for socioeconomics
#'
#' @author Pallavi Das

module_socio_India_update <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/India_BaseGDP",
             FILE = "gcam-india/India_LaborProd_rate",
             FILE = "gcam-india/India_LaborForce",
             FILE = "gcam-india/India_Population",
             FILE = "gcam-india/A44.india_state_subregional_pop_share",
             FILE = "gcam-india/A44.india_state_subregional_income_share",
             FILE = "gcam-india/India_GDP"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.India_BaseGDP",
             "L102.India_LaborProd_rate",
             "L102.India_LaborForce",
             "L102.India_Population",
             "L100.india_state_pop_ruralurban",
             "L100.india_state_pcGDP_thous90usd_ruralurban"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    socioeconomics_India_update.xml <- NULL  # silence package check notes

    # Load required inputs
    India_BaseGDP <- get_data(all_data, "gcam-india/India_BaseGDP")
    India_LaborProd_rate <- get_data(all_data, "gcam-india/India_LaborProd_rate")
    India_LaborForce <- get_data(all_data, "gcam-india/India_LaborForce")
    India_Population <- get_data(all_data, "gcam-india/India_Population")
    A44.india_state_subregional_pop_share <- get_data(all_data, "gcam-india/A44.india_state_subregional_pop_share")
    A44.india_state_subregional_income_share <- get_data(all_data, "gcam-india/A44.india_state_subregional_income_share")
    India_GDP <- get_data(all_data, "gcam-india/India_GDP") %>% gather_years()

    #Mutating Data

    L102.India_BaseGDP <- India_BaseGDP %>% mutate (region = region) %>%
      mutate(baseGDP = baseGDP*1)

    L102.India_LaborProd_rate <- India_LaborProd_rate %>% mutate (region = region)%>%
      mutate(laborproductivity = laborproductivity*1)

    L102.India_LaborForce <- India_LaborForce %>% mutate (region = region)%>%
      mutate(laborforce = laborforce*1)

    L102.India_Population <- India_Population %>% mutate (region = region)%>%
      mutate(totalPop = totalPop*1)

    #Estimating Rural, Urban & Commercial Population

    L100.india_pop_rural <- A44.india_state_subregional_pop_share %>%
      filter (gcam.consumer == 'resid rural') %>%
      select(region, gcam.consumer, year = pop.year.fillout, subregional.population.share) %>%
      left_join(L102.India_Population, by = c("region", "year")) %>%
      mutate(rural_pop = totalPop * subregional.population.share) %>%
      select (region, sector = gcam.consumer, year, population = rural_pop) %>%
      filter(year %in% MODEL_BASE_YEARS)

    L100.india_pop_urban <- A44.india_state_subregional_pop_share %>%
      filter (gcam.consumer == 'resid urban') %>%
      select(region, gcam.consumer, year = pop.year.fillout, subregional.population.share) %>%
      left_join(L102.India_Population, by = c("region", "year")) %>%
      mutate(urban_pop = totalPop * subregional.population.share) %>%
      select (region, sector = gcam.consumer, year, population = urban_pop) %>%
      filter(year %in% MODEL_BASE_YEARS)

    L100.india_pop_com <- L102.India_Population %>% gather_years() %>% mutate(sector = 'comm') %>%
      filter(year %in% MODEL_BASE_YEARS) %>% select (region, sector, year, population = totalPop)

    L100.india_state_pop_ruralurban <- bind_rows(L100.india_pop_rural,L100.india_pop_urban,L100.india_pop_com) %>%
      mutate(value = population * CONV_ONES_THOUS) %>% rename (pop = population) %>%
      select (region, sector, year, pop)


    #Estimating Rural, Urban & Commercial GDP/Income
    L100.india_GDP_rural <- A44.india_state_subregional_income_share %>%
      filter (gcam.consumer == 'resid rural') %>%
      select(region, gcam.consumer, year = inc.year.fillout, subregional.income.share) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(India_GDP, by = c("region", "year")) %>%
      mutate(value = value * subregional.income.share) %>%
      select (region, sector = gcam.consumer, year, value)

    L100.india_GDP_urban <- A44.india_state_subregional_income_share %>%
      filter (gcam.consumer == 'resid urban') %>%
      select(region, gcam.consumer, year = inc.year.fillout, subregional.income.share) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(India_GDP, by = c("region", "year")) %>%
      mutate(value = value * subregional.income.share) %>%
      select (region, sector = gcam.consumer, year, value)

    L100.india_GDP_comm <- India_GDP %>%
      mutate (sector = 'comm')

    L100.india_state_GDP_ruralurban <- bind_rows(L100.india_GDP_rural,L100.india_GDP_urban,L100.india_GDP_comm) %>%
      filter(year %in% MODEL_BASE_YEARS)

    #GDP per capita estimations
    L100.india_state_pcGDP_thous90usd_ruralurban <- L100.india_state_GDP_ruralurban %>%
      left_join_keep_first_only(L100.india_state_pop_ruralurban, by = c('region', 'sector', 'year')) %>%
      mutate (pcGDP = value / pop) %>%
      select(region, sector, year, pcGDP)

    # Produce output
    L102.India_BaseGDP %>%
      add_title("Base GDP for India") %>%
      add_units("Million 1990 USD") %>%
      add_comments("Update for India") %>%
      add_legacy_name("L102.India_BaseGDP") %>%
      add_precursors("gcam-india/India_BaseGDP")  ->
      L102.India_BaseGDP

    L102.India_LaborProd_rate %>%
      add_title("Labor prodctivity rate update for India") %>%
      add_units("Unitless") %>%
      add_comments("Update for India") %>%
      add_legacy_name("L102.India_LaborProd_rate") %>%
      add_precursors("gcam-india/India_LaborProd_rate")  ->
      L102.India_LaborProd_rate

    L102.India_LaborForce %>%
      add_title("Labor Force update for India") %>%
      add_units("Unitless") %>%
      add_comments("Update for India") %>%
      add_legacy_name("L102.India_LaborForce") %>%
      add_precursors("gcam-india/India_LaborForce")  ->
      L102.India_LaborForce

    L102.India_Population %>%
      add_title("Population update for India") %>%
      add_units("thausands") %>%
      add_comments("Update for India") %>%
      add_legacy_name("L102.India_Population") %>%
      add_precursors("gcam-india/India_Population")  ->
      L102.India_Population

    L100.india_state_pop_ruralurban %>%
      add_title("Population rural and urban update for India") %>%
      add_units("thausands") %>%
      add_comments("Update for India") %>%
      add_legacy_name("L100.india_state_pop_ruralurban") %>%
      add_precursors("gcam-india/A44.india_state_subregional_pop_share")  ->
      L100.india_state_pop_ruralurban

    L100.india_state_pcGDP_thous90usd_ruralurban %>%
      add_title("GDP per capita update for India") %>%
      add_units("thausands") %>%
      add_comments("Update for India") %>%
      add_legacy_name("L100.india_state_pcGDP_thous90usd_ruralurban") %>%
      add_precursors("gcam-india/A44.india_state_subregional_income_share",
                     "gcam-india/India_GDP")  ->
      L100.india_state_pcGDP_thous90usd_ruralurban

    return_data(L102.India_BaseGDP,L102.India_LaborProd_rate,L102.India_LaborForce,L102.India_Population,L100.india_state_pop_ruralurban,L100.india_state_pcGDP_thous90usd_ruralurban)
  } else {
    stop("Unknown command")
  }
}
