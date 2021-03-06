#' Fishery age composition
#'
#' @param year assessment year
#' @param rec_age recruitment age
#' @param plus_age plus age group
#'
#' @return
#' @export  fish_age_comp
#'
#' @examples
fish_age_comp <- function(year, rec_age, plus_age){

read.csv(here::here(year, "data", "raw", "fishery_age_comp_data.csv"),
         colClasses = c(HAUL_JOIN = "character",
                        PORT_JOIN = "character")) %>%
  dplyr::rename_all(tolower) %>%
  dplyr::filter(specimen_type!=3, !is.na(age), age>=rec_age) %>%
  dplyr::mutate(age = ifelse(age>plus_age, plus_age, age)) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(tot = dplyr::n()) %>%
  dplyr::filter(tot>49) %>%
  dplyr::mutate(n_h = length(unique(na.omit(haul_join))) + length(unique(na.omit(port_join)))) %>%
  dplyr::group_by(year, age) %>%
  dplyr::summarise(n_s = mean(tot),
            n_h = mean(n_h),
            age_tot = dplyr::n()) %>%
  dplyr::mutate(prop = age_tot / n_s) %>%
  dplyr::left_join(expand.grid(year = unique(.$year), age = rec_age:plus_age), .) %>%
  tidyr::replace_na(list(prop = 0)) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(AA_Index = 1,
         n_s = mean(n_s, na.rm = T),
         n_h = mean(n_h, na.rm = T)) %>%
  dplyr::select(-age_tot) %>%
  tidyr::pivot_wider(names_from = age, values_from = prop) -> fish_age_comp

  readr::write_csv(fish_age_comp, here::here(year, "data", "output", "fish_age_comp.csv"))

  fish_age_comp

}
