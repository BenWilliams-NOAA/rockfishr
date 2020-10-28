#' Calculate Mohn's rho "mean version"
#'
#' @param year     assessment year
#' @param model     folder model is in
#' @param n_retro     number of retrospective analyses to do (default = 10)
#' @return
#' @export mohn
#'
#' @examples
#' mohn(year, model)
mohn <- function(year, model, n_retro = 10){

    peels = n_retro - 1
    max_year = year
    retro_yrs = (year - n_retro + 1):year
    yrs = read.csv(here::here(year, model, "processed", "ages_yrs.csv")) %>%
      dplyr::pull(yrs)

    # functions for quantiles
    q_name <- purrr::map_chr(c(.025,.975), ~ paste0("q", .x*100))
    q_fun <- purrr::map(c(.025,.975), ~ purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>%
      purrr::set_names(nm = q_name)

    read.csv(here::here(year, model, "processed", "retro_mcmc.csv")) %>%
      dplyr::select(paste0("spawn_biom_", yrs), retro_year) %>%
      tidyr::pivot_longer(c(-retro_year), values_to = "biomass") %>%
      dplyr::mutate(year = as.numeric(stringr::str_extract(name, "[[:digit:]]+")),
                    biomass = biomass / 1000) %>%
      dplyr::group_by(year, retro_year) %>%
      dplyr::summarise_at(dplyr::vars(biomass), tibble::lst(!!!q_fun, median)) %>%
      dplyr::select(year, retro_year, median) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(pdiff = (median - median[retro_year==max_year]) /
                      median[retro_year==max_year]) %>%
      tidyr::drop_na() %>%
      dplyr::filter(year %in% (max_year-peels):max_year) %>%
      dplyr::ungroup() %>%
      dplyr::filter(year == retro_year, year !=max_year) %>%
      dplyr::summarise(rho = mean(pdiff)) %>%
      dplyr::pull(rho) -> ssb_rho

    ssb_rho

}
