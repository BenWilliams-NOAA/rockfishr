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
    # loop through mcmc output

    dat = list()

    for(i in 1:n_retro) {

      read.delim(here::here(year, model, "retro", "results",
                            paste0("mcmc_", retro_yrs[i], ".std")),
                 sep = "",  header = FALSE) -> df

      df = df[(0.2 * nrow(df)):nrow(df),] # drop burn in

      colnames(df) = c("sigr", "q_srv1", "q_srv2", "F40", "natmort", "spawn_biom_proj",
                       "ABC", "obj_fun",
                       paste0("tot_biom_", yrs[1]:retro_yrs[i]),
                       paste0("log_rec_dev_", styr_rec:retro_yrs[i]),
                       paste0("spawn_biom_", yrs[1]:retro_yrs[i]),
                       "log_mean_rec",
                       paste0("spawn_biom_proj_", max(retro_yrs[i]) + 1:15),
                       paste0("pred_catch_proj_", max(retro_yrs[i]) + 1:15),
                       paste0("rec_proj_", max(retro_yrs[i]) + 1:10),
                       paste0("tot_biom_proj_", max(retro_yrs[i]) + 1:15))

      dat[[i]] = df %>% dplyr::mutate(retro_year = retro_yrs[i])

    }

    # clean up columns so can bind all together
    col <- unique(unlist(sapply(dat, names)))
    dat <- lapply(dat, function(df) {
      df[, setdiff(col, names(df))] <- NA
      df
    })

    do.call(rbind, dat)  -> retro_mc

    # save output
    write.csv(retro_mc, here::here(year, model, "processed", "retro_mcmc.csv"), row.names = FALSE)

    # functions for quantiles
    q_name <- purrr::map_chr(c(.025,.975), ~ paste0("q", .x*100))
    q_fun <- purrr::map(c(.025,.975), ~ purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>%
      purrr::set_names(nm = q_name)

    retro_mc %>%
      dplyr::select(paste0("spawn_biom_", yrs), retro_year) %>%
      tidyr::pivot_longer(c(-retro_year), values_to = "biomass") %>%
      dplyr::mutate(year = as.numeric(stringr::str_extract(name, "[[:digit:]]+")),
                    biomass = biomass / 1000) %>%
      dplyr::group_by(year, retro_year) %>%
      dplyr::summarise_at(dplyr::vars(biomass), tibble::lst(!!!q_fun, median)) %>%
      dplyr::mutate(Retro = factor(retro_year)) %>%
      dplyr::ungroup() -> dat

    dat %>%
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
