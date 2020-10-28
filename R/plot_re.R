#' plot random effects model by GOA area
#'
#' @param location  where the files live e.g., "Y:/ABL_MESA/SAFES2020/Apportionment/Plot data - NR"
#'
#' @return
#' @export plot_re
#'
#' @examples plot_re("Y:/ABL_MESA/SAFES2020/Apportionment/Plot data - NR")
#'
plot_re <- function(location){

  list.files(location,
             pattern="_re", full.names = TRUE) %>%
    purrr::map_df( ~ read.csv(.), .id = "region") %>%
    dplyr::mutate(region = dplyr::case_when(region==1 ~ "CGOA",
                                            region==2 ~ "EGOA",
                                            region==3 ~ "WGOA")) %>%
    dplyr::left_join(list.files(location,
                                pattern="_srv", full.names = TRUE) %>%
                       purrr::map_df( ~ read.csv(.), .id = "region") %>%
                       dplyr::mutate(region = dplyr::case_when(region==1 ~ "CGOA",
                                                               region==2 ~ "EGOA",
                                                               region==3 ~ "WGOA")) %>%
                       dplyr::mutate(lci = srv_est - srv_est * srv_sd * 1.96,
                                     uci = srv_est + srv_est * srv_sd * 1.96,
                                     yrs = yrs_srv,
                                     lci = ifelse(lci <0, 0, lci)), by = c("region", "yrs")) %>%
    ggplot2::ggplot(ggplot2::aes(yrs, biomA / 1000)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = LCI / 1000, ymax = UCI / 1000), alpha = 0.15) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lci / 1000, ymax = uci / 1000), color = "darkgray") +
    ggplot2::geom_point(ggplot2::aes(y = srv_est / 1000), color = "darkgray") +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~region, scales = "free_y", nrow = 3) +
    ggplot2::ylab("Survey biomass (kt)") +
    ggplot2::xlab("\nYear") +
    funcr::theme_report()

  ggplot2::ggsave(here::here(year, model, "figs", "random_effect.png"),
                  width = 6.5, height = 6.5, units = "in", dpi = 200)
}
