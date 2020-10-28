#' retrospective survey plot
#'
#' @param year assessment year
#' @param model folder the model is in
#' @param n_retro default is 10
#'
#' @return
#' @export plot_retro_survey
#'
#' @examples plot_retro_survey(year, model)
plot_retro_survey <- function(year, model, n_retro = 10){

  # helper function
  readLines_srv <- function(x){

    cleaner <- function(x){
      x = strsplit(x, " ")
      x = subset(x[[1]], x[[1]] != "")
      as.numeric(x[2:length(x)])
    }

    REP = readLines(x, warn = FALSE)

    year = REP[grep("Survey Biomass", REP)[1]:(grep("Survey Biomass", REP)[2]-2)][2]
    year = cleaner(year)

    pred = REP[grep("Survey Biomass", REP)[1]:(grep("Survey Biomass", REP)[2]-2)][3]
    pred = cleaner(pred)

    obs = REP[grep("Survey Biomass", REP)[1]:(grep("Survey Biomass", REP)[2]-2)][4]
    obs = cleaner(obs)

    se = REP[grep("Survey Biomass", REP)[1]:(grep("Survey Biomass", REP)[2]-2)][6]
    se = cleaner(se)


    data.frame(year = year, pred = pred, obs = obs, se = se)

  }

  # pull in data
    styr_rec = age_yr$styr_rec[1]

  retro_yrs = (year - n_retro + 1):year

  list.files(here::here(year, model, "retro", "results"),
             pattern="rep_", full.names = TRUE) %>%
    purrr::map_df( ~ readLines_srv(.), .id = "retro") %>%
    dplyr::mutate(retro_year = retro_yrs[as.numeric(retro)],
                  model = model,
                  lci = obs - 1.96 * se,
                  uci = obs + 1.96 * se) %>%
    dplyr::select(-retro) -> reps

  write.csv(reps, here::here(year, model, "processed", "retro_reps.csv"), row.names = FALSE)

  reps %>%
    ggplot2::ggplot(ggplot2::aes(year, obs/1000)) +
    ggplot2::geom_point(color = "lightgray") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lci/1000, ymax = uci/1000), color = "lightgray") +
    ggplot2::geom_line(ggplot2::aes(y = pred/1000, color = factor(retro_year))) +
    scico::scale_color_scico_d(palette = "roma") +
    funcr::theme_report() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_y_continuous(name = "Biomass (kt)\n", labels = scales::comma) +
    ggplot2::xlab("\nYear") +
    ggplot2::scale_x_continuous(breaks = funcr::tickr(reps, year, start = 1980)$breaks,
                                labels = funcr::tickr(reps, year, start = 1980)$labels)

  ggplot2::ggsave(here::here(year, model, "figs", "retro_srv1_biomass.png"),
                  width = 6.5, height = 6.5, units = "in", dpi = 200)

  }
