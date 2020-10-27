#' all basic plost forsafe doc
#'
#' @param year is the model year
#' @param model is the folder the model is in
#' @param model_name the name of the tpl file (no extension)
#' @param rec_age is the recruitment age
#'
#' @return
#' @export base_plots
#'
#' @examples
base_plots <- function(year, model, model_name, rec_age){

  plot_catch(year, model)
  plot_survey(year, model)
  plot_selex(year, model)
  plot_params(year, model, model_name)
  plot_phase(year, model, model_name)
  plot_rec(year, model)
  plot_rec_ssb(year, model, rec_age)
  plot_swath(year, model)
  plot_comps(year, model)
}
