#' Run the ADMB model
#'
#' @param year
#' @param model
#' @param model_name
#' @param mcmc
#' @param mcsave
#'
#' @return
#' @export run_admb
#'
#' @examples
run_admb <- function(year, model, model_name, mcmc, mcsave){

  # run_admb <- function(year, model, model_name, mcmc, mcsave){
  setwd(here::here(year, model))

  # compile
  R2admb::compile_admb(model_name, verbose = TRUE)

  #Run ADMB Model with MCMC
  system(paste(model_name, "-mcmc", 1000, "-mcsave", 10))
  system(paste(model_name, "-mceval"))  # to get "evalout.proj"

  # reset environment
  setwd(here::here())
}
