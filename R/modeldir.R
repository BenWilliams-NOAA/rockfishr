
#' model function directory and tpl files
#'
#' @description creates folders for storing data and output with the top folder being the assessment year
#'
#' @param year assessment year
#'
#' @return
#' @export modeldir
#'
#' @examples
#' modeldir(2020)
#'
modeldir <- function(year){

  if (!dir.exists(here::here(year, "data", "output"))){
    dir.create(here::here(year, "data", "output"), recursive=TRUE)
  }

  if (!dir.exists(here::here(year, "data", "raw"))){
    dir.create(here::here(year, "data", "raw"))
  }

  if (!dir.exists(here::here(year, "data", "user_input"))){
    dir.create(here::here(year, "data", "user_input"))
  }

  if (!dir.exists(here::here(year, "data", "SARA"))){
    dir.create(here::here(year, "data", "SARA"))
  }

  if (!dir.exists(here::here(year, "data", "models", "ageage"))){
    dir.create(here::here(year, "data", "models", "ageage"), recursive=TRUE)
  }

  if (!dir.exists(here::here(year,"data", "models", "allometric"))){
    dir.create(here::here(year,"data", "models", "allometric"))
  }

  if (!dir.exists(here::here(year,"data", "models", "vbl"))){
    dir.create(here::here(year,"data", "models", "VBL"))
  }

  if (!dir.exists(here::here(year,"data", "models", "wvbl"))){
    dir.create(here::here(year,"data", "models", "wVBL"))
  }

  if (!dir.exists(here::here(year,"data", "models", "length_sd"))){
    dir.create(here::here(year,"data/models", "length_sd"))
  }


  file.copy(system.file("models", "AGEAGE.tpl", package = "datacall"),
            here::here(year, "data", "models", "ageage"))

  file.copy(system.file("models", "allometric.tpl", package = "datacall"),
            here::here(year, "data", "models", "allometric"))

  file.copy(system.file("models", "VBL.tpl", package = "datacall"),
            here::here(year, "data", "models", "vbl"))

  file.copy(system.file("models", "wVBL.tpl", package = "datacall"),
            here::here(year, "data", "models", "wvbl"))

  file.copy(system.file("models", "lvb.ctl", package = "datacall"),
            here::here(year, "data", "models", "wvbl"))

  file.copy(system.file("models", "lengthSD.tpl", package = "datacall"),
            here::here(year, "data", "models", "length_sd"))

}
