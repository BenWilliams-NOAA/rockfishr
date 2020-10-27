#' Run retrospectives
#'
#' @param year assessment year
#' @param model folder model is in
#' @param tpl_name name of tpl file
#' @param n_retro how many retros? default is 10
#' @param admb_home where is admb on your computer
#' @param mcon currently nulll
#' @param mcmc how many mcmcs to run
#' @param mcsave how many mcmcs to save
#'
#' @return
#' @export run_retro
#'
#' @examples
run_retro <- function(year, model, tpl_name, n_retro = 10, admb_home = NULL, mcon = NULL, mcmc = 500000, mcsave = 100){

  if (!dir.exists(here::here(year, model, "retro"))){
    dir.create(here::here(year, model, "retro", "model"), recursive=TRUE)
    dir.create(here::here(year, model, "retro", "results"), recursive=TRUE)
  }

  file.copy(here::here(year, model, paste0(tpl_name, ".tpl")),
            here::here(year, model, "retro", "model"),
            overwrite = TRUE)

  file.copy(here::here(year, model, "mat.dat"),
            here::here(year, model, "retro", "model"))

  if(is.null(admb_home)){
    R2admb::setup_admb()
  } else {
    R2admb::setup_admb(admb_home)
  }

  setwd(here::here(year, model, "retro", "model"))

  #Compile the Model
  R2admb::compile_admb(tpl_name)

  setwd(here::here())

  # model .dat and ctl files
  CTL = read.delim(here::here(year, model, paste0("goa_nr_", year, ".ctl")), header = FALSE)
  DAT = readLines(here::here(year, model, paste0("goa_nr_", year, ".dat")), warn = FALSE)


  # define .dat file breaks
  sec_st = grep("#-", DAT)
  sec_end = grep("#!", DAT)

  st_end = matrix(NA, nrow=length(sec_st), ncol=2)
  st_end[,1] = sec_st
  st_end[,2] = sec_end

  # model dims
  styr = as.numeric(DAT[sec_st[2] - 3]) # start of model (example 1961 for POP)
  nages = as.numeric(DAT[sec_st[2] + 3]) # number of age bins
  nlens = as.numeric(DAT[sec_st[2] + 5]) # number of length bins

  # retro data loop

  for(y in 1:n_retro){
    # Set endyr
    yrs_retro = seq(year - n_retro + 1, year)
    endyr = yrs_retro[y]
    nyrs = endyr - styr + 1

    DAT_retro = c(DAT[st_end[1,1]:st_end[1,2]], as.character(endyr), DAT[st_end[2,1]:st_end[2,2]])

    # Fishery catch
    DAT_retro = c(DAT_retro,
                  paste(scan(text=DAT[sec_st[3] - 1])[1:nyrs], collapse=" "),
                  DAT[st_end[3,1]:st_end[3,2]])


    # Trawl survey biomass
    BTSb_yrs = length(which(scan(text=DAT[sec_st[5] - 1]) <= endyr))
    DAT_retro<-c(
      DAT_retro,
      as.character(BTSb_yrs),
      DAT[st_end[4,1]:st_end[4,2]],
      paste(scan(text=DAT[sec_st[5]-1])[1:BTSb_yrs],collapse=" "),
      DAT[st_end[5,1]:st_end[5,2]],
      paste(scan(text=DAT[sec_st[6]-1])[1:BTSb_yrs],collapse=" "),
      DAT[st_end[6,1]:st_end[6,2]],
      paste(scan(text=DAT[sec_st[7]-1])[1:BTSb_yrs],collapse=" "),
      DAT[st_end[7,1]:st_end[7,2]],
      paste(scan(text=DAT[sec_st[8]-1])[1:BTSb_yrs],collapse=" "),
      DAT[st_end[8,1]:st_end[8,2]],
      paste(scan(text=DAT[sec_st[9]-1])[1:BTSb_yrs],collapse=" "),
      DAT[st_end[9,1]:st_end[9,2]])

    # Fish age comp
    FAC_yrs<-length(which(scan(text=DAT[sec_st[11]-1])<(endyr-1)))
    DAT_retro<-c(DAT_retro,
                 as.character(FAC_yrs),
                 DAT[st_end[10,1]:st_end[10,2]],
                 paste(scan(text=DAT[sec_st[11]-1])[1:FAC_yrs],collapse=" "),
                 DAT[st_end[11,1]:st_end[11,2]],
                 paste(scan(text=DAT[sec_st[12]-1])[1:FAC_yrs],collapse=" "),
                 DAT[st_end[12,1]:st_end[12,2]],
                 paste(scan(text=DAT[sec_st[13]-1])[1:FAC_yrs],collapse=" "),
                 DAT[st_end[13,1]:st_end[13,2]],
                 paste(scan(text=DAT[sec_st[14]-1])[1:FAC_yrs],collapse=" "),
                 DAT[st_end[14,1]:st_end[14,2]])

    for(i in 1:FAC_yrs){DAT_retro = c(DAT_retro,
                                      paste(scan(text=DAT[sec_st[15] - FAC_yrs - 1 + i]), collapse = " "))
    }

    DAT_retro = c(DAT_retro,
                  DAT[st_end[15,1]:st_end[15,2]])

    # Survey age comp
    SAC_yrs = length(which(scan(text=DAT[sec_st[17] - 1]) <= (endyr - 1)))
    DAT_retro<-c(DAT_retro,
                 as.character(SAC_yrs),
                 DAT[st_end[16,1]:st_end[16,2]],
                 paste(scan(text = DAT[sec_st[17] - 1])[1:SAC_yrs], collapse=" "),
                 DAT[st_end[17,1]:st_end[17,2]],
                 paste(scan(text = DAT[sec_st[18] - 1])[1:SAC_yrs], collapse=" "),
                 DAT[st_end[18,1]:st_end[18,2]],
                 paste(scan(text = DAT[sec_st[19] - 1])[1:SAC_yrs], collapse=" "),
                 DAT[st_end[19,1]:st_end[19,2]],
                 paste(scan(text = DAT[sec_st[20] - 1])[1:SAC_yrs], collapse=" "),
                 DAT[st_end[20,1]:st_end[20,2]])
    for(i in 1:SAC_yrs) {
      DAT_retro = c(DAT_retro, paste(scan(text = DAT[sec_st[21] - SAC_yrs - 1 + i]), collapse = " "))
    }
    DAT_retro = c(DAT_retro, DAT[st_end[21,1]:st_end[21,2]])

    # Fish size comp
    FSC_yrs = length(which(scan(text = DAT[sec_st[23] - 1]) <= (endyr - 1)))

    DAT_retro<-c(DAT_retro,
                 as.character(FSC_yrs),
                 DAT[st_end[22,1]:st_end[22,2]],
                 paste(scan(text = DAT[sec_st[23] - 1])[1:FSC_yrs], collapse=" "),
                 DAT[st_end[23,1]:st_end[23,2]],
                 paste(scan(text = DAT[sec_st[24] - 1])[1:FSC_yrs], collapse=" "),
                 DAT[st_end[24,1]:st_end[24,2]],
                 paste(scan(text = DAT[sec_st[25] - 1])[1:FSC_yrs], collapse=" "),
                 DAT[st_end[25,1]:st_end[25,2]],
                 paste(scan(text = DAT[sec_st[26] - 1])[1:FSC_yrs], collapse=" "),
                 DAT[st_end[26,1]:st_end[26,2]])
    for(i in 1:FSC_yrs){
      DAT_retro = c(DAT_retro,
                    paste(scan(text = DAT[sec_st[27] - FSC_yrs - 1 + i]), collapse = " "))
    }
    DAT_retro = c(DAT_retro,
                  DAT[st_end[27,1]:st_end[27,2]])

    # Survey size comp
    SSC_yrs = length(which(scan(text = DAT[sec_st[29] - 1]) <= endyr))
    DAT_retro<-c(DAT_retro,
                 as.character(SSC_yrs),
                 DAT[st_end[28,1]:st_end[28,2]],
                 paste(scan(text = DAT[sec_st[29] - 1])[1:SSC_yrs], collapse=" "),
                 DAT[st_end[29,1]:st_end[29,2]],
                 paste(scan(text = DAT[sec_st[30] - 1])[1:SSC_yrs], collapse=" "),
                 DAT[st_end[30,1]:st_end[30,2]],
                 paste(scan(text = DAT[sec_st[31] - 1])[1:SSC_yrs], collapse=" "),
                 DAT[st_end[31,1]:st_end[31,2]],
                 paste(scan(text = DAT[sec_st[32] - 1])[1:SSC_yrs], collapse=" "),
                 DAT[st_end[32,1]:st_end[32,2]])
    for(i in 1:SSC_yrs)
    {DAT_retro = c(DAT_retro,
                   paste(scan(text = DAT[sec_st[33] - SSC_yrs - 1 + i]), collapse = " "))
    }
    DAT_retro = c(DAT_retro,
                  DAT[st_end[33,1]:st_end[33,2]])

    # Write data and control file
    write.table(DAT_retro,
                file = here::here(year, model, "retro", "model", paste0("goa_nr_", endyr, ".dat")),
                quote = FALSE, row.names = FALSE, col.names = FALSE)

    CTL_retro = as.matrix(CTL)
    CTL_retro[2,1] = paste0("goa_nr_", endyr, ".dat")
    CTL_retro[5,1] = as.character(endyr)

    #Updated to account for fact that .tpl is looking for current model year
    write.table(CTL_retro,
                file = here::here(year, model, "retro", "model", paste0("goa_nr_", year, ".ctl")),
                quote = FALSE, row.names = FALSE, col.names = FALSE)

    # run retro models

    ## set your number of MCMC runs at the top of the program...
    setwd(here::here(year, model, "retro", "model"))

    #Determine Operating system

    system(paste0(tpl_name,'.exe', ' -mcmc ', mcmc, ' -mcsave ', mcsave))
    system(paste0(tpl_name,'.exe',' -mceval'))

    file.copy("evalout.prj",
              here::here(year, model, "retro", "results", paste0("mcmc_", endyr, ".std")), overwrite = TRUE)

    file.copy(paste0(tpl_name, ".std"),
              here::here(year, model, "retro", "results", paste0("std_", endyr, ".std")), overwrite = TRUE)

    file.copy(here::here(year, model, "retro", "model", paste0(tpl_name, ".rep")),
              here::here(year, model, "retro", "results", paste0("rep_", endyr, ".rep")), overwrite = TRUE)

    # # sometimes I get weird output from admb it drops the model name and produces these files

    file.copy(here::here(year, model, "retro", "model", "update~1.rep"),
              here::here(year, model, "retro", "results", paste0("rep_", endyr, ".rep")), overwrite = TRUE)

    file.copy("update~1.std",
              here::here(year, model, "retro", "results", paste0("std_", endyr, ".std")), overwrite = TRUE)

  }
}
