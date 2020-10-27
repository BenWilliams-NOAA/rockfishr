# rockfishr

## Legal Disclaimer

This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). 
All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. 
Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. 
The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.

<!-- badges: start -->
<!-- badges: end -->

## rockfishr
The goal of rockfishr is to create a clear workflow for pulling and cleaning data for rockfish fishery stock assessments based upon the ADMB rockfish assessment model.
It utilizes a "project oriented workflow" via RStudio.
You must be able to have a connection to the AFSC & AKFIN (Answers) data servers (e.g., VPN if offsite), and have usernames/passwords setup.

## Installation

You can install the released version of datacall from [github](https://github.com/BenWilliams-NOAA/rockfishr) with:

``` r
# install.packages("devtools")
devtools::install_github("BenWilliams/rockfishr")
```

## Example


``` r
library(datacall)

# gloabls ----

species = "NORK" # northern rockfish
year = 2020 # assessment year
afsc_user = "your_afsc_username"
afsc_pwd = "your_afsc_password"
akfin_user = "your_akfn_username"
akfin_pwd = "your_akfin_password"
admb_home = "C:/Program Files (x86)/ADMB-12.1" # location ADMB exists on *my* computer
TAC = c(3786, 3681, 4528) # last three years of TAC (year-3, year-2, year-1)
rec_age = 2 # recruitment age
plus_age = 45 # plus group age
model_name = "updated_nr"
dat_name = "goa_nr_2020"

# setup folders -----
modeldir(year)

# query databases ----
raw_data(species, year, afsc_user, afsc_pwd, akfin_user, akfin_pwd)
```

The initial output will be raw data pulls that will be placed in the  `year/data/raw` folder.

Then clean up the raw data which will be placed in the `year/data/output` folder.


```{r}
# catch and biomass data ----
survey_biomass(year) # if no file is identified then the DB survey estimate is used
clean_catch(year, TAC)

# the reader_tester file can  be changed
ageage(reader_tester = "reader_tester_old.csv", species, year, admb_home) # file provide in "user_input" folder
fish_age_comp(year, rec_age, plus_age)
survey_age_comp(year, rec_age, plus_age)
fish_size_comp(year, rec_age)
survey_size_comp(year)
size_at_age(year, admb_home, rec_age)
weight_at_age(year, admb_home, rec_age)
```

*note: must provide file for VAST estimates in the "user_input" folder, otherwise this will output the design-based biomass estimate*

```{r}
survey_biomass(year, "VAST_estimates.csv")

# biological data ----
clean_catch(year, TAC)
survey_biomass(year)
fish_age_comp(year, rec_age, plus_age)
survey_age_comp(year, rec_age, plus_age)
fish_size_comp(year, rec_age)
survey_size_comp(year)
size_at_age(year, admb_home, rec_age, plus_age)
weight_at_age(year, admb_home, rec_age, plus_age)
ageage(reader_tester = NULL, species, year, admb_home) # read_tester file is provide in the "user_input" folder

```

A .dat file will be placed in the model folder (users choice)

```{r}
# create dat file ----
model = "m1"
concat_dat(year, model, rec_age, plus_age)
```

Run the ADMB model.
Clean up model results.

```{r}
process_results(year, model, model_name, dat_name, rec_age, plus_age, mcmcm = 10000000, mcsave = 2000, survey = "VAST_estimates.csv"")
base_plots(year, model, model_name, rec_AGE)

```
