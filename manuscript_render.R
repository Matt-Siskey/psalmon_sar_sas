# rm(list=ls())
library(quarto)
library(glue)

smolt_param <-"true"
species_param  <-"1"
run_param      <-"1"
system_param   <-"r5_lewis"
yr_trim_param  <-"true"
yr_start_param <-2010
yr_end_param   <-2020
adult_age_param <-3
expansion_param <-"sar_total" # sar_cwt

wd_manu  <-"C:/Users/sism1477/OneDrive - Washington State Executive Branch Agencies/DFW-Team FP Westside Hatchery MandE - R/psalmon_sar_sas/"
wd_calc  <-"C:/Users/sism1477/OneDrive - Washington State Executive Branch Agencies/DFW-Team FP Westside Hatchery MandE - R/psalmon_sar_sas/notebooks/"
setwd(wd_calc)
filename <-paste0(system_param,"_",species_param, "_", run_param, "_", Sys.Date(), "_sars")

##### SARS Calculator
quarto_render(input = "psalmon_sar_sas.qmd", 
              metadata = list(
                lightbox = TRUE,
                format = list(
                  html = list(
                    toc = TRUE,
                    "embed-resources" = TRUE,
                    "self-contained-math" = TRUE
                  )
                ),
                comments = list(hypothesis = TRUE),
                scrolling = TRUE),
              execute_params = list(system    = system_param,
                                    sp        = species_param,
                                    run       = run_param,
                                    yr_trim   = yr_trim_param,
                                    yr_start  = yr_start_param,
                                    yr_end    = yr_end_param,
                                    smolt     = smolt_param),
              output_file = filename,
              output_format = "dashboard")


##### Manuscript
quarto_render(input = paste0(wd_manu,"manuscript.qmd"),
              execute_params = list(system    = system_param,
                                    sp        = species_param,
                                    run       = run_param,
                                    yr_trim   = yr_trim_param,
                                    yr_start  = yr_start_param,
                                    yr_end    = yr_end_param,
                                    smolt     = smolt_param),
              output_file = filename,
              output_format = c("pdf","html"))
