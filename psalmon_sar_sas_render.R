library(quarto)
library(glue)

system_param   <-"r5_cowlitz"
species_param  <-"1"
run_param      <-"1"
yr_trim_param  <-"true"
yr_start_param <-2013
yr_end_param   <-2020
smolt_param    <-"true"
wd             <-"C:/Users/sism1477/OneDrive - Washington State Executive Branch Agencies/DFW-Team FP Westside Hatchery MandE - R/psalmon_sar_sas/"
filename       <-paste0(system_param, "_", species_param, "_", run_param, "_", Sys.Date(), "_sars.html")
setwd(wd)

quarto_render(
  input = "psalmon_sar_sas.qmd",
  execute_params = list(system    = system_param, 
                        sp        = species_param, 
                        run       = run_param, 
                        yr_trim   = yr_trim_param,
                        yr_start  = yr_start_param,
                        yr_end    = yr_end_param,
                        smolt     = smolt_param),
  output_file = filename, 
  output_format = "html",
  )

