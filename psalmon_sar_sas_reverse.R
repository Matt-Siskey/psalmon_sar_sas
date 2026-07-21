params<-list()
# params$smolt<-"true"
params$sp <-"1" # Chinook=1; Coho=2; Steelhead=3; 
# params$run <-"1" # spring == 1; summer == 2; fall == 3; winter == 4
# params$system <-"r5_lewis"
params$yr_window <-c(seq(2011,2021,1)) 

##############################
### Clear workspace & load libraries
# rm(list=ls())

# First-time rmisr install
# options(download.file.method = "wininet")
# devtools::install_github("MattCallahan-NOAA/rmisr", force=TRUE)

# Load packages, several aren't used in this workflow
pacman::p_load(rmisr, httr, magrittr, dplyr, purrr, stringr, tidyverse, knitr, kableExtra, ggplot2, plotly,
               RColorBrewer, jsonlite, janitor, keyring, mgcv, here, lubridate, tidygam, ggeffects, readr)

# Supply API key through keyring to access RMIS server & maintain credential security
token <- key_get("rmis_api")

##############################
### Set up drives & controls
my_drive    <-paste0("C:/Users/sism1477/OneDrive - Washington State Executive Branch Agencies/")
share_drive <-paste0(my_drive,"DFW-Team FP Westside Hatchery MandE - R/")
proj_drive  <-paste0(share_drive,"psalmon_sar_sas/")
`%nin%`     <-negate(`%in%`) # create an exclude operator
source(paste0(proj_drive,"functions/functions.R",sep="")) #Source functions for use below

fishery_id <-read.csv(paste0(proj_drive,"data/fishery.csv",sep=""))
fishery_id$fishery <-as.character(fishery_id$fishery)
ruleset <-read.csv(paste0(proj_drive,"data/hatchery_program_ruleset.csv",sep=""), fileEncoding = "latin1", check.names=F) %>%
          filter(., system==params$system)
  

ruleset %>%
  # filter(., system==params$system) %>%
  # rename("month" = first_release_date_month) %>%
  # arrange(brood_year, month,avg_weight) %>%
  # select(-brood_year)%>%
  kbl(row.names = NA)%>%
  kable_classic(position="left",full_width = F, html_font = "Cambria")%>%
  # kable_material_dark() %>%
  kable_styling(bootstrap_options = c("condensed")) #%>% pack_rows(index = table(sar_df$brood_year))


### location info
loc_all <-get_location(token = token)
# loc_code_qequ <-unique(filter(loc_all, psc_basin=="QEQU")$location_code)

### pull recovery data
yrs<-params$yr_window
rec_cwt <-list()

for(t in 1:length(yrs)){
  print(t)
  rec_cwt[[t]] <-get_recovery(token = token, species = "1", run_year= yrs[t])
}
names(rec_cwt) <-yrs

rec_cwt_df <-rec_cwt %>% map_df(., ~as.data.frame(.)) %>% filter(., recovery_location_code %in% loc_code_qequ)
rec_cwt_df$recorded_mark <-as.numeric(rec_cwt_df$recorded_mark)

rec_cwt_fd <-rec_cwt_df %>% filter(., sampled_run=="1") %>%
  filter(., recorded_mark < 5000) %>%
  filter(., recovery_location_code %in% ruleset$loc_code)

cust_tag_codes <-unique(rec_cwt_fd$tag_code)
cust_tag_codes <-cust_tag_codes[!is.na(cust_tag_codes)]

### pull catchsample data
cs_id <-unique(rec_cwt_df$catch_sample_id)
cs    <-list()
# filter(., psc_basin=="QEQU")

for(c in 1:length(cs_id)){
  cs[[c]] <-get_catchsample(token = token, catch_sample_id = cs_id[t], species = "1")
}
names(cs) <-cs_id
cs_df     <-map_df(cs, ~as.data.frame(.))

cs_df <-filter(cs_df, catch_sample_id != "NA")
rec_cwt_df <-filter(rec_cwt_df, catch_sample_id != "NA")

cwt <-merge(cs_df[,c("catch_year","catch_sample_id","number_recovered_decoded")], 
            rec_cwt_df[,c("species","run_year","fishery","catch_sample_id","number_cwt_estimated","recovery_location_code","tag_code")], 
            by = "catch_sample_id", all = TRUE)


### pull release data
rel_h <-list()

for(h in 1:length(cust_tag_codes)){
  rel_h[[h]] <-rmisr::get_release(token = token,
                                  species = params$sp,
                                  tag_code_or_release_id = cust_tag_codes[h])
}
names(rel_h) <-cust_tag_codes
rel_df <-map_df(rel_h, ~as.data.frame(.))



### join dfs
rel_df <-rename(rel_df, "tag_code"=tag_code_or_release_id)

rec_rel_df <-rec_cwt_fd %>% filter(!is.na(tag_code)) %>%
                            left_join(., rel_df[,c("hatchery_location_code","tag_code")], by="tag_code") %>%
                            rename("location_code" = hatchery_location_code) #%>%
                            
                          
rec_rel_df2 <-rec_rel_df %>% filter(!is.na(location_code)) %>%
                             left_join(., loc_all[,c("location_code","name")], by="location_code", relationship = "many-to-many") %>%
                             rename("hatchery_location_code" = location_code) %>%
                             aggregate(number_cwt_estimated ~ species + sampled_run + recorded_mark + recovery_location_code + hatchery_location_code + name, data=.,FUN=sum) %>%
                             arrange(desc(number_cwt_estimated))



rec_rel_df2 %>%
  # filter(., location_code %in% hatch_recover) %>%
  # rename("month" = first_release_date_month) %>%
  # arrange(brood_year, month,avg_weight) %>%
  # select(-brood_year)%>%
  kbl(row.names = NA)%>%
  kable_classic(position="left",full_width = F, html_font = "Cambria")%>%
  # scroll_box(width = "100%", height = "400px") %>%
  kable_styling(bootstrap_options = c("condensed")) #%>% pack_rows(index = table(sar_df$brood_year))


