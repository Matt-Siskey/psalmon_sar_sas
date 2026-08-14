##############################
### Clear workspace & load libraries
# rm(list=ls())

# Load packages, several aren't used in this workflow
pacman::p_load(rmisr, httr, magrittr, dplyr, purrr, stringr, tidyverse, knitr, kableExtra, ggplot2, plotly,
               RColorBrewer, jsonlite, janitor, keyring, mgcv, here, lubridate, tidygam, ggeffects, readr)

# Supply API key through keyring to access RMIS server & maintain credential security
token <- key_get("rmis_api")

### Set up drives & controls
my_drive    <-paste0("C:/Users/sism1477/OneDrive - Washington State Executive Branch Agencies/")
share_drive <-paste0(my_drive,"DFW-Team FP Westside Hatchery MandE - R/")
proj_drive  <-paste0(share_drive,"psalmon_sar_sas/")
`%nin%`     <-negate(`%in%`) # create an exclude operator

##############################
##### Read df
# ruleset <-read.csv(paste0(proj_drive,"data/hatchery_program_ruleset.csv",sep=""), fileEncoding = "latin1", check.names=F)
fb_loc <-read.csv(paste0(proj_drive,"data/hatcheries_by_region_all.csv"))

##### RMIS
loc_all <-rmisr::get_location(token = token)


##### FishBooks
confb <- dbConnect(odbc(), #same thing, line by line rather than the canned config in odbc
                   Driver = "SQL Server", 
                   Server = "DFWDBOLYPWSQL3\\BUSPROD",
                   Database = "Hatcheries",
                   Trusted_Connection = "True")

fac <- dbGetQuery(confb, "SELECT * FROM hatcheries.dbo.FACILITY")


##### 
# fac <-rename(fac, Short_Name = Name)
# fac_region <-left_join(fac, fb_loc[,c("Short_Name","PSC_Code","WDFW_REGION_LUT_Id")], by = "Short_Name")
# wdfw_hatcheries_region <-fac_region[,c("Short_Name","PSC_Code","WDFW_REGION_LUT_Id")]
# names(wdfw_hatcheries_region) <-c("hatchery_name","location_code", "region")

# write.csv(wdfw_hatcheries_region, paste0(proj_drive,"data/wdfw_hatcheries_region.csv"))
# write.csv(filter(wdfw_hatcheries_region, region==4), paste0(proj_drive,"data/wdfw_hatcheries_region4.csv"))
# write.csv(filter(wdfw_hatcheries_region, region==5), paste0(proj_drive,"data/wdfw_hatcheries_region5.csv"))
# write.csv(filter(wdfw_hatcheries_region, region==6), paste0(proj_drive,"data/wdfw_hatcheries_region6.csv"))


#####
fb_loc_trim <-filter(fb_loc, PSC_Code %in% loc_all$location_code)
test <- fb_loc_trim[grepl("H", fb_loc_trim$PSC_Code),]
test2 <-test[grepl("HATCHERY", test$Short_Name),]
colnames(test2) <-c("X","WALOC_Id","location_code","name","Long_Desc","WRIA_LUT_Id","region","COMPLEX_Id")

# write.csv(filter(test, WDFW_REGION_LUT_Id==4), paste0(proj_drive,"data/wdfw_hatcheries_region4.csv"))
# write.csv(filter(test, WDFW_REGION_LUT_Id==5), paste0(proj_drive,"data/wdfw_hatcheries_region5.csv"))
# write.csv(filter(test, WDFW_REGION_LUT_Id==6), paste0(proj_drive,"data/wdfw_hatcheries_region6.csv"))


##### RMIS
rel_h <-list()
# hatcheries <-unique(filter(ruleset, loc_col=="hatchery_location_code")$loc_code)
hatcheries <-unique(test2$location_code)

for(h in 1:length(hatcheries)){
  rel_h[[h]] <-rmisr::get_release(token = token,
                                  hatchery_location_code = hatcheries[h])
}
names(rel_h) <-hatcheries
rel_df <-map_df(rel_h, ~as.data.frame(.))
rel_df_trim <-rel_df[,c("species","run","release_location_code","hatchery_location_code","stock_location_code")]
rel_df_hatch_stock <-unique(rel_df_trim[,c("species","run","hatchery_location_code")])

rel_df_3 <-filter(rel_df_hatch_stock, species %in% c("1","2","3","5"))
rel_df_4 <-filter(rel_df_3, run %in% c("1","2","3","4","5","7","8","9"))
rel_df_4$loc_col <-rep("hatchery_location_code",nrow(rel_df_4))
names(rel_df_4) <-c("species","run","location_code","loc_col")

loc_unique <-unique(loc_all[,c("location_code","name")])
rel_df_final <-left_join(rel_df_4, loc_unique[,c("location_code","name")], by="location_code")
hatchery_program_ruleset <-left_join(rel_df_final, test2[,c("location_code","region")], by="location_code")
# hatchery_program_ruleset$system <-paste0("r",hatchery_program_ruleset$region,"_",tolower(hatchery_program_ruleset$name))
hatchery_program_ruleset <-rename(hatchery_program_ruleset, loc_code = location_code)

write.csv(hatchery_program_ruleset, paste0(proj_drive,"hatchery_program_ruleset.csv"),row.names=FALSE)
write.csv(filter(hatchery_program_ruleset,region==4), paste0(proj_drive,"hatchery_program_ruleset_r4.csv"),row.names = FALSE)
write.csv(filter(hatchery_program_ruleset,region==5), paste0(proj_drive,"hatchery_program_ruleset_r5.csv"),row.names = FALSE)
write.csv(filter(hatchery_program_ruleset,region==6), paste0(proj_drive,"hatchery_program_ruleset_r6.csv"),row.names = FALSE)

# unique(filter(rel_df_4, hatchery_location_code == hatcheries[1])$hatchery_location_code)
# unique(filter(rel_df_4, hatchery_location_code == hatcheries[1])$species)
# unique(filter(rel_df_4, hatchery_location_code == hatcheries[1])$run)
