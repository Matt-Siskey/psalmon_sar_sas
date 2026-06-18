#####################################################################################################################################################
### [RELEASES] Pull in RMIS release data
get_releases <-function(drive, sy_stages, yrl_stages, smolt_stages){
  
  # need to filter chinook yearling release category using avg_weight cutoff of 73 & release timing (early or late?)
  # probably re-categorize smaller, earlier releases into subyearling
  # might want to tabulate or plot avg_weight of all stages -- do we want a third release group (smolt)?
  # df<-releases %>%
  #      filter(species=="1") %>%
  #      filter(release_stage=="Y") %>%
  #      filter(avg_weight < 1000)
  # hist(df$avg_weight,breaks=100)
  # releases <-read.csv(paste0(drive,"Data/RMIS_releases.csv"))
  releases <-read.csv(paste0(drive,"Data/RMIS_releases_v2.csv"))
  releases[is.na(releases)] <-0
  releases$event_released <-releases$cwt_1st_mark_count + 
    releases$cwt_2nd_mark_count + 
    releases$non_cwt_1st_mark_count + 
    releases$non_cwt_2nd_mark_count
  
  ### assigning WRIA (if necessary)
  # releases$wria <-NULL
  # releases[grepl("\\.", releases$release_location_name),"wria"] <-parse_number(releases[grepl("\\.", releases$release_location_name),
  #                                                                                       "release_location_name"])
  # releases$wria <-abs(releases$wria)
  # # n=600 rows without wria info; probably easiest to export and solve manually
  # # there are also some errors b/c other numbers besides WRIAs are included in release_location_name, so must QAQC at this stage
  # # write.csv(releases,"releases_wria.csv")
  # releases_fixed <-read.csv(paste0(drive,"releases_wria_fixed.csv"))
  # releases_fixed$wria <-as.character(as.integer(releases_fixed$wria))
  # # add RMIS region based on WRIAs
  # wria_region <-c("1"="nowa","2"="nowa",
  #                 "3"="nps","4"="nps","5"="nps","6"="nps","7"="nps",
  #                 "8"="mps","9"="mps","10"="mps",
  #                 "11"="sps","12"="sps","13"="sps","14"="sps",
  #                 "15"="hood","16"="hood","17"="hood",
  #                 "18"="juan","19"="juan")
  # releases_fixed$region <-as.character(wria_region[releases_fixed$wria])
  
  ### aggregate release time series by species, region, and stage; calc total_released
  # releases aggregated by brood_yr, species, basin, and stage
  # table(releases$species,releases$release_stage)
  releases$release_stage <-as.factor(releases$release_stage)
  # levels(releases$release_stage)[levels(releases$release_stage) == ""] <- "no_stage"
  rel_agg <-aggregate(event_released~species+brood_year+release_location_rmis_region+release_stage, data=releases, FUN=sum)
  
  rel_wide <-rel_agg %>% pivot_wider(names_from = release_stage, values_from = event_released) # %>% filter(release_stage!="") 
  rel_wide[is.na(rel_wide)] <-0
  rel_wide$sy_index <-rowSums(rel_wide[,sy_stages])  
  rel_wide$yrl_index <-rowSums(rel_wide[,yrl_stages])  
  rel_wide$smolt_index <-rowSums(rel_wide[,smolt_stages])  
  names(rel_wide)[names(rel_wide) == 'brood_year'] <- 'brood_yr'
  names(rel_wide)[names(rel_wide) == 'release_location_rmis_region'] <- 'region'
  rel_wide$total_released <-rel_wide$sy_index + rel_wide$yrl_index + rel_wide$smolt_index + rel_wide$no_stage
  # add species_name
  specCode_rename_rel <-c("1"="chinook", "2"="coho", "3"="steelhead", "4"="sockeye", "5"="chum", "6"="pink")
  rel_wide$species_name <-as.character(specCode_rename_rel[rel_wide$species])

  write.csv(rel_wide,paste0(drive,"Data/rel_wide.csv"))
  return(rel_wide)
  print("got releases")
}