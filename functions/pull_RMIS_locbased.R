params$smolt <-TRUE
params$sp <-"1"
params$run <-"3"
params$system <-"r5_cowlitz"
params$yr_trim <-TRUE
params$yr_start <-1970
params$yr_end <-2021
params$adult_age <-3
params$cwt_expanded <-"sar_total" # sar_total, sar_cwt
# params$loc_based: TRUE # FALSE

######################################################################################################################################################
##### [LOCATION-BASED RECOVERY PULL] Pull in WDFW FishBooks return data
# RETURNS
loc_based     <-unique(filter(loc_all, psc_basin=="COWL")$location_code)
rec_cwt_loc <-list()

for(t in 1:length(loc_based)){
  print(t)
  rec_pull <- tryCatch({rmisr::get_recovery(token = token,
                                            recovery_location_code = loc_based[[t]],
                                            species = params$sp)}, 
                       error = function(e) {message(paste("No data found or error for location", loc_based[[t]], "- skipping to next pull."))
                         return(NULL)})
  if (is.null(rec_pull) || nrow(rec_pull) == 0) {next}
  rec_cwt_loc[[t]] <- rec_pull
}
names(rec_cwt_loc) <-loc_based[1:length(rec_cwt_loc)]

rec_cwt_loc_df <-rec_cwt_loc %>% 
  compact(.) %>%
  map_df(., ~as.data.frame(.)) %>%
  mutate(recovery_date = make_date(recovery_date_year, recovery_date_month, recovery_date_day)) %>%
  mutate(jday_rec = yday(recovery_date)) 

write.csv(rec_cwt_loc_df, paste0(proj_drive,"data/rec_cwt_loc_df.csv"))

rec_cwt_loc_df <-read.csv(paste0(proj_drive,"data/rec_cwt_loc_df.csv"))
loc_tag_codes <-unique(rec_cwt_loc_df$tag_code)

# RELEASES
rel_loc_based <-list()

for(h in 1:length(loc_tag_codes)){
  rel_pull <- tryCatch({rmisr::get_release(token = token,
                                           tag_code_or_release_id = loc_tag_codes[[h]])}, 
                       error = function(e) {message(paste("No data found ", loc_tag_codes[[h]], "- skipping to next pull."))
                         return(NULL)})
  if (is.null(rel_pull) || nrow(rel_pull) == 0) {next}
  rel_loc_based[[h]] <-rel_pull
}
names(rel_loc_based) <-loc_tag_codes
rel_loc_based_df     <-map_df(rel_loc_based, ~as.data.frame(.))

cols_to_fix <- c("cwt_1st_mark_count", "cwt_2nd_mark_count", "non_cwt_1st_mark_count", "non_cwt_2nd_mark_count")
rel_loc_based_df[cols_to_fix][is.na(rel_loc_based_df[cols_to_fix])] <- 0
rel_loc_based_df$cwt_released         <-rel_loc_based_df$cwt_1st_mark_count +
  rel_loc_based_df$cwt_2nd_mark_count
# rel_loc_based_df$release_stage_assigned <-as.factor(rel_loc_based_df$release_stage_assigned)
rel_loc_based_df$non_cwt_released <-rel_loc_based_df$non_cwt_1st_mark_count +
  rel_loc_based_df$non_cwt_2nd_mark_count    
rel_loc_based_df$total_released <-rel_loc_based_df$cwt_1st_mark_count +
  rel_loc_based_df$cwt_2nd_mark_count +
  rel_loc_based_df$non_cwt_1st_mark_count +
  rel_loc_based_df$non_cwt_2nd_mark_count    


### Calculate CWT tag rate
rel_dat_locbased <-rel_loc_based_df %>%  
  filter(run==params$run) %>%
  filter(brood_year >= params$yr_start & brood_year <=params$yr_end) %>%
  mutate(avg_weight = ifelse(avg_weight < 5, round(avg_weight),round(avg_weight, digits = -1))) %>%
  mutate(first_release_date = make_date(first_release_date_year, first_release_date_month, first_release_date_day)) %>%
  mutate(jday = yday(first_release_date)) %>%
  aggregate(cbind(cwt_released,non_cwt_released,total_released) ~ species + run + brood_year + release_location_code + hatchery_location_code + stock_location_code +
              first_release_date_month + tag_code_or_release_id + jday + avg_weight,
            data=., FUN=sum) %>%
  rename(tag_code = tag_code_or_release_id)

rel_dat_locbased_cwt <-rel_dat_locbased %>%
  select(species,brood_year,hatchery_location_code,first_release_date_month,tag_code,cwt_released) %>%
  filter(!grepl("!",tag_code))

tag_rate_locbased <-rel_dat_locbased %>%
  # filter(release_stage_assigned == "smolt") %>%
  aggregate(cbind(cwt_released)~species + brood_year + hatchery_location_code + first_release_date_month + tag_code,
            data=., FUN=sum) %>%
  filter(!grepl("!",tag_code))

untag_rate_locbased <-rel_dat_locbased %>%
  # filter(release_stage_assigned == "smolt") %>%
  aggregate(cbind(total_released)~species + brood_year + hatchery_location_code + first_release_date_month,
            data=., FUN=sum)

tag_untag_locbased       <-tag_rate_locbased %>% left_join(., untag_rate_locbased, by=c("species","brood_year","hatchery_location_code","first_release_date_month"))
tag_untag_locbased$tag_rate <-tag_untag_locbased$cwt_released/tag_untag_locbased$total_released
tag_rate_locbased_df       <-tag_untag_locbased %>% left_join(., rel_dat_locbased_cwt, by=c("species","brood_year","hatchery_location_code","first_release_date_month", "tag_code", "cwt_released"))

sar_df_locbased_pull <-rel_dat_locbased %>%
  left_join(., rec_cwt_loc_df[,c("tag_code","number_cwt_estimated","recovery_location_code","fishery","gear")], by = "tag_code") %>%
  aggregate(cbind(cwt_released, number_cwt_estimated) ~ species + run + brood_year + release_location_code + hatchery_location_code +
              first_release_date_month + jday + avg_weight + tag_code + 
              recovery_location_code + fishery + gear,
            data=., FUN=sum) %>%
  left_join(., tag_rate_locbased_df[,c("tag_code", "tag_rate","total_released")], by="tag_code")

sar_df_locbased_pull$sar_cwt             <-sar_df_locbased_pull$number_cwt_estimated/sar_df_locbased_pull$cwt_released
sar_df_locbased_pull$number_cwt_expanded <-sar_df_locbased_pull$number_cwt_estimated/sar_df_locbased_pull$tag_rate 
sar_df_locbased_pull$sar_total           <-sar_df_locbased_pull$number_cwt_expanded/sar_df_locbased_pull$total_released

sar_df_locbased <-sar_df_locbased_pull %>%
  select(-c(number_cwt_expanded, total_released, sar_total)) %>%
  rename(recoveries = number_cwt_estimated, releases = cwt_released, sar = sar_cwt)

### Recoveries by brood year & hatchery
ggplot(data=sar_df_locbased, aes(x=as.factor(brood_year), y=log(recoveries), group=hatchery_location_code, fill=hatchery_location_code, color=hatchery_location_code))+
  geom_bar(stat="summary",fun=sum)+
  # geom_boxplot()+
  # geom_point(pch=21)+
  # geom_line()
  # scale_x_discrete(breaks=seq(1990,2020,5))
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability

### Recoveries by state of origin
sar_df_locbased_temp <-sar_df_locbased 
sar_df_locbased_temp$state <-str_extract(sar_df_locbased_temp$hatchery_location_code, "^.")

sar_df_locbased_state <-sar_df_locbased_temp %>%
  select(-sar) %>%
  mutate(state_name = case_when(
    state == "1" ~ "AK",
    state == "2" ~ "CAN",
    state == "3" ~ "WA",
    state == "4" ~ "ID",
    state == "5" ~ "OR",
    state == "6" ~ "CA",
    state == "7" ~ "Ocean")) %>%
  aggregate(recoveries ~ species + run + brood_year + release_location_code + hatchery_location_code + 
              first_release_date_month + jday + avg_weight + state_name + tag_code + releases, 
            data=., FUN=sum)

sar_df_locbased_state$sar <-sar_df_locbased_state$recoveries/sar_df_locbased_state$releases

ggplot(data=sar_df_locbased_state, aes(x=as.factor(state_name),y=recoveries, fill=as.factor(state_name)))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
  labs(x = "State of Origin", y = "Expanded Recoveries", fill = "State")+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability

### Recoveries by hatchery system for WA
sar_df_locbased_temp <-sar_df_locbased 
sar_df_locbased_temp$state <-str_extract(sar_df_locbased_temp$hatchery_location_code, "^.")

sar_df_locbased_WA <-sar_df_locbased_temp %>%
  select(-sar) %>%
  mutate(state_name = case_when(
    state == "1" ~ "AK",
    state == "2" ~ "CAN",
    state == "3" ~ "WA",
    state == "4" ~ "ID",
    state == "5" ~ "OR",
    state == "6" ~ "CA",
    state == "7" ~ "Ocean")) %>%
  filter(., state=="3")

cowl_recover      <-unique(sar_df_locbased_WA$hatchery_location_code)
cowl_recover_name <-list()

for(i in 1:length(cowl_recover)){
  cowl_recover_name[[i]] <-filter(loc_all, location_code == cowl_recover[i])[1,c("location_code","name","latitude","longitude","psc_region")]
}

cowl_recover_sum <-aggregate(recoveries~species + run + brood_year + hatchery_location_code, data=sar_df_locbased_WA,FUN=sum)
cowl_recover_df  <-cowl_recover_name %>% map_df(., ~as.data.frame(.)) %>%
  rename("hatchery_location_code" = location_code) %>%
  left_join(., cowl_recover_sum, by="hatchery_location_code") %>%
  arrange(desc(recoveries))

# ggplot(data=cowl_recover_df, aes(x=as.factor(brood_year), y=recoveries, group=name, fill=name, color=name))+
ggplot(data=filter(cowl_recover_df, name!="COWLITZ SALMON HATCHERY"), aes(x=as.factor(brood_year), y=recoveries, group=name, fill=name, color=name))+
  geom_bar(stat="summary",fun=sum)+
  # geom_boxplot()+
  # geom_point(pch=21)+
  # geom_line()
  # scale_x_discrete(breaks=seq(1990,2020,5))
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability

cowl_recover_df %>%
  filter(., brood_year > 2018) %>%
  # rename("month" = first_release_date_month) %>%
  arrange(brood_year, name) %>%
  select(-brood_year)%>%
  kbl(format="html", row.names = NA)%>%
  kable_classic(position="left", html_font = "Cambria")%>%
  kable_styling(full_width = F, bootstrap_options = c("condensed")) %>% 
  pack_rows(index = table(filter(cowl_recover_df, brood_year > 2018)$brood_year))
