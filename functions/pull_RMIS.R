pull_RMIS <-function(){
  ##############################
  # [LOCATION]
  loc_all <-rmisr::get_location(token = token)

  ##############################
  # [RELEASES] Pull in RMIS release data
  rel_h      <-list()
  
  for(h in 1:length(hatcheries)){
    rel_h[[h]] <-rmisr::get_release(token = token,
                                    species = params$sp,
                                    run = params$run,
                                    hatchery_location_code = hatcheries[h])
  }
  names(rel_h) <-hatcheries
  rel_df       <-map_df(rel_h, ~as.data.frame(.))
  
  
  ### Assign release stage based on length-at-smolt and length-weight relationship
  len_wt_temp <-rel_df %>% drop_na(avg_length) 
  len_wt_df   <-len_wt_temp %>% filter(avg_length >= (quantile(avg_length, 0.25) - 1.5 * IQR(avg_length)) &
                                       avg_length <= (quantile(avg_length, 0.75) + 1.5 * IQR(avg_length)))
  len_wt_df   <-len_wt_df[order(len_wt_df$avg_length), ]
  len_wt_fit  <-lm(log(avg_weight) ~ avg_length, data=len_wt_df)

  if(params$sp==1){smolt_mm <-60}  # chinook
  if(params$sp==2){smolt_mm <-105} # coho
  if(params$sp==3){smolt_mm <-200} # steelhead
  if(params$sp==4){smolt_mm <-80}  # sockeye
  if(params$sp==5){smolt_mm <-40}  # chum
  if(params$sp==6){smolt_mm <-32}  # pink
  if(params$sp==8){smolt_mm <-190} # cutthroat
  
  len_wt_coefs <-coef(len_wt_fit)
  a            <-as.numeric(exp(len_wt_coefs[1]))  # 'a' is the initial value
  r            <-as.numeric(len_wt_coefs[2])       # 'r' is the growth/decay rate
  len_wt_pred  <-a * exp(r * smolt_mm)
  
  
  rel_dat <-rel_df %>%
    mutate(release_stage_assigned = case_when(
      avg_weight > len_wt_pred ~ "smolt",
      avg_weight >= 0 ~ "fry_parr"
      )
    )
  
  cols_to_fix <- c("cwt_1st_mark_count", "cwt_2nd_mark_count", "non_cwt_1st_mark_count", "non_cwt_2nd_mark_count")
  rel_dat[cols_to_fix][is.na(rel_dat[cols_to_fix])] <- 0
  rel_dat$event_released         <-rel_dat$cwt_1st_mark_count +
                                   rel_dat$cwt_2nd_mark_count
  rel_dat$release_stage_assigned <-as.factor(rel_dat$release_stage_assigned)
  
  rel_agg <-rel_dat %>% filter(!grepl("!",tag_code_or_release_id)) %>%
    filter(run==params$run) %>%
    mutate(avg_weight = ifelse(avg_weight < 5, round(avg_weight),round(avg_weight, digits = -1))) %>%
    mutate(first_release_date = make_date(first_release_date_year, first_release_date_month, first_release_date_day)) %>%
    mutate(jday = yday(first_release_date)) %>%
    aggregate(event_released ~ species + brood_year + release_location_code + hatchery_location_code + stock_location_code +
                first_release_date_month + tag_code_or_release_id + jday + avg_weight + release_stage_assigned,
              data=., FUN=sum) %>%
    rename(tag_code = tag_code_or_release_id)
  
  ### TRUE needed to run .qmd; "true" needed for quarto_render
  if(params$yr_trim==TRUE){rel_agg <-filter(rel_agg, brood_year %in% seq(params$yr_start,params$yr_end,1))}
  if(params$smolt==TRUE){rel_agg <-filter(rel_agg,release_stage_assigned=="smolt")}
  # if(params$yr_trim=="true"){rel_agg <-filter(rel_agg, brood_year %in% seq(params$yr_start,params$yr_end,1))}
  # if(params$smolt=="true"){rel_agg <-filter(rel_agg,release_stage_assigned=="smolt")}
  
  ##############################
  # [RETURNS] Pull in WDFW FishBooks return data
  # use the tag codes from the release query to grab recoveries; pull capped at 500k records
  rel_agg_cwt <-unique(rel_agg$tag_code)
  rec_cwt     <-list()
  
  for(t in 1:length(rel_agg_cwt)){
    rec_cwt[[t]] <-rmisr::get_recovery(token = token, tag_code = rel_agg_cwt[[t]], species = params$sp)
  }
  names(rec_cwt) <-rel_agg_cwt
  
  rec_cwt_df <-rec_cwt %>% map_df(., ~as.data.frame(.)) %>%  
    mutate(recovery_date = make_date(recovery_date_year, recovery_date_month, recovery_date_day)) %>%
    mutate(jday_rec = yday(recovery_date)) %>%
    left_join(., rel_agg[,c("brood_year","tag_code")], by = "tag_code")
  
  rec_cwt_df$age <-rec_cwt_df$recovery_date_year-rec_cwt_df$brood_year
  
  rec_join <-rec_cwt_df %>% filter(., age >= params$adult_age) %>%
    aggregate(number_cwt_estimated ~ species + recovery_location_code + fishery + tag_code, data=., FUN=sum)

  ########################## Join Release & Return Data for SAR ##########################
  # even if release_location_code is the same across recoveries, event_released gets multi-counted if you dont distinguish separate SAR calculations for tag_code
  sar_df <-left_join(rel_agg, rec_join[,c("tag_code","number_cwt_estimated","recovery_location_code","fishery")], by = "tag_code") %>%
    aggregate(cbind(event_released, number_cwt_estimated) ~ species + brood_year + release_location_code + hatchery_location_code +
                first_release_date_month + jday + avg_weight + release_stage_assigned + tag_code + 
                recovery_location_code + fishery,
              data=., FUN=sum)
  
  sar_df$sar <-sar_df$number_cwt_estimated/sar_df$event_released
  return(list(len_wt_df = len_wt_df, sar_df = sar_df, smolt_mm = smolt_mm, loc_all = loc_all))
  print("got RMIS data")
}