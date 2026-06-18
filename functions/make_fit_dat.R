make_fit_dat <-function(rel_wide,age_comps,age_comps_w,returns_HOR_agg){  
  # WDFW species codes in age_and_scales (diff from PSC species codes); turn all to common names
  # age_and_scales[WDFW]: 1 = Chinook; 2 = Chum; 3 = Pink; 4 = Coho; 5 = Sockeye; 6 = Steelhead; CT = Cutthroat
  # releases & returns[RMIS/FishBooks]: 1 = chinook; 2 = coho; 3 = steelhead; 4- = sockeye; 5 = chum; 6 = pink; 7 = masu; 8 = cutthroat
  
  ### release data
  rel_fit <-reshape2::melt(rel_wide[,c("species_name","region","brood_yr","sy_index","yrl_index","smolt_index","no_stage","total_released")], 
                           id.vars=c("species_name", "region", "brood_yr"), variable.name = "rel_stage")
  names(rel_fit)[names(rel_fit)=="value"] <-"releases"
  names(rel_fit)[names(rel_fit)=="species_name"] <-"species"
  
  
  ### age data
  # age comps: hood canal
  age_comps_hood_df     <-map_df(age_comps[[1]][["hood"]],~as.data.frame(.))
  age_comps_hood_df$age <-rep(seq(1,8,1),nrow(age_comps_hood_df)/8)
  age_comps_hood_df$region <-rep("hood",nrow(age_comps_hood_df))
  age_comps_hood_df$return_yr <-rep(names(age_comps[[1]][["hood"]]),each=8)
  # age comps: strait of juan de fuca
  age_comps_juan_df     <-map_df(age_comps[[1]][["juan"]],~as.data.frame(.))
  age_comps_juan_df$age <-rep(seq(1,8,1),nrow(age_comps_juan_df)/8)
  age_comps_juan_df$region <-rep("juan",nrow(age_comps_juan_df))
  age_comps_juan_df$return_yr <-rep(names(age_comps[[1]][["juan"]]),each=8)
  # age comps: nowa
  age_comps_nowa_df     <-map_df(age_comps[[1]][["nowa"]],~as.data.frame(.))
  age_comps_nowa_df$age <-rep(seq(1,8,1),nrow(age_comps_nowa_df)/8)
  age_comps_nowa_df$region <-rep("nowa",nrow(age_comps_nowa_df))
  age_comps_nowa_df$return_yr <-rep(names(age_comps[[1]][["nowa"]]),each=8)
  # age comps: nps
  age_comps_nps_df     <-map_df(age_comps[[1]][["nps"]],~as.data.frame(.))
  age_comps_nps_df$age <-rep(seq(1,8,1),nrow(age_comps_nps_df)/8)
  age_comps_nps_df$region <-rep("nps",nrow(age_comps_nps_df))
  age_comps_nps_df$return_yr <-rep(names(age_comps[[1]][["nps"]]),each=8)
  # age comps: mps
  age_comps_mps_df     <-map_df(age_comps[[1]][["mps"]],~as.data.frame(.))
  age_comps_mps_df$age <-rep(seq(1,8,1),nrow(age_comps_mps_df)/8)
  age_comps_mps_df$region <-rep("mps",nrow(age_comps_mps_df))
  age_comps_mps_df$return_yr <-rep(names(age_comps[[1]][["mps"]]),each=8)
  # age comps: sps
  age_comps_sps_df     <-map_df(age_comps[[1]][["sps"]],~as.data.frame(.))
  age_comps_sps_df$age <-rep(seq(1,8,1),nrow(age_comps_sps_df)/8)
  age_comps_sps_df$region <-rep("sps",nrow(age_comps_sps_df))
  age_comps_sps_df$return_yr <-rep(names(age_comps[[1]][["sps"]]),each=8)
  # COMBINE
  age_comps_long <-rbind(age_comps_hood_df,age_comps_juan_df,age_comps_nowa_df,age_comps_nps_df,age_comps_mps_df,age_comps_sps_df) # make wide df long
  colnames(age_comps_long)[1] <-"P_rya"
  
  
  ### return data
  # apply age comp data to total return data to get age-specific returns
  # returns are reported by hatchery; for each year, check for age data from the wria (build in search for hatchery-specific ages first)
  # sp_name_list <-unique(returns_H_agg$species_name)
  # most WRIAs only have reliable age data from 2013-present; count yrs of return data for each hatchery; only use hatcheries w/ 10+ yrs? 
  # hatch_return_yrs <-returns_H_agg %>%  group_by(hatchery_clean, species_name) %>% summarise(count = n_distinct(trap_yr))
  returns_swya <-list()
  comps_temp_sp <-list()
  
  for(sp in 1:length(sp_name_list)){
    returns_w <-list()
    comps_temp_w <-list()
    # hatch_return_list <-filter(hatch_return_yrs, species_name==sp_name_list[sp] & count>=10)$hatchery_clean
    for(w in 1:length(ps_regions)){
      returns_ya <-list()
      comps_temp_y <-list()
      comps_w    <-age_comps_w[[sp_name_list[sp]]][[ps_regions[w]]]
      age_yrs    <-sort(unique(as.numeric(names(age_comps[[sp_name_list[sp]]][[ps_regions[w]]]))))
      for(y in 1:length(age_yrs)){
        returns_swy     <-returns_HOR_agg %>% filter(species_name==sp_name_list[sp] & trap_yr==age_yrs[y] & region==ps_regions[w])
        comps_w_y       <-age_comps[[sp_name_list[sp]]][[ps_regions[w]]][[as.character(age_yrs[y])]]
        comps_temp      <-if(length(comps_w_y)!=0) comps_w_y else comps_w
        comps_temp_y[[y]] <-comps_temp
        returns_ya[[y]] <-sum(returns_swy$returns) * comps_temp
      }
      names(comps_temp_y) <-age_yrs
      comps_temp_w[[w]] <-comps_temp_y
      names(returns_ya) <-age_yrs
      returns_w[[w]] <-returns_ya
    }
    names(comps_temp_w) <-ps_regions
    comps_temp_sp[[sp]] <-comps_temp_w
    names(returns_w) <-ps_regions
    returns_swya[[sp]] <-returns_w
  }
  names(comps_temp_sp) <-sp_name_list
  names(returns_swya) <-sp_name_list
  
  # convert hatchery returns-at-age time series list to df; attach age-classes & wria 
  if("chinook" %in% sp_name_list){
    returns_swya_df_ck <-map(returns_swya[["chinook"]], as.data.table) %>% rbindlist(., fill = TRUE, idcol = T) 
    returns_swya_df_ck$age <-rep(seq(1,8,1),length(unique(returns_swya_df_ck$.id)))
    # returns_swya_df_ck$wria <-as.character(hatch_wria[returns_swya_df_ck$.id])
    returns_swya_df_ck$species <-rep("chinook",nrow(returns_swya_df_ck))
  }
  if("coho" %in% sp_name_list){
    returns_swya_df_co <-map(returns_swya[["coho"]], as.data.table) %>% rbindlist(., fill = TRUE, idcol = T) 
    returns_swya_df_co$age <-rep(seq(1,8,1),length(unique(returns_swya_df_co$.id)))
    # returns_swya_df_co$wria <-as.character(hatch_wria[returns_swya_df_co$.id])
    returns_swya_df_co$species <-rep("coho",nrow(returns_swya_df_co))
  }
  if("chum" %in% sp_name_list){
    returns_swya_df_ch <-map(returns_swya[["chum"]], as.data.table) %>% rbindlist(., fill = TRUE, idcol = T) 
    returns_swya_df_ch$age <-rep(seq(1,8,1),length(unique(returns_swya_df_ch$.id)))
    # returns_swya_df_ch$wria <-as.character(hatch_wria[returns_swya_df_ch$.id])
    returns_swya_df_ch$species <-rep("chum",nrow(returns_swya_df_ch))
  }
  # if("pink" %in% sp_name_list){
  #   returns_swya_df_pk <-map(returns_swya[["pink"]], as.data.table) %>% rbindlist(., fill = TRUE, idcol = T)
  #   returns_swya_df_pk$age <-rep(seq(1,8,1),length(unique(returns_swya_df_pk$.id)))
  #   returns_swya_df_pk$wria <-as.character(hatch_wria[returns_swya_df_pk$.id])
  #   returns_swya_df_pk$species <-rep("pink",nrow(returns_swya_df_pk))
  # }
  returns_swya_df <-rbind(returns_swya_df_ck)
  # returns_swya_df <-rbind(returns_swya_df_ck,returns_swya_df_co,returns_swya_df_ch)
  names(returns_swya_df)[names(returns_swya_df)==".id"] <-"region"
  
  # setDT(returns_swya_df)
  ret_fit_temp <-returns_swya_df %>% melt(., id.vars = c("species","region","age"), variable.name = "year")
  names(ret_fit_temp)[names(ret_fit_temp)=="value"] <-"returns"
  ret_fit_temp$brood_yr <-as.numeric(as.character(ret_fit_temp$year))-ret_fit_temp$age
  ret_fit <-aggregate(returns~species+region+brood_yr, data=ret_fit_temp, FUN=sum)
  
  # add brood_yr & combine release-return data by RMIS region
  rel_fit$region <-tolower(rel_fit$region)
  
  fit_dat <-rel_fit %>% 
    filter(., region %in% ps_regions) %>%
    left_join(., filter(ret_fit, region %in% ps_regions), by=c("species", "region", "brood_yr"), relationship="many-to-many") %>%
    # filter(wria %in% puget) %>%
    na.omit(.)
  
  fit_dat$surv <-fit_dat$returns/fit_dat$releases

  ### Additions to fit_dat pipe
  # > define release yr for SY (+1) & Y (+2); they are currently wrong
  # > make stage a binary factor?
  # > release yr effect (numbered list)
  # > do we want to include a pink index or lose it b/c its incorporated in the multi-species index?
  #   >> change pink_fry$year to migration yr (+1)
  #   >> make pink_yr a binary factor?  
  # > potential transformations 
  #   >> releases in millions
  #   >> pink_fry in millions
  #   >> log(seals); log(herring)
  #   >> remove mean(cov$sst) from annual measurements
  #   >> sd_std(releases in millions) == z-score? 
  # > Check numbers against other pubs to make sure w/i the same scale (i.e., not missing data)

  
  # ## Z-score
  # sd_std<- function(x, sd_group) {std=(x-mean(sd_group))/(sd(sd_group)); return(std)}
  
  
  write.csv(fit_dat,paste0(drive,"Data/fit_dat.csv"))
  return(fit_dat)
}