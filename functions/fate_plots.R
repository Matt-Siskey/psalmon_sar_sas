fate_plots <-function(fate_type, rec_type){
  
  if(fate_type!="fishery"){
    # sar by brood year & mean wt
    rel_size <-ggplot(data=filter(sar_df_fates, fate == fate_type & recovery_location_code %in% rec_type),aes(x=as.factor(brood_year),y=sar*100, fill=as.factor(avg_weight)))+
                 geom_boxplot(outlier.shape = NA)+
                 geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
                 labs(x = "Brood Year", y = "SAR (%)", fill = "Mean Weight (g)")+
                 theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    # sar by brood year & release month
    rel_month <-ggplot(data=filter(sar_df_fates, fate == fate_type & recovery_location_code %in% rec_type),aes(x=as.factor(brood_year),y=sar*100, fill=as.factor(first_release_date_month)))+
                  geom_boxplot(outlier.shape = NA)+
                  geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
                  labs(x = "Brood Year", y = "SAR (%)", fill = "Release Month")+
                  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
    
    # sar by release location
    rel_loc <-ggplot(data=filter(sar_df_fates, fate == fate_type & recovery_location_code %in% rec_type),aes(x=as.factor(brood_year),y=sar*100, fill=as.factor(release_location_code)))+
                geom_boxplot(outlier.shape = NA)+
                geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
                labs(x = "Brood Year", y = "SAR (%)", fill = "Release Site")+
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
    
    # sar by release timing (jday)
    rel_jday <-ggplot(data=filter(sar_df_fates, fate == fate_type & recovery_location_code %in% rec_type),aes(x=as.factor(jday),y=sar*100))+
                 geom_boxplot(outlier.shape = NA)+
                 geom_jitter(pch=21,size=2,width = 0.2, alpha=0.75,aes(group=as.factor(brood_year),fill=as.factor(brood_year)))+
                 labs(x = "Julian Day", y = "SAR (%)", fill = "Release Site")+
                 theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # Angle x-axis labels for readability
                 theme(legend.position = "right")
    return(list(rel_size = rel_size, rel_month = rel_month, rel_loc = rel_loc, rel_jday = rel_jday))
  }
  
  if(fate_type=="fishery"){
    # ##### number_cwt_estimated figures
    # ### commercial vs. treaty vs. sport
    # fleet_fishery <-ggplot(data=fishery_recover_df, aes(x=as.factor(fishery_type),y=sar*100, fill=as.factor(fishery_type)))+
    #   geom_boxplot(outlier.shape = NA)+
    #   geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
    #   labs(x = "Fishery Type", y = "log(number_cwt_estimated)", fill = "Fishery Type")+
    #   scale_y_log10() +
    #   theme(legend.position = "none") +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
    # 
    # ### southern US (oregon vs. wa) vs. BC vs. AK
    # # 1 = AK
    # # 2 = CAN
    # # 3 = WA
    # # 4 = ID
    # # 5 = OR
    # # 6 = CA
    # # 7 = Ocean
    # state_fishery <-ggplot(data=fishery_recover_df, aes(x=as.factor(state_name),y=sar*100, fill=as.factor(state_name)))+
    #   geom_boxplot(outlier.shape = NA)+
    #   geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
    #   labs(x = "State", y = "log(number_cwt_estimated)", fill = "State")+
    #   scale_y_log10() +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
    # 
    ### mainstem vs. trib
    stem_trib_fishery <-ggplot(data=filter(fishery_recover_df, water_type=="F"), aes(y=as.factor(stem_trib),x=log(number_cwt_estimated), fill=as.factor(stem_trib)))+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
      scale_x_log10() +
      labs(y = "Fresh Fishery", x = "log(number_cwt_estimated)", fill = "Fresh Fishery")+
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability

    ### marine location parse
    marine_fishery <-ggplot(data=filter(fishery_recover_df, water_type=="M"), aes(y=as.factor(fishery_short_name),x=log(number_cwt_estimated), fill=as.factor(fishery_type)))+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
      labs(y = "Marine Fishery", x = "log(number_cwt_estimated)", fill = "Marine Fishery")+
      # facet_grid(.~state)+
      scale_x_log10()+
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
    
    ##### SAR figs
    ### commercial vs. treaty vs. sport
    sar_df_fleet <-sar_df %>%
      select(-sar) %>%
      mutate(fishery_type = case_when(
        fishery %in% sport ~ "Sport",
        fishery %in% commercial ~ "Commercial",
        fishery %in% treaty ~ "Treaty",
        fishery %nin% c(sport, commercial, treaty) ~ "Other"
      )) %>%
      mutate(.,fate = case_when(fishery %in% fate_fishery ~ "fishery",
                                fishery %in% fate_hatchery ~ "hatchery",
                                fishery %in% fate_spawngr ~ "spawngr"))%>%
      filter(fate=="fishery")%>%
      aggregate(number_cwt_estimated ~ species + brood_year + release_location_code + hatchery_location_code + 
                  first_release_date_month + jday + avg_weight + fishery_type + tag_code + event_released, 
                data=., FUN=sum)
    
    sar_df_fleet$sar <-sar_df_fleet$number_cwt_estimated/sar_df_fleet$event_released
    
      
    fleet_fishery <-ggplot(data=sar_df_fleet, aes(x=as.factor(fishery_type),y=sar*100, fill=as.factor(fishery_type)))+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
      labs(x = "Fishery Type", y = "SAR (%)", fill = "Fishery Type")+
      # scale_y_log10() +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
      
    
    # ### southern US (oregon vs. wa) vs. BC vs. AK
    sar_df_temp <-sar_df 
    sar_df_temp$state <-str_extract(sar_df_temp$recovery_location_code, "^.")
    
    sar_df_state <-sar_df_temp %>%
      select(-sar) %>%
      mutate(fate = case_when(fishery %in% fate_fishery ~ "fishery",
                                fishery %in% fate_hatchery ~ "hatchery",
                                fishery %in% fate_spawngr ~ "spawngr"))%>%
      mutate(state_name = case_when(
        state == "1" ~ "AK",
        state == "2" ~ "CAN",
        state == "3" ~ "WA",
        state == "4" ~ "ID",
        state == "5" ~ "OR",
        state == "6" ~ "CA",
        state == "7" ~ "Ocean")) %>%
      filter(fate=="fishery") %>%
      aggregate(number_cwt_estimated ~ species + brood_year + release_location_code + hatchery_location_code + 
                  first_release_date_month + jday + avg_weight + state_name + tag_code + event_released, 
                data=., FUN=sum)
    

    sar_df_state$sar <-sar_df_state$number_cwt_estimated/sar_df_state$event_released
    
    state_fishery <-ggplot(data=sar_df_state, aes(x=as.factor(state_name),y=sar*100, fill=as.factor(state_name)))+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
      labs(x = "State", y = "SAR (%)", fill = "State")+
      theme(legend.position="none")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability

    
    
    
    return(list(fleet_fishery = fleet_fishery, state_fishery = state_fishery, stem_trib_fishery = stem_trib_fishery, marine_fishery = marine_fishery))
  }
}
  