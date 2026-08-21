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
                 geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
                 labs(x = "Julien Day", y = "SAR (%)", fill = "Release Site")+
                 theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # Angle x-axis labels for readability
                 theme(legend.position = "none")
    return(list(rel_size = rel_size, rel_month = rel_month, rel_loc = rel_loc, rel_jday = rel_jday))
  }
  
  if(fate_type=="fishery"){
    
    # ### psc_basin
    # ggplot(data=fishery_recover_df, aes(y=as.factor(psc_basin),x=number_cwt_estimated, fill=as.factor(psc_basin)))+
    #   geom_boxplot(outlier.shape = NA)+
    #   geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
    #   labs(x = "PSC Basin", y = "number_cwt_estimated", fill = "PSC Basin")+
    #   theme(legend.position = "none") +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
    
    ### fishery_short_name
    name_fishery <-ggplot(data=fishery_recover_df, aes(y=as.factor(fishery_short_name),x=number_cwt_estimated, fill=as.factor(fishery_short_name)))+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
      labs(x = "Fishery", y = "number_cwt_estimated", fill = "Fishery")+
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
    
    # ### commercial vs. treaty vs. sport
    # type_fishery <-ggplot(data=fishery_recover_df, aes(y=as.factor(fishery_type),x=number_cwt_estimated, fill=as.factor(fishery_type)))+
    #   geom_boxplot(outlier.shape = NA)+
    #   geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
    #   labs(x = "Fishery Type", y = "number_cwt_estimated", fill = "Fishery Type")+
    #   theme(legend.position = "none") +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
    
    ### southern US (oregon vs. wa) vs. BC vs. AK
    # 1 = AK
    # 2 = CAN
    # 3 = WA
    # 4 = ID
    # 5 = OR
    # 6 = CA
    # 7 = Ocean
    state_fishery <-ggplot(data=fishery_recover_df, aes(x=as.factor(state_name),y=number_cwt_estimated, fill=as.factor(state_name)))+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
      labs(x = "State", y = "number_cwt_estimated", fill = "State")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
    
    ### fresh vs. ocean
    water_type_fishery <-ggplot(data=fishery_recover_df, aes(x=as.factor(water_type),y=number_cwt_estimated, fill=as.factor(water_type)))+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
      labs(x = "Water Type", y = "number_cwt_estimated", fill = "Water Type")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
      
    ### mainstem vs. trib
    stem_trib_fishery <-ggplot(data=filter(fishery_recover_df, water_type=="F"), aes(y=as.factor(stem_trib),x=number_cwt_estimated, fill=as.factor(stem_trib)))+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
      labs(y = "Fresh Fishery", x = "number_cwt_estimated", fill = "Fresh Fishery")+
      # facet_grid(.~state)+
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
    
    
    return(list(name_fishery = name_fishery, state_fishery = state_fishery, water_type_fishery = water_type_fishery, stem_trib_fishery = stem_trib_fishery))
  }
}
  