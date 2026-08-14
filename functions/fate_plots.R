fate_plots <-function(fate_type, rec_type){
  # sar by brood year & mean wt
  rel_size <-ggplot(data=filter(sar_df_fates, fate == fate_type & recovery_location_code %in% rec_type),aes(x=as.factor(brood_year),y=sar*100, fill=as.factor(avg_weight)))+
               geom_boxplot(outlier.shape = NA)+
               geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
               labs(x = "Brood Year", y = "SAR (%)", fill = "Mean Weight (g)", title = paste0("Return Location: ", rec_type))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # sar by brood year & release month
  rel_month <-ggplot(data=filter(sar_df_fates, fate == fate_type & recovery_location_code %in% rec_type),aes(x=as.factor(brood_year),y=sar*100, fill=as.factor(first_release_date_month)))+
                geom_boxplot(outlier.shape = NA)+
                geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
                labs(x = "Brood Year", y = "SAR (%)", fill = "Release Month", title = paste0("Return Location: ", rec_type))+
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
  
  # sar by release location
  rel_loc <-ggplot(data=filter(sar_df_fates, fate == fate_type & recovery_location_code %in% rec_type),aes(x=as.factor(brood_year),y=sar*100, fill=as.factor(release_location_code)))+
              geom_boxplot(outlier.shape = NA)+
              geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
              labs(x = "Brood Year", y = "SAR (%)", fill = "Release Site", title = paste0("Return Location: ", rec_type))+
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Angle x-axis labels for readability
  
  # sar by release timing (jday)
  rel_jday <-ggplot(data=filter(sar_df_fates, fate == fate_type & recovery_location_code %in% rec_type),aes(x=as.factor(jday),y=sar*100))+
               geom_boxplot(outlier.shape = NA)+
               geom_jitter(pch=21,size=2,width = 0.2, alpha=0.6)+
               labs(x = "Julien Day", y = "SAR (%)", fill = "Release Site", title = paste0("Return Location: ", rec_type))+
               theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # Angle x-axis labels for readability
               theme(legend.position = "none")
  
  return(list(rel_size = rel_size, rel_month = rel_month, rel_loc = rel_loc, rel_jday = rel_jday))
}
  