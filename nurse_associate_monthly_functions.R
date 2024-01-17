nursing_associate_stock <- function() {
  #Amend column names so the dots are replaced with spaces
  colnames(Raw_Data_y2) <- str_replace_all(colnames(Raw_Data_y2), " ", "_")
  colnames(nationality) <- str_replace_all(colnames(nationality), " ", "_")
  
  #filter to midwives only in active assignment
  nursing_associate <- Raw_Data_y2 %>% 
    mutate(Staff_group = if_else(substr(Occupation_Code, 1, 1) %in% c(0,1,2,8,9) ,"x.Medical", 
                                 if_else(Occupation_Code %in% c("NGA", "NGB", "NGC", "NGD", "NGE", "NGF", "NGG", "NGH"),"nursing associate",
                                         if_else(Occupation_Code %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L", 
                                                                        "N1A","N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                                                        "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                                                        "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                                                        "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                                                        "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL", "NNN",
                                                                        "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A"), "x.Nurse", 
                                                 if_else(Occupation_Code %in% "N3H", "x.health_visitor",
                                                         if_else(Occupation_Code %in% c("H1A", "N9A", "NFA"), "Support","x.Other"))))))%>%
    filter(Status %in% c("Active Assignment", "Internal Secondment", "Acting Up"))%>%
    filter(Asg_Type_Of_Contract %in% c("Locum", "Fixed Term Temp", "Permanent")) %>%
    filter(Staff_group=="nursing associate")
  
  #merge on NHS D organisation codes and nationality file
  nursing_associate <- full_join(nursing_associate,NHS_orgs)
  nursing_associate <- full_join(nursing_associate,nationality)
  
  #filter to only NHS trusts and CCGs and override NA nationality with Unknowns
  nursing_associate <- nursing_associate %>% filter(NHSD_trust_or_CCG==1 & is.na(Unique_Nhs_Identifier)==FALSE) %>%
    mutate(Nationality_grouping = if_else(is.na(Nationality_grouping)==TRUE, 'Unknown',Nationality_grouping)) %>%
    filter(is.na(Unique_Nhs_Identifier)==FALSE) %>%
    mutate (Nationality_grouping_v2 = if_else(Nationality_grouping %in% c('UK','Unknown'),'Domestic',
                                              if_else(Nationality_grouping %in% c('EU','ROW'),'IR','Other')))
  
  
  ###################################Summaries###################################
  #nationality split
  summary_nat <- nursing_associate %>% group_by(Tm_Year_Month,Nationality_grouping) %>%
    summarise (FTE=sum(Contracted_Wte)) %>% 
    arrange(Tm_Year_Month,Nationality_grouping)
  
  #FTE by nat summary
  summary_nat_2 <- nursing_associate %>% group_by(Tm_Year_Month,Nationality_grouping_v2) %>%
    summarise (FTE=sum(Contracted_Wte)) %>% 
    arrange(Tm_Year_Month,Nationality_grouping_v2)
  
  #FTE summary
  summary <- nursing_associate %>% group_by(Tm_Year_Month) %>%
    summarise (FTE=sum(Contracted_Wte))
  
  #align columns for FTE summary to FTE by nat
  summary <- summary %>% mutate (Nationality_grouping="All")
  
  summary_nat_2 <- rename (summary_nat_2, Nationality_grouping=Nationality_grouping_v2)
  
  #bind grouping split and total and add date
  summary <- bind_rows(summary, summary_nat,summary_nat_2) %>%
    mutate(Date=as.Date(str_replace_all(str_replace_all(paste(Tm_Year_Month,"01")," ",""),"-",""),"%Y%B%d")) %>%
    arrange(Date,Nationality_grouping)
  
  #Extract csv
  write.csv(summary, paste("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/Nursing Associates/",str_replace_all(Sys.Date(),"-",""),"stock.csv"))
  
}

nursing_associate_stock_headcount <- function() {
  #Amend column names so the dots are replaced with spaces
  colnames(Raw_Data_y2) <- str_replace_all(colnames(Raw_Data_y2), " ", "_")
  colnames(nationality) <- str_replace_all(colnames(nationality), " ", "_")
  
  #filter to midwives only in active assignment
  nursing_associate <- Raw_Data_y2 %>% 
    mutate(Staff_group = if_else(substr(Occupation_Code, 1, 1) %in% c(0,1,2,8,9) ,"x.Medical", 
                                 if_else(Occupation_Code %in% c("NGA", "NGB", "NGC", "NGD", "NGE", "NGF", "NGG", "NGH"),"nursing associate",
                                         if_else(Occupation_Code %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L", 
                                                                        "N1A","N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                                                        "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                                                        "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                                                        "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                                                        "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL", "NNN",
                                                                        "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A"), "x.Nurse", 
                                                 if_else(Occupation_Code %in% "N3H", "x.health_visitor",
                                                         if_else(Occupation_Code %in% c("H1A", "N9A", "NFA"), "Support","x.Other"))))))%>%
    filter(Status %in% c("Active Assignment", "Internal Secondment", "Acting Up"))%>%
    filter(Asg_Type_Of_Contract %in% c("Locum", "Fixed Term Temp", "Permanent")) %>%
    filter(Staff_group=="nursing associate")
  
  #Active to put in headcount over FTE
  nursing_associate$Contracted_Wte <- ifelse(nursing_associate$Contracted_Wte > 0, 1, nursing_associate$Contracted_Wte)
  
  
  #merge on NHS D organisation codes and nationality file
  nursing_associate <- full_join(nursing_associate,NHS_orgs)
  nursing_associate <- full_join(nursing_associate,nationality)
  
  #filter to only NHS trusts and CCGs and override NA nationality with Unknowns
  nursing_associate <- nursing_associate %>% filter(NHSD_trust_or_CCG==1 & is.na(Unique_Nhs_Identifier)==FALSE) %>%
    mutate(Nationality_grouping = if_else(is.na(Nationality_grouping)==TRUE, 'Unknown',Nationality_grouping)) %>%
    filter(is.na(Unique_Nhs_Identifier)==FALSE) %>%
    mutate (Nationality_grouping_v2 = if_else(Nationality_grouping %in% c('UK','Unknown'),'Domestic',
                                              if_else(Nationality_grouping %in% c('EU','ROW'),'IR','Other')))
  
  
  ###################################Summaries###################################
  #nationality split
  summary_nat <- nursing_associate %>% group_by(Tm_Year_Month,Nationality_grouping) %>%
    summarise (FTE=sum(Contracted_Wte)) %>% 
    arrange(Tm_Year_Month,Nationality_grouping)
  
  #FTE by nat summary
  summary_nat_2 <- nursing_associate %>% group_by(Tm_Year_Month,Nationality_grouping_v2) %>%
    summarise (FTE=sum(Contracted_Wte)) %>% 
    arrange(Tm_Year_Month,Nationality_grouping_v2)
  
  #FTE summary
  summary <- nursing_associate %>% group_by(Tm_Year_Month) %>%
    summarise (FTE=sum(Contracted_Wte))
  
  #align columns for FTE summary to FTE by nat
  summary <- summary %>% mutate (Nationality_grouping="All")
  
  summary_nat_2 <- rename (summary_nat_2, Nationality_grouping=Nationality_grouping_v2)
  
  #bind grouping split and total and add date
  summary <- bind_rows(summary, summary_nat,summary_nat_2) %>%
    mutate(Date=as.Date(str_replace_all(str_replace_all(paste(Tm_Year_Month,"01")," ",""),"-",""),"%Y%B%d")) %>%
    arrange(Date,Nationality_grouping)
  
  #Extract csv
  write.csv(summary, paste("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/Nursing Associates/",str_replace_all(Sys.Date(),"-",""),"stock headcount.csv"))
  
}

nursing_associate_nationality <- function() {
  colnames(Raw_Data_y1) <- str_replace_all(colnames(Raw_Data_y1), " ", "_")
  colnames(Raw_Data_y2) <- str_replace_all(colnames(Raw_Data_y2), " ", "_")
  colnames(nationality) <- str_replace_all(colnames(nationality), " ", "_")
  
  #join NHS organisation codes and nationality
  Raw_Data_y1 <- full_join(Raw_Data_y1,NHS_orgs)
  Raw_Data_y1 <- full_join(Raw_Data_y1,nationality)
  Raw_Data_y2 <- full_join(Raw_Data_y2,NHS_orgs)
  Raw_Data_y2 <- full_join(Raw_Data_y2,nationality)
  
  #Adding sufixes to variables to separate variable names between datasets
  colnames(Raw_Data_y1) <- paste(colnames(Raw_Data_y1), "y1", sep = "_")
  colnames(Raw_Data_y2) <- paste(colnames(Raw_Data_y2), "y2", sep = "_")
  
  
  
  #Rename unique identifier to remove the suffix so it's easier to merge datasets later on
  Raw_Data_y1 <- rename(Raw_Data_y1, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y1)
  Raw_Data_y2 <- rename(Raw_Data_y2, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y2)
  
  #create flags for active nurses, on both datasets, will be important depending on whether looking at joiners or leavers later
  Raw_Data_y1 <- Raw_Data_y1 %>% 
    mutate(Staff_group_y1 = if_else(substr(Occupation_Code_y1, 1, 1) %in% c(0,1,2,8,9) ,"x.Medical", 
                                    if_else(Occupation_Code_y1 %in% c("NGA", "NGB", "NGC", "NGD", "NGE", "NGF", "NGG", "NGH"),"nursing_associate",
                                            if_else(Occupation_Code_y1 %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L", 
                                                                              "N1A","N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                                                              "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                                                              "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                                                              "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                                                              "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL", "NNN",
                                                                              "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A"), "x.Nurse", 
                                                    if_else(Occupation_Code_y1 %in% "N3H", "x.health_visitor",
                                                            if_else(Occupation_Code_y1 %in% c("H1A", "N9A", "NFA"), "Support","x.Other"))))))%>%
    mutate(Status_orig_y1 = Status_y1) %>%
    mutate(Status_y1 = if_else(Status_y1 %in% c("Active Assignment", "Internal Secondment", "Acting Up"),"Active","Not active"))%>%
    mutate(Asg_Type_Of_Contract_y1 = if_else(Asg_Type_Of_Contract_y1 %in% c("Locum", "Fixed Term Temp", "Permanent"),"Permanent/fixed term/locum","Other")) %>%
    filter (Asg_Type_Of_Contract_y1=='Permanent/fixed term/locum') %>%
    mutate (Nationality_grouping_y1 =if_else(is.na(Nationality_grouping_y1) == FALSE, Nationality_grouping_y1, 'Unknown')) %>%
    mutate (Nationality_grouping_y1_v2 = if_else(Nationality_grouping_y1 %in% c('ROW','EU'), 'IR',
                                                 if_else(Nationality_grouping_y1 %in% c('UK','Unknown'),'Domestic','Other')))
  
  Raw_Data_y2 <- Raw_Data_y2 %>%
    mutate(Staff_group_y2 = if_else(substr(Occupation_Code_y2, 1, 1) %in% c(0,1,2,8,9) ,"x.Medical", 
                                    if_else(Occupation_Code_y2 %in% c("NGA", "NGB", "NGC", "NGD", "NGE", "NGF", "NGG", "NGH"),"nursing_associate",
                                            if_else(Occupation_Code_y2 %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L", 
                                                                              "N1A","N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                                                              "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                                                              "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                                                              "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                                                              "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL", "NNN",
                                                                              "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A"), "x.Nurse", 
                                                    if_else(Occupation_Code_y2 %in% "N3H", "x.health_visitor",
                                                            if_else(Occupation_Code_y2 %in% c("H1A", "N9A", "NFA"), "Support","x.Other"))))))%>%  
    mutate(Status_orig_y2 = Status_y2) %>%
    mutate(Status_y2 = if_else(Status_y2 %in% c("Active Assignment", "Internal Secondment", "Acting Up"),"Active","Not active"))%>%
    mutate(Asg_Type_Of_Contract_y2 = if_else(Asg_Type_Of_Contract_y2 %in% c("Locum", "Fixed Term Temp", "Permanent"),"Permanent/fixed term/locum","Other")) %>%
    filter (Asg_Type_Of_Contract_y2=='Permanent/fixed term/locum') %>%
    mutate (Nationality_grouping_y2 =if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, 'Unknown')) %>%
    mutate (Nationality_grouping_y2_v2 = if_else(Nationality_grouping_y2 %in% c('ROW','EU'), 'IR',
                                                 if_else(Nationality_grouping_y2 %in% c('UK','Unknown'),'Domestic','Other')))
  
  #ordering data by fte and then staff group
  Raw_Data_y1 <- Raw_Data_y1[order(Raw_Data_y1$Staff_group_y1,-Raw_Data_y1$Contracted_Wte_y1),]
  Raw_Data_y2 <- Raw_Data_y2[order(Raw_Data_y2$Staff_group_y2,-Raw_Data_y2$Contracted_Wte_y2),]
  
  ###################   New code to try and get headcount over FTE    ########################
  
  #Raw_Data_y1$Contracted_Wte_y1 <- ifelse(Raw_Data_y1$Contracted_Wte_y1 > 0, 1, Raw_Data_y1$Contracted_Wte_y1)
  #Raw_Data_y2$Contracted_Wte_y2 <- ifelse(Raw_Data_y2$Contracted_Wte_y2 > 0, 1, Raw_Data_y2$Contracted_Wte_y2)
  
  #removing all duplications in Person_Unique_Nhs_Identifier so there's only one entry for each
  Raw_Data_y1_dedup <- Raw_Data_y1[ !duplicated(Raw_Data_y1$Unique_Nhs_Identifier), ]
  Raw_Data_y2_dedup <- Raw_Data_y2[ !duplicated(Raw_Data_y2$Unique_Nhs_Identifier), ]
  
  #Join datasets
  Data <- full_join(Raw_Data_y1_dedup, Raw_Data_y2_dedup, by = "Unique_Nhs_Identifier")
  
  #get rid of unsused data
  rm(Raw_Data_y1_dedup)
  
  #merge nationality into a single field and override NA nationality with Unknowns and NA NHS providers with 0s
  Data <- Data %>%
    mutate (Nationality = if_else(is.na(Nationality_y2) == FALSE, Nationality_y2, Nationality_y1)) %>%
    mutate (Nationality_grouping =if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, Nationality_grouping_y1)) %>%
    mutate (Nationality_grouping = if_else(is.na(Nationality_grouping) == TRUE, 'Unknown',Nationality_grouping)) %>%
    mutate (Nationality_grouping_v2 = if_else(Nationality_grouping %in% c('ROW','EU'), 'IR',
                                              if_else(Nationality_grouping %in% c('UK','Unknown'),'Domestic','Other'))) %>%
    mutate (NHSD_trust_or_CCG_y1 = if_else(is.na(NHSD_trust_or_CCG_y1) == FALSE, NHSD_trust_or_CCG_y1,0)) %>% 
    mutate (NHSD_trust_or_CCG_y2 = if_else(is.na(NHSD_trust_or_CCG_y2) == FALSE, NHSD_trust_or_CCG_y2,0))
  
  
  #joiner/ leaver flags
  Data <- Data %>%
    #joiner flags
    mutate(joiner = if_else(is.na(Staff_group_y1) == TRUE & Staff_group_y2 %in% c("nursing_associate") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(occ_joiner = if_else(Staff_group_y1 != "nursing_associate" & NHSD_trust_or_CCG_y1 == "1" & Staff_group_y2 == c("nursing_associate") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(non_active_to_active = if_else(Staff_group_y1 == "nursing_associate" & Status_y1 != "Active" & NHSD_trust_or_CCG_y1 == "1" & Staff_group_y2 == "nursing_associate" & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(nhs_provider_joiner = if_else(Staff_group_y1 == "nursing_associate" & NHSD_trust_or_CCG_y1 == "0" & Staff_group_y2 == "nursing_associate" & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(other_joiner = if_else(Staff_group_y1 != "nursing_associate" & NHSD_trust_or_CCG_y1 == "0" & Staff_group_y2 == "nursing_associate" & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    #leaver flags
    mutate(leaver = if_else(is.na(Staff_group_y2) == TRUE & Staff_group_y1 %in% c("nursing_associate") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(occ_leaver = if_else(Staff_group_y2 != "nursing_associate" & NHSD_trust_or_CCG_y2 == "1" & Staff_group_y1 == c("nursing_associate") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(active_to_non_active = if_else(Staff_group_y2 == "nursing_associate" & Status_y2 != "Active" & NHSD_trust_or_CCG_y2 == "1" & Staff_group_y1 == c("nursing_associate") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(nhs_provider_leaver = if_else(Staff_group_y2 == "nursing_associate" & NHSD_trust_or_CCG_y2 == "0" & Staff_group_y1 == "nursing_associate" & Status_y1 == "Active" & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(other_leaver = if_else(Staff_group_y2 != "nursing_associate" & NHSD_trust_or_CCG_y2 == "0" & Staff_group_y1 == "nursing_associate" & Status_y1 == "Active" & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    #FTE change
    mutate(FTE_change = if_else(Staff_group_y2 == "nursing_associate" & Staff_group_y1 == "nursing_associate" & Status_y1 == "Active" & Status_y2 == "Active" & NHSD_trust_or_CCG_y1 == "1" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2-Contracted_Wte_y1, 0))
  
  #override NAs in joiner/ leaver flags
  Data <- Data %>%
    mutate (nhs_provider_joiner = if_else(is.na(nhs_provider_joiner)==FALSE,nhs_provider_joiner,0)) %>%
    mutate (other_joiner = if_else(is.na(other_joiner)==FALSE,other_joiner,0)) %>%
    mutate (nhs_provider_leaver = if_else(is.na(nhs_provider_leaver)==FALSE,nhs_provider_leaver,0)) %>%
    mutate (other_leaver = if_else(is.na(other_leaver)==FALSE,other_leaver,0)) #%>%
  #mutate (Age = if_else(is.na(Age_In_Years_y2)==TRUE,Age_In_Years_y1,Age_In_Years_y2)) %>%
  #filter(is.na(Age_In_Years_y1)==FALSE & is.na(Age_In_Years_y2)==FALSE) #%>%
  #mutate (Age = if_else(Age>55,'55+',if_else(Age<=55,'<=55','Other')))
  
  #duplication check
  Data$Unique_Nhs_Identifier[duplicated(Data$Unique_Nhs_Identifier)]
  
  #Join datasets to overwrite Y1 nationality
  Raw_Data_y1 <- left_join(Raw_Data_y1, Raw_Data_y2_dedup, by = "Unique_Nhs_Identifier")
  
  #get rid of unsused data
  rm(Raw_Data_y2_dedup)
  
  #overwrite Y1 nationality
  Raw_Data_y1<- Raw_Data_y1 %>%
    mutate (Nationality_grouping_y1_v2_2=if_else(is.na(Nationality_grouping_y2_v2)==FALSE,Nationality_grouping_y2_v2,Nationality_grouping_y1_v2)) %>%
    mutate (Nationality_grouping_y1_v2=if_else(is.na(Nationality_grouping_y2)==FALSE,Nationality_grouping_y2,Nationality_grouping_y1))
  
  
  ###################################joiner/ leaver summaries###################################
  #Total joiners/ leavers
  summary <- Data %>%
    summarise (joiner=sum(joiner),
               occ_joiner=sum(occ_joiner),
               non_active_to_active=sum(non_active_to_active),
               nhs_provider_joiner=sum(nhs_provider_joiner),
               other_joiner=sum(other_joiner),
               leaver=sum(leaver),
               occ_leaver=sum(occ_leaver),
               active_to_non_active=sum(active_to_non_active),
               nhs_provider_leaver=sum(nhs_provider_leaver),
               other_leaver=sum(other_leaver),
               FTE_change=sum(FTE_change)
    )
  
  #insert nationality column
  summary <- summary %>% mutate (Nationality_grouping="All") %>% select (12,1:11)
  
  #Split by nationality
  summary_nat <- Data %>%
    group_by(Nationality_grouping) %>%
    summarise (joiner=sum(joiner),
               occ_joiner=sum(occ_joiner),
               non_active_to_active=sum(non_active_to_active),
               nhs_provider_joiner=sum(nhs_provider_joiner),
               other_joiner=sum(other_joiner),
               leaver=sum(leaver),
               occ_leaver=sum(occ_leaver),
               active_to_non_active=sum(active_to_non_active),
               nhs_provider_leaver=sum(nhs_provider_leaver),
               other_leaver=sum(other_leaver),
               FTE_change=sum(FTE_change)
    )
  
  summary_nat_group <- Data %>%
    group_by(Nationality_grouping_v2) %>%
    summarise (joiner=sum(joiner),
               occ_joiner=sum(occ_joiner),
               non_active_to_active=sum(non_active_to_active),
               nhs_provider_joiner=sum(nhs_provider_joiner),
               other_joiner=sum(other_joiner),
               leaver=sum(leaver),
               occ_leaver=sum(occ_leaver),
               active_to_non_active=sum(active_to_non_active),
               nhs_provider_leaver=sum(nhs_provider_leaver),
               other_leaver=sum(other_leaver),
               FTE_change=sum(FTE_change)
    )
  
  #rename nationality grouping to match above summaries
  summary_nat_group <- rename (summary_nat_group, Nationality_grouping=Nationality_grouping_v2)
  
  #combine total with nat split
  summary <- bind_rows(summary_nat,summary,summary_nat_group) 
  
  #remove helper tables
  rm(summary_nat,summary_nat_group)
  
  
  ###################################FTE summaries###################################
  #FTE - year 1
  #total
  FTE_y1_1 <- Raw_Data_y1 %>%
    filter(Staff_group_y1 %in% c("nursing_associate") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
    summarise (FTE_y1 = sum(Contracted_Wte_y1))
  
  FTE_y1_1 <- FTE_y1_1 %>% mutate (Nationality_grouping='All')
  
  #nationality split 1
  FTE_y1_2 <- Raw_Data_y1 %>%
    filter(Staff_group_y1 %in% c("nursing_associate") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
    mutate (Nationality_grouping_y1 = if_else(is.na(Nationality_grouping_y1) == FALSE, Nationality_grouping_y1, 'Unknown')) %>%
    group_by(Nationality_grouping_y1_v2) %>%
    summarise (FTE_y1 = sum(Contracted_Wte_y1))
  
  FTE_y1_2 <- rename (FTE_y1_2, Nationality_grouping=Nationality_grouping_y1_v2)
  
  #nationality split 2
  FTE_y1_3 <- Raw_Data_y1 %>%
    filter(Staff_group_y1 %in% c("nursing_associate") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
    group_by(Nationality_grouping_y1_v2_2) %>%
    summarise (FTE_y1 = sum(Contracted_Wte_y1))
  
  FTE_y1_3 <- rename (FTE_y1_3, Nationality_grouping=Nationality_grouping_y1_v2_2)
  
  #combine all FTE
  FTE_y1 <- bind_rows(FTE_y1_2,FTE_y1_1,FTE_y1_3) 
  
  
  #FTE - year 2
  #total
  FTE_y2_1 <- Raw_Data_y2 %>%
    filter(Staff_group_y2 %in% c("nursing_associate") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
    summarise (FTE_y2 = sum(Contracted_Wte_y2))
  
  FTE_y2_1 <- FTE_y2_1 %>% mutate (Nationality_grouping='All')
  
  #nationality split 1
  FTE_y2_2 <- Raw_Data_y2 %>%
    filter(Staff_group_y2 %in% c("nursing_associate") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
    mutate (Nationality_grouping_y2 = if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, 'Unknown')) %>%
    group_by(Nationality_grouping_y2) %>%
    summarise (FTE_y2 = sum(Contracted_Wte_y2))
  
  FTE_y2_2 <- rename (FTE_y2_2, Nationality_grouping=Nationality_grouping_y2)
  
  #nationality split 2
  FTE_y2_3 <- Raw_Data_y2 %>%
    filter(Staff_group_y2 %in% c("nursing_associate") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
    group_by(Nationality_grouping_y2_v2) %>%
    summarise (FTE_y2 = sum(Contracted_Wte_y2))
  
  FTE_y2_3 <- rename (FTE_y2_3, Nationality_grouping=Nationality_grouping_y2_v2)
  
  #combine all FTE
  FTE_y2 <- bind_rows(FTE_y2_2,FTE_y2_1,FTE_y2_3) 
  
  ###################################Final combined summary output###################################
  #combine joiners/ leavers with FTE
  summary <- bind_cols(summary,FTE_y1,FTE_y2) %>%
    select (1:12,14,16)
  
  #remove helper tables
  rm(FTE_y1_1,FTE_y1_2,FTE_y1_3,FTE_y2_1,FTE_y2_2,FTE_y2_3,FTE_y1,FTE_y2)
  
  #leaver rates
  summary <- summary %>%
    mutate(leaver_rate = as.numeric(leaver)/as.numeric(FTE_y1)) %>%
    mutate(leaver_rate_occ = (as.numeric(occ_leaver))/as.numeric(FTE_y1))
  
  #pivot data into long format
  pivot <- pivot_longer(summary, c(2:16))
  
  ###################################Other summary outputs###################################
  
  #Summary for reason for leaving
  Reasonleaving <- Data %>%
    group_by(Reason_For_Leaving_y1)%>%
    summarise(leaver=sum(leaver))
  
  #Summary of joiners by country
  Country <- Data %>%
    group_by(Nationality_y2)%>%
    summarise(joiners=sum(joiner,occ_joiner))
  
  ###################################Pull joiner/ leaver period name###################################
  #extract joiner/ leaver period name
  pivot_final <- pivot
  colnames(pivot_final) <- c("Nationality_grouping", "name", paste(substr(Data$Tm_Year_Month_y1,1,8)[1],"to",substr(Data$Tm_Year_Month_y2,1,8)[2]))
  
  ##preparing extract for nurses dashboard time series
  long_format <- pivot %>%
    mutate(Date_from = paste(substr(Data$Tm_Year_Month_y1,1,8)[1],"to",substr(Data$Tm_Year_Month_y2,1,8)[2])) %>%
    mutate(Date = as.Date(paste0(substr(Date_from,13,16),"-", substr(Date_from,6,8), "-01"),"%Y-%b-%d")) %>%
    select(1,4,2,3,5)
  
  ################################### Exports ###################################
  
  #export to shared area with today's date
  write.csv(pivot_final, paste("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/Nursing Associates/",str_replace_all(Sys.Date(),"-",""),"joiners_leavers",long_format$Date_from[1],".csv"))
  
}

nursing_associate_nationality_headcount <- function() {
  colnames(Raw_Data_y1) <- str_replace_all(colnames(Raw_Data_y1), " ", "_")
  colnames(Raw_Data_y2) <- str_replace_all(colnames(Raw_Data_y2), " ", "_")
  colnames(nationality) <- str_replace_all(colnames(nationality), " ", "_")
  
  #join NHS organisation codes and nationality
  Raw_Data_y1 <- full_join(Raw_Data_y1,NHS_orgs)
  Raw_Data_y1 <- full_join(Raw_Data_y1,nationality)
  Raw_Data_y2 <- full_join(Raw_Data_y2,NHS_orgs)
  Raw_Data_y2 <- full_join(Raw_Data_y2,nationality)
  
  #Adding sufixes to variables to separate variable names between datasets
  colnames(Raw_Data_y1) <- paste(colnames(Raw_Data_y1), "y1", sep = "_")
  colnames(Raw_Data_y2) <- paste(colnames(Raw_Data_y2), "y2", sep = "_")
  
  
  
  #Rename unique identifier to remove the suffix so it's easier to merge datasets later on
  Raw_Data_y1 <- rename(Raw_Data_y1, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y1)
  Raw_Data_y2 <- rename(Raw_Data_y2, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y2)
  
  #create flags for active nurses, on both datasets, will be important depending on whether looking at joiners or leavers later
  Raw_Data_y1 <- Raw_Data_y1 %>% 
    mutate(Staff_group_y1 = if_else(substr(Occupation_Code_y1, 1, 1) %in% c(0,1,2,8,9) ,"x.Medical", 
                                    if_else(Occupation_Code_y1 %in% c("NGA", "NGB", "NGC", "NGD", "NGE", "NGF", "NGG", "NGH"),"nursing_associate",
                                            if_else(Occupation_Code_y1 %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L", 
                                                                              "N1A","N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                                                              "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                                                              "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                                                              "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                                                              "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL", "NNN",
                                                                              "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A"), "x.Nurse", 
                                                    if_else(Occupation_Code_y1 %in% "N3H", "x.health_visitor",
                                                            if_else(Occupation_Code_y1 %in% c("H1A", "N9A", "NFA"), "Support","x.Other"))))))%>%
    mutate(Status_orig_y1 = Status_y1) %>%
    mutate(Status_y1 = if_else(Status_y1 %in% c("Active Assignment", "Internal Secondment", "Acting Up"),"Active","Not active"))%>%
    mutate(Asg_Type_Of_Contract_y1 = if_else(Asg_Type_Of_Contract_y1 %in% c("Locum", "Fixed Term Temp", "Permanent"),"Permanent/fixed term/locum","Other")) %>%
    filter (Asg_Type_Of_Contract_y1=='Permanent/fixed term/locum') %>%
    mutate (Nationality_grouping_y1 =if_else(is.na(Nationality_grouping_y1) == FALSE, Nationality_grouping_y1, 'Unknown')) %>%
    mutate (Nationality_grouping_y1_v2 = if_else(Nationality_grouping_y1 %in% c('ROW','EU'), 'IR',
                                                 if_else(Nationality_grouping_y1 %in% c('UK','Unknown'),'Domestic','Other')))
  
  Raw_Data_y2 <- Raw_Data_y2 %>%
    mutate(Staff_group_y2 = if_else(substr(Occupation_Code_y2, 1, 1) %in% c(0,1,2,8,9) ,"x.Medical", 
                                    if_else(Occupation_Code_y2 %in% c("NGA", "NGB", "NGC", "NGD", "NGE", "NGF", "NGG", "NGH"),"nursing_associate",
                                            if_else(Occupation_Code_y2 %in% c("N0A", "N0B", "N0C", "N0D", "N0E", "N0F", "N0G", "N0H", "N0J", "N0K", "N0L", 
                                                                              "N1A","N1B", "N1C", "N1H", "N1J", "N1L", "N4D", "N4F", "N4H", "N5D", "N5F", "N5H",
                                                                              "N6A", "N6B", "N6C", "N6D", "N6E", "N6F", "N6G", "N6H", "N6J", "N6K", "N6L",
                                                                              "N7A", "N7B", "N7C", "N7D", "N7E", "N7F", "N7G", "N7H", "N7K", "N7L", "NAA",
                                                                              "NAB", "NAC", "NAD", "NAE", "NAF", "NAG", "NAH", "NAJ", "NAK", "NAL", "NBK",
                                                                              "NCA", "NCB", "NCC", "NCD", "NCE", "NCF", "NCG", "NCH", "NCJ", "NCK", "NCL", "NNN",
                                                                              "NEH", "P2B", "P2E", "P2C", "P2D", "P3C", "P3D", "P3E", "N7J","P2A","P3A"), "x.Nurse", 
                                                    if_else(Occupation_Code_y2 %in% "N3H", "x.health_visitor",
                                                            if_else(Occupation_Code_y2 %in% c("H1A", "N9A", "NFA"), "Support","x.Other"))))))%>%  
    mutate(Status_orig_y2 = Status_y2) %>%
    mutate(Status_y2 = if_else(Status_y2 %in% c("Active Assignment", "Internal Secondment", "Acting Up"),"Active","Not active"))%>%
    mutate(Asg_Type_Of_Contract_y2 = if_else(Asg_Type_Of_Contract_y2 %in% c("Locum", "Fixed Term Temp", "Permanent"),"Permanent/fixed term/locum","Other")) %>%
    filter (Asg_Type_Of_Contract_y2=='Permanent/fixed term/locum') %>%
    mutate (Nationality_grouping_y2 =if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, 'Unknown')) %>%
    mutate (Nationality_grouping_y2_v2 = if_else(Nationality_grouping_y2 %in% c('ROW','EU'), 'IR',
                                                 if_else(Nationality_grouping_y2 %in% c('UK','Unknown'),'Domestic','Other')))
  
  #ordering data by fte and then staff group
  Raw_Data_y1 <- Raw_Data_y1[order(Raw_Data_y1$Staff_group_y1,-Raw_Data_y1$Contracted_Wte_y1),]
  Raw_Data_y2 <- Raw_Data_y2[order(Raw_Data_y2$Staff_group_y2,-Raw_Data_y2$Contracted_Wte_y2),]
  
  ###################   New code to try and get headcount over FTE    ########################
  
  Raw_Data_y1$Contracted_Wte_y1 <- ifelse(Raw_Data_y1$Contracted_Wte_y1 > 0, 1, Raw_Data_y1$Contracted_Wte_y1)
  Raw_Data_y2$Contracted_Wte_y2 <- ifelse(Raw_Data_y2$Contracted_Wte_y2 > 0, 1, Raw_Data_y2$Contracted_Wte_y2)
  
  #removing all duplications in Person_Unique_Nhs_Identifier so there's only one entry for each
  Raw_Data_y1_dedup <- Raw_Data_y1[ !duplicated(Raw_Data_y1$Unique_Nhs_Identifier), ]
  Raw_Data_y2_dedup <- Raw_Data_y2[ !duplicated(Raw_Data_y2$Unique_Nhs_Identifier), ]
  
  #Join datasets
  Data <- full_join(Raw_Data_y1_dedup, Raw_Data_y2_dedup, by = "Unique_Nhs_Identifier")
  
  #get rid of unsused data
  rm(Raw_Data_y1_dedup)
  
  #merge nationality into a single field and override NA nationality with Unknowns and NA NHS providers with 0s
  Data <- Data %>%
    mutate (Nationality = if_else(is.na(Nationality_y2) == FALSE, Nationality_y2, Nationality_y1)) %>%
    mutate (Nationality_grouping =if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, Nationality_grouping_y1)) %>%
    mutate (Nationality_grouping = if_else(is.na(Nationality_grouping) == TRUE, 'Unknown',Nationality_grouping)) %>%
    mutate (Nationality_grouping_v2 = if_else(Nationality_grouping %in% c('ROW','EU'), 'IR',
                                              if_else(Nationality_grouping %in% c('UK','Unknown'),'Domestic','Other'))) %>%
    mutate (NHSD_trust_or_CCG_y1 = if_else(is.na(NHSD_trust_or_CCG_y1) == FALSE, NHSD_trust_or_CCG_y1,0)) %>% 
    mutate (NHSD_trust_or_CCG_y2 = if_else(is.na(NHSD_trust_or_CCG_y2) == FALSE, NHSD_trust_or_CCG_y2,0))
  
  
  #joiner/ leaver flags
  Data <- Data %>%
    #joiner flags
    mutate(joiner = if_else(is.na(Staff_group_y1) == TRUE & Staff_group_y2 %in% c("nursing_associate") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(occ_joiner = if_else(Staff_group_y1 != "nursing_associate" & NHSD_trust_or_CCG_y1 == "1" & Staff_group_y2 == c("nursing_associate") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(non_active_to_active = if_else(Staff_group_y1 == "nursing_associate" & Status_y1 != "Active" & NHSD_trust_or_CCG_y1 == "1" & Staff_group_y2 == "nursing_associate" & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(nhs_provider_joiner = if_else(Staff_group_y1 == "nursing_associate" & NHSD_trust_or_CCG_y1 == "0" & Staff_group_y2 == "nursing_associate" & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(other_joiner = if_else(Staff_group_y1 != "nursing_associate" & NHSD_trust_or_CCG_y1 == "0" & Staff_group_y2 == "nursing_associate" & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    #leaver flags
    mutate(leaver = if_else(is.na(Staff_group_y2) == TRUE & Staff_group_y1 %in% c("nursing_associate") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(occ_leaver = if_else(Staff_group_y2 != "nursing_associate" & NHSD_trust_or_CCG_y2 == "1" & Staff_group_y1 == c("nursing_associate") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(active_to_non_active = if_else(Staff_group_y2 == "nursing_associate" & Status_y2 != "Active" & NHSD_trust_or_CCG_y2 == "1" & Staff_group_y1 == c("nursing_associate") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(nhs_provider_leaver = if_else(Staff_group_y2 == "nursing_associate" & NHSD_trust_or_CCG_y2 == "0" & Staff_group_y1 == "nursing_associate" & Status_y1 == "Active" & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(other_leaver = if_else(Staff_group_y2 != "nursing_associate" & NHSD_trust_or_CCG_y2 == "0" & Staff_group_y1 == "nursing_associate" & Status_y1 == "Active" & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    #FTE change
    mutate(FTE_change = if_else(Staff_group_y2 == "nursing_associate" & Staff_group_y1 == "nursing_associate" & Status_y1 == "Active" & Status_y2 == "Active" & NHSD_trust_or_CCG_y1 == "1" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2-Contracted_Wte_y1, 0))
  
  #override NAs in joiner/ leaver flags
  Data <- Data %>%
    mutate (nhs_provider_joiner = if_else(is.na(nhs_provider_joiner)==FALSE,nhs_provider_joiner,0)) %>%
    mutate (other_joiner = if_else(is.na(other_joiner)==FALSE,other_joiner,0)) %>%
    mutate (nhs_provider_leaver = if_else(is.na(nhs_provider_leaver)==FALSE,nhs_provider_leaver,0)) %>%
    mutate (other_leaver = if_else(is.na(other_leaver)==FALSE,other_leaver,0)) #%>%
  #mutate (Age = if_else(is.na(Age_In_Years_y2)==TRUE,Age_In_Years_y1,Age_In_Years_y2)) %>%
  #filter(is.na(Age_In_Years_y1)==FALSE & is.na(Age_In_Years_y2)==FALSE) #%>%
  #mutate (Age = if_else(Age>55,'55+',if_else(Age<=55,'<=55','Other')))
  
  #duplication check
  Data$Unique_Nhs_Identifier[duplicated(Data$Unique_Nhs_Identifier)]
  
  #Join datasets to overwrite Y1 nationality
  Raw_Data_y1 <- left_join(Raw_Data_y1, Raw_Data_y2_dedup, by = "Unique_Nhs_Identifier")
  
  #get rid of unsused data
  rm(Raw_Data_y2_dedup)
  
  #overwrite Y1 nationality
  Raw_Data_y1<- Raw_Data_y1 %>%
    mutate (Nationality_grouping_y1_v2_2=if_else(is.na(Nationality_grouping_y2_v2)==FALSE,Nationality_grouping_y2_v2,Nationality_grouping_y1_v2)) %>%
    mutate (Nationality_grouping_y1_v2=if_else(is.na(Nationality_grouping_y2)==FALSE,Nationality_grouping_y2,Nationality_grouping_y1))
  
  
  ###################################joiner/ leaver summaries###################################
  #Total joiners/ leavers
  summary <- Data %>%
    summarise (joiner=sum(joiner),
               occ_joiner=sum(occ_joiner),
               non_active_to_active=sum(non_active_to_active),
               nhs_provider_joiner=sum(nhs_provider_joiner),
               other_joiner=sum(other_joiner),
               leaver=sum(leaver),
               occ_leaver=sum(occ_leaver),
               active_to_non_active=sum(active_to_non_active),
               nhs_provider_leaver=sum(nhs_provider_leaver),
               other_leaver=sum(other_leaver),
               FTE_change=sum(FTE_change)
    )
  
  #insert nationality column
  summary <- summary %>% mutate (Nationality_grouping="All") %>% select (12,1:11)
  
  #Split by nationality
  summary_nat <- Data %>%
    group_by(Nationality_grouping) %>%
    summarise (joiner=sum(joiner),
               occ_joiner=sum(occ_joiner),
               non_active_to_active=sum(non_active_to_active),
               nhs_provider_joiner=sum(nhs_provider_joiner),
               other_joiner=sum(other_joiner),
               leaver=sum(leaver),
               occ_leaver=sum(occ_leaver),
               active_to_non_active=sum(active_to_non_active),
               nhs_provider_leaver=sum(nhs_provider_leaver),
               other_leaver=sum(other_leaver),
               FTE_change=sum(FTE_change)
    )
  
  summary_nat_group <- Data %>%
    group_by(Nationality_grouping_v2) %>%
    summarise (joiner=sum(joiner),
               occ_joiner=sum(occ_joiner),
               non_active_to_active=sum(non_active_to_active),
               nhs_provider_joiner=sum(nhs_provider_joiner),
               other_joiner=sum(other_joiner),
               leaver=sum(leaver),
               occ_leaver=sum(occ_leaver),
               active_to_non_active=sum(active_to_non_active),
               nhs_provider_leaver=sum(nhs_provider_leaver),
               other_leaver=sum(other_leaver),
               FTE_change=sum(FTE_change)
    )
  
  #rename nationality grouping to match above summaries
  summary_nat_group <- rename (summary_nat_group, Nationality_grouping=Nationality_grouping_v2)
  
  #combine total with nat split
  summary <- bind_rows(summary_nat,summary,summary_nat_group) 
  
  #remove helper tables
  rm(summary_nat,summary_nat_group)
  
  
  ###################################FTE summaries###################################
  #FTE - year 1
  #total
  FTE_y1_1 <- Raw_Data_y1 %>%
    filter(Staff_group_y1 %in% c("nursing_associate") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
    summarise (FTE_y1 = sum(Contracted_Wte_y1))
  
  FTE_y1_1 <- FTE_y1_1 %>% mutate (Nationality_grouping='All')
  
  #nationality split 1
  FTE_y1_2 <- Raw_Data_y1 %>%
    filter(Staff_group_y1 %in% c("nursing_associate") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
    mutate (Nationality_grouping_y1 = if_else(is.na(Nationality_grouping_y1) == FALSE, Nationality_grouping_y1, 'Unknown')) %>%
    group_by(Nationality_grouping_y1_v2) %>%
    summarise (FTE_y1 = sum(Contracted_Wte_y1))
  
  FTE_y1_2 <- rename (FTE_y1_2, Nationality_grouping=Nationality_grouping_y1_v2)
  
  #nationality split 2
  FTE_y1_3 <- Raw_Data_y1 %>%
    filter(Staff_group_y1 %in% c("nursing_associate") & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
    group_by(Nationality_grouping_y1_v2_2) %>%
    summarise (FTE_y1 = sum(Contracted_Wte_y1))
  
  FTE_y1_3 <- rename (FTE_y1_3, Nationality_grouping=Nationality_grouping_y1_v2_2)
  
  #combine all FTE
  FTE_y1 <- bind_rows(FTE_y1_2,FTE_y1_1,FTE_y1_3) 
  
  
  #FTE - year 2
  #total
  FTE_y2_1 <- Raw_Data_y2 %>%
    filter(Staff_group_y2 %in% c("nursing_associate") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
    summarise (FTE_y2 = sum(Contracted_Wte_y2))
  
  FTE_y2_1 <- FTE_y2_1 %>% mutate (Nationality_grouping='All')
  
  #nationality split 1
  FTE_y2_2 <- Raw_Data_y2 %>%
    filter(Staff_group_y2 %in% c("nursing_associate") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
    mutate (Nationality_grouping_y2 = if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, 'Unknown')) %>%
    group_by(Nationality_grouping_y2) %>%
    summarise (FTE_y2 = sum(Contracted_Wte_y2))
  
  FTE_y2_2 <- rename (FTE_y2_2, Nationality_grouping=Nationality_grouping_y2)
  
  #nationality split 2
  FTE_y2_3 <- Raw_Data_y2 %>%
    filter(Staff_group_y2 %in% c("nursing_associate") & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
    group_by(Nationality_grouping_y2_v2) %>%
    summarise (FTE_y2 = sum(Contracted_Wte_y2))
  
  FTE_y2_3 <- rename (FTE_y2_3, Nationality_grouping=Nationality_grouping_y2_v2)
  
  #combine all FTE
  FTE_y2 <- bind_rows(FTE_y2_2,FTE_y2_1,FTE_y2_3) 
  
  ###################################Final combined summary output###################################
  #combine joiners/ leavers with FTE
  summary <- bind_cols(summary,FTE_y1,FTE_y2) %>%
    select (1:12,14,16)
  
  #remove helper tables
  rm(FTE_y1_1,FTE_y1_2,FTE_y1_3,FTE_y2_1,FTE_y2_2,FTE_y2_3,FTE_y1,FTE_y2)
  
  #leaver rates
  summary <- summary %>%
    mutate(leaver_rate = as.numeric(leaver)/as.numeric(FTE_y1)) %>%
    mutate(leaver_rate_occ = (as.numeric(occ_leaver))/as.numeric(FTE_y1))
  
  #pivot data into long format
  pivot <- pivot_longer(summary, c(2:16))
  
  ###################################Other summary outputs###################################
  
  #Summary for reason for leaving
  Reasonleaving <- Data %>%
    group_by(Reason_For_Leaving_y1)%>%
    summarise(leaver=sum(leaver))
  
  #Summary of joiners by country
  Country <- Data %>%
    group_by(Nationality_y2)%>%
    summarise(joiners=sum(joiner,occ_joiner))
  
  ###################################Pull joiner/ leaver period name###################################
  #extract joiner/ leaver period name
  pivot_final <- pivot
  colnames(pivot_final) <- c("Nationality_grouping", "name", paste(substr(Data$Tm_Year_Month_y1,1,8)[1],"to",substr(Data$Tm_Year_Month_y2,1,8)[2]))
  
  ##preparing extract for nurses dashboard time series
  long_format <- pivot %>%
    mutate(Date_from = paste(substr(Data$Tm_Year_Month_y1,1,8)[1],"to",substr(Data$Tm_Year_Month_y2,1,8)[2])) %>%
    mutate(Date = as.Date(paste0(substr(Date_from,13,16),"-", substr(Date_from,6,8), "-01"),"%Y-%b-%d")) %>%
    select(1,4,2,3,5)
  
  ################################### Exports ###################################
  
  #export to shared area with today's date
  write.csv(pivot_final, paste("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/Nursing Associates/",str_replace_all(Sys.Date(),"-",""),"joiners_leavers headcount",long_format$Date_from[1],".csv"))
  
}