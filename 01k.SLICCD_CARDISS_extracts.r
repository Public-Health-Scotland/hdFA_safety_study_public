sliccd_ntds <- readRDS("/conf/FolicAcid/data/extracts/sliccd_ntds.rds")
sliccd_singletons <- readRDS("/conf/FolicAcid/data/extracts/sliccd_singletons.rds")
cardriss_ntds <- readRDS("/conf/FolicAcid/data/extracts/cardriss_ntds.rds")
cardriss_singletons <- readRDS("/conf/FolicAcid/data/extracts/cardriss_singletons.rds")
slipbd1<- readRDS(paste0(folder_data_path, "extracts/slipbd_extract.rds"))

##Link mothers history
history_ntd <- sliccd_ntds %>% filter(cardriss_mother_upi %in% slipbd1$mother_upi) %>%
  select( cardriss_mother_upi,date_end_of_pregnancy, ALL_1_1_NEURAL_TUBE_DEFECTS ) %>%
  rename(mother_upi = cardriss_mother_upi)
history_cardriss_ntd <- cardriss_ntds %>%  filter(mother_upi  %in% slipbd1$mother_upi) %>%
  select(mother_upi, date_pregnancy_ended, ALL_1_1_NEURAL_TUBE_DEFECTS) %>%
  rename(date_end_of_pregnancy = date_pregnancy_ended)
history_ntd <-rbind(history_cardriss_ntd, history_ntd)
slipbd_dates <- slipbd1 %>% select(pregnancy_id, mother_upi, date_end_pregnancy)

df <- left_join(slipbd_dates, history_ntd, by =c("mother_upi" ))

df <- df %>%
  mutate(diff = as.Date(date_end_pregnancy)- as.Date(date_end_of_pregnancy)) %>%
  mutate(ntd_history = case_when(diff >84  ~1, T~0)) %>%# dates for previous preg are positive 
#add 84 to avoid picking up the same preg with different dates from different sources
  #(in practice they are within a few days of each other)
### link babies by triplicate ID
  filter(ntd_history==1)
saveRDS(df, paste0(folder_data_path, "processed_extracts/linked_ntd_history.rds"))

##Join singletons to SLiPBD
names(cardriss_singletons)[which((!names(cardriss_singletons) %in% names(sliccd_singletons) )) ]

cardriss_singletons <- cardriss_singletons %>%
  rename(ALL_13_1_1_LETHAL_SKELETAL_DYSPLASIAS=ALL_13_1_1_SEVERE_SKELETAL_DYSPLASIAS)
cardriss_singletons <- cardriss_singletons %>%  select(date_pregnancy_ended, 
        baby_upi, mother_upi, ALL_0_ALL_CONDITIONS, ALL_1_NERVOUS_SYSTEM,                          
         ALL_1_1_NEURAL_TUBE_DEFECTS, ALL_1_1_1_ANENCEPHALUS, ALL_1_1_2_ENCEPHALOCELE,ALL_1_1_3_SPINA_BIFIDA,                        
         ALL_1_2_HYDROCEPHALUS, ALL_1_3_MICROCEPHALY, ALL_1_4_ARHINENCEPHALY_HOLOPROSENCEPHALY, 
         ALL_2_EYE, ALL_2_1_ANOPHTHALMOS_MICROPHTALMOS, ALL_2_1_1_ANOPHTHALMOS, 
         ALL_2_2_CONGENITAL_CATARACT, ALL_2_3_CONGENITAL_GLAUCOMA, ALL_3_EAR_FACE_AND_NECK, ALL_3_1_ANOTIA, 
        ALL_4_CONGENITAL_HEART_DEFECTS, ALL_4_1_SEVERE_CHD, ALL_4_1_1_COMMON_ARTERIAL_TRUNCUS, 
        ALL_4_1_2_DOUBLE_OUTLET_RIGHT_VENTRICLE, ALL_4_1_4_COMPLETE_TRANSPOSITION_ARTERIES, 
        ALL_4_1_5_SINGLE_VENTRICLE,  ALL_4_2_VSD, ALL_4_3_ASD, ALL_4_1_7_AVSD, ALL_4_1_8_TETRALOGY_OF_FALLOT,                
         ALL_4_1_10_TRISCUSPID_ATRESIA_AND_STENOSIS, ALL_4_1_11_EBSTEINS_ANOMALY,                   
         ALL_4_4_PULMONARY_VALVE_STENOSIS ,  ALL_4_1_9_PULMONARY_VALVE_ATRESIA,           
         ALL_4_1_13_AORTIC_VALVE_ATRESIA,  ALL_4_1_14_MITRAL_VALVE_ATRESIA_STENOSIS,      
         ALL_4_1_15_HYPOPLASTIC_LEFT_HEART ,   ALL_4_1_12_HYPOPLASTIC_RIGHT_HEART,           
         ALL_4_1_16_COARCTATION_OF_AORTA,  ALL_4_1_17_AORTIC_ATRESIA_INTERRUPTED_ARCH,   
         ALL_4_1_18_TOTAL_ANOMALOUS_PULM_VENOUS_RETURN, ALL_4_5_PATENT_DUCTUS_ARTERIOSUS,              
         ALL_5_RESPIRATORY, ALL_5_1_CHOANAL_ATRESIA, ALL_5_2_CPAM, ALL_6_ORO_FACIAL_CLEFTS,                      
        ALL_6_1_CLEFT_LIP, ALL_6_2_CLEFT_PALATE,ALL_7_GASTRO_INTESTINAL, ALL_7_1_OESOPHAGEAL_ATRESIA,                   
        ALL_7_2_DUODENAL_ATRESIA_STENOSIS, ALL_7_3_ATRESIA_STENOSIS,   ALL_7_4_ANORECTAL_ATRESIA_STENOSIS, 
        ALL_7_5_HIRSCHSPRUNGS_DISEASE, ALL_7_6_ATRESIA_BILE_DUCTS, ALL_7_7_ANNULAR_PANCREAS, 
        ALL_7_9_DIAPHRAGMATIC_HERNIA,  ALL_8_ABDOMINAL_WALL_DEFECTS, ALL_8_1_GASTROSCHISIS, 
        ALL_8_2_OMPHALOCELE,                           
        ALL_9_KIDNEY_AND_URINARY_TRACT, ALL_9_2_BILATERAL_RENAL_AGENESIS,              
         ALL_9_3_MULTICYSTIC_RENAL_DYSPLASIA, ALL_9_4_CONGENITAL_HYDRONEPHROSIS,             
         ALL_9_6_BLADDER_EXSTROPHY, ALL_9_7_POSTERIOR_URETHRAL_VALVES,             
         ALL_10_GENITAL, ALL_10_1_HYPOSPADIAS,   ALL_10_2_INDETERMINATE_SEX,
         ALL_11_LIMB, ALL_11_1_LIMB_REDUCTION, ALL_11_2_CLUB_FOOT,                            
         ALL_11_3_HIP_DISLOCATION, ALL_11_4_POLYDACTYLY,   ALL_11_5_SYNDACTYLY, 
         ALL_12_OTHER_CONDITIONS, ALL_12_1_CRANIOSYNOSTOSIS, 
         ALL_12_2_CONSTRICTION_AMNIOTIC_BAND, ALL_12_3_SITUS_INVERSUS, ALL_12_5_VATER_VACTERL, 
         ALL_12_10_VASCULAR_DISRUPTION, ALL_12_11_LATERALITY_ANOMALIES,               
         ALL_12_12_TERTOGENIC_SYNDROMES, ALL_12_12_2_MATERNAL_INFECTIONS,              
         ALL_13_GENETIC_CONDITIONS, ALL_13_2_DOWN_SYNDROME,
        ALL_13_3_PATAU_SYNDROME, ALL_13_4_EDWARDS_SYNDROME,                    
         ALL_13_5_TURNER_SYNDROME, ALL_13_1_SKELETAL_DYSPLASIAS,                 
         ALL_12_4_CONJOINED_TWINS, ALL_12_12_1_VALPROATE_SYNDROME,ALL_1_5_AGENESIS_CORPUS_CALLOSUM, 
        ALL_4_1_3_DOUBLE_OUTLET_LEFT_VENTRICLE, ALL_4_1_6_CORRECTED_TRANSPOSITION_ARTERIES, 
        ALL_7_8_INTESTINAL_FIXATION, ALL_9_1_UNILATERAL_RENAL_AGENESIS, 
        ALL_9_5_HORSESHOE_ECTOPIC_KIDNEY, ALL_9_8_PRUNE_BELLY, ALL_11_1_1_TRANSVERSE_LIMB_REDUCTION,          
         ALL_11_1_2_PREAXIAL_LIMB_REDUCTION,ALL_11_1_3_POSTAXIAL_LIMB_REDUCTION,          
         ALL_11_1_4_CENTRAL_LIMB_REDUCTION,ALL_11_1_5_INTERCALARY_LIMB_REDUCTION,        
         ALL_12_6_PIERRE_ROBIN_SEQUENCE,ALL_12_7_CAUDAL_REGRESSION_SEQUENCE,          
         ALL_12_8_SIRENOMELIA,ALL_12_9_SEPTO_OPTIC_DYSPLASIA,               
         ALL_13_6_TRIPLOIDY_POLYPLOIDY,ALL_13_1_1_LETHAL_SKELETAL_DYSPLASIAS  )%>%
  rename( date_end_of_pregnancy = date_pregnancy_ended)


sliccd_singletons <- sliccd_singletons %>%
 select(cardriss_mother_upi, cardriss_baby_upi, nrs_triplicate_id, date_end_of_pregnancy,                            
 ALL_0_ALL_CONDITIONS, ALL_1_NERVOUS_SYSTEM, ALL_1_1_NEURAL_TUBE_DEFECTS, 
 ALL_1_1_1_ANENCEPHALUS, ALL_1_1_2_ENCEPHALOCELE, ALL_1_1_3_SPINA_BIFIDA, ALL_1_2_HYDROCEPHALUS, 
 ALL_1_3_MICROCEPHALY, ALL_1_4_ARHINENCEPHALY_HOLOPROSENCEPHALY ,ALL_1_5_AGENESIS_CORPUS_CALLOSUM, 
 ALL_2_EYE, ALL_2_1_ANOPHTHALMOS_MICROPHTALMOS, ALL_2_1_1_ANOPHTHALMOS, ALL_2_2_CONGENITAL_CATARACT, 
 ALL_2_3_CONGENITAL_GLAUCOMA, ALL_3_EAR_FACE_AND_NECK, ALL_3_1_ANOTIA, 
 ALL_4_CONGENITAL_HEART_DEFECTS, ALL_4_1_SEVERE_CHD, ALL_4_1_1_COMMON_ARTERIAL_TRUNCUS, 
 ALL_4_1_2_DOUBLE_OUTLET_RIGHT_VENTRICLE, ALL_4_1_3_DOUBLE_OUTLET_LEFT_VENTRICLE, 
 ALL_4_1_4_COMPLETE_TRANSPOSITION_ARTERIES, ALL_4_1_5_SINGLE_VENTRICLE, 
 ALL_4_1_6_CORRECTED_TRANSPOSITION_ARTERIES, ALL_4_1_7_AVSD, ALL_4_1_8_TETRALOGY_OF_FALLOT,  
 ALL_4_1_9_PULMONARY_VALVE_ATRESIA, ALL_4_1_10_TRISCUSPID_ATRESIA_AND_STENOSIS,
 ALL_4_1_11_EBSTEINS_ANOMALY,                   
ALL_4_1_12_HYPOPLASTIC_RIGHT_HEART, ALL_4_1_13_AORTIC_VALVE_ATRESIA,               
ALL_4_1_14_MITRAL_VALVE_ATRESIA_STENOSIS, ALL_4_1_15_HYPOPLASTIC_LEFT_HEART,             
ALL_4_1_16_COARCTATION_OF_AORTA, ALL_4_1_17_AORTIC_ATRESIA_INTERRUPTED_ARCH,    
ALL_4_1_18_TOTAL_ANOMALOUS_PULM_VENOUS_RETURN, ALL_4_2_VSD, ALL_4_3_ASD, 
ALL_4_4_PULMONARY_VALVE_STENOSIS, ALL_4_5_PATENT_DUCTUS_ARTERIOSUS, ALL_5_RESPIRATORY,                             
ALL_5_1_CHOANAL_ATRESIA, ALL_5_2_CPAM, ALL_6_ORO_FACIAL_CLEFTS, 
ALL_6_1_CLEFT_LIP, ALL_6_2_CLEFT_PALATE, ALL_7_GASTRO_INTESTINAL, ALL_7_1_OESOPHAGEAL_ATRESIA, 
ALL_7_2_DUODENAL_ATRESIA_STENOSIS,             
ALL_7_3_ATRESIA_STENOSIS, ALL_7_4_ANORECTAL_ATRESIA_STENOSIS ,    
ALL_7_5_HIRSCHSPRUNGS_DISEASE, ALL_7_6_ATRESIA_BILE_DUCTS, ALL_7_7_ANNULAR_PANCREAS,
ALL_7_8_INTESTINAL_FIXATION ,   ALL_7_9_DIAPHRAGMATIC_HERNIA, ALL_8_ABDOMINAL_WALL_DEFECTS ,       
ALL_8_1_GASTROSCHISIS, ALL_8_2_OMPHALOCELE  ,  ALL_9_KIDNEY_AND_URINARY_TRACT , 
ALL_9_1_UNILATERAL_RENAL_AGENESIS,ALL_9_2_BILATERAL_RENAL_AGENESIS,
ALL_9_3_MULTICYSTIC_RENAL_DYSPLASIA,ALL_9_4_CONGENITAL_HYDRONEPHROSIS,
ALL_9_5_HORSESHOE_ECTOPIC_KIDNEY, ALL_9_6_BLADDER_EXSTROPHY, ALL_9_7_POSTERIOR_URETHRAL_VALVES,           ,
ALL_9_8_PRUNE_BELLY, ALL_10_GENITAL, ALL_10_1_HYPOSPADIAS, ALL_10_2_INDETERMINATE_SEX,       
ALL_11_LIMB, ALL_11_1_LIMB_REDUCTION, ALL_11_1_1_TRANSVERSE_LIMB_REDUCTION, 
ALL_11_1_2_PREAXIAL_LIMB_REDUCTION, ALL_11_1_3_POSTAXIAL_LIMB_REDUCTION, 
ALL_11_1_4_CENTRAL_LIMB_REDUCTION, ALL_11_1_5_INTERCALARY_LIMB_REDUCTION, 
ALL_11_2_CLUB_FOOT, ALL_11_3_HIP_DISLOCATION, ALL_11_4_POLYDACTYLY,                         
ALL_11_5_SYNDACTYLY, ALL_12_OTHER_CONDITIONS,                     
ALL_12_1_CRANIOSYNOSTOSIS, ALL_12_2_CONSTRICTION_AMNIOTIC_BAND,           
ALL_12_3_SITUS_INVERSUS, ALL_12_4_CONJOINED_TWINS, ALL_12_5_VATER_VACTERL, 
ALL_12_6_PIERRE_ROBIN_SEQUENCE, ALL_12_7_CAUDAL_REGRESSION_SEQUENCE, ALL_12_8_SIRENOMELIA,                          
ALL_12_9_SEPTO_OPTIC_DYSPLASIA, ALL_12_10_VASCULAR_DISRUPTION,                 
ALL_12_11_LATERALITY_ANOMALIES, ALL_12_12_TERTOGENIC_SYNDROMES,                
ALL_12_12_1_VALPROATE_SYNDROME, ALL_12_12_2_MATERNAL_INFECTIONS,               
ALL_13_GENETIC_CONDITIONS, ALL_13_1_SKELETAL_DYSPLASIAS, ALL_13_1_1_LETHAL_SKELETAL_DYSPLASIAS, 
ALL_13_2_DOWN_SYNDROME, ALL_13_3_PATAU_SYNDROME, ALL_13_4_EDWARDS_SYNDROME, ALL_13_5_TURNER_SYNDROME,
ALL_13_6_TRIPLOIDY_POLYPLOIDY) %>%
  rename(baby_upi = cardriss_baby_upi, mother_upi = cardriss_mother_upi)

all_anomalies <- bind_rows(sliccd_singletons, cardriss_singletons)

slipbd_babies <- slipbd1 %>% select(pregnancy_id, baby_upi, nrs_triplicate_id,date_end_pregnancy)

##join on nrs id
df_b <- left_join(slipbd_babies,sliccd_singletons, by=("nrs_triplicate_id") ) %>%
  filter(ALL_0_ALL_CONDITIONS==1) %>%
  mutate(any_congenital_condition=1) 
df_b <-df_b %>%
  select(pregnancy_id, any_congenital_condition,   ALL_1_NERVOUS_SYSTEM, ALL_2_EYE,ALL_3_EAR_FACE_AND_NECK, 
         ALL_4_CONGENITAL_HEART_DEFECTS, ALL_5_RESPIRATORY, ALL_6_ORO_FACIAL_CLEFTS, 
         ALL_7_GASTRO_INTESTINAL, ALL_8_ABDOMINAL_WALL_DEFECTS, ALL_9_KIDNEY_AND_URINARY_TRACT, 
         ALL_10_GENITAL, ALL_11_LIMB, ALL_12_OTHER_CONDITIONS, ALL_13_GENETIC_CONDITIONS)
  
remaining_babies <-slipbd_babies %>% filter(!nrs_triplicate_id %in% sliccd_singletons$nrs_triplicate_id)
#join on the chi
df_b2 <- left_join(slipbd_babies,cardriss_singletons, by=("baby_upi") ) %>% 
  filter(ALL_0_ALL_CONDITIONS==1)%>%
  mutate(any_congenital_condition=1) %>%
  select(pregnancy_id, any_congenital_condition,   ALL_1_NERVOUS_SYSTEM, ALL_2_EYE,ALL_3_EAR_FACE_AND_NECK, 
            ALL_4_CONGENITAL_HEART_DEFECTS, ALL_5_RESPIRATORY, ALL_6_ORO_FACIAL_CLEFTS, 
           ALL_7_GASTRO_INTESTINAL, ALL_8_ABDOMINAL_WALL_DEFECTS, ALL_9_KIDNEY_AND_URINARY_TRACT, 
           ALL_10_GENITAL, ALL_11_LIMB, ALL_12_OTHER_CONDITIONS, ALL_13_GENETIC_CONDITIONS)

df_b <- rbind(df_b, df_b2)

no_anomalies <- slipbd_babies %>% 
  filter(!(nrs_triplicate_id %in% sliccd_singletons$nrs_triplicate_id) &
           !(baby_upi %in% cardriss_singletons$baby_upi))  %>%
  mutate(any_congenital_condition=0) %>%
  select(pregnancy_id, any_congenital_condition)

df <- bind_rows(no_anomalies, df_b)

saveRDS(df, paste0(folder_data_path, "processed_extracts/linked_congenital_conditions.rds"))

###
sliccd_singletons <- sliccd_singletons %>%
  mutate(est_date_conception = as.Date(date_end_of_pregnancy) - (gestation*7) +14)
unmatched <- sliccd_singletons %>% filter(!nrs_triplicate_id %in% slipbd1$nrs_triplicate_id)


summary(unmatched$est_date_conception)
hist(unmatched$est_date_conception, breaks=100)
