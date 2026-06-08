###Demographics Table  Sensitivty analyses 
## any indicator

###restrict to those with an indication we could find for hdfa####
df_asm <- df  %>% 
  filter(asm_flag=="Yes") 

##exposure numbers####
t_n_exposed <- df_asm %>% group_by(hdfa_preg) %>% count() %>% pivot_wider(names_from = hdfa_preg, values_from = n)
t_n_exposed$var <- "Total"
t_n_exposed$values <- NA
total_cohort <- nrow(df_asm)
t_n_exposed <-t_n_exposed %>%
  ungroup() %>%
  ##row totals for the total exposures
  mutate(perc_exposed = (exposed_hdfa/(exposed_hdfa+ unexposed_hdfa)) *100,
         perc_unexp= ( unexposed_hdfa/(exposed_hdfa+ unexposed_hdfa))*100 ) %>%
  mutate(perc_exposed = case_when(perc_exposed < 1~round_half_up(perc_exposed,2),
                                  perc_exposed >= 1~round_half_up(perc_exposed,1)),
         perc_unexp= case_when(perc_unexp <1 ~ round_half_up(perc_unexp,2), 
                               perc_unexp >=1 ~ round_half_up(perc_unexp,1) )) %>% 
  mutate(Total = exposed_hdfa+ unexposed_hdfa) %>% 
  mutate(`% of Total` = Total/total_cohort*100) %>%
  mutate(`% of Total` = format(round_half_up(`% of Total`,1),nsmall=1)) %>%
  mutate(exposed_hdfa = formatC(exposed_hdfa, big.mark=","),
         unexposed_hdfa = formatC(unexposed_hdfa, big.mark=","), 
         Total = formatC(Total, big.mark=",")) %>% 
  mutate(exposed = paste0(exposed_hdfa, " (", perc_exposed, ")" )) %>%
  mutate(unexposed = paste0(unexposed_hdfa, " (", perc_unexp, ")" )) %>%
  mutate(Total = paste0(Total," (",  `% of Total`, ")" )) %>%
  select(var, values, Total, exposed, unexposed) %>%
  tab_add_heads() 
t_n_exposed$values_by_var <- c(NA, "Total")
#outcome by exposure#
t_n_cancer   <- tab_fun(df_asm, "cancer_outcome", "Cancer outcome")%>%
  tab_add_heads()

##deaths#
t_death  <- tab_fun(df_asm, "death", "Death - at any before study end")%>%
  tab_add_heads()


#comorbidities etc by exposure####
##Child factors#
###baby sex#
t_sex <- tab_fun(df_asm, "baby_sex", "Baby sex")%>%
  tab_add_heads()
###Child congenital condition
t_CC <- tab_fun(df_asm, "any_congenital_condition", "Baby congenital condition")%>%
  tab_add_heads()

##Maternal factors####
###year conception#
t_yr <- tab_fun(df_asm, "year_conception", "Year of conception")%>%
  tab_add_heads()

###Mother age##
df_asm<- df_asm%>% mutate(age_group = factor(age_group, c("<20" , "20-24", "25-29", "30-34", "35-39", "40+" )))
t_age <- tab_fun(df_asm, "age_group", "Maternal age at conception (years)")%>%
  tab_add_heads()
###Maternal simd##
t_simd <- tab_fun(df_asm, "maternal_simd", "Maternal deprivation\u1d43")%>%
  tab_add_heads()
###maternal ethnicity#
ethnic_group_order <- c("African, Scottish African or British African", 
                        "Asian, Scottish Asian or British Asian",
                        "Caribbean or Black", "Mixed or multiple ethnic groups",
                        "White",
                        "Other ethnic group",
                        "Unknown/unclassified")
df_asm<- df_asm%>% 
  mutate(mat_ethnicity_broad_groups_fill = factor(mat_ethnicity_broad_groups_fill,ethnic_group_order ))


t_ethnicity_broad_filled <- tab_fun(df_asm, "mat_ethnicity_broad_groups_fill", "Maternal ethnicity")%>%
  tab_add_heads()
###mother BMI##
t_bmi <- tab_fun(df_asm, "bmi_group", "Maternal BMI at antenatal booking")%>%
  tab_add_heads()

###mother cancer history
t_cancer_history <-tab_fun(df_asm, "cancer_history", "Maternal cancer history")%>%
  tab_add_heads()
###Mother comorbidities and inidcators#
###ASM use###
t_asm1  <-tab_fun(df_asm, "asm_flag", "Maternal ASM use\u1d48")%>%
  tab_add_heads()

###Methotrexate##
t_mtx1 <- tab_fun(df_asm, "mtx_flag", "Maternal methotrexate use")%>%
  tab_add_heads()

###Coeliac##
t_coeliac <- tab_fun(df_asm, "ind_coeliac", "Maternal coeliac disease")%>%
  tab_add_heads()

###obesity
df_asm<- df_asm%>% mutate(obese = case_when(bmi_group=="Obese"~ "Yes", 
                                     bmi_group=="Unknown"~ "Unknown",T~"No" )) %>%
  mutate(obese = factor(obese, c( "No","Yes", "Unknown")))
t_obese  <- tab_fun(df_asm, "obese", "Maternal obesity at antenatal booking")%>%
  tab_add_heads()

###THalessaemia major##
t_thaless  <- tab_fun(df_asm, "ind_thalassaemia", "Maternal thalassaemia major")%>%
  tab_add_heads()

###prexisting diabetes
t_diabetes <-tab_fun(df_asm, "ind_preexist_diabetes", "Maternal pre-pregnancy diabetes")%>%
  tab_add_heads()

###SIckle cell
t_sickle <-tab_fun(df_asm, "ind_sickle_cell", "Maternal sickle cell anaemia") %>%
  tab_add_heads()

##Family history NTD
t_ntd <-tab_fun(df_asm, "ind_ntd", "Family history of NTD\u1d9c") %>%
  tab_add_heads()
##any indicator
df_asm<-df_asm%>% mutate(any_indicator = case_when( obese=="Yes" | ind_ntd=="Yes" | ind_coeliac=="Yes" |
                                               ind_sickle_cell=="Yes" |ind_thalassaemia=="Yes" |
                                               ind_preexist_diabetes=="Yes" |mtx_flag=="Yes"| asm_flag=="Yes" ~"Yes" ,
                                             T~"No"))


##smoking###
t_smoke <-tab_fun(df_asm, "maternal_smoking", "Maternal smoking at antenatal booking")%>%
  tab_add_heads()
###Comorbidity#
####ANY#
t_comorb <-tab_fun(df_asm, "any_comorb", "Maternal comorbidity (excluding indication for hdFA or history of cancer)")%>%
  tab_add_heads()

total_cohort <- (t_n_exposed$exposed_hdfa[2] + t_n_exposed$unexposed_hdfa[2])

##build table####
blank_row <- t_n_exposed %>% slice(1)
indications <- blank_row %>% mutate(values_by_var="Indications for hdFA\u1d47")
t2_baseline_asm <- rbind(t_n_exposed,blank_row ,
                     t_yr,  blank_row, t_age,  blank_row,
                     t_simd,  blank_row, t_ethnicity_broad_filled,  blank_row,
                     blank_row, t_ntd,  blank_row, t_diabetes,  blank_row,t_obese, blank_row ,
                     t_mtx1,  blank_row,
                     t_coeliac,  blank_row,t_sickle,  blank_row,t_thaless,   blank_row,
                     t_cancer_history,  blank_row,
                     t_comorb,   blank_row,
                     t_smoke) %>%
  select(values_by_var, Total,  everything()) 
names(t2_baseline_asm) <- c("Characteristics" , "N (%)",   "N (%)", "N (%)")
