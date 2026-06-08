##import lookup for eurocat definitions
CC_codelist <- read_excel(paste0(folder_data_path,"lookups/2025-10-28_congenitalconditionsinscotland2023_codelist_final.xlsx"), 
                          sheet = "EUROCAT ICD10-BPA Code List", 
                          range = "A4:E109") %>% clean_names()
CC_codelist <-CC_codelist %>% filter(eurocat_congenital_condition_subgroup==eurocat_congenital_condition_group) %>%
  select(-eurocat_congenital_condition_subgroup, -eurocat_specific_congenital_condition)


cc_tab_fun <- function(df,x, varname){
  var <- sym(x)
  df %>%
    group_by(!!var, hdfa_preg) %>% count()%>%
    pivot_wider(names_from = hdfa_preg, values_from=n) %>%
    mutate(var=varname)%>%
    rename(values =!!var) %>%
    ungroup() %>% 
    mutate(exposed_hdfa = replace_na(exposed_hdfa,0)) %>%
    mutate(total_ex = sum(exposed_hdfa, na.rm=T), total_un = sum(unexposed_hdfa)) %>%
    mutate(perc_exposed = round(exposed_hdfa/total_ex *100,2),
           perc_unexp= round(unexposed_hdfa/total_un*100,2)) %>%
    filter(values=="Yes"| values==1) %>%
    select(var, exposed_hdfa, perc_exposed, unexposed_hdfa , perc_unexp)
  
}
t_anycc <- cc_tab_fun(df, "any_congenital_condition", varname = "Any major congenital conditions")
t_1 <- cc_tab_fun(df, "ALL_1_NERVOUS_SYSTEM", varname = "Nervous system conditions")
t_2 <- cc_tab_fun(df, "ALL_2_EYE", varname = "Eye conditions")
t_3 <- cc_tab_fun(df, "ALL_3_EAR_FACE_AND_NECK", varname = "Ear, face, and neck conditions")
t_4 <- cc_tab_fun(df, "ALL_4_CONGENITAL_HEART_DEFECTS", varname = "Congenital heart conditions")
t_5 <- cc_tab_fun(df, "ALL_5_RESPIRATORY", varname = "Respiratory conditions")
t_6 <- cc_tab_fun(df, "ALL_6_ORO_FACIAL_CLEFTS", varname = "Oro-facial clefts")
t_7 <- cc_tab_fun(df, "ALL_7_GASTRO_INTESTINAL", varname = "Gastro-intestinal conditions")
t_8 <- cc_tab_fun(df, "ALL_8_ABDOMINAL_WALL_DEFECTS", varname = "Abdominal wall defects")
t_9 <- cc_tab_fun(df, "ALL_9_KIDNEY_AND_URINARY_TRACT", varname = "Kidney and urinary tract conditions")
t_10 <- cc_tab_fun(df, "ALL_10_GENITAL", varname = "Genital conditions")
t_11 <- cc_tab_fun(df, "ALL_11_LIMB", varname = "Limb conditions")
t_12 <- cc_tab_fun(df, "ALL_12_OTHER_CONDITIONS", varname = "Other conditions")
t_13 <- cc_tab_fun(df, "ALL_13_GENETIC_CONDITIONS", varname = "Genetic conditions")

t_ccs <- rbind(t_1, t_2,t_3,
               t_4, t_5, t_6, t_7, t_8, t_9, 
               t_10, t_11, t_12, t_13,t_anycc)
#names(CC_codelist)
CC_codelist$eurocat_congenital_condition_group %in% t_ccs$var
CC_codelist <- CC_codelist %>% 
  mutate(eurocat_congenital_condition_group 
         = case_when(eurocat_congenital_condition_group== 
                       "All conditions"~ "Any major congenital conditions", 
                     eurocat_congenital_condition_group==
                       "Congenital heart disease (CHD)"  ~ "Congenital heart conditions",
                     eurocat_congenital_condition_group==
                       "Other conditions/syndromes"~ "Other conditions",
                     eurocat_congenital_condition_group== 
                       "Ear, face and neck conditions" ~  "Ear, face, and neck conditions",
                     T~eurocat_congenital_condition_group))
t_ccs <- left_join(CC_codelist, t_ccs, by = c("eurocat_congenital_condition_group"="var"))
names(t_ccs) <- c("Eurocat congenital condition group", "ICD10-BPA codes",
                  "Exclusions", "N", "%", "N", "%")
