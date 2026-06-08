total_cohort <- nrow(df)
###Additional characteristics####
tab_add_heads <- function(tab){
  tab %>% group_by(var) %>%
  slice(1)  %>% 
  ungroup() %>%
  mutate(across(-var, ~ .[NA])) %>%
  bind_rows(tab) %>% 
  arrange(var) %>%
  mutate(values_by_var = case_when(is.na(values)~ var, 
                                   T~ paste0("  ", values))) %>%
  select(values_by_var, Total, exposed,  unexposed)
}
##table function
tab_fun <- function(df,x, varname){

  var <- sym(x)
df %>%
    group_by(!!var, hdfa_preg) %>% count()%>%
    pivot_wider(names_from = hdfa_preg, values_from=n) %>%
    mutate(var=varname)%>%
    rename(values =!!var) %>%
    ungroup() %>% 
    mutate(exposed_hdfa = replace_na(exposed_hdfa,0), unexposed_hdfa = replace_na(unexposed_hdfa, 0)) %>%
    mutate(total_ex = sum(exposed_hdfa, na.rm=T), total_un = sum(unexposed_hdfa)) %>% 
  mutate(Total = exposed_hdfa+ unexposed_hdfa) %>%
  mutate(`% of Total` = Total/total_cohort*100) %>%
  mutate(`% of Total` = str_trim(format(round_half_up(`% of Total`,1),nsmall=1)))  %>%
    mutate(perc_exposed = exposed_hdfa/total_ex *100,
           perc_unexp= unexposed_hdfa/total_un*100) %>%
    mutate(perc_exposed = str_trim(format(round_half_up(perc_exposed,1),nsmall=1)),
           perc_unexp= str_trim(format(round_half_up(perc_unexp,1),nsmall=1)) )%>%
    mutate(perc_unexp = case_when(perc_unexp %in% c("  0.0"," 0.0","0.0" ) & unexposed_hdfa!=0 ~ "<0.1",
                                  perc_unexp == "100.0" & unexposed_hdfa!=total_un ~ ">99.9",
                                  T~as.character(perc_unexp)),
           perc_exposed = case_when(perc_exposed %in% c("  0.0"," 0.0","0.0" ) & exposed_hdfa!=0 ~ "<0.1",
                                    perc_exposed  == "100.0"  & exposed_hdfa!=total_ex~ ">99.9",
                                    T~as.character(perc_exposed))) %>%
  mutate(`% of Total` = case_when(`% of Total`=="   NA"~ "", 
                                  `% of Total`%in% c("0.0", " 0.0", "  0.0") & Total !=0 ~"<0.1",
                                  T~`% of Total`) )%>%
  mutate(`% of Total` = case_when(`% of Total`=="   NA"~ "", 
                                  `% of Total`%in% c("100.0", " 100.0", "  100.0") & Total !=0 ~">99.9",
                                  T~`% of Total`) )%>%
  mutate(exposed_hdfa = formatC(exposed_hdfa, big.mark=","),
         unexposed_hdfa = formatC(unexposed_hdfa, big.mark=","), 
         Total = formatC(Total, big.mark=",")) %>%
      select(-total_ex, -total_un) %>%
  mutate(exposed = paste0(exposed_hdfa, " (", perc_exposed, ")" )) %>%
  mutate(unexposed = paste0(unexposed_hdfa, " (", perc_unexp, ")" )) %>%
  mutate(Total = paste0(Total," (",  `% of Total`, ")" )) %>%
  select(var, values, Total, exposed, unexposed)
  
}

##exposure numbers####
t_n_exposed <- df %>% group_by(hdfa_preg) %>% count() %>% pivot_wider(names_from = hdfa_preg, values_from = n)
t_n_exposed$var <- "Total"
t_n_exposed$values <- NA
t_n_exposed <- t_n_exposed %>%
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
##imputed gestation
##emigration end follow
##deaths only where it is the censor point
df<- df %>%
  mutate(dth_censor = case_when(end_follow_type=="censor - death" ~"yes" , T~"no"))

t_dth_censor <- tab_fun(df, "dth_censor", "Due to death")%>%
  tab_add_heads()

##censor emigration
df <- df %>% 
  mutate(emigration = case_when(end_follow_type=="censor - emigration" ~"yes" , T~"no"))

t_emigrate <- tab_fun(df, "emigration", "Due to emigration")%>%
  tab_add_heads()

## N study end date
df <- df %>% 
  mutate(censor_study_end = case_when(end_follow_type=="end study" ~"yes" , T~"no"))

t_endstudy <- tab_fun(df, "censor_study_end", "At study end date (31 December 2023)")%>%
  tab_add_heads()

##imputed gestatoin
df <- df %>% mutate(imputed = case_when(gestation_ascertainment=="Gestation imputed based on outcome of pregnancy" ~"Yes", 
                                        T~"No"))
t_impute <- tab_fun(df, "imputed", "Imputed gestation at birth")%>%
  tab_add_heads()
###Birthweight groups#
df <- df %>% 
  mutate(bw_group=
           factor(bw_group,
                  c("<1500", "1500-2499",  "2500-3999", 
                    ">4000", "Unknown"))) 

t_bw <- tab_fun(df, "bw_group", "birthweight")%>%
  tab_add_heads()
###sex
t_sex <- tab_fun(df, "baby_sex", "Sex")%>%
  tab_add_heads()
###Child congenital condition
t_CC <- tab_fun(df, "any_congenital_condition", "Major congenital condition")%>%
  tab_add_heads()

df <- df %>% 
  mutate(bw_group=
           factor(bw_group,
                  c("<1500", "1500-2499",  "2500-3999", 
                    ">4000", "Unknown"))) 
t_bw <- tab_fun(df, "bw_group", "Birthweight (grams)")%>%
  tab_add_heads()

blank_row <- t_n_exposed %>% slice(1)
uncertain <- blank_row %>% 
  mutate(values_by_var="Uncertain exposure period hence uncertain EDC\u1d43 and exposure period/timing")


compete <- blank_row %>% mutate(values_by_var="Competing exposure")
mediate <- blank_row %>% mutate(values_by_var="Mediators")

table_addchar <- rbind(t_n_exposed,
                       blank_row,
                       uncertain,
                       t_impute,
                       blank_row,
                                      compete,
                       t_sex,blank_row, mediate, t_bw,blank_row, t_CC) %>%

   rename(`Characteristics` = values_by_var, `High dose folic acid treatment` = exposed, 
         `No high dose folic acid treatment` = unexposed
        )
names(table_addchar) <- c("Characteristics" , "N (%)",  "N (%)", "N (%)")
