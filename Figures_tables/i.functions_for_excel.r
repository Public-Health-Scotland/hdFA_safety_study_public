
##functions####
tab_add_heads <- function(tab){
  tab %>% group_by(var) %>%
    slice(1)  %>% 
    ungroup() %>%
    mutate(across(-var, ~ .[NA])) %>%
    bind_rows(tab) %>% 
    arrange(var) %>%
    mutate(values_by_var = case_when(is.na(values)~ var, 
                                     T~ paste0("  ", values))) %>%
    select(values_by_var, exposed_hdfa, perc_exposed, unexposed_hdfa, perc_unexp)
}
convert_YN <- function(x){case_when(x==1 ~ "Yes", 
                                    x==0 ~ "No", 
                                    is.na(x)~ "No", 
                                    T~ as.character(x))}

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
    mutate(perc_exposed = exposed_hdfa/total_ex *100,
           perc_unexp= unexposed_hdfa/total_un*100) %>%
    mutate(perc_exposed = format(round_half_up(perc_exposed,1),nsmall=1),
           perc_unexp= format(round_half_up(perc_unexp,1),nsmall=1)) %>%
    mutate(perc_unexp = case_when(perc_unexp %in% c("  0.0"," 0.0","0.0" ) & unexposed_hdfa!=0 ~ "<0.1",
                                  perc_unexp == "100.0" & unexposed_hdfa!=total_un ~ ">99.9",
                                  T~as.character(perc_unexp)),
           perc_exposed = case_when(perc_exposed %in% c("  0.0"," 0.0","0.0" ) & exposed_hdfa!=0 ~ "<0.1",
                                    perc_exposed  == "100.0"  & exposed_hdfa!=total_ex~ ">99.9",
                                    T~as.character(perc_exposed))) %>%
    select(-total_ex, -total_un) 
  
}

#### 